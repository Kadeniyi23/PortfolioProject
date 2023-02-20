library(data.table)
library(ggplot2)
library(tidyr)
library(readr)

#to load the csv file
storedata <- read.csv(""C:/Users/HP ELITEBOOK 840 G2/Downloads/QVI_data.csv"")

###The client wants to select control store that are similar to trial stores in terms 
###of being operational during the whole trial period
###We would want to match trial stores to control stores that are similar to the trial
###store prior to the trial period of Feb 2019 in terms of :
###- Monthly overall sales revenue
###- Monthly number of customers
###- Monthly number of transactions per customer


#Add a new month ID column in the data with the format yyyymm.
library(data.table)
setDT(storedata)
storedata[, YEARMONTH := year(DATE)*100 + month(DATE)]


# Calculate measures over time for each store
measureOverTime <- storedata[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
  nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
  avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)
), by = c("STORE_NBR", "YEARMONTH")]


#### Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in%
                                      storesWithFullObs, ]

# to calculate correlation for a measure, looping through each control store
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure =
                               numeric())
  
  # Get unique store numbers from inputTable
  storeNumbers <- unique(inputTable$STORE_NBR)
  
  for (i in storeNumbers) {
    if (i == storeComparison) {
      next
    }
    
    corr_measure <- cor(inputTable[STORE_NBR == storeComparison, get(metricCol)], 
                        inputTable[STORE_NBR == i, get(metricCol)], use = "complete.obs")
    
    calcCorrTable <- rbind(calcCorrTable, data.table(Store1 = storeComparison, Store2 = i, corr_measure = corr_measure))
  }
  
  return(calcCorrTable)
}

#to calculate a standardised magnitude distance for a measure, looping through each control store
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
                               numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison
                                   , "Store2" = i
                                   , "YEARMONTH" = inputTable[STORE_NBR ==
                                                                storeComparison, YEARMONTH]
                                   , "measure" = abs(inputTable[STORE_NBR ==
                                                                  storeComparison, eval(metricCol)]
                                                     - inputTable[STORE_NBR == i,
                                                                  eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  
  #### Standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                                .(Store1, Store2)]
  return(finalDistTable)
}



#to calculate the metrics
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#to calculate magnitude.
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales),
                                               trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,
                                                   quote(nCustomers), trial_store)

#to Create a combined score composed of correlation and magnitude, by first merging the correlations table with the magnitude table.

corr_weight <- 0.5

# Merge correlation and magnitude tables for nSales
combined_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))
combined_nSales[, score_nSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

# Merge correlation and magnitude tables for nCustomers
combined_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))
combined_nCustomers[, score_nCustomers := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

#Merge the the combine_nSales and the combined_nCustomers table to get the score control
score_Control <- merge(combined_nSales, combined_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := score_nSales * 0.5 + score_nCustomers * 0.5]

#selecting the control stores based on the highest matching store (closest to 1 but not the store itself, i.e. the second ranked highest store)
control_store <- 233
control_store
### The control Store is store 233

#To check visually the total sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==
                                                           trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
                                       "Store_type")
][, TransactionMonth := as.Date(paste0(substr(as.character(YEARMONTH), 1, 4), "-", substr(as.character(YEARMONTH), 5, 6), "-01"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#to visually check for number of customers
measureOverTimeCusts <- measureOverTime

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))]
pastCustomers[, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")]
pastCustomers[, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")]

pastCustomers <- pastCustomers[YEARMONTH < 201903,]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

### Comparison of results during trial
# Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
                                                   YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
                                                                                                         YEARMONTH < 201902, sum(totSales)]
# Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales",
                                                                    "YEARMONTH")],
                        by = "YEARMONTH"
)[, percentageDiff :=
    abs(controlSales-totSales)/controlSales]
#### As our null hypothesis is that the trial period is the same as the pre-trial
###period, let's take the standard deviation based on the scaled percentage difference
###in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 

#Since there are 8 months in the trial period
degreesOfFreedom <- 7 
# Calculate the t-values for the trial months
percentageDiff[, tValue := (percentageDiff - 0) / stdDev]

# Find the 95th percentile of the t distribution with the appropriate degrees of freedom
tValueCutoff <- qt(0.95, degreesOfFreedom)

###the tValueCutoff is 1.894

###We can observe that the t-value is much larger than the 95th percentile value of
###the t-distribution for March and April - i.e. the increase in sales in the trial
###store in March and April is statistically greater than in the control store.


#To plot a graph showing the sales of the control store and create a visual assessment of the trial

# Trial and control store total sales
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,"Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
][Store_type %in% c("Trial", "Control"), ]

# Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]

# Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

#Plotting the graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 ,
                ymax = Inf, color = NULL), show.legend = FALSE) +
geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by  month")

###The results show that the trial in store 77 is significantly different to its control store in the trial period as
###the trial store performance lies outside the 5% to 95% confidence interval of the control store in two of the
###three trial months.


#Assessing for number of customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store &
                                                  YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR ==
                                                                                                          control_store & YEARMONTH < 201902, sum(nCustomers)]
#Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][ , controlCustomers := nCustomers * scalingFactorForControlCust
][, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store,"Control", "Other stores"))
]
#### Calculate the percentage difference between scaled control sales and trial sales 
 percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],                        measureOverTimeCusts[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH"
)[, percentageDiff := abs(controlCustomers-nCustomers)/controlCustomers]

#### As our null hypothesis is that the trial period is the same as the
###pre-trial period, let's take the standard deviation based on the scaled
###percentage difference in the pre‐trial period

 
 
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

# Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

# Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

# Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",][, nCusts := nCusts * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

# Plotting a graph for this 
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 ,
ymax = Inf, color = NULL), show.legend = FALSE) +
geom_line() +
labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")
