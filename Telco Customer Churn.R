#Installing the necessary packages
install.packages("readr")
install.packages("dplyr", version = "1.0.7")
install.packages("ggplot2")

library(dplyr)
library(readr)
#to load the dataset
Telco_Customer_Churn <- read_csv("elco-Customer-Churn.csv")

#To view the data
head(Telco_Customer_Churn)
str(Telco_Customer_Churn)

# Check for missing values in each column
colSums(is.na(Telco_Customer_Churn))

# Check the total number of missing values
sum(is.na(Telco_Customer_Churn))

# Remove rows with missing values in TotalCharges column
Telco_Customer_Churn <- na.omit(Telco_Customer_Churn)

# Check for duplicates
dups <- duplicated(Telco_Customer_Churn)
# Remove duplicates
Telco_Customer_Churn <- Telco_Customer_Churn[!dups, ]

# Generate summary statistics for numerical variables
summary(Telco_Customer_Churn)

library(ggplot2)
# Calculate the percentages
churn_counts$Percent <- round(churn_counts$Count/sum(churn_counts$Count) * 100, 1)

# Create a bar chart of the counts with percentages
ggplot(churn_counts, aes(x = Churn, y = Count, fill = Churn)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5)) +
  scale_x_discrete(labels = c("Not Churned", "Churned")) +
  labs(x = "", y = "Count", title = "Count of Customers by Churn Status")

####26.6% of customer churned.

# Change column name from "tenure(in months)" to "tenure"
names(Telco_Customer_Churn)[names(Telco_Customer_Churn) == "tenure(in months)"] <- "tenure"

# Verify the column name change
colnames(Telco_Customer_Churn)


# Create a density plot of MonthlyCharges by Churn
library(ggplot2)
ggplot(Telco_Customer_Churn, aes(x = MonthlyCharges, fill = Churn)) +
  geom_density(alpha = 0.5) +
  labs(x = "Monthly Charges", y = "Density", title = "Density Plot of Monthly Charges by Churn") +
  scale_fill_discrete(name = "Churn", labels = c("Not Churned", "Churned"))

#to visualize the disttibution of total charges by churn
library(ggplot2)

# create a new data frame with TotalCharges and Churn columns
total_charges_by_churn <- Telco_Customer_Churn[, c("TotalCharges", "Churn")]

# create a density plot with separate density curves for each churn group
ggplot(total_charges_by_churn, aes(x = TotalCharges, fill = Churn)) +
  geom_density(alpha = 0.5) +
  labs(x = "Total Charges", y = "Density", title = "Distribution of Total Charges by Churn")

#to visualize the distribution of tenure by churn we use a histogram
ggplot(Telco_Customer_Churn, aes(x = as.numeric(tenure))) +
  geom_histogram(binwidth = 5, color = "black", fill = "blue") +
  labs(x = "Tenure (months)", y = "Count", title = "Distribution of Tenure")

#to visualize distribution of tenure by contract type
ggplot(Telco_Customer_Churn, aes(x = tenure, fill = Contract)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge") +
  facet_wrap(~ Contract, ncol = 3) +
  labs(x = "Tenure (months)", y = "Count", title = "Distribution of Tenure by Contract Type")

###to visualize the effect of monthly charge ,total charges and tenure on
###whether the customer churned or not
library(ggplot2)
library(gridExtra)

# Create individual plots
plot1 <- ggplot(Telco_Customer_Churn, aes(x=tenure, y=MonthlyCharges, color=Churned)) +
  geom_point(size = .5) +
  labs(x="Tenure", y="Monthly Charges", color="Churned") +
  ggtitle("Monthly Charges by Tenure and Churned")

plot2 <- ggplot(Telco_Customer_Churn, aes(x=tenure, y=TotalCharges, color=Churned)) +
  geom_point(size = .5) +
  labs(x="Tenure", y="Total Charges", color="Churned") +
  ggtitle("Total Charges by Tenure and Churned")

plot3 <- ggplot(Telco_Customer_Churn, aes(x=TotalCharges, y=MonthlyCharges, color=Churned)) +
  geom_point(size = .5) +
  labs(x="Total Charges", y="Monthly Charges", color="Churned") +
  ggtitle("Total Charges vs Monthly Charges")

# Combine plots into a multi-panel plot
grid.arrange(plot1, plot2, plot3, ncol=3)

names(Telco_Customer_Churn)[names(Telco_Customer_Churn) == "Senior Citizen"] <- "SeniorCitizen"

#to plot for each for each of the demographics ie gender, senior citizen,dependents or partner against churn
library(ggplot2)

# calculate the percentages of churned and non-churned customers by gender
percentages <- Telco_Customer_Churn %>%
  group_by(gender, Churned) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100)

# plot for gender vs churn with percentages
ggplot(Telco_Customer_Churn, aes(x=gender, fill=Churned)) +
  geom_bar(position = position_dodge(0.9)) +
  geom_text(data = percentages, aes(x = gender, y = count, label = paste0(round(percent, 1),"%")),
            position = position_dodge(0.9), vjust = -0.5, color = "black", size = 3) +
  labs(x="Gender", y="Count", fill="Churned") +
  ggtitle("Gender vs Churned") 

# calculate the percentages of churned and non-churned customers by dependents
percentages <- Telco_Customer_Churn %>%
  group_by(Dependents, Churned) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100)

# plot for dependents vs churn with percentages
ggplot(Telco_Customer_Churn, aes(x=Dependents, fill=Churned)) +
  geom_bar(position = position_dodge(0.9)) +
  geom_text(data = percentages, aes(x = Dependents, y = count, label = paste0(round(percent, 1),"%")),
            position = position_dodge(0.9), vjust = -0.5, color = "black", size = 3) +
  labs(x="Dependents", y="Count", fill="Churned") +
  ggtitle("Dependents vs Churned")


# calculate the percentages of churned and non-churned customers by partner status
percentages <- Telco_Customer_Churn %>%
  group_by(Partner, Churned) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100)

# plot for partner vs churn with percentages
ggplot(Telco_Customer_Churn, aes(x=Partner, fill=Churned)) +
  geom_bar(position = position_dodge(0.9)) +
  geom_text(data = percentages, aes(x = Partner, y = count, label = paste0(round(percent,1),"%")),
            position = position_dodge(0.9),vjust = -0.5, color = "black", size = 3) +
  labs(x="Partner", y="Count", fill="Churned") +
  ggtitle("Partner vs Churned")


#calculate the percentages of churned and non-churned customers by senior citizen status

percentages <- Telco_Customer_Churn %>%
  group_by(SeniorCitizen, Churned) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100)

#plot for senior citizen vs churn with percentages
ggplot(Telco_Customer_Churn, aes(x=SeniorCitizen, fill=Churned)) +
  geom_bar(position = position_dodge(0.9)) +
  geom_text(data = percentages, aes(x = SeniorCitizen, y = count, label = paste0(round(percent, 1),"%")),
            position = position_dodge(0.9), vjust = -0.5, color = "black", size = 3) +
  labs(x="Senior Citizen", y="Count", fill="Churned") +
  ggtitle("Senior Citizen vs Churned")


### For each product service we create a bar plot to visualize the relatioship between each variable and churn
library(ggplot2)

# PhoneService
ggplot(Telco_Customer_Churn, aes(x = PhoneService, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Phone Service", x = "Phone Service", y = "Count") +
  scale_fill_manual(values = c("#009E73", "#D55E00"), name = "Churn")


## MultipleLines
ggplot(Telco_Customer_Churn, aes(x = MultipleLines, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Multiple Lines", x = "Multiple Lines", y = "Count") +
  scale_fill_manual(values = c("#009E73", "#D55E00"), name = "Churn")

# InternetService
ggplot(Telco_Customer_Churn, aes(x = InternetService, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Internet Service", x = "Internet Service", y = "Count") +
  scale_fill_manual(values = c("#009E73", "#D55E00"), name = "Churn")

# StreamingTV
ggplot(Telco_Customer_Churn, aes(x = StreamingTV, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Streaming TV", x = "Streaming TV", y = "Count") +
  scale_fill_manual(values = c("#009E73", "#D55E00"), name = "Churn")

# StreamingMovies
ggplot(Telco_Customer_Churn, aes(x = StreamingMovies, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Streaming Movies", x = "Streaming Movies", y = "Count") +
  scale_fill_manual(values = c("#009E73", "#D55E00"), name = "Churn")

###For each service that Telco provides, we visaulaize the effects it has on churn

# Define color palette
my_colors <- c("#3182bd", "#9ecae1")

# OnlineSecurity
ggplot(Telco_Customer_Churn, aes(x = OnlineSecurity, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Online Security", x = "Online Security", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

# OnlineBackup
ggplot(Telco_Customer_Churn, aes(x = OnlineBackup, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Online Backup", x = "Online Backup", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

# DeviceProtection
ggplot(Telco_Customer_Churn, aes(x = DeviceProtection, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Device Protection", x = "Device Protection", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

# TechSupport
ggplot(Telco_Customer_Churn, aes(x = TechSupport, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Tech Support", x = "Tech Support", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

###to visualize the effect of payment methods, paperless billing and contract type
my_colors <- c("#7CFC00", "#006400")

# Contract
ggplot(Telco_Customer_Churn, aes(x = Contract, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Contract Type", x = "Contract Type", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

# Paperless Billing
ggplot(Telco_Customer_Churn, aes(x = PaperlessBilling, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Paperless Billing", x = "Paperless Billing", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn")

# Payment Method
ggplot(Telco_Customer_Churn, aes(x = PaymentMethod, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn by Payment Method", x = "Payment Method", y = "Count") +
  scale_fill_manual(values = my_colors, name = "Churn") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####STATISTICAL ANALYSIS
#to perform chisquare tests of independence on ecah of the variables

#Gender
# Create a contingency table of Gender vs Churn
gender_churn_table <- table(Telco_Customer_Churn$gender, Telco_Customer_Churn$Churn)

# Perform a chi-squared test of independence
chisq.test(gender_churn_table)

#Partnered
# Conduct chi-square test of independence for Partnered against Churn
chisq.test(Telco_Customer_Churn$Partner, Telco_Customer_Churn$Churn)

#Senior Citizen
senior_churn <- table(Telco_Customer_Churn$SeniorCitizen, Telco_Customer_Churn$Churn)
chisq.test(senior_churn)


