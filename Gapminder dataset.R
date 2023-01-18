
#Using te gapminder dataset and library 

#to select the year column
library(gapminder)
library(dplyr)
gapminder %>% select(year)

# to count the number of observations for each year and addthe number of gdpper capita while sorting fron lowestb to highest
library(gapminder)
library(dplyr)
gapminder %>% count(year , wt=gdpPercap , sort= TRUE)

#to filter out only the years in 2007
gapminder%>%filter(year==2007) 

#to filter out only the dta for the United States
gapminder%>%filter(country=='United States')

#to filter oot only the data for United states in 2007
gapminder%>%filter(country=='United States',year==2007)

#to arrange the dataset by the gdppercentage
gapminder%>%arrange(gdpPercap)

#to arrange the dataset by the gdppercentage
gapminder%>%arrange(gdpPercap)

#to filter for the year 2007 and arrange the dataset by gdpPercapita
gapminder%>% filter(year==2007)%>%arrange(gdpPercap)

# Filter for the year 1957, then arrange in descending order of population
gapminder%>%filter(year==1957)%>%arrange(desc(pop))

#using the mutate verb,you can change a column's content
#to divide the population column by 1000000
gapminder%>%mutate(pop=pop/1000000)

#using the mutate verb you can add a new column based on existing columns
#to add a gdp column which is gotten by multiplying the population by the gdgpercap
gapminder%>%mutate(gdp=pop*gdpPercap)

#to filter out the highest gdp in the year 2007
gapminder %>%  mutate(gdp = gdpPercap * pop)%>%  filter(year ==2007)%>%  arrange(desc(gdp))

#to create a new dataframe containing onlt data from the year 2007
gapminder2007<-gapminder %>% filter(year==2007)
gapminder2007

library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# to create a scatter point of gdpper cap against leife xpentancy
ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

#to create a logathrmic scale of the x axis, just add scale_x_log10()
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp))+  geom_point()+  scale_x_log10()

#to create a scale having different continents on different colors and different population on different sizes
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+  geom_point()+  scale_x_log10()

#Create a scatter plot of gapminder_1952 with the x-axis 
#representing population (pop), the y-axis representing life expectancy (lifeExp),
# and faceted to have one subplot per continent (continent). Put the x-axis on a log scale.
library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952,aes(x=pop,y=lifeExp)) + geom_point() + scale_x_log10() + facet_wrap(~continent)

#to create a scatterplot of gapminder data
library(gapminder)
library(dplyr)
library(ggplot2)

# Scatter plot comparing gdpPercap and lifeExp, with color representing 
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=continent))+ geom_point() + scale_x_log10() + facet_wrap(~year)

#to find the mean,max,median,min vaues use the summarize functiion
#to find the median life expentancy and maximum gdp per capita in the year 1957
#filterthe year for 1957 and the summarize using the median and max functions#
library(gapminder)
library(dplyr)

gapminder %>% filter(year==1957) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))

#using the group by function
#to find th max gdppercap and median life expentancy for each year
library(gapminder)
library(dplyr)

gapminder %>% group_by(year) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))

#  to find the median life expectancy and maximum GDP per capita in each continent in 1957
library(gapminder)
library(dplyr)
gapminder %>% filter(year==1957) %>% group_by(continent) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))

# to find median life expectancy and maximum GDP per capita in each continent/year combination
library(gapminder)
library(dplyr)

gapminder %>% group_by(continent,year) %>% summarize(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))

# to create a scatter plot showing the change in medianLifeExp over time
#first create datframe by_year then graph it
library(gapminder)
library(dplyr)
library(ggplot2)

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

ggplot(by_year,aes(x=year,y=medianLifeExp)) + geom_point() + expand_limits(y = 0)

#to plot the median gdppercap grouped by year and continent
library(gapminder)
library(dplyr)
library(ggplot2)

by_year_continent<-gapminder %>% group_by(continent,year) %>% summarize(medianGdpPercap=median(gdpPercap))

ggplot(by_year_continent,aes(x=year,y=medianGdpPercap),color=continent) +geom_point() + expand_limits(y=0)

# Create a line plot showing the change in medianGdpPercap over time
library(gapminder)
library(dplyr)
library(ggplot2)

by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPercap=median(gdpPercap))

ggplot(by_year,aes(x=year,y=medianGdpPercap)) + geom_line() + expand_limits(y=0)

#to create a line plot showing the change in medianGdpPercap over time by continent
library(gapminder)
library(dplyr)
library(ggplot2)

by_year_continent <- gapminder %>% group_by(year,continent) %>% summarize(medianGdpPercap=median(gdpPercap))
ggplot(by_year_continent,aes(x=year,y=medianGdpPercap,color=continent)) + geom_line() + expand_limits(y=0)

# to Create a bar plot showing medianGdp by continent
library(gapminder)
library(dplyr)
library(ggplot2)

by_continent <- gapminder %>% filter(year==1952) %>% group_by(continent) %>% summarize(medianGdpPercap=median(gdpPercap))
ggplot(by_continent,aes(x=continent,y=medianGdpPercap)) + geom_col()

#to create a histogram of population (pop_by_mil)b bin size 50
library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

ggplot(gapminder_1952, aes(x = pop_by_mil)) +
  geom_histogram(bins = 50)

#  to Create a boxplot comparing gdpPercap among continents and adda title
library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

ggplot(gapminder_1952,aes(x=continent,y=gdpPercap)) + geom_boxplot() + scale_y_log10() + ggtitle('Comparing GDP per capita across continents')