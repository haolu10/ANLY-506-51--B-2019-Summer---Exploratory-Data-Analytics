#Week 3 Code: Exploratory data analysis

library(readr) #Readr package
ozone <- read_csv("/Users/haolu/Desktop/ANLY506/US EPA data 2017.csv", 
                  col_types = "ccccinnccccccncnncccccc") #define ozone

names(ozone) <- make.names(names(ozone)) # remove spaces

nrow(ozone) #number of rows
ncol(ozone) #number of columns

str(ozone) #show structure of ozone data

head(ozone[, c(6:7, 10)]) #look at top of the data
tail(ozone[, c(6:7, 10)]) #look at bottom of the data

table(ozone$Time.Local) #look at number of observations

library(dplyr) #load dplyr package
filter(ozone, Time.Local == "13:14") %>% 
 select(State.Name, County.Name, Date.Local, 
 Time.Local, Sample.Measurement) #filter data

filter(ozone, State.Code == "36" 
 & County.Code == "033" 
 & Date.Local == "2014-09-30") %>%
 select(Date.Local, Time.Local, 
 Sample.Measurement) %>% 
 as.data.frame #Multiple filters

select(ozone, State.Name) %>% unique %>% nrow #how many states are represented in this dataset

unique(ozone$State.Name) #Unique elements of the State.Name variable

summary(ozone$Sample.Measurement) #Hourly measurements of ozone

quantile(ozone$Sample.Measurement, seq(0, 1, 0.1)) #Calculate quantile

ranking <- group_by(ozone, State.Name, County.Name) %>%
 summarize(ozone = mean(Sample.Measurement)) %>%
 as.data.frame %>%
 arrange(desc(ozone)) #Ranking in a descending order

head(ranking, 10) #display top 10 of the rank
tail(ranking, 10) #diaplay bottom 10 of the rank

filter(ozone, State.Name == "California" & County.Name == "Mariposa") %>% nrow #how many observations in the higest level counties, Mariposa County, California

ozone <- mutate(ozone, Date.Local = as.Date(Date.Local)) #monthly average]

filter(ozone, State.Name == "California" & County.Name == "Mariposa") %>%
 mutate(month = factor(months(Date.Local), levels = month.name)) %>%
 group_by(month) %>%
 summarize(ozone = mean(Sample.Measurement)) #average hourly levels by month

filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>% nrow #Number of observations in  the lowest level counties, Caddo County, Oklahoma

filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>%
 mutate(month = factor(months(Date.Local), levels = month.name)) %>%
 group_by(month) %>%
 summarize(ozone = mean(Sample.Measurement)) #More filters with the data

set.seed(10234) #number of sample
N <- nrow(ozone)
idx <- sample(N, N, replace = TRUE)
ozone2 <- ozone[idx, ] #simulation

ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
 summarize(ozone = mean(Sample.Measurement)) %>%
 as.data.frame %>%
 arrange(desc(ozone)) #reconstruct ranking

cbind(head(ranking, 10), head(ranking2, 10)) #Compare actuals to simulation results for top 10
cbind(tail(ranking, 10), tail(ranking2, 10)) #Compare actuals to simulation results for bottom 10



