#Week 5 Code: Tidy Data

#load tidyverse package
library(tidyverse)

#run tables
table1
table2
table3
table4a
table4b

# use mutate function to compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Count cases per year
table1 %>% 
  count(year, wt = cases)

# Visualise changes over time with ggplot
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#use gather function to group variables together
table4a
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

#spread function - opposite of gathering
table2
table2 %>%
  spread(key = type, value = count)

#exercise
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)

#use seperate function to seperate a column (rate was calculated before)
table3
table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/") #different method

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE) #different method

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) #seperate into century and year

table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "")

#exercise
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))

#Missing value - create a dataframe with missing value
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>% 
  spread(year, return)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>% 
  complete(year, qtr)

#create another data frame with missing values
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

#fill in missing values with previous values
treatment %>% 
  fill(person)

#exercises
who
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE) 
who1

who1 %>% 
  count(key) #learn the structure

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3) #drop iso2 and iso3

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1) #seperate sexage into sex and age
who5

#combine the functions together into one:
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)








