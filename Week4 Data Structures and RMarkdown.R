###Week 4 Code: Metrices and data frames

##Phillips, N. D. (2016). Chapter 8. Yarrr! The pirate’s guide to R

#set values for x y z
x <- 1:5
y <- 6:10
z <- 11:15

# Create a matrix with columns x y z
cbind(x, y, z)

# Create a matrix with rows x y z
rbind(x, y, z)

# Creating a matrix with numeric and character columns will make everything a character:
cbind(c(1, 2, 3, 4, 5), c("a", "b", "c", "d", "e"))

# head() function
head(ChickWeight)

# tail() function
tail(ChickWeight)

# View() opens the entire dataframe in a new window
View(ChickWeight)

# Print summary statistics of ToothGrowth
summary(ToothGrowth)

# Print additional information about ToothGrowth
str(ToothGrowth)

# What are the names of columns in the ToothGrowth dataframe?
names(ToothGrowth)

#the len column of ToothGrowth
ToothGrowth$len

# the mean of the len column of ToothGrowth
mean(ToothGrowth$len)

# Give me a table of the supp column of ToothGrowth.
table(ToothGrowth$supp)

# Give me the len AND supp columns of ToothGrowth
head(ToothGrowth[c("len", "supp")])

#Create a new dataframe called survey
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "age" = c(24, 25, 42, 56, 22))

# Add a new column called sex to survey
survey$sex <- c("m", "m", "f", "f", "m")

# survey with new sex column
survey

# Change the name of the first column of survey to "participant.number"
names(survey)[1] <- "participant.number"

# Change the column name from age to age.years
names(survey)[names(survey) == "age"] <- "years"

# Give me the rows 1-6 and column 1 of ToothGrowth
ToothGrowth[1:6, 1]

# Give me rows 1-3 and columns 1 and 3 of ToothGrowth
ToothGrowth[1:3, c(1,3)]

# Give me the 1st row (and all columns) of ToothGrowth
ToothGrowth[1, ]

# Give me the 2nd column (and all rows) of ToothGrowth
ToothGrowth[,2]

# Create a new df with only the rows of ToothGrowth where supp equals VC
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC",]

# Create a new df with only the rows of ToothGrowth where supp equals OJ and dose < 1
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose < 1,]

# Get rows of ToothGrowth where len < 20 AND supp == "OJ" AND dose >= 1
subset(x = ToothGrowth, subset = len < 20 & supp == "OJ" & dose >= 1)

# Get rows of ToothGrowth where len > 30 AND supp == "VC", but only return the len and dose columns
subset(x = ToothGrowth, subset = len > 30 & supp == "VC", select = c(len, dose))

# What is the mean tooth length of Guinea pigs given OJ?

# Step 1: Create a subsettted dataframe called oj
oj <- subset(x = ToothGrowth,
             subset = supp == "OJ")

# Step 2: Calculate the mean of the len column from
#  the new subsetted dataset
mean(oj$len)

# Step 1: Create a subsettted dataframe called oj
oj <- ToothGrowth[ToothGrowth$supp == "OJ",]

# Step 2: Calculate the mean of the len column from
#  the new subsetted dataset
mean(oj$len)

#another method to calculate mean
mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])

#create a dataframe called health
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))

# Calculate bmi
health$weight / health$height ^ 2

# Save typing by using with()
with(health, weight/ height ^ 2)

# Long code
health$weight + health$height / health$age + 2 * health$height

# Short code that does the same thing
with(health, weight + height / age + 2 * height)


## Chapter 20 Vectors, r for Data Science
#load tidyverse package
library(tidyverse) 

#return the type of the input
typeof(letters) #character
typeof(1:10) #integer

#return the length of input
x <- list("a", "b", 1:10)
length(x)

#logical inputs
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)

#numeric inputs
typeof(1)
typeof(1L)
1.5L

#what is square of the square root of two
x <- sqrt(2) ^ 2
x
x - 2

#division
c(-1, 0, 1) / 0

#logical
NA

#integer
NA_integer_ 

#double
NA_real_

#character
NA_character_

#create a sample
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)  # how many are greater than 10?
mean(y) # what proportion are greater than 10?

#vector with multiple types
typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

#run another sample
sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3

#tibble function
tibble(x = 1:4, y = 1:2)
#recycle with rep
tibble(x = 1:4, y = rep(1:2, 2))

#name x y z
c(x = 1, y = 2, z = 4)

#set names
set_names(1:3, c("a", "b", "c"))

#subsetting
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]

x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]

#subset with logical vectors
x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

#subset with charactor
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

#list function
x <- list(1, 2, 3)
x
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

#list that contains other list
z <- list(list(1, 2), list(3, 4))
str(z)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

#different way to subset
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])

#shorter code
a$a
a[["a"]]

#attributes
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

#method function
methods("as.Date")
#specific method implementation
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

#factor function
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
#attribute
attributes(x)

#dates in r
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

#tzone attribute
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

#POSIXlt
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

#tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

#use data frame structure
df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

