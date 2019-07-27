#Week 2 Code: 
YaRrr! The Pirate’s Guide to R. Chapter 9 Importing, Saving and Managing Data

#print my working directory
getwd()

# Change the path for my working directory
setwd(dir = "/Users/haolu/Desktop/")

#print all items in the working directory
ls()

# Create some random objects
study1.df <- data.frame(id = 1:5, 
                        sex = c("m", "m", "f", "f", "m"), 
                        score = c(51, 20, 67, 52, 42))

score.by.sex <- aggregate(score ~ sex, 
                          FUN = mean, 
                          data = study1.df)

study1.htest <- t.test(score ~ sex, 
                       data = study1.df)

# Save two objects as a new .RData file in the data folder of my current working directory
save(study1.df, score.by.sex, study1.htest,
     file = "data/study1.RData")

# Save my workspace to complete_image.RData in the data folder of my working directory
save.image(file = "data/projectimage.RData")

# Load objects in study1.RData into my workspace
load(file = "data/study1.RData")

# Load objects in projectimage.RData into my workspace
load(file = "data/projectimage.RData")

# Remove huge.df from workspace
rm(huge.df)

# Remove ALL objects from workspace
rm(list = ls())

# Write the pirates dataframe object to a tab-delimited text file called pirates.txt in my working directory
libary(pirates)
write.table(x = pirates,
            file = "pirates.txt",  # Save the file as pirates.txt
            sep = "\t")            # Make the columns tab-delimited

# Write the pirates dataframe object to a tab-delimited text file called pirates.txt to my desktop

write.table(x = pirates,
            file = "Users/haolu/Desktop/pirates.txt",  # Save the file as pirates.txt to my desktop
            sep = "\t")                                    # Make the columns tab-delimited

# Read a tab-delimited text file called mydata.txt from the data folder in my working directory into
R and store as a new object called mydata

mydata <- read.table(file = 'data/mydata.txt',    # file is in a data folder in my working directory
                     sep = '\t',                  # file is tab--delimited
                     header = TRUE,               # the first row of the data is a header row
                     stringsAsFactors = FALSE)    # do NOT convert strings to factors!!

# Read a text file from the web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE)

# Print the result
fromweb

Chapter 9 The Base Plotting System

library(datasets)
# Draw a new plot on the screen device
hist(airquality$Ozone)  

# Draw a box plot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

#Draw a scatter plot
with(airquality, plot(Wind, Ozone))

#Default values for parameters
par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")

#Add title to the scatterplot
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City") 

#Base plot with annotation, color in blue
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

#Color in red and blue, and with legend
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

#Set Plot
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
#Fit a simple linear regression model
model <- lm(Ozone ~ Wind, airquality)
#Draw regression line on plot
abline(model, lwd = 2)

#Draw multiple base plot
par(mfrow = c(1, 2))
with(airquality, {plot(Wind, Ozone, main = "Ozone and Wind")
plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")})

#Panel plot with two plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE) #Title
})

