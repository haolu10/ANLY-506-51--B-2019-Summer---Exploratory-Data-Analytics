#Week 9: Plotting Systems and Graphics

#Hao Lu

###Chapter 7 Plotting Systems

#add regression line and smoother to the plot
data(airquality)
with(airquality, {
 plot(Temp, Ozone)
 lines(loess.smooth(Temp, Ozone))
 })


data(cars)
## Create the plot / draw canvas
with(cars, plot(speed, dist))

## Add annotation
title("Speed vs. Stopping distance")

#load lattice package
library(lattice)

#A lattice plot that looks at the relationship between life expectancy and income and how that relationship varies by region in the United States

state <- data.frame(state.x77, region = state.region) #set state as a new dataframe
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

#load ggplot2 package
library(ggplot2)

data(mpg)
qplot(displ, hwy, data = mpg)

###Chapter 8 Graphic Devices

## Make plot appear on screen device
with(faithful, plot(eruptions, waiting)) 

## Annotate with a title
title(main = "Old Faithful Geyser data")

## Open PDF device; create 'myplot.pdf' in my working directory
pdf(file = "/Users/haolu/Desktop/myplot.pdf")  

## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))  

## Annotate plot; still nothing on screen
title(main = "Old Faithful Geyser data")  

## Close the PDF file device
dev.off()  

library(datasets)

## Create plot on screen device
with(faithful, plot(eruptions, waiting))  

## Add a main title
title(main = "Old Faithful Geyser data")  

## Copy my plot to a PNG file
dev.copy(png, file = "/Users/haolu/Desktop/geyserplot.png")  

## Don't forget to close the PNG device!
dev.off()  