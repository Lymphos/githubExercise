library("circular")
library("knitr")
library("readxl")
library("ggplot2")
library("dplyr")

#### Parameters to change #####
circularData <- read_excel("Example data.xlsx")

columnParameter = 2
myTitle <- "Saliva Cort"
colGraph = "green"
################################################################################
hour_labels <- c("0", "3", "6", "9", "12", "15", "18", "21")
hour_pos <- as.numeric(hour_labels)

# Adjust angles for clock24 template (clockwise, 0 at top)
label_angles <- (0.5 * pi - (hour_pos / 24) * 2 * pi) %% (2 * pi)

# Compute coordinates for labels
x_pos <- 0.8 * cos(label_angles)
y_pos <- 0.8 * sin(label_angles)
################################################################################
myParam <- circularData[columnParameter]

myParam.circ <- circular(myParam, units = "hours", template = "clock24")
myParam.circ.mean <- mean(myParam.circ, na.rm = T)
myParam.circ.var <- var(myParam.circ, na.rm = T)

plot.circular(myParam.circ, col = colGraph, ccex = 0.72, pch = 16,
              stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
              bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
              template = "clock24", rotation = "clock", zero = 0, lwd = 1,
              control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
title(myTitle, line = -0.5)

arrows.circular(myParam.circ.mean, length = 0.1, lwd = 2, col = colGraph, shrink = 0.65)
################################################################################
# Add labels to the circular plot
text(x_pos, y_pos, labels = hour_labels, cex = 0.8)


