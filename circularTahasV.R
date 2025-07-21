library("circular")
library("dplyr")

#### Parameters to change #####
# Data file name to read;
circularData <- read.csv("example_hormone_acrophase_data.csv")
# Choose the column (by index) with time data;
columnParameter <- 2
# Title of the circular plot;
graphTitle <- "Saliva Cortisol"
# Color for data points and arrow;
colorGraph <- "blue"
# Show arrow for circular mean or not (Assign TRUE or FALSE);
circMeanArrowBool <- FALSE
# Show/Create circular mean and variance legend at the bottom left of graph (Assign TRUE or FALSE):
legendCircMeanVar <- TRUE
######### Hour labels on the graph #############################################
hour_labels <- c("0", "3", "6", "9", "12", "15", "18", "21")
hourLabel_positions <- as.numeric(hour_labels)

# Adjust angles for clock24 template (clockwise, 0 at top)
label_angles <- (0.5 * pi - (hourLabel_positions / 24) * 2 * pi) %% (2 * pi)

# Compute coordinates for labels
x_pos_label <- 0.8 * cos(label_angles)
y_pos_label <- 0.8 * sin(label_angles)

########## Creating the Graph ##################################################
paramToGraph <- circularData[columnParameter]

paramToGraph.circ <- circular(paramToGraph, units = "hours", template = "clock24")
paramToGraph.circ.mean <- mean(paramToGraph.circ, na.rm = T)
paramToGraph.circ.var <- var(paramToGraph.circ, na.rm = T)

plot.circular(paramToGraph.circ, col = colorGraph, cex = 0.72, pch = 16,
              stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
              bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
              template = "clock24", rotation = "clock", zero = 0, lwd = 1,
              control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
title(graphTitle, line = -0.5)
if(circMeanArrowBool){arrows.circular(paramToGraph.circ.mean, length = 0.1, lwd = 2, col = colorGraph, shrink = 0.65)}
################# Label adding to the graph ####################################
# Add labels to the circular plot
text(x_pos_label, y_pos_label, labels = hour_labels, cex = 0.8)

##################### Legend and Table ###################################
if(legendCircMeanVar){legend("bottomleft",
       legend = c(
         paste("Mean:", round(paramToGraph.circ.mean, 2)),
         paste("Var.:", round(paramToGraph.circ.var, 2))
       ),
       text.col = colorGraph,
       bty = "n", cex = 0.8)

stats_table <- data.frame(
  Metric = c("Circular Mean", "Circular Variance"),
  Value = round(c(as.numeric(paramToGraph.circ.mean), paramToGraph.circ.var), 3)
)
}
View(stats_table)

