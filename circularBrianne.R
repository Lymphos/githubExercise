library("circular")
library("knitr")
library("readxl")
library("ggplot2")
library("shiny")

circularData1 <- read_excel("Circcular Plot Data Corrected Acrophases May 9 2025.xlsx", sheet = "Collection 1")
circularData2 <- read_excel("Circcular Plot Data Corrected Acrophases May 9 2025.xlsx", sheet = "Collection 2")
circularData3 <- read_excel("Circcular Plot Data Corrected Acrophases May 9 2025.xlsx", sheet = "Collection 1 and 2 combined")

################################################################################

hour_labels <- c("0", "3", "6", "9", "12", "15", "18", "21")
hour_pos <- as.numeric(hour_labels)

# Adjust angles for clock24 template (clockwise, 0 at top)
label_angles <- (0.5 * pi - (hour_pos / 24) * 2 * pi) %% (2 * pi)

# Compute coordinates for labels
x_pos <- 0.8 * cos(label_angles)
y_pos <- 0.8 * sin(label_angles)
  
#################################amt6###########################################
  amt6_1 <- circularData1[1]
  
  amt6_1.circ <- circular(amt6_1, units = "hours", template = "clock24")
  amt6_1.circ.mean <- mean(amt6_1.circ, na.rm = T)
  amt6_1.circ.var <- var(amt6_1.circ, na.rm = T)
  
  plot1 <- plot.circular(amt6_1.circ, col = "blue", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
  title("aMT6s", line = -0.5)
  
  arrows.circular(amt6_1.circ.mean, length = 0.1, lwd = 2, col = "blue", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################saliva cort###########################################
  salvCort_1 <- circularData1[2]
  
  salvCort_1.circ <- circular(salvCort_1, units = "hours", template = "clock24")
  salvCort_1.circ.mean <- mean(salvCort_1.circ, na.rm = T)
  salvCort_1.circ.var <- var(salvCort_1.circ, na.rm = T)
  
  plot2 <- plot.circular(salvCort_1.circ, col = "red", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Salivary Cortisol", line = -0.5)
  arrows.circular(salvCort_1.circ.mean, length = 0.1, lwd = 2, col = "red", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Urin cort###########################################
  urnCort_1 <- circularData1[[3]]
  
  urnCort_1.circ <- circular(urnCort_1, units = "hours", template = "clock24")
  urnCort_1.circ.mean <- mean(urnCort_1.circ, na.rm = T)
  urnCort_1.circ.var <- var(urnCort_1.circ, na.rm = T)
  
  plot3 <- plot.circular(urnCort_1.circ, col = "orange", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7) 
  title("Urinary Cortisol", line = - 0.5) 
  arrows.circular(urnCort_1.circ.mean, length = 0.1, lwd = 2, col = "orange", shrink = 0.65) 
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Total Cholesterol###########################################
  totChol_1 <- circularData1[4]
  
  totChol_1.circ <- circular(totChol_1, units = "hours", template = "clock24")
  totChol_1.circ.mean <- mean(totChol_1.circ, na.rm = T)
  totChol_1.circ.var <- var(totChol_1.circ, na.rm = T)
  
  plot4 <- plot.circular(totChol_1.circ, col = "green", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Total Cholesterol", line = -0.5)
  arrows.circular(totChol_1.circ.mean, length = 0.1, lwd = 2, col = "green", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
#################################################################################
  
  #################################amt6###########################################
  amt6_2 <- circularData2[1]
  
  amt6_2.circ <- circular(amt6_2, units = "hours", template = "clock24")
  amt6_2.circ.mean <- mean(amt6_2.circ, na.rm = T)
  amt6_2.circ.var <- var(amt6_2.circ, na.rm = T)
  
  plot.circular(amt6_2.circ, col = "blue", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
  title("aMT6s", line = - 0.5)
  
  arrows.circular(amt6_2.circ.mean, length = 0.1, lwd = 2, col = "blue", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################saliva cort###########################################
  salvCort_2 <- circularData2[2]
  
  salvCort_2.circ <- circular(salvCort_2, units = "hours", template = "clock24")
  salvCort_2.circ.mean <- mean(salvCort_2.circ, na.rm = T)
  salvCort_2.circ.var <- var(salvCort_2.circ, na.rm = T)
  
  plot.circular(salvCort_2.circ, col = "red", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Salivary Cortisol", line = - 0.5)
  arrows.circular(salvCort_2.circ.mean, length = 0.1, lwd = 2, col = "red", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Urin cort###########################################
  urnCort_2 <- circularData2[[3]]
  
  urnCort_2.circ <- circular(urnCort_2, units = "hours", template = "clock24")
  urnCort_2.circ.mean <- mean(urnCort_2.circ, na.rm = T)
  urnCort_2.circ.var <- var(urnCort_2.circ, na.rm = T)
  
  plot.circular(urnCort_2.circ, col = "orange", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Urinary Cortisol", line = - 0.5)
  arrows.circular(urnCort_2.circ.mean, length = 0.1, lwd = 2, col = "orange", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Total Cholesterol###########################################
  totChol_2 <- circularData2[4]
  
  totChol_2.circ <- circular(totChol_2, units = "hours", template = "clock24")
  totChol_2.circ.mean <- mean(totChol_2.circ, na.rm = T)
  totChol_2.circ.var <- var(totChol_2.circ, na.rm = T)
  
  plot.circular(totChol_2.circ, col = "green", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Total Cholesterol", line = -0.5)
  arrows.circular(totChol_2.circ.mean, length = 0.1, lwd = 2, col = "green", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  ####################### Fused #############################################
  
  #################################amt6###########################################
  amt6_3 <- circularData3[1]
  
  amt6_3.circ <- circular(amt6_3, units = "hours", template = "clock24")
  amt6_3.circ.mean <- mean(amt6_3.circ, na.rm = T)
  amt6_3.circ.var <- var(amt6_3.circ, na.rm = T)
  
  plot.circular(amt6_3.circ, col = "blue", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
  title("aMT6s", line = - 0.5)
  
  arrows.circular(amt6_3.circ.mean, length = 0.1, lwd = 2, col = "blue", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################saliva cort###########################################
  salvCort_3 <- circularData3[2]
  
  salvCort_3.circ <- circular(salvCort_3, units = "hours", template = "clock24")
  salvCort_3.circ.mean <- mean(salvCort_3.circ, na.rm = T)
  salvCort_3.circ.var <- var(salvCort_3.circ, na.rm = T)
  
  plot.circular(salvCort_3.circ, col = "red", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Salivary Cortisol", line = - 0.5)
  arrows.circular(salvCort_2.circ.mean, length = 0.1, lwd = 2, col = "red", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Urin cort###########################################
  urnCort_3 <- circularData3[[3]]
  
  urnCort_3.circ <- circular(urnCort_3, units = "hours", template = "clock24")
  urnCort_3.circ.mean <- mean(urnCort_3.circ, na.rm = T)
  urnCort_3.circ.var <- var(urnCort_3.circ, na.rm = T)
  
  plot.circular(urnCort_3.circ, col = "orange", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Urinary Cortisol", line = - 0.5)
  arrows.circular(urnCort_3.circ.mean, length = 0.1, lwd = 2, col = "orange", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  #################################Total Cholesterol###########################################
  totChol_3 <- circularData3[4]
  
  totChol_3.circ <- circular(totChol_3, units = "hours", template = "clock24")
  totChol_3.circ.mean <- mean(totChol_3.circ, na.rm = T)
  totChol_3.circ.var <- var(totChol_3.circ, na.rm = T)
  
  plot.circular(totChol_3.circ, col = "green", cex = 0.72, pch = 16,
                stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
                bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
                template = "clock24", rotation = "clock", zero = 0, lwd = 1,
                control.circle = circle.control(lwd = 2.5, cex = 0.5), shrink = 0.7)
  title("Total Cholesterol", line = -0.5)
  arrows.circular(totChol_3.circ.mean, length = 0.1, lwd = 2, col = "green", shrink = 0.65)
  ################################################################################
  # Add labels to the circular plot
  text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
  
  
###################### Transform shrink values ##############################
# # Function to transform a single value to a new range
# transform_variable <- function(value, new_min = 0.3, new_max = 0.6) {
#   original_min <- 0  # Set the original min value (you can define it according to your needs)
#   original_max <- 1  # Set the original max value (you can define it according to your needs)
#   return(new_min + (value - original_min) * (new_max - new_min) / (original_max - original_min))
# }
# 
# shrink_factor_amt6 <- transform_variable(shrink_factor_amt6)
# shrink_factor_salvCort <- transform_variable(shrink_factor_salvCort)
# shrink_factor_totCholesterol <- transform_variable(shrink_factor_totCholesterol)
# shrink_factor_urinCort <- transform_variable(shrink_factor_urinCort)
# ############################ Graphs ##########################################
# labelsGraph = c("6", "3", "0", "21", "18", "15", "12", "9")
# 
# par(mfrow = c(2,2))
# plot.circular(amt6.circ, stack = TRUE, main = "aMT6s",col = "blue", axes = FALSE, sep = 0.05, start.sep = 0.02)
# symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 2)
# axis.circular(at=circular(seq(0, 2*pi - pi/4, by = pi/4)), labels=labelsGraph, tick = F, tcl.text = 0.2)
# arrows.circular(amt6.circ.mean, col = "black", shrink = 0.65, length = 0.1, lwd = 3, angle = 20)
# 
# plot.circular(salvCort.circ, stack = TRUE, main = "Saliva Cortisol", col = "red", axes = FALSE, sep = 0.05, start.sep = 0.02)
# symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 2)
# axis.circular(at=circular(seq(0, 2*pi - pi/4, by = pi/4)), labels=labelsGraph, tick = F, tcl.text = 0.2)
# arrows.circular(salvCort.circ.mean, shrink = 0.65, col = "black", length = 0.1, lwd = 2, angle = 20)
# 
# plot.circular(urinCort.circ, stack = TRUE, main = "Urine Cortisol", col = "orange", axes = FALSE, sep = 0.05, start.sep = 0.02)
# symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 2)
# axis.circular(at=circular(seq(0, 2*pi - pi/4, by = pi/4)), labels=labelsGraph, tick = F, tcl.text = 0.2)
# arrows.circular(urinCort.circ.mean, shrink = 0.65, col = "black", length = 0.1, lwd = 2, angle = 20)
# 
# plot.circular(totCholesterol.circ, stack = TRUE, main = "Total Cholesterol", col = "green", axes = FALSE, sep = 0.05, start.sep = 0.02)
# symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 2)
# axis.circular(at=circular(seq(0, 2*pi - pi/4, by = pi/4)), labels=labelsGraph, tick = F, tcl.text = 0.2)
# arrows.circular(totCholesterol.circ.mean, shrink = 0.65, col = "black", length = 0.1, lwd = 2, angle = 20)
# 
# par(mfrow = c(1, 1))
# 
# ########################### Table #############################
# Circular_Data_Summary <- data.frame(
#   Variable = c("Urinary aMT6s", "Saliva Cort", "Urine Cort", "Total Cholesterol"),
#   Mean = round(c(amt6.circ.mean, salvCort.circ.mean, urinCort.circ.mean, totCholesterol.circ.mean), 3),
#   Variance = round(c(amt6.circ.var, salvCort.circ.var, urinCort.circ.var, totCholesterol.circ.var), 3)
# )
# 
# print(summary_table)
# 
# write.csv(Circular_Data_Summary, "Circular_Data_Summary.csv", row.names = FALSE)
# 
# ##############################################################
# 
# circularData <- read_excel("Example data.xlsx")
# 
# #################################amt6###########################################
# 
# amt6 <- circularData$aMT6s
# 
# amt6.circ <- circular(amt6, units = "hours", template = "clock24")
# amt6.circ.mean <- mean(amt6.circ, na.rm = T)
# amt6.circ.var <- var(amt6.circ, na.rm = T)
# shrink_factor_amt6 <- 1 - amt6.circ.var
# 
# # 1. Input: hours from your data
# hours <- amt6
# # Convert to radians (0–24 hours → 0–2π)
# # 1. Input: hours from your data
# 
# 
# # 2. Convert hours to radians (0–24h → 0–2π)
# radians <- (hours / 24) * 2 * pi
# 
# # 3. Create circular object
# x <- circular(radians,
#               units = "radians",
#               template = "clock24",
#               modulo = "2pi",
#               zero = 0,
#               rotation = "clock")
# 
# # 4. Plot using your custom layout
# plot.circular(x,
#               cex = 0.72,
#               pch = 16,
#               stack = TRUE,
#               axes = FALSE,
#               start.sep = 0.1,
#               sep = 0.25,
#               shrink = 1.50,
#               bins = 72,
#               ticks = FALSE,
#               tcl = 0.05,
#               tol = 0.77,
#               units = "hours",
#               template = "clock24",
#               rotation = "clock",
#               zero = 0,
#               lwd = 1,
#               col = 19,
#               control.circle = circle.control(lwd = 2.5, cex = 0.5))
# 
# # 5. Calculate mean direction and vector length from CircStats
# mean_dir <- circ.mean(radians)
# rho <- est.rho(radians)
# 
# # 6. Draw mean arrow
# arrows.circular(mean_dir,
#                 length = 0.1,
#                 lwd = 2,
#                 col = "red",
#                 shrink = 0.65)
# 
# hour_labels <- c("0", "3", "6", "9", "12", "15", "18", "21")
# hour_pos <- as.numeric(hour_labels)
# 
# # Adjust angles for clock24 template (clockwise, 0 at top)
# label_angles <- (0.5 * pi - (hour_pos / 24) * 2 * pi) %% (2 * pi)
# 
# # Compute coordinates for labels
# x_pos <- 0.8 * cos(label_angles)
# y_pos <- 0.8 * sin(label_angles)
# 
# # Add labels to the circular plot
# text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
