library("circular")
library("knitr")
library("readxl")
library("ggplot2")
library("dplyr")
library("shiny")
library("shinyFiles")

######### Notes for myself ###########
# Make shiny version of the code
ui <- fluidPage(
  titlePanel("Circular Plot for 24h Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      mainPanel(
        plotOutput("plot1"),
        dataTableOutput("table1")
    #  checkboxInput("header", "Header", TRUE),
     # numericInput("columnParameter", "Column Number of the Parameter", value = 1, min = 1),
      #textInput("graphTitle", "Title of the Graph?"),
      #textInput("colGraph", "Color for Data Points and Arrow"),
    #  textInput("folderPath", "Paste the path for the folder location here. Dont forget to duplicate the slashes!!!!", value = "C:\\Users\\drtfy\\Desktop\\Lab work\\TNL\\PAL\\Data Analysis"),
      #actionButton("goButton", "Generate Plot")),
    
    )
  )))


server <- function(input, output) {
  output$contents <- renderTable({
    req(input$datafile)  # Wait until the file is uploaded
    read.csv(input$datafile$datapath)
  })
   #  file1 <- input$file1
   #  read.csv(file1$datapath, header = input$header)
   # # folderPath <- input$folderPath
   #  columnParameter <- input$columnParameter
   #  graphTitle <- input$graphTitle
   #  colGraph <- input$colGraph
   #  
   #  # Generate the first plot (individual animal data)
   #  output$plot1 <- renderPlot({
   #    
   #  })
   
  }
# Run the application 
shinyApp(ui = ui, server = server)
# 
# #### Parameters to change #####
# folderPath <- read.csv("example_hormone_acrophase_data.csv")
# columnParameter = 2
# graphTitle <- "Saliva Cortisol"
# colGraph = "green"
# ################################################################################
# hour_labels <- c("0", "3", "6", "9", "12", "15", "18", "21")
# hour_pos <- as.numeric(hour_labels)
# 
# # Adjust angles for clock24 template (clockwise, 0 at top)
# label_angles <- (0.5 * pi - (hour_pos / 24) * 2 * pi) %% (2 * pi)
# 
# # Compute coordinates for labels
# x_pos <- 0.8 * cos(label_angles)
# y_pos <- 0.8 * sin(label_angles)
# ################################################################################
# myParam <- folderPath[columnParameter]
# 
# myParam.circ <- circular(myParam, units = "hours", template = "clock24")
# myParam.circ.mean <- mean(myParam.circ, na.rm = T)
# myParam.circ.var <- var(myParam.circ, na.rm = T)
# 
# plot.circular(myParam.circ, col = colGraph, cex = 0.72, pch = 16,
#               stack = TRUE, axes = FALSE, start.sep = 0.05, sep = 0.07,
#               bins = 72, ticks = FALSE, tcl = 0.05, tol = 0.77, units = "hours",
#               template = "clock24", rotation = "clock", zero = 0, lwd = 1,
#               control.circle = circle.control(lwd = 2.5, cex = 2), shrink = 0.7)
# title(graphTitle, line = -0.5)
# 
# arrows.circular(myParam.circ.mean, length = 0.1, lwd = 2, col = colGraph, shrink = 0.65)
# ##################### Legend and Table ###################################
# legend("bottomleft",
#        legend = c(
#          paste("Mean:", round(myParam.circ.mean, 2)),
#          paste("Var.:", round(myParam.circ.var, 2))
#        ),
#        text.col = colGraph,
#        bty = "n", cex = 0.8)
# 
# stats_table <- data.frame(
#   Metric = c("Circular Mean", "Circular Variance"),
#   Value = round(c(as.numeric(myParam.circ.mean), myParam.circ.var), 3)
# ) %>% View
# ################################################################################
# # Add labels to the circular plot
# text(x_pos, y_pos, labels = hour_labels, cex = 0.8)
# 
