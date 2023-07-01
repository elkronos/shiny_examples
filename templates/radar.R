library(shiny)
library(fmsb)
library(RColorBrewer)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("High School Grades"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("radar_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create data
  set.seed(99)
  data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
  colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
  rownames(data) <- paste("student" , letters[1:3] , sep="-")
  
  # To use the fmsb package, add the max and min of each variable to the dataframe!
  data <- rbind(rep(20,5) , rep(0,5) , data)
  
  # Set graphic colors
  coul <- brewer.pal(3, "BuPu")
  colors_border <- coul
  colors_in <- alpha(coul,0.3)
  
  # Render radar plot
  output$radar_plot <- renderPlot({
    radarchart(data[-c(1,2),], axistype=0, maxmin=F,
               pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
               cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
               vlcex=0.8)
    # Add a legend
    legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20,
           col=colors_in, text.col = "grey", cex=1.2, pt.cex=3)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
