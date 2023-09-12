library(shiny)
library(tidyverse)

# Load the data
landline <- read_csv("landline.csv")
mobile <- read_csv("mobile.csv")

ui <- fluidPage(
  
  titlePanel("Plot of Telephone Subscribers by Continent"),
  
  sidebarLayout(
    sidebarPanel(
      # Select the continent
      radioButtons("continent", "Continent:", choices = unique(landline$continent)),
      
      # Select the subscription type
      radioButtons("subscription", "Subscription Type:", choices = c("Mobile", "Landline"))
    ),
    
    mainPanel(
      # Plot the histogram
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # Determine which data to use based on subscription type
    data <- if (input$subscription == "Mobile") {
      mobile
    } else {
      landline
    }
    
    # Filter the data for the selected continent
    filtered_data <- data %>%
      filter(continent == input$continent)
    
    # Create the plot
    ggplot(filtered_data, aes_string(x = ifelse(input$subscription == "Mobile", "mobile_subs", "landline_subs"))) +
      geom_histogram(bins = 30) +
      theme_bw(base_size = 14) +
      labs(x = ifelse(input$subscription == "Mobile", "Mobile Subscribers", "Landline Subscribers"),
           y = "Frequency")
  })
}

shinyApp(ui = ui, server = server)
