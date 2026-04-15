#What factors most strongly influence a car’s price(MSRP)?


library(shiny)
library(plotly)
library(dplyr)
ui <- fluidPage(
  titlePanel("Interactive MSRP Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "xvar",
        "Choose influencing factor:",
        choices = c("Engine Horsepower" = "Engine.HP",
                    "Highway MPG" = "highway.MPG",
                    "City MPG" = "city.mpg")
      ),
      
      selectInput(
        "make_filter",
        "Filter by Make:",
        choices = c("All", sort(unique(data$Make))),
        selected = "All"
      )
    ),
    
    mainPanel(
      plotlyOutput("msrp_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$make_filter == "All") {
      data
    } else {
      data %>% filter(Make == input$make_filter)
    }
  })
  
  output$msrp_plot <- renderPlotly({
    df <- filtered_data()
    
    x_values <- df[[input$xvar]]
    
    plot_ly(
      data = df,
      x = ~x_values,
      y = ~MSRP,
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "Make: ", Make,
        "<br>Model: ", Model,
        "<br>", input$xvar, ": ", x_values,
        "<br>MSRP: $", MSRP
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste(input$xvar, "vs MSRP"),
        xaxis = list(title = input$xvar),
        yaxis = list(title = "MSRP")
      )
  })
}

shinyApp(ui = ui, server = server)
