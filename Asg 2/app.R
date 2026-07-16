








# app.R

library(shiny)
library(dplyr)
library(plotly)
library(scales)

# Load cleaned datasets
orders <- read.csv("clean_superstore.csv", stringsAsFactors = FALSE)
state_data <- read.csv("final_merged_data.csv", stringsAsFactors = FALSE)

# Make sure columns are in the correct format
orders$Order_Date <- as.Date(orders$Order_Date)
orders$Ship_Date <- as.Date(orders$Ship_Date)
orders$Year <- as.numeric(orders$Year)
orders$Month <- as.numeric(orders$Month)

# Create shipping duration variable
orders$Days_to_Ship <- as.numeric(orders$Ship_Date - orders$Order_Date)

state_data$Income <- as.numeric(gsub(",", "", state_data$Income))
state_data$GDP <- as.numeric(state_data$GDP)
state_data$Unemployment <- as.numeric(state_data$Unemployment)
state_data$Total_Sales <- as.numeric(state_data$Total_Sales)
state_data$Total_Profit <- as.numeric(state_data$Total_Profit)
state_data$Total_Quantity <- as.numeric(state_data$Total_Quantity)

# Dashboard page
dashboard_page <- fluidPage(
  
  h1(strong("Superstore Sales and Economic Factors Dashboard"), align = "center"),
  
  p("This dashboard explores how Superstore sales and profit vary across states, product categories, shipping modes, and economic factors such as GDP, income, and unemployment."),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      selectInput(
        inputId = "selected_region",
        label = "Choose Region",
        choices = c("All", unique(orders$Region))
      ),
      
      selectInput(
        inputId = "selected_category",
        label = "Choose Product Category",
        choices = c("All", unique(orders$Category))
      ),
      
      sliderInput(
        inputId = "selected_year",
        label = "Choose Year Range",
        min = min(orders$Year),
        max = max(orders$Year),
        value = c(min(orders$Year), max(orders$Year)),
        step = 1
      )
    ),
    
    mainPanel(
      width = 10,
      
      fluidRow(
        column(4, h4(strong("Total Sales"), align = "center"), h3(textOutput("total_sales"), align = "center")),
        column(4, h4(strong("Total Profit"), align = "center"), h3(textOutput("total_profit"), align = "center")),
        column(4, h4(strong("Total Quantity"), align = "center"), h3(textOutput("total_quantity"), align = "center"))
      ),
      
      hr(),
      
      fluidRow(
        column(4, h4(strong("Average Income"), align = "center"), h3(textOutput("average_income"), align = "center")),
        column(4, h4(strong("Average Unemployment"), align = "center"), h3(textOutput("average_unemployment"), align = "center")),
        column(4, h4(strong("Average GDP"), align = "center"), h3(textOutput("average_gdp"), align = "center"))
      ),
      
      hr(),
      
      fluidRow(
        column(
          6,
          h4(strong("Sales Trend Over Time"), align = "center"),
          plotlyOutput("sales_trend", height = "280px")
        ),
        column(
          6,
          h4(strong("Top States by Sales"), align = "center"),
          plotlyOutput("state_sales", height = "280px")
        )
      ),
      
      fluidRow(
        column(
          6,
          h4(strong("Profit by Product Category"), align = "center"),
          plotlyOutput("category_profit", height = "280px")
        ),
        column(
          6,
          h4(strong("Shipping Mode Efficiency"), align = "center"),
          plotlyOutput("shipping_mode_box", height = "280px")
        )
      ),
      
      fluidRow(
        column(
          12,
          h4(strong("Sales vs GDP with Profit, Income and Unemployment"), align = "center"),
          plotlyOutput("economic_sales", height = "340px"),
          p("Each point represents one state. Hover over a point to view GDP, income, unemployment, sales and profit details.")
        )
      )
    )
  )
)

# Dashboard server
dashboard_server <- function(input, output) {
  
  filtered_orders <- reactive({
    selected_data <- orders
    
    if (input$selected_region != "All") {
      selected_data <- selected_data %>%
        filter(Region == input$selected_region)
    }
    
    if (input$selected_category != "All") {
      selected_data <- selected_data %>%
        filter(Category == input$selected_category)
    }
    
    selected_data <- selected_data %>%
      filter(
        Year >= input$selected_year[1],
        Year <= input$selected_year[2]
      )
    
    selected_data
  })
  
  filtered_state_data <- reactive({
    selected_states <- unique(filtered_orders()$State)
    
    state_data %>%
      filter(State %in% selected_states)
  })
  
  output$total_sales <- renderText({
    dollar(sum(filtered_orders()$Sales, na.rm = TRUE))
  })
  
  output$total_profit <- renderText({
    dollar(sum(filtered_orders()$Profit, na.rm = TRUE))
  })
  
  output$total_quantity <- renderText({
    comma(sum(filtered_orders()$Quantity, na.rm = TRUE))
  })
  
  output$average_income <- renderText({
    dollar(mean(filtered_state_data()$Income, na.rm = TRUE))
  })
  
  output$average_unemployment <- renderText({
    paste0(round(mean(filtered_state_data()$Unemployment, na.rm = TRUE), 1), "%")
  })
  
  output$average_gdp <- renderText({
    comma(round(mean(filtered_state_data()$GDP, na.rm = TRUE), 0))
  })
  
  output$sales_trend <- renderPlotly({
    sales_trend_data <- filtered_orders() %>%
      group_by(Year, Month) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      data = sales_trend_data,
      x = ~Month,
      y = ~Total_Sales,
      color = ~as.factor(Year),
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "Year: %{fullData.name}<br>Month: %{x}<br>Total Sales: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Sales"),
        legend = list(title = list(text = "Year"))
      )
  })
  
  output$state_sales <- renderPlotly({
    state_sales_data <- filtered_orders() %>%
      group_by(State) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Total_Sales) %>%
      tail(10)
    
    plot_ly(
      data = state_sales_data,
      x = ~Total_Sales,
      y = ~reorder(State, Total_Sales),
      type = "bar",
      orientation = "h",
      hovertemplate = "State: %{y}<br>Total Sales: $%{x:,.0f}<extra></extra>",
      marker = list(
        color = ~Total_Sales,
        colorscale = list(
          c(0, "#cfe2ff"),
          c(0.5, "#5b8ff9"),
          c(1, "#0b3d91")
        )
      )
    ) %>%
      layout(
        xaxis = list(title = "Total Sales"),
        yaxis = list(title = "State")
      )
  })
  
  output$category_profit <- renderPlotly({
    category_profit_data <- filtered_orders() %>%
      group_by(Category) %>%
      summarise(
        Total_Profit = sum(Profit, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      data = category_profit_data,
      x = ~Category,
      y = ~Total_Profit,
      type = "bar",
      color = ~Category,
      hovertemplate = "Category: %{x}<br>Total Profit: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Product Category"),
        yaxis = list(title = "Total Profit"),
        showlegend = FALSE
      )
  })
  
  output$shipping_mode_box <- renderPlotly({
    plot_ly(
      data = filtered_orders(),
      x = ~Ship.Mode,
      y = ~Days_to_Ship,
      type = "box",
      color = ~Ship.Mode,
      boxpoints = "outliers",
      hovertemplate = "Shipping Mode: %{x}<br>Days to Ship: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Shipping Mode"),
        yaxis = list(title = "Days to Ship"),
        showlegend = FALSE
      )
  })
  
  output$economic_sales <- renderPlotly({
    plot_ly(
      data = filtered_state_data(),
      x = ~GDP,
      y = ~Total_Sales,
      color = ~Total_Profit,
      colors = c("blue", "orange", "yellow"),
      text = ~paste(
        "State:", State,
        "<br>Total Sales:", dollar(Total_Sales),
        "<br>Total Profit:", dollar(Total_Profit),
        "<br>GDP:", comma(GDP),
        "<br>Income:", dollar(Income),
        "<br>Unemployment:", Unemployment, "%"
      ),
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      marker = list(
        size = 10,
        opacity = 0.8
      )
    ) %>%
      layout(
        xaxis = list(title = "State GDP"),
        yaxis = list(title = "Total Sales"),
        legend = list(title = list(text = "Total Profit"))
      )
  })
}

shinyApp(ui = dashboard_page, server = dashboard_server)