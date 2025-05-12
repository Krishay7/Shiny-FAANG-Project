library(shiny)
library(tidyquant)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(bslib)

FAANG <- c("META", "AMZN", "AAPL", "NFLX", "GOOG")
faang_prices <- tq_get(FAANG, get = "stock.prices", from = Sys.Date() - years(5), complete_cases = TRUE)

ui <- fluidPage(
  titlePanel("FAANG Stock Prices & Monte Carlo Simulations"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_years", "Years to Simulate", value = 5, min = 1, max = 10),
      numericInput("n_sim", "Number of Simulations", value = 500, min = 100, max = 1000),
      actionButton("simulate", "Run Monte Carlo Simulation"),
      numericInput("investment", "Initial Investment ($)", value = 1000, min = 100),
      selectInput("stock", "Choose a Stock:", choices = FAANG),
      dateRangeInput("date_range", "Select Date Range:", start = Sys.Date() - years(5), end = Sys.Date(), min = min(faang_prices$date), max = max(faang_prices$date)),
      
      tags$hr(),
      
      tags$div(class = "info-block", h4("What does this App do?"),
               p("This app allows users to explore FAANG stock prices, visualize historical trends, and simulate future performance using the GBM model. Users can choose a stock, adjust the simulation settings and run the model to view future projections.")),
      br(),
      
      tags$div(class = "info-block", h4("Geometric Brownian Motion (GBM) Model"),
               p("GBM is a stochastic process where stock prices follow a logarithmic Brownian motion with drift and volatility. It is used to model stock prices, ensuring they remain positive and show random variations, like in real-world market behavior.")),
      br(),
      
      tags$div(class = "info-block", h4("Monte Carlo Simulations"),
               p("Monte Carlo simulations use repeated random sampling to generate a range of possible future stock price outcomes. It helps estimate risks and returns by running multiple scenarios based on historical data."))
    ),
    
    mainPanel(
      plotlyOutput("stock_plot"),
      plotlyOutput("hist_plot")
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$stock, input$date_range)
    faang_prices %>%
      filter(symbol == input$stock, date >= input$date_range[1], date <= input$date_range[2])
  })
  
  output$stock_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) 
      return(NULL)
    
    p <- ggplot(data, aes(x = date, y = close)) + geom_line(color = "steelblue", size = 1) + labs(title = paste(input$stock, "Stock Price"), x = "Date", y = "Closing Price (USD)") + theme_minimal()
    ggplotly(p)
  })
  
  simulated_final_value <- eventReactive(input$simulate, {
    data <- filtered_data()
    req(nrow(data) > 1)
    
    log_returns <- diff(log(data$close))
    mu <- mean(log_returns)
    sigma <- sd(log_returns)
    S0 <- tail(data$close, 1)
    
    n_days <- input$n_years * 252
    dt <- 1 / 252
    n_sim <- input$n_sim
    investment <- input$investment
    
    Z <- matrix(rnorm(n_days * n_sim), nrow = n_days, ncol = n_sim)
    S <- matrix(0, nrow = n_days + 1, ncol = n_sim)
    S[1, ] <- S0
    
    for (t in 2:(n_days + 1)) {
      S[t, ] <- S[t - 1, ] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[t - 1, ])
    }
    
    final_prices <- S[n_days + 1, ]
    final_values <- investment * (final_prices / S0)
    
    return(final_values)
  })
  
  output$hist_plot <- renderPlotly({
    final_vals <- simulated_final_value()
    req(final_vals)
    
    df <- data.frame(FinalValue = final_vals)
    p10 <- quantile(df$FinalValue, 0.10)
    p90 <- quantile(df$FinalValue, 0.90)
    med <- median(df$FinalValue)
    
    p <- ggplot(df, aes(x = FinalValue)) +
      geom_histogram(bins = 50, fill = "skyblue", color = "black") + geom_vline(xintercept = p10, color = "red", linetype = "dashed", size = 1) + geom_vline(xintercept = p90, color = "green", linetype = "dashed", size = 1) + geom_vline(xintercept = med, color = "blue", linetype = "dashed", size = 1) + annotate("text", x = p10, y = -2, label = "10th %tile", vjust = 0, hjust = 1, color = "red") + annotate("text", x = p90, y = -2, label = "90th %tile", vjust = 0, hjust = 1, color = "green") + annotate("text", x = med, y = -2, label = "median", vjust = 0, hjust = 1, color = "blue") + labs(title = paste("Simulated Future Value of $", input$investment, "in", input$stock, "Over an Investment Horizon of", input$n_years, "Years"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                x = "Future Value ($)", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
