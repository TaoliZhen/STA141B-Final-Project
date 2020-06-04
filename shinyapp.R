library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
readRenviron(".Renviron")

## Get Stock Symbol and Company names first
nasdaq <- read_csv("NASDAQ_symbols.csv")
nyse <- read_csv("NYSE_symbols.csv")
stock <- bind_rows(nasdaq, nyse)

check_symbol <- function(text){
  # A function to check the input exits in the US stock market.
  if (toupper(text) %in% stock$symbol){
    df = stock %>% filter(symbol == toupper(text))
    return(df)
  } else {
    #return("Sorry the symbol you enter is not found, please double check and try again.")
    return(NA)
  }
}

get_minly <- function(symbol){
  r <- GET(
    "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY",
    query = list(
      symbol = symbol,
      interval = "1min",
      outputsize = "full",
      apikey = Sys.getenv("AVS_API_KEY")
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  df <- fromJSON(json, flatten = TRUE) %>% 
    as.data.frame() %>% 
    select(symbol = ends_with("Symbol"), ends_with("close")) %>% 
    pivot_longer(
      starts_with("Time.Series.."),
      names_to = "Date",
      names_prefix = "Time.Series..1min.."
    ) %>% 
    head(390) %>% 
    mutate(Date = gsub("^.........$","", Date),
           Date = as_datetime(Date),
           Price = as.numeric(as.character(value))
    )
  
  return(df)
}



ui <- fluidPage(
  
  titlePanel("Stock Price"),
  
  # Choose a overall layout
  sidebarLayout(
    
  # Choose a input panel
  sidebarPanel(
    
  # Text input  
  p("Please enter a stock symbol to start."),
  textInput (
    inputId = "sb",
    label = "Stock Symbol",
    value = "AAPL"
      ),
  actionButton(inputId ="go", label = "Show Results")
  ),
  
  # Output panel
  mainPanel(
    tabsetPanel(
      tabPanel("Latest Trade Day", plotOutput("one")),
      tabPanel("Six Month", verbatimTextOutput("two")),
      tabPanel("One Year", verbatimTextOutput("three")),
      tabPanel("Historic", verbatimTextOutput("four")),
      tabPanel("Simple Moving Average", verbatimTextOutput("five")),
      tabPanel("Weighted Moving Average", verbatimTextOutput("five"))
      ),
    position = c("left"),
    fluid = FALSE
        )
  )
)

server <- function(input, output, session) {
  
#  sn <- check_symbol(input$sb)
  
  # Text input reactive
  data <- reactive(
    get_minly(input$sb)
    #print(sn)
  )

  # Filter data based on inputs

  
  # Histogram output
  output$one <- renderPlot({
    
   # req(is.na(check_symbol(input$sb))[1] == FALSE, cancelOutput = TRUE)

    ggplot(data(), aes(x = Date, y = Price)) +
      geom_line()# +
      ggtitle(paste0("Latest Stock Prices on ", df_minly$Date[1] ," : ", check_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 0.7, face = "bold"))
    
  })
  
  output$two <- renderPrint({
    print("input$sb")
  })
  
  # Summary stat output
#  output$stats <- renderPrint({
  #  req(input$dest != "-", cancelOutput = TRUE)
  #  summary(f_dat()$arr_delay)
#  })
  
  # Monthly flights output
#  output$mon <- renderPrint({
  # req(input$dest != "-", cancelOutput = TRUE)
  #  f_dat() %>% 
  #   group_by(month) %>% 
  #  summarize(n=n())
#  })
  
}

shinyApp(ui, server)
