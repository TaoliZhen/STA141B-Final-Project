# 03-reactive

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
readRenviron(".Renviron")

stock <- read_csv("stock_symbols.csv")
check_symbol <- function(syb){
  # A function to check the input exits in the US stock market.
  if (toupper(syb) %in% stock$symbol){
    df = stock %>% filter(symbol == toupper(syb))
    return(df)
  } else {
    #return("Sorry the symbol you enter is not found, please double check and try again.")
    return(NA)
  }
}

get_minly <- function(syb){
  r <- GET(
    "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY",
    query = list(
      symbol = syb,
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
  df
}




ui <- fluidPage(
  textInput(inputId = "sb",
    label = "Enter Stock Symbol",
    value = "AAPL"),
  plotOutput("hist"),
  actionButton(inputId ="go", label = "Show Results"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {

  data <- eventReactive(input$go, {
    get_minly(input$sb)
  })

  output$hist <- renderPlot({
    ggplot(data(), aes(x = Date, y = Price)) +
      geom_line() # +
      #ggtitle(paste0("Latest Stock Prices on ", df_minly$Date[1] ," : ", check_symbol(input$sb)$stock_name)) +
      #theme(plot.title = element_text(lineheight = 0.7, face = "bold"))
  })
  output$stats <- renderPrint({
    summary(data()$Price)
  })
}

shinyApp(ui = ui, server = server)
