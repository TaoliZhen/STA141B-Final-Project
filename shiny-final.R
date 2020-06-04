# 03-reactive

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
readRenviron(".Renviron")

stock <- read_csv("stock_symbols.csv")
get_symbol <- function(syb){
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

get_daily <- function(syb){
  r <- GET(
    "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY",
    query = list(
      symbol = syb,
      outputsize = "full",
      apikey = Sys.getenv("AVS_API_KEY")
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  df_daily <- fromJSON(json, flatten = TRUE) %>% 
    as.data.frame() %>%
    select(symbol = ends_with("Symbol"), ends_with("close")) %>% 
    pivot_longer(
      starts_with("Time.Series..Daily.."),
      names_to = "Date",
      names_prefix = "Time.Series..Daily.."
    ) %>% 
    separate(col = "Date", c("year", "month","date"), sep = "[.]") %>% 
    unite(col = "Date", 2:4, sep = "/") %>% 
    mutate(
      Date = gsub("^.........$","", Date),
      Date = as.Date(Date),
      Price = as.numeric(as.character(value))
    )
  df_daily
}

get_wma5 <- function(syb){
  r <- GET(
    "https://www.alphavantage.co/query?function=WMA",
    query = list(
      symbol = syb,
      interval = "daily",
      time_period = 5,
      series_type = "close",
      apikey = Sys.getenv("AVS_API_KEY")
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  df_wma5 <- fromJSON(json, flatten = TRUE) %>% 
    as.data.frame() %>%
    select(symbol = ends_with("Symbol"), time_point = ends_with("Type"), starts_with("Technical.Analysis")) %>% 
    pivot_longer(
      starts_with("Technical.Analysis"),
      names_to = "ma_lag",
      names_prefix = "Technical.Analysis..WMA."
    ) %>% 
    head(91) %>% 
    separate(col = "ma_lag", c("indicator", "lag"), sep = "[.]") %>% 
    mutate(lag = replace_na(lag, "0"),
           lag = as.numeric(lag),
           value = as.numeric(as.character(value))
    )
  df_wma5
}

get_wma30 <- function(syb){
  r <- GET(
    "https://www.alphavantage.co/query?function=WMA",
    query = list(
      symbol = syb,
      interval = "daily",
      time_period = 30,
      series_type = "close",
      apikey = "G3O0P1B2VGOZYKZN"
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  df_wma30 <- fromJSON(json, flatten = TRUE) %>% 
    as.data.frame() %>%
    select(symbol = ends_with("Symbol"), time_point = ends_with("Type"), starts_with("Technical.Analysis")) %>% 
    pivot_longer(
      starts_with("Technical.Analysis"),
      names_to = "ma_lag",
      names_prefix = "Technical.Analysis..WMA."
    ) %>% 
    head(91) %>% 
    separate(col = "ma_lag", c("indicator", "lag"), sep = "[.]") %>% 
    mutate(lag = replace_na(lag, "0"),
           lagf = as.numeric(lag),
           valuef = as.numeric(as.character(value))
    )
  df_wma30
}

ui <- fluidPage(
  
  titlePanel("Stock Price"),
  
  # Choose a overall layout
  sidebarLayout(
    
  # Choose a input panel
    sidebarPanel(

      textInput(inputId = "sb",
        label = "Enter Stock Symbol",
        value = "PDD"),
      p("You may try PDD, LYFT, NIO, IQ ,TSLA to get a taste of the WebApp. These stocks are relatively fast to load."),
      p("It takes a while to load datas, please be patient if entered a stock with a long history such as AAPL."),
      p("It may also takes a little bit to reload when entered a new stock. Switch stocks too quickly might cause a connection lost to the data base."),
      actionButton(inputId ="go", label = "Show Results"),
    ),
  
  # Output panel
    mainPanel(
      tabsetPanel(
        tabPanel("Latest Trade Day", plotOutput("one"), verbatimTextOutput("onetwo")),
        tabPanel("Six Months", plotOutput("two"), verbatimTextOutput("twotwo")),
        tabPanel("One Year", plotOutput("three"), verbatimTextOutput("threetwo")),
        tabPanel("Historic", plotOutput("four"), verbatimTextOutput("fourtwo")),
        tabPanel("Weighted Moving Average", plotOutput("five"), verbatimTextOutput("fivetwo"))
      ),
      position = c("left"),
      fluid = FALSE
    )
  )
)

server <- function(input, output) {

  # Data and Output for the first tap "Latest Trade Day"
  data_1 <- eventReactive(input$go, {
    dat <- get_symbol(input$sb)
    get_minly(dat$symbol)
  })
  output$one <- renderPlot({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      cancelOutput = TRUE
    } else {
      ggplot(data_1(), aes(x = Date, y = Price)) +
        geom_line()  +
        ggtitle(paste0("Latest Stock Prices on ", data_1()$Date[1] ,": ", dat$stock_name)) +
        theme(plot.title = element_text(lineheight = 0.7, face = "bold"))
    }
  })
  output$onetwo <- renderPrint({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      print("Sorry the symbol you enter is not found, please double check and try again.")
    } else {
      summary(data_1()$Price)
    }
  })
  
  # Data and Output for the second tap "Six Months"
  data_2 <- eventReactive(input$go, {
    dat <- get_symbol(input$sb)
    get_daily(dat$symbol)
  })
  output$two <- renderPlot({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      cancelOutput = TRUE
    } else {
      ggplot(data_2() %>% head(127), aes(x = Date, y = Price)) +
        geom_line() +
        ggtitle(paste0("Stock Prices for the Past 6 Months: ", dat$stock_name)) +
        theme(plot.title = element_text(lineheight = 1, face = "bold"))
    }
  })
  output$twotwo <- renderPrint({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      print("Sorry the symbol you enter is not found, please double check and try again.")
    } else {
      summary(data_2()$Price %>% head(127))
    }
  })
  
  # Output for the third tap "One Year"
  output$three <- renderPlot({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      cancelOutput = TRUE
    } else {
      ggplot(data_2() %>% head(253), aes(x = Date, y = Price)) +
        geom_line() +
        ggtitle(paste0("Stock Prices for the Past Year: ", dat$stock_name)) +
        theme(plot.title = element_text(lineheight = 1, face = "bold"))
    }
  })
  output$threetwo <- renderPrint({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      print("Sorry the symbol you enter is not found, please double check and try again.")
    } else {
      summary(data_2()$Price %>% head(253))
    }
  })
  
  # Output for the forth tap "Historic"
  output$four <- renderPlot({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      cancelOutput = TRUE
    } else {
      ggplot(data_2(), aes(x = Date, y = Price)) +
        geom_line() +
        ggtitle(paste0("Historic Stock Prices: ", dat$stock_name)) +
        theme(plot.title = element_text(lineheight = 1, face = "bold"))
    }
  })
  output$fourtwo <- renderPrint({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      print("Sorry the symbol you enter is not found, please double check and try again.")
    } else {
      summary(data_2()$Price)
    }
  })
  
  # Data and Output for the fifth tap "Weighted Moving Average"
  data_3 <- eventReactive(input$go, {
    dat <- get_symbol(input$sb)
    get_wma5(dat$symbol)
  })
  data_4 <- eventReactive(input$go, {
    dat <- get_symbol(input$sb)
    get_wma30(dat$symbol)
  })

  output$five <- renderPlot({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      cancelOutput = TRUE
    } else {
      ggplot() +
        geom_line(data = data_3(), mapping = aes(x = lag, y = value, color = "WMA5")) +
        geom_line(data = data_4(), mapping = aes(x = lagf, y = valuef, color = "WMA30")) +
        scale_x_reverse() +
        labs(color = "MA Days in Period") +
        ggtitle(paste0("Weighted Moving Average: ", dat$stock_name)) +
        theme(plot.title = element_text(lineheight = 1, face = "bold"))
    }
  })
  
  output$fivetwo <- renderPrint({
    dat <- get_symbol(input$sb)
    if (is.na(dat[1]) == TRUE){
      print("Sorry the symbol you enter is not found, please double check and try again.")
    } else {
      print("When WMA5 crosses WMA30 on a upward trend, it may indicates a buying opportunity.")
      print("When WMA5 crosses WMA30 on a downward trend, you might want to set a loss limit.")
    }
  })
  
}

shinyApp(ui = ui, server = server)
