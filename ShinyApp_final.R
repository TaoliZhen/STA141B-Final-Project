
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
    head(380) %>% 
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
    transmute(
           lag = as.numeric(replace_na(lag, "0")),
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
    transmute(
           lagf = as.numeric(replace_na(lag, "0")),
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
      p("You may try PDD, LYFT, NIO, IQ, TSLA to get a taste of the WebApp. These stocks are relatively fast to load."),
      p("It takes a while to load data, please be patient if entered a stock with a long history such as AAPL."),
      p("Due to the limitation of the my free API access which only allow 5 queries per minute, 
        checking more than one stock in a minute will cause errors as checking one stock will make 4 queries."),
      p("When an error occurs, wait ONE minute and click the \"Show Results\" button again."),
      actionButton(inputId ="go", label = "Show Results"),
    ),
  
  # Output panel
    mainPanel(
      textOutput("checksb"),
      tabsetPanel(
        tabPanel("Latest Trade Day", plotOutput("one"), verbatimTextOutput("onetwo")),
        tabPanel("Six Months", plotOutput("two"), verbatimTextOutput("twotwo")),
        tabPanel("One Year", plotOutput("three"), verbatimTextOutput("threetwo")),
        tabPanel("Historic", plotOutput("four"), verbatimTextOutput("fourtwo")),
        tabPanel("Weighted Moving Average", plotOutput("five"), textOutput("fivetwo"))
      ),
      position = c("left"),
      fluid = FALSE
    )
  )
)

server <- function(input, output) {
  
  output$checksb <- renderText({
    req(is.na(get_symbol(input$sb)[1]))
    print("The stock symbol you entered is not found, please double check and try again.")
  })
    
  # Data for the first tap "Latest Trade Day"
  data_1 <- eventReactive(input$go, {
    dat = get_symbol(input$sb)
    req(is.na(dat[1]) == FALSE)
    get_minly(dat$symbol)
  })
  
  # Data and for the second to fourth tap "Six Months"
  data_2 <- eventReactive(input$go, {
    dat = get_symbol(input$sb)
    req(is.na(dat[1]) == FALSE)
    get_daily(dat$symbol)
  })
  
  # Data for the fifth tap "Weighted Moving Average"
  data_3 <- eventReactive(input$go, {
    dat = get_symbol(input$sb)
    req(is.na(dat[1]) == FALSE)
    df_1 = get_wma5(dat$symbol)
    df_2 = get_wma30(dat$symbol)
    bind_cols(df_1, df_2)
  })
  
  # Output for the first tap "Latest Trade Day"
  g_1 <- eventReactive(input$go,{
    req(is.na(get_symbol(input$sb)[1]) == FALSE)
    ggplot(data_1(), aes(x = Date, y = Price)) +
      geom_line()  +
      ggtitle(paste0("Latest Stock Prices on ", data_1()$Date[1] ,": ", get_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 0.7, face = "bold"))
  })
  output$one <- renderPlot({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    g_1()
  })
  output$onetwo <- renderPrint({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    summary(data_1()$Price)
  })
  
  # Output for the second tab
  g_2 <- eventReactive(input$go,{
    req(is.na(get_symbol(input$sb)[1]) == FALSE)
    ggplot(data_2() %>% head(127), aes(x = Date, y = Price)) +
      geom_line() +
      ggtitle(paste0("Stock Prices for the Past 6 Months: ", get_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 1, face = "bold"))
  })
  output$two <- renderPlot({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    g_2()
  })
  output$twotwo <- renderPrint({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    summary(data_2()$Price %>% head(127))
  })
  
  # Output for the third tap "One Year"
  g_3 <- eventReactive(input$go,{
    req(is.na(get_symbol(input$sb)[1]) == FALSE)
    ggplot(data_2() %>% head(253), aes(x = Date, y = Price)) +
      geom_line() +
      ggtitle(paste0("Stock Prices for the Past Year: ", get_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 1, face = "bold"))
  })
  output$three <- renderPlot({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    g_3()
  })
  output$threetwo <- renderPrint({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    summary(data_2()$Price %>% head(253))
  })
  
  # Output for the forth tap "Historic"
  g_4 <- eventReactive(input$go,{
    req(is.na(get_symbol(input$sb)[1]) == FALSE)
    ggplot(data_2(), aes(x = Date, y = Price)) +
      geom_line() +
      ggtitle(paste0("Historic Stock Prices: ", get_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 1, face = "bold"))
  })
  output$four <- renderPlot({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    g_4()
  })
  output$fourtwo <- renderPrint({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    summary(data_2()$Price)
  })
  
  # Output for the fifth tap "Weighted Moving Average"
  g_5 <- eventReactive(input$go,{
    req(is.na(get_symbol(input$sb)[1]) == FALSE)
    ggplot() +
      geom_line(data = data_3(), mapping = aes(x = lag, y = value, color = "WMA5")) +
      geom_line(data = data_3(), mapping = aes(x = lagf, y = valuef, color = "WMA30")) +
      scale_x_reverse() +
      labs(color = "MA: Days in Period") +
      ggtitle(paste0("Weighted Moving Average: ", get_symbol(input$sb)$stock_name)) +
      theme(plot.title = element_text(lineheight = 1, face = "bold"))
  })
  output$five <- renderPlot({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    g_5()
  })
  output$fivetwo <- renderText({
    req(is.na(get_symbol(input$sb)[1]) == FALSE, cancelOutput = TRUE)
    print("When WMA5 crosses WMA30 on a upward trend, it may indicates a buying opportunity. 
          When WMA5 crosses WMA30 on a downward trend, you might want to set a loss limit.")
  })
  
}

shinyApp(ui = ui, server = server)
