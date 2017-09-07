## app.R ##
library(shinydashboard)
library(RColorBrewer)
library(plotly)

source("./global.R")

ui <- dashboardPage(
   dashboardHeader(title = "Crypto Portfolio"),
   dashboardSidebar(
      sidebarMenu(
         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("Info", tabName = "info", icon = icon("info"))
      )
   ),
   dashboardBody(
      tabItems(
         # First tab 
         tabItem(tabName = "dashboard",
                 fluidRow(
                    box(plotlyOutput("historical_chart"), width = 12),
                    box(
                       title = "Top 300",
                       selectizeInput(inputId = 'select.coins', 
                                      label= "Select your cryptos", 
                                      choices= coins$slug, 
                                      multiple = TRUE),
                       dateRangeInput(
                          'dateRange',
                          label =  h3(HTML("<i class='glyphicon glyphicon-calendar'></i> Date Range")),
                          start = Sys.Date() - 30*2, end = Sys.Date()-1,
                          max = Sys.Date(),
                          separator = " - ", format = "dd/mm/yy",
                          startview = 'day')), 
                    actionButton("run.query", HTML("<i class='glyphicon glyphicon-send'></i> Run")), 
                    br(), br(), bookmarkButton(),  
                    verbatimTextOutput("dateRangeText2")
                 )
         ),
         
         # Second tab content
         tabItem(tabName = "info",
                 h2("About")
         )
      )
   )
)

server <- function(input, output, session) {
   updateSelectizeInput(session, selected = 'ethereum',
                        inputId = 'select.coins', 
                        choices = coins$slug, 
                        server = TRUE)
   
   data <- eventReactive(input$run.query,{
      # Convert dates to format for the scraper
      start <- gsub(x = input$dateRange[1], pattern = "-", replacement = "")
      end   <- gsub(x = input$dateRange[2], pattern = "-", replacement = "")

      data <- download_crypto_data_byName(
             cpu   = 3, 
             start = start, 
             end   = end,
             names = input$select.coins)
      # Debug
      # load("./data/dummy_data.RData")
      # data <- data %>%  filter(name %in% input$select.coins)
      return(data)
   })
   
   output$historical_chart <- renderPlotly({
      plot_ly(data = data(), x = ~date, y = ~close, 
              color = ~name, text = ~name, 
              mode = 'lines') 
   })
}

shinyApp(ui, server,  enableBookmarking = "url")