library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

rm(list = ls())

### SETUP ######################################
source("datamanagement.R")
source("f_getTable.R")
source("f_getScatter.R")
source("f_getRetsTS.R")

dims <- data.frame(Name = c("Manager", "Asset Class", "Region", "Style", "Fund"),
                   Codes = c("mgrName", "AssetClass", "Region", "Style", "FundName"),
                   stringsAsFactors = F)

################################################

ui <- fluidPage(theme=shinytheme("lumen"),
                useShinyjs(),
                fluidRow(
                  column(3,br(),
                         img(src = "logo.png", width = "100")),
                  column(9,
                         titlePanel("Managed account returns dashboard"),
                         h4("MIFL = internal sleeves"),
                         h4("non-MIFL = external delegates"),
                         hr())
                ),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    # radioButtons("filter1",
                    #              "Manager Filter",
                    #              choices = c("Internal", "All"),
                    #              selected = "Internal"),
                    radioButtons("filter2",
                                 "Representative Filter",
                                 choices = c("Main", "All"),
                                 selected = "Main"),
                    radioButtons("filter3",
                                 "Live Filter",
                                 choices = c("Live", "All"),
                                 selected = "Live"),
                    hr(),
                    selectInput("filter1",
                                "Managers: 1 or more",
                                unique(MAP$mgrName),
                                multiple = T,
                                selected = "MIFL"),
                    hr(),
                    selectInput("Group1", 
                                "Grouping 1:", 
                                dims$Name, 
                                multiple = F,
                                selected = "Manager"),
                    selectInput("Group2", 
                                "Grouping 2:", 
                                choices = c("", dims$Name), 
                                multiple = F,
                                selected = NULL,
                                ),
                    selectInput("Group3", 
                                "Grouping 3:", 
                                choices = c("", dims$Name),
                                multiple = F,
                                selected = NULL),
                    selectInput("Group4", 
                                "Grouping 4:", 
                                choices = c("", dims$Name),
                                multiple = F,
                                selected = NULL),
                    width = 2), 
                  mainPanel = mainPanel(
                    fluidRow(dateInput("refDate", "Reference date", value = max(RETS$Date), 
                              format = "d-M-yy", width = "100px",
                              weekstart = 1)),
                             #tableOutput("dates")),
                    #fluidRow(div(DT::dataTableOutput("table"), style = "font-size:60%")),
                    div(DT::dataTableOutput("table"), style = "font-size:60%"),
                    h5("Click on the table to get the chart of that delegate returns for the time frame specificed below"),
                    #verbatimTextOutput("selected"),
                    br(),
                    selectInput("chartFrame", "Select time frame for chart:", 
                                choices = c("1 day", "1 week", "MtD", "YtD", "QtD", "SI"),
                                selected = "YtD",
                                multiple = F),
                    fluidRow(column(6, plotOutput("scatter")),
                             #column(6, verbatimTextOutput("startDate")))
                             column(6, plotOutput("retsTS")))
                  )
                )
)

server <- function(input, output, session) {
  observeEvent(input$Group1, {
    if(nchar(input$Group1) > 0){
      enable("Group2")
      updateSelectInput(session,
                        inputId = "Group2",
                        choices = c("", setdiff(dims$Name, input$Group1)),
                        selected = NULL)
      updateSelectInput(session, inputId = "Group3", choices = c("", dims$Name), selected = NULL)
      updateSelectInput(session, inputId = "Group4", choices = c("", dims$Name), selected = NULL)
      disable("Group3")
      disable("Group4")
    } else {
      disable("Group2")
      disable("Group3")
      disable("Group4")
    }})
  observeEvent(input$Group2, {
    if(nchar(input$Group2) > 0){
      enable("Group3")
      updateSelectInput(session,
                        inputId = "Group3",
                        choices = c("", setdiff(dims$Name, 
                                                c(input$Group1, input$Group2))),
                        selected = NULL)
      updateSelectInput(session, inputId = "Group4", choices = c("", dims$Name), selected = NULL)
      disable("Group4")
    } else {
      disable("Group3")
      disable("Group4")
    }})
  observeEvent(input$Group3, {
    if(nchar(input$Group3) > 0){
      enable("Group4")
      updateSelectInput(session,
                        inputId = "Group4",
                        choices = c("", setdiff(dims$Name, 
                                                c(input$Group1, input$Group2, input$Group3))),
                        selected = NULL)
    } else {
      disable("Group4")
    }})
  
  datesResult <- reactiveValues(datesFrame = 0)
  tableData <- reactiveValues(fullMap = 0) 
  
  observeEvent(input$refDate, {
    d1 <- max(RETS$Date[RETS$Date <= (input$refDate-1)])
    w1 <- max(RETS$Date[RETS$Date <= (input$refDate-7)])
    #y1 <- max(RETS$Date[RETS$Date <= (input$refDate %m-% months(12))])
    QtD <- max(RETS$Date[RETS$Date <= (yq(quarter(input$refDate, with_year = TRUE)) - days(1))])
    MtD <- max(RETS$Date[RETS$Date <= as.Date(format(input$refDate, "%Y-%m-01"))-1])
    YtD <- max(RETS$Date[RETS$Date <= as.Date(format(input$refDate, "%Y-01-01"))-1])
    
    datesResult$datesFrame <- data.frame(Label = c("1 day", "1 week", "MtD", "YtD", "QtD"), 
                                         Date = c(d1, w1, MtD, YtD, QtD),
                                         stringsAsFactors = F)
    rm(d1, w1, QtD, MtD, YtD)
    })
  
  output$groupings <- renderText({
    paste("Selected groups:", paste(input$Group1, input$Group2, input$Group3, input$Group4, sep = "|"))
    
  })
  
  output$dates <- renderTable(datesResult$datesFrame %>%
                                mutate(Date = format(Date, "%d-%h-%y")))
  
  output$table <- DT::renderDataTable({
    
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
  
    tableData$fullMap <- f_getTable(groups, input$filter1, input$filter2, input$filter3,
               input$refDate, datesResult$datesFrame, input$chartFrame)
    
    return(tableData$fullMap)
    }, 
    selection = "single", #list(target= "cell"),
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE)
  
  output$startDate<- renderPrint({
    req(length(input$table_cell_clicked) > 0)
    #input$table_cell_clicked["value"]
    
    startDate <- if(input$chartFrame == "SI") {
      as.Date("1900-01-01")
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(tableData$fullMap[as.numeric(input$table_cell_clicked["row"]),"DelCode"])
    # return(c(startDate = format(as.Date(startDate), "%d-%m-%Y"), 
    #          delegate = input$table_cell_clicked["value"], 
    #          refDate = format(input$refDate, "%d-%m-%Y"), 
    #          frame = input$chartFrame))
  })
  
  output$retsTS <- renderPlot({
    req(length(input$table_cell_clicked) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      as.Date("1900-01-01")
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(f_getRetsTS(delCode = as.character(tableData$fullMap[as.numeric(input$table_cell_clicked["row"]),
                                                                "DelCode"]),
                          #input$table_cell_clicked["value"], 
                       refDate = format(input$refDate, "%Y-%m-%d"), 
                       startDate = format(as.Date(startDate), "%Y-%m-%d"), 
                       chartFrame = input$chartFrame))
    
  })
  
  output$scatter <- renderPlot({
    
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
    
    f_getScatter(groups, input$filter1, input$filter2, input$filter3,
                 input$refDate, datesResult$datesFrame, input$chartFrame)
    
  })
}

shinyApp(ui = ui, server = server)