library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(DT)
library(tidyquant)

rm(list = ls())

### SETUP ######################################
source("datamanagement.R")
source("f_getTable.R")
source("f_getScatter_v2.R")
source("f_getRetsTS.R")
source("f_getRetsStats.R")

dims <- data.frame(Name = c("Manager", "Asset Class", "Region", "Style", "Fund"),
                   Codes = c("mgrName", "AssetClass", "Region", "Style", "FundName"),
                   stringsAsFactors = F)

################################################

ui <- fluidPage(theme=shinytheme("lumen"),
                useShinyjs(),
                fluidRow(
                  column(2,
                         br(), br(),
                         div(img(src = "logo.png", width = "100")), style="text-align: center;"),
                  column(10,
                         titlePanel("Managed account returns dashboard"),
                         h5("MIFL manager -> internal sleeves"),
                         h5("non-MIFL manager -> external delegates"),
                         h6("Data as per Fusion (Portfolio) and Rimes (SAA) extracts", style ="color: red;"),
                         hr())
                ),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
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
                                sort(unique(MAP$mgrName)),
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
                    fluidRow(column(3, dateInput("refDate", "Reference date", value = max(RETS$Date), 
                              format = "d-M-yy", width = "100px",
                              weekstart = 1)),
                             column(3, selectInput("datesGroup",
                                                   "Periods: 1 or more",
                                                   c("1d", "1w", "1m", "3m", "6m", "MtD", "YtD", "QtD", "SI"),
                                                   multiple = T,
                                                   selected = c("1d", "1w", "MtD", "YtD", "QtD", "SI")))),
<<<<<<< HEAD
                    div(paste("Main Table: Click on the table to get the chart/CAPM statistics of that",
                              "delegate returns for the time frame specificed."), style ="color: red;"),
=======
>>>>>>> 0816470966aa14d30ef8596d74d5e0b1b01e4f53
                    div(DTOutput("table"), style = "font-size:70%"),
                    br(),
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Charts/Stats",
                               br(),
                               fluidRow(column(3, selectInput("chartFrame", "Select time frame:", 
                                                              choices = c("1d", "1w", "1m", "3m", "6m", 
                                                                          "MtD", "YtD", "QtD", "SI", "Custom ..."),
                                                              selected = "YtD",
                                                              multiple = F)),
                                        column(2, dateInput("startCust", "Start Date:", value = as.Date("2020-12-31"),
                                                            format = "d-M-yy", width = "100px", weekstart = 1))),
                               fluidRow(column(6, plotOutput("scatter")),
                                        #column(6, verbatimTextOutput("startDate")),
                                        #column(6, verbatimTextOutput("tableSelection")),
                                        column(6, plotOutput("retsTS"))),
                               br(),
                               h4("CAPM statistics on weekly returns"),
                               h6("(Returns, Alphas and Tracking error x 100)"),
                               div(tableOutput("selectStats"), style = "font-size:70%")),
                      tabPanel("Delegates full map",
                               div(dataTableOutput("fullMap"), style = "font-size:70%"))
                      )
                  , width = 8))
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
  
  observeEvent(input$chartFrame, {
    if(input$chartFrame == "Custom ...") {
      enable("startCust")
    } else {
      stCust <- datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
      if (is.na(stCust)) {
        stCust <- max(RETS$Date[RETS$Date <= as.Date(format(input$refDate, "%Y-01-01"))-1])
      }
      
      updateDateInput(session, "startCust", 
                      label = "Start date:",
                      value = stCust)
      disable("startCust")
    }
  })
  
  datesResult <- reactiveValues(datesFrame = 0)
  tableData <- reactiveValues(fullMap = 0) 
  framesSelected <- reactiveValues(selFrames = 0)
  
  observeEvent(input$datesGroup, {framesSelected$selFrames <- input$datesGroup})
  
  observeEvent(input$refDate, {
    d1 <- max(RETS$Date[RETS$Date <= (input$refDate-1)])
    w1 <- max(RETS$Date[RETS$Date <= (input$refDate-7)])
    m1 <- max(RETS$Date[RETS$Date <= (input$refDate-months(1))])
    m3 <- max(RETS$Date[RETS$Date <= (input$refDate-months(3))])
    m6 <- max(RETS$Date[RETS$Date <= (input$refDate-months(6))])
    QtD <- max(RETS$Date[RETS$Date <= (yq(quarter(input$refDate, with_year = TRUE)) - days(1))])
    MtD <- max(RETS$Date[RETS$Date <= as.Date(format(input$refDate, "%Y-%m-01"))-1])
    YtD <- max(RETS$Date[RETS$Date <= as.Date(format(input$refDate, "%Y-01-01"))-1])
    
    datesResult$datesFrame <- data.frame(Label = c("1d", "1w", "1m", "3m", "6m","MtD", "YtD", "QtD"),
                                         Date = c(d1, w1, m1, m3, m6, MtD, YtD, QtD),
                                         stringsAsFactors = F)
    rm(d1, w1, m1, m3, m6, QtD, MtD, YtD)
    })
  
  output$groupings <- renderText({
    paste("Selected groups:", paste(input$Group1, input$Group2, input$Group3, input$Group4, sep = "|"))
    
  })
  
  output$dates <- renderTable(datesResult$datesFrame %>%
                                mutate(Date = format(Date, "%d-%h-%y")))
  
  output$fullMap <- renderDataTable(
    MAP %>%
      {if (input$filter2 == "Main") filter(., IsRepresentative) else .} %>%
      {if (input$filter3 == "Live") filter(., is.na(EndDate)|EndDate > input$refDate) else .} %>%
      select(DelCode, SAACode = RimesBlendID, AM = mgrName, AssetClass, Region, Style, StartDate, EndDate,
             SAAdef,FundName),
    rownames = FALSE,
    filter= "bottom",
    class = "compact")
  
  output$table <- renderDT({
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
  
    tableData$fullMap <- f_getTable(groups, input$filter1, input$filter2, input$filter3,
               input$refDate, datesResult$datesFrame, input$chartFrame, input$datesGroup)
    
    return(tableData$fullMap)
    },
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = ncol(tableData$fullMap)-3 * length(framesSelected$selFrames), 'IDs'),
          if("1d" %in% framesSelected$selFrames) th(colspan = 3, '1 Day'),
          if("1w" %in% framesSelected$selFrames) th(colspan = 3, '1 Week'),
          if("1m" %in% framesSelected$selFrames) th(colspan = 3, '1 Month'),
          if("3m" %in% framesSelected$selFrames) th(colspan = 3, '3 Months'),
          if("6m" %in% framesSelected$selFrames) th(colspan = 3, '6 Months'),
          if("MtD" %in% framesSelected$selFrames) th(colspan = 3, 'MtD'),
          if("QtD" %in% framesSelected$selFrames) th(colspan = 3, 'QtD'),
          if("YtD" %in% framesSelected$selFrames) th(colspan = 3, 'YtD'),
          if("SI" %in% framesSelected$selFrames) th(colspan = 3, 'SI')),
        tr(
          lapply(c(names(tableData$fullMap[1:(ncol(tableData$fullMap)-3*length(framesSelected$selFrames))]),
                   rep(c('Del', 'SAA', 'ER'), length(framesSelected$selFrames))), th)
        )
      )
    )),
    ######selection = "single", #list(target= "cell"), ### TAKE THIS OUT FOR MULTIPLE SELECTION (DEFAULT)
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE,
    filter= "bottom",
    class = "compact cell-border")#,
    #caption = paste('Main Table: Click on the table to get the chart/CAPM statistics of that',
    #                 'delegate returns for the time frame specificed.'))
  
  output$startDate <- renderPrint({
    req(length(input$table_cell_clicked) > 0)

    startDate <- if(input$chartFrame == "SI") {
      as.Date("1900-01-01")
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(tableData$fullMap[as.numeric(input$table_cell_clicked["row"]),"DelCode"])
  })
  
  output$tableSelection <- renderPrint({
    #req(length(input$table_cell_clicked) > 0)
    #return(input$table_rows_selected)
    #return(as.data.frame(tableData$fullMap[,1]))
    #return(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))
  })
  
  output$retsTS <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      as.Date("1900-01-01")
    } else if (input$chartFrame == "Custom ...") {
      as.Date(input$startCust)
      } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(f_getRetsTS(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
                       refDate = format(as.Date(input$refDate), "%Y-%m-%d"), 
                       startDate = format(as.Date(startDate), "%Y-%m-%d")))
  })
  
  output$scatter <- renderPlot({
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
    
    f_getScatter(as.data.frame(tableData$fullMap[,1]), 
                 input$refDate, datesResult$datesFrame, 
                 input$chartFrame, input$startCust)
    
  })
  
  output$selectStats <- renderTable({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      as.Date("1900-01-01")
    } else if (input$chartFrame == "Custom ...") {
      as.Date(input$startCust)
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
  
    # startDate <- if(input$chartFrame == "SI") {
    #   as.Date("1900-01-01")
    # } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getRetsStats(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
                   refDate = format(input$refDate, "%Y-%m-%d"), 
                   startDate = format(as.Date(startDate), "%Y-%m-%d"))
  })
}

shinyApp(ui = ui, server = server)