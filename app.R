## TO DO
### Keep checking dates might still be a problem (leave the display of the data frame)

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyauthr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(DT)
library(tidyquant)
library(janitor)
library(ggcorrplot)
library(Microsoft365R)
library(shinyWidgets)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)

### SETUP ######################################
source("datamanagement.R")
source("f_getTable.R")
source("f_getScatter_v2.R")
source("f_getRetsTS.R")
source("f_getRetsStats.R")
source("f_getDiscPeriod.R")
source("f_getAUM.R")
source("f_getFundA.R")
source("f_getSelCorr.R")
source("f_getRanks.R")
source("f_delComp.R")
source("f_RBC_Fus.R")
source("f_RBC_Fus_rets.R")

dims <- data.frame(Name = c("Manager", "Asset Class", "Region", "Style", "Fund"),
                   Codes = c("mgrName", "AssetClass", "Region", "Style", "FundName"),
                   stringsAsFactors = F)

################################################

ui <- fluidPage(theme=shinytheme("lumen"),
                htmlwidgets::getDependency('sparkline'),
                useShinyjs(),
                
                # logout button
                div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                # login section
                shinyauthr::loginUI(id = "login"),
                
                fluidRow(
                  column(2,
                         br(), br(),
                         div(img(src = "logo.png", width = "100")),
                         br(), br(), style="text-align: center;"),
                  column(10,
                         titlePanel("Managed account returns dashboard"),
                         radioButtons("MainRetSource",
                                      "Main returns source (NOT IMPLEMENTED - RBC currently is main returns source):",
                                      choices = c("RBC", "FUSION"),
                                      selected = "RBC",
                                      inline = T,
                                      width = NULL),
                         #h5("MIFL manager -> internal sleeves"),
                         #h5("non-MIFL manager -> external delegates"),
                         #h6("Data as per Fusion (Portfolio) and Rimes (SAA) extracts", style ="color: red;"),
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
                    radioButtons("filter4",
                                 "Include accounts with issues?",
                                 choices = c("Yes", "No"),
                                 selected = "Yes"),
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
                    fluidRow(column(2, dateInput("refDate", "Reference date", value = max(RBCidxData$Date), 
                              format = "d-M-yy", width = "100px",
                              weekstart = 1)),
                             column(3, selectInput("datesGroup",
                                                   "Periods: 1 or more",
                                                   c("1d", "1w", "1m", "3m", "6m", "1y","MtD", "YtD", "QtD", "SI"),
                                                   multiple = T,
                                                   selected = c("1d", "1w", "MtD", "YtD", "QtD", "SI"))),
                             column(3, br(), br(), materialSwitch("annualizedOn", "Annualize (periods over 1 year)", 
                                                            value = T, status = "primary")),
                             column(2, br(), downloadButton("mainTable", "Generate XL")),
                             column(2, br(), downloadButton("fullMainTable", "Generate full XL"))),
                    div(paste("Main Table: Click on the table to get the chart/CAPM statistics of that",
                              "delegate returns for the time frame specificed."), style ="color: red;"),
                    div(DTOutput("table"), style = "font-size:70%"),
                    #br(),
                    #verbatimTextOutput("tableSelection", placeholder = FALSE),
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Charts/Stats",
                               br(),
                               fluidRow(column(3, selectInput("chartFrame", "Select time frame:", 
                                                              choices = c("1d", "1w", "1m", "3m", "6m", "1y",
                                                                          "MtD", "YtD", "QtD", "SI", "Custom ..."),
                                                              selected = "YtD",
                                                              multiple = F)),
                                        column(2, dateInput("startCust", "Start Date:", value = as.Date("2020-12-31"),
                                                            format = "d-M-yy", width = "100px", weekstart = 1)),
                                        column(1, NULL),
                                        column(2, br(),
                                               downloadButton("idxData", "Generate XL")),
                                        column(4, br(),
                                               materialSwitch("cfOn", "Display cashflows", 
                                                              value = F, status = "primary"))),
                               fluidRow(column(6, plotOutput("scatter")),
                                        #column(6, verbatimTextOutput("startDate")),
                                        #column(6, verbatimTextOutput("tableSelection")),
                                        column(6, plotOutput("retsTS"))),
                               br(),
                               h4("CAPM statistics"),
                               h6("(Returns, Alphas and Tracking error x 100)"),
                               div(tableOutput("selectStats"), style = "font-size:70%"),
                               br(),
                               plotOutput("discPeriod"),
                               br(),
                               fluidRow(column(6, h5("Daily absolute returns correlation"),
                                               plotOutput("selAbsCorr")),
                                        column(6, h5("Daily relative returns correlation"),
                                               plotOutput("selRelCorr"))),
                               hr(),
                               h4("Comparison Fusion-based RBC-based daily returns"),
                               plotOutput("retsTSFusRBC"),
                               hr(),
                               h4("Comparison with selected returns and delegates-submitted monthly returns"),
                               plotOutput("delDataComp")),
                      tabPanel("AUM",
                               br(),
                               fluidRow(column(6, plotOutput("AUMmgr")),
                                        column(6, plotOutput("AUMstrat"))),
                               fluidRow(column(6, plotOutput("AUMstratDet")),
                                        column(6, 
                                               br(),
                                               h5("AUM (EUR mn) for reference date above"),
                                               div(tableOutput("AUMlast"), style = "font-size:80%")))),
                      tabPanel("Turnover",
                               br(),
                               fluidRow(column(12, 
                                               br(),
                                               h4("Turnover (UCITS definition) over last 12 months, previous EoM"),
                                               div(dataTableOutput("turnover"), style = "font-size:80%"),
                                               br(),
                                               withMathJax(),
                                               helpText("\\(\\frac{(buy - sell) - (subscriptions - redemptions)}{NAV}\\)"),
                                               div("Only funded instruments considered ex-cash"),
                                               div("RBC data for both holdings and cashflows"),
                                               div("In decimals: 0.01 = 1%"),
                                               br()))),
                      tabPanel("Fund analysis",
                               fluidRow(column(3, selectInput("fundName", "Select Fund:", 
                                                              choices = MAP %>%
                                                                filter(hasRets) %>%
                                                                group_by(FundName) %>%
                                                                summarise(count = n()) %>%
                                                                filter(count > 1) %>%
                                                                select(FundName) %>%
                                                                arrange(),
                                                              selected = "MBB GLOBAL HIGH YIELD",
                                                              multiple = F))),
                               br(),
                               fluidRow(column(6, plotOutput("FundWgtHst")),
                                        column(6, h5("Realized volatility and tracking error"),
                                               tableOutput("volStats"))),
                               br(),
                               fluidRow(textOutput("compCasesNo")),
                               fluidRow(column(6, h5("Correlation of daily absolute returns"), 
                                               plotOutput("absCorr")),
                                        column(6, h5("Correlation of daily relative returns"), 
                                               plotOutput("relCorr")))),
                      tabPanel("Rankings",
                               br(),
                               h4("Only available for internal delegates where relevant"),
                               tableOutput("ranksTable"),
                               plotOutput("ranksChart")),
                      tabPanel("Delegates full map",
                               div(dataTableOutput("fullMap"), style = "font-size:70%"),
                               hr(),
                               div(dataTableOutput("exceptMap"), style = "font-size:80%")),
                      tabPanel("Fusion Checks",
                               br(),
                               div(dataTableOutput("statTable"), style = "font-size:80%"),
                               br(),
                               #verbatimTextOutput("tableSelection", placeholder = FALSE),
                               downloadButton("XLDelCount", "Generate XL"),
                               plotOutput("RBC_Fus"),
                               br(),
                               downloadButton("XLday", "Generate XL"),
                               h4("Check on last date for selected stats date|€mn|percentages"),
                               div(dataTableOutput("lastRBCFus"), style = "font-size:80%")))
                    , width = 8))
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE#,
    #log_out = reactive(logout_init())
  )
  
  # Logout to hide
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  
  showModal(modalDialog(
    
    title = "Data information",
    tags$ul(
      tags$li(paste("Returns available for", length(unique(RETS$DelCode)), "delegates")),
      tags$li(paste("Last checks statistics:", format(max(tTests$StatDate), "%d-%h-%y"))),
      tags$li(paste("Last Fusion returns:", format(max(RETS$Date), "%d-%h-%y"))),
      tags$li(paste("Last RBC returns:", format(max(RBCidxData$Date), "%d-%h-%y"))),
      tags$li(paste("Last RBC cashflow (trade date):", format(max(RBCflows$TradeDate), "%d-%h-%y"))),
      tags$li(paste("Last RBC cashflow (value date):", format(max(RBCflows$ValueDate), "%d-%h-%y"))),
      tags$li("Live accounts without returns:",
              div(paste(MAP[!is.na(MAP$SophisID) &
                        is.na(MAP$EndDate) &
                        !(MAP$DelCode %in% RETS$DelCode)
                        , c("DelCode", "mgrName")], collapse = "\n"))),
      tags$li("(FUSION) Delegates with issues (odd returns/mismatch vs. delegates info):",
              length(EXCP$DelCode)),
      tags$li("(FUSION) Delegates with issues (significant deviation vs. RBC valuations):",
              length(tTests$DelCode[tTests$StatDate == max(tTests$StatDate) &
                                      abs(tTests$t)>=2]))
    )
  ))
  
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
    } else if(input$chartFrame == "SI") {
      stCust <- as.Date("2000-12-31")

      updateDateInput(session, "startCust", 
                      label = "Start date:",
                      value = stCust)
      disable("startCust")
    } else {
      stCust <- datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
      #trick for YtD
      if (is.na(stCust)) {
        stCust <- max(RBCidxData$Date[RBCidxData$Date <= as.Date(format(input$refDate, "%Y-01-01"))-1])
      }
      
      updateDateInput(session, "startCust", 
                      label = "Start date:",
                      value = stCust)
      disable("startCust")
    }
  })
  
  datesResult <- reactiveValues(datesFrame = 0)
  tableData <- reactiveValues(fullMap = 0) 
  tableData <- reactiveValues(allMap = 0)
  #tTests <- reactiveValues(df = 0)
  framesSelected <- reactiveValues(selFrames = 0)
  RBCFUSdayCheck <- reactiveValues(df = 0)
  RBCFUSDelCode <- reactiveValues(df = 0)
  idxRets <- reactiveValues(df = 0)
  
  observeEvent(input$datesGroup, {framesSelected$selFrames <- input$datesGroup})
  
  observeEvent(input$refDate, {
    RBCFUSdayCheck$df <- Del_recon %>%
      filter(Date == max(tTests$Date[tTests$StatDate == as.Date(input$refDate)])) %>%
      #mutate(DelCode = as.numeric(DelCode)) %>%
      left_join(MAP, by = "DelCode") %>%
      mutate(DelCode = as.character(DelCode)) %>%
      select(Date, DelCode, FundName, DelDispName, Fusion = Fus_AUM, RBC = RBC_AUM, 
             Adjustment = NewCfl, Diff = AUMdiff, DiffPerc = AUMdiffPerc) %>%
      arrange(desc(abs(DiffPerc))) %>%
      as.data.frame()
  })
  
  observeEvent(input$refDate, {
    d1 <- max(RBCidxData$Date[RBCidxData$Date <= (input$refDate-1)])
    w1 <- max(RBCidxData$Date[RBCidxData$Date <= (input$refDate-7)])
    # m1 <- max(RETS$Date[RETS$Date <= ceiling_date(input$refDate %m-% months(1), "month")-1])
    # m3 <- max(RETS$Date[RETS$Date <= ceiling_date(input$refDate %m-% months(3), "month")-1])
    # m6 <- max(RETS$Date[RETS$Date <= ceiling_date(input$refDate %m-% months(6), "month")-1])
    m1 <- max(RBCidxData$Date[RBCidxData$Date <= input$refDate %m-% months(1)])
    m3 <- max(RBCidxData$Date[RBCidxData$Date <= input$refDate %m-% months(3)])
    m6 <- max(RBCidxData$Date[RBCidxData$Date <= input$refDate %m-% months(6)])
    y1 <- max(RBCidxData$Date[RBCidxData$Date <= (input$refDate-months(12))])
    QtD <- max(RBCidxData$Date[RBCidxData$Date <= (yq(quarter(input$refDate, with_year = TRUE)) - days(1))])
    MtD <- max(RBCidxData$Date[RBCidxData$Date <= as.Date(format(input$refDate, "%Y-%m-01"))-1])
    YtD <- max(RBCidxData$Date[RBCidxData$Date <= as.Date(format(input$refDate, "%Y-01-01"))-1])
    
    datesResult$datesFrame <- data.frame(Label = c("1d", "1w", "1m", "3m", "6m", "1y", "MtD", "YtD", "QtD"),
                                         Date = c(d1, w1, m1, m3, m6, y1, MtD, YtD, QtD),
                                         stringsAsFactors = F)
    rm(d1, w1, m1, m3, m6, y1, QtD, MtD, YtD)
    })
  
  output$fullMap <- renderDataTable(
    MAP %>%
      {if (input$filter2 == "Main") filter(., IsRepresentative) else .} %>%
      {if (input$filter3 == "Live") filter(., is.na(EndDate)|EndDate > input$refDate) else .} %>%
      mutate(IsRepresentative = ifelse(IsRepresentative == 1, "TRUE", "FALSE"),
             IsFund = ifelse(IsFund == 1, "TRUE", "FALSE"),
             IsAdvisory = ifelse(IsAdvisory == 1, "TRUE", "FALSE")) %>%
      select(DelCode, SAACode = RimesBlendID, AM = mgrName, AssetClass, Region, Style, StartDate, EndDate,
             SAAdef,FundName, Main = IsRepresentative, IsFund, IsAdvisory, hasRets),
    rownames = FALSE,
    filter= "bottom",
    class = "compact")
  
  output$exceptMap <- renderDataTable(
    EXCP,
    rownames = FALSE,
    filter= "bottom",
    class = "compact")
  
  output$table <- renderDataTable(
    {
      req(credentials()$user_auth)
      
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
  
    tableData$fullMap <- f_getTable(groups, input$filter1, input$filter2, input$filter3, input$filter4, 
                                    input$refDate, datesResult$datesFrame, input$chartFrame, input$datesGroup, input$annualizedOn)[[1]]
    tableData$allMap <- f_getTable(groups, input$filter1, input$filter2, input$filter3, input$filter4, 
                                    input$refDate, datesResult$datesFrame, input$chartFrame, input$datesGroup, input$annualizedOn)[[2]]
    
    datatable(tableData$fullMap,
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
          if("1y" %in% framesSelected$selFrames) th(colspan = 3, '1 Year'),
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
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE,
    filter= "bottom",
    class = "compact cell-border") %>%
      formatStyle(columns = which(grepl("ER", colnames(tableData$fullMap))),
                  color = styleInterval(cuts = 0, values = c("red", "green")),
                  fontWeight = "bold")
  })
  
  output$startDate <- renderPrint({
    req(length(input$table_cell_clicked) > 0)

    startDate <- if(input$chartFrame == "SI") {
      as.Date("2000-12-31")
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(tableData$fullMap[as.numeric(input$table_cell_clicked["row"]),"DelCode"])
  })
  
  output$tableSelection <- renderPrint({
    #req(length(input$table_cell_clicked) > 0)
    #as.character(tTests$df[input$statTable_rows_selected,"DelCode"])
    #as.character(input$statTable_rows_selected)
    #return(as.data.frame(tableData$fullMap[,1]))
    #return(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))
    #return(as.data.frame(datesResult$datesFrame))
    #unique(MAP$mgrName[MAP$DelCode %in% as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"])[,1]])
  })
  
  output$retsTSFusRBC <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    return(f_RBC_Fus_rets(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected, "DelCode"]),
                          refDate = format(as.Date(input$refDate), "%Y-%m-%d"), 
                          startDate = format(as.Date(startDate), "%Y-%m-%d"),
                          showCf = input$cfOn))
  })

  
  output$retsTS <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
      } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    idxRets$df <- f_getRetsTS(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected, "DelCode"]),
                              refDate = format(as.Date(input$refDate), "%Y-%m-%d"), 
                              startDate = format(as.Date(startDate), "%Y-%m-%d"),
                              showCf = input$cfOn)[2]
    
    return(f_getRetsTS(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected, "DelCode"]),
                       refDate = format(as.Date(input$refDate), "%Y-%m-%d"), 
                       startDate = format(as.Date(startDate), "%Y-%m-%d"),
                       showCf = input$cfOn)[1])
    
  })
  
  output$scatter <- renderPlot({
    groups <- setdiff(c(dims$Code[dims$Name == input$Group1],
                        dims$Code[dims$Name == input$Group2],
                        dims$Code[dims$Name == input$Group3],
                        dims$Code[dims$Name == input$Group4]),
                      "")
    
    f_getScatter(as.data.frame(tableData$fullMap[,1]), 
                 input$refDate, datesResult$datesFrame, 
                 input$chartFrame, 
                 input$startCust,
                 as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))
    
  })
  
  output$selectStats <- renderTable({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]

    f_getRetsStats(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
                   refDate = format(input$refDate, "%Y-%m-%d"), 
                   startDate = format(as.Date(startDate), "%Y-%m-%d"))
  })
  
  output$discPeriod <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getDiscPeriod(delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
                    refDate = format(input$refDate, "%Y-%m-%d"), 
                    startDate = format(as.Date(startDate), "%Y-%m-%d"))
    })
  
  output$AUMmgr <- renderPlot({
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getAUM(input$filter1, 
             delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
             refDate = format(input$refDate, "%Y-%m-%d"), 
             startDate = format(as.Date(startDate), "%Y-%m-%d"))[1]
  })
  
  output$AUMstrat <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getAUM(input$filter1, 
             delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
             refDate = format(input$refDate, "%Y-%m-%d"), 
             startDate = format(as.Date(startDate), "%Y-%m-%d"))[2]
  })
  
  output$AUMstratDet <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getAUM(input$filter1, 
             delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
             refDate = format(input$refDate, "%Y-%m-%d"), 
             startDate = format(as.Date(startDate), "%Y-%m-%d"))[3]
  })
  
  output$AUMlast <- renderTable({
    startDate <- if(input$chartFrame == "SI") {
      "2000-12-31"
    } else if (input$chartFrame == "Custom ...") {
      input$startCust
    } else datesResult$datesFrame["Date"][datesResult$datesFrame["Label"] == input$chartFrame]
    
    f_getAUM(input$filter1, 
             delCode = as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]),
             refDate = format(input$refDate, "%Y-%m-%d"), 
             startDate = format(as.Date(startDate), "%Y-%m-%d"))[4]
  })
  
  output$FundWgtHst <- renderPlot(f_getFundA(input$fundName)[1])
  output$absCorr <- renderPlot(f_getFundA(input$fundName)[2])
  output$relCorr <- renderPlot(f_getFundA(input$fundName)[3])
  output$volStats <- renderTable(f_getFundA(input$fundName)[4])
  output$compCasesNo <- renderText(paste("Complete cases for correlation calcs:", f_getFundA(input$fundName)[5]))
  
  output$selAbsCorr <- renderPlot({
    req(length(input$table_rows_selected) > 1)
    f_getSelCorr(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))[1]
  })
  
  output$selRelCorr <- renderPlot({
    req(length(input$table_rows_selected) > 1)
    f_getSelCorr(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))[2]
  })
  
  output$ranksTable <- renderTable({
    req(length(input$table_rows_selected) > 0)
    f_getRanks(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))[1]
  })
  
  output$ranksChart <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    f_getRanks(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))[2]
  })
  
  output$delDataComp <- renderPlot({
    req(length(input$table_rows_selected) > 0)
    req(unique(MAP$mgrName[MAP$DelCode %in% as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"])[,1]]) != "MIFL")
    f_delComp(as.data.frame(tableData$fullMap[input$table_rows_selected,"DelCode"]))
  })
  
  output$statTable <- renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    sprKL <- tTests %>%
      #filter(StatDate <= input$refDate) %>%
      arrange(StatDate) %>%
      mutate(StatDate = as.numeric(StatDate),
             t = abs(t)) %>%
      group_by(DelCode) %>%
      summarise("Trend" = sparkline::spk_chr(t,
                                             xvalues = StatDate,
                                             tooltipFormat = '{{y}}'))
    
    datatable(tTests %>%
                mutate(StatDate = as.Date(StatDate)) %>%
                #filter(StatDate == input$refDate) %>%
                filter(StatDate == max(StatDate, na.rm = T)) %>%
                mutate(t = abs(t)) %>%
                mutate_at(c("p", "mean", "max", "min"), ~ . * 100) %>%
                mutate_if(is.numeric, ~ round(.,2)) %>%
                select(-StatDate) %>%
                arrange(desc(t)) %>%
                left_join(sprKL, by = "DelCode") %>%
                rename(`|t|` = t),
              escape = F,
              options = list(drawCallback = cb),
              #server = T,
              selection = "single",
              rownames = FALSE,
              filter= "bottom")})
  
  output$RBC_Fus <- renderPlot({
    req(length(input$statTable_rows_selected) > 0)
    
    #theseTests <- filter(tTests,  StatDate == input$refDate)
    theseTests <- filter(tTests,  StatDate == max(StatDate, na.rm = T))
    
    RBCFUSDelCode$df <- Del_recon %>%
        filter(DelCode == as.character(theseTests$DelCode[input$statTable_rows_selected])) %>%
        arrange(Date)

    return(f_RBC_Fus(as.character(theseTests$DelCode[input$statTable_rows_selected])))
  })
  
  output$lastRBCFus <- renderDataTable(RBCFUSdayCheck$df %>%
                                         mutate(Date = format(Date, "%d-%h-%y"),
                                                Fusion = round(Fusion/1000000,3),
                                                RBC = round(RBC/1000000,3),
                                                Diff = round(Diff/1000000,3),
                                                Adjustment = round(Adjustment/1000000,3),
                                                DiffPerc = round(DiffPerc * 100, 3)),
                                       server = T,
                                       selection = "single",
                                       rownames = FALSE,
                                       filter= "bottom")
  output$turnover <- renderDataTable(TURN,
                                     server = T,
                                     rownames = FALSE,
                                     filter = "bottom")
  
  output$XLday <- downloadHandler(filename = "dayRBCFusion.xlsx", 
                                  content = function(file) {
                                    openxlsx::write.xlsx(RBCFUSdayCheck$df, file)})
  
  output$XLDelCount <- downloadHandler(filename = "Code_Date.xlsx", 
                                       content = function(file) {
                                         openxlsx::write.xlsx(RBCFUSDelCode$df, file)})
  
  output$mainTable <- downloadHandler(filename = "mainTable.xlsx", 
                                  content = function(file) {
                                    openxlsx::write.xlsx(tableData$fullMap, file)})
  
  output$fullMainTable <- downloadHandler(filename = "fullMainTable.xlsx", 
                                      content = function(file) {
                                        openxlsx::write.xlsx(tableData$allMap, file)})
  
  output$idxData <- downloadHandler(filename = "idxData.xlsx", 
                                    content = function(file) {
                                      openxlsx::write.xlsx(idxRets$df, file)})
}


shinyApp(ui = ui, server = server)
