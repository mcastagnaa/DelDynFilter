# delCode = as.data.frame(c('701880','701879', '610249'))
# refDate <- as.Date("2021-11-04")
# startDate <- as.Date("2021-10-29")
# showCf = T

f_getRetsTS <- function(delCode, refDate, startDate, showCf) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  if(showCf) {
    Hline <- data.frame(panel = c("Relative", "Absolute", "Subs-Reds (% over prev. AUM)"), Y = c(0,100,0))
  } else {
    Hline <- data.frame(panel = c("Relative", "Absolute"), Y = c(0,100))
  }
  
  cFlows <- RBCidxData %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(panel = "Subs-Reds (% over prev. AUM)",
           CashflowPerc = ifelse(is.na(Cashflow), 0, Cashflow/lag(AUM)*100),
           DelCode = as.character(DelCode)) 

  chartSet <- RBCidxData %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(PortIndex = PortIndex/first(PortIndex)*100,
           SAAIndex = SAAIndex/first(SAAIndex)*100,
           ERIndex = PortIndex-SAAIndex) %>%
    select(Date, DelCode, SAAIndex, PortIndex, ERIndex) %>%
    pivot_longer(-c(Date, DelCode), names_to = "Variable") %>%
    mutate(panel = ifelse(Variable %in% c("ERIndex", "Cflow"), "Relative", "Absolute"),
           Line = ifelse(Variable == "SAAIndex", "SAA", "Portfolio"),
           DelCode = as.character(DelCode)) 
  
  lastObs <- chartSet %>%
    top_n(1, Date) %>%
    mutate(label = ifelse(panel == "Absolute", 
                          paste0(round((value/100-1)*100, 2), "%"),
                          paste(round(value,2), "%")))
  
  chart <- chartSet %>%
    ggplot() +
    geom_hline(data = Hline, aes(yintercept = Y), color = "dark grey", linetype = "dashed") +
    {if(showCf) geom_bar(data = cFlows, stat = "identity", aes(x = Date, y = CashflowPerc, color = DelCode), fill = NA)} +
    geom_line(aes(x = Date, y = value,  linetype = Line, color = DelCode)) +
    geom_text_repel(aes(x = Date, y = value, label = label), data = lastObs, size = 3) +
    facet_wrap(~panel, ncol = 1, scales="free_y") +
    theme_bw() +
    labs(#title = paste(delCode,"-", MAP$mgrName[MAP$DelCode == delCode]),
         y = "", x= "")
  
  return(list(chart, chartSet))
}

