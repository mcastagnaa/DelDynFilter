delCode = as.data.frame(c('701880','701879', '610249'))
refDate <- as.Date("2021-09-16")
startDate <- as.Date("2020-12-31")

f_getRetsTS <- function(delCode, refDate, startDate, showCf) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  if(showCf) {
    Hline <- data.frame(panel = c("Relative", "Absolute", "Subs-Reds (% over prev. AUM)"), Y = c(0,100,0))
  } else {
    Hline <- data.frame(panel = c("Relative", "Absolute"), Y = c(0,100))
  }
  
  cFlows <- RETS %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(panel = "Subs-Reds (% over prev. AUM)",
           CashflowPerc = ifelse(is.na(Cashflow), 0, Cashflow/lag(AUM)*100),
           DelCode = as.character(DelCode)) 

  chart <- RETS %>%
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
           DelCode = as.character(DelCode)) %>%
    ggplot() +
    geom_hline(data = Hline, aes(yintercept = Y), color = "dark grey", linetype = "dashed") +
    {if(showCf) geom_bar(data = cFlows, stat = "identity", aes(x = Date, y = CashflowPerc, color = DelCode), fill = NA)} +
    geom_line(aes(x = Date, y = value,  linetype = Line, color = DelCode)) +
    facet_wrap(~panel, ncol = 1, scales="free_y") +
    theme_bw() +
    labs(#title = paste(delCode,"-", MAP$mgrName[MAP$DelCode == delCode]),
         y = "", x= "")
  
  return(chart)
}

