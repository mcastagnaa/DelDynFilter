#writeLines("Loading f_RBC_Fus_rets.R ...")
 
# delCode = as.data.frame(c('694826'))
# refDate <- as.Date("2022-02-17")
# startDate <- as.Date("2021-01-31")
# showCf = F

f_RBC_Fus_rets <- function(delCode, refDate, startDate, showCf) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  if(showCf) {
    Hline <- data.frame(panel = c("Indices", "Subs-Reds (% over prev. AUM)"), Y = c(100,0))
  } else {
    Hline <- data.frame(panel = c("Indices"), Y = c(100))
  }
  
  cFlows <- RBCflows %>%
    rename(Date = TradeDate) %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    left_join(RBCidxData[, c("DelCode", "AUM", "Date")], by = c("DelCode", "Date")) %>%
    group_by(DelCode) %>%
    mutate(panel = "Subs-Reds (% over prev. AUM)",
           CashflowPerc = ifelse(is.na(Amount), 0, Amount/lag(AUM)*100),
           DelCode = as.character(DelCode)) 
  
  chartSet <- rbind(
    RETS %>%
      filter(DelCode %in% delCode[,1],
             Date >= startDate,
             Date <= refDate) %>%
      arrange(Date) %>%
      group_by(DelCode) %>%
      mutate(Index = PortIndex/first(PortIndex)*100,
             Source = "Fusion") %>%
      select(Date, DelCode, Index, Source),
    RBCidxData %>%
      filter(DelCode %in% delCode[,1],
             Date >= startDate,
             Date <= refDate) %>%
      arrange(Date) %>%
      group_by(DelCode) %>%
      mutate(Index = PortIndex/first(PortIndex)*100,
             Source = "RBC") %>%
      select(Date, DelCode, Index, Source)
  ) %>%
    mutate(panel = "Indices")
  
  lastObs <- chartSet %>%
    group_by(DelCode, Source) %>%
    top_n(1, Date) %>%
    mutate(label = paste0(round((Index/100-1)*100, 2), "%"))
  
  chart <- chartSet %>%
    ggplot() +
    geom_hline(data = Hline, aes(yintercept = Y), color = "dark grey", linetype = "dashed") +
    {if(showCf) geom_bar(data = cFlows, stat = "identity", aes(x = Date, y = CashflowPerc), fill = NA, color = "black")} +
    geom_line(aes(x = Date, y = Index, color = Source)) +
    geom_text_repel(aes(x = Date, y = Index, label = label, color = Source), data = lastObs, size = 3) +
    {if(showCf) facet_grid(panel ~ DelCode,  scales="free_y") else facet_grid(~DelCode, scales="free_y")} +
    theme_bw() +
    labs(y = "", x= "")
  
  return(chart)
  
}
