# delCode = '697132'
# refDate <- as.Date("2021-07-13")
# chartFrame <- "SI"
# startDate <- as.Date("2020-12-31")

f_getRetsTS <- function(delCode, refDate, startDate, chartFrame) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)

  chart <- RETS %>%
    filter(DelCode == delCode,
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    mutate(PortIndex = PortIndex/first(PortIndex)*100,
           SAAIndex = SAAIndex/first(SAAIndex)*100,
           ERIndex = PortIndex-SAAIndex) %>%
    select(Date, DelCode, DelegateManager, SAAIndex, PortIndex, ERIndex) %>%
    pivot_longer(-c(Date, DelegateManager, DelCode), names_to = "Variable") %>%
    mutate(panel = ifelse(Variable == "ERIndex", "Relative", "Absolute"),
           Line = ifelse(Variable == "SAAIndex", "SAA", "Portfolio")) %>%
    ggplot() +
    #geom_hline(yintercept = 100, color = "grey") +
    geom_line(aes(x = Date, y = value, linetype = Line)) +
    facet_wrap(~panel, ncol = 1, scales="free_y") +
    theme_bw() +
    labs(title = paste(delCode,"-", MAP$mgrName[MAP$DelCode == delCode]),
         y = "", x= "")
  
  return(chart)
}

