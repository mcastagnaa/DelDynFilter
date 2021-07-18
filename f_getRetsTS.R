# delCode = as.data.frame(c('701880','701879'))
# refDate <- as.Date("2021-07-13")
# chartFrame <- "SI"
# startDate <- as.Date("2020-12-31")

f_getRetsTS <- function(delCode, refDate, startDate, chartFrame) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)

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
    mutate(panel = ifelse(Variable == "ERIndex", "Relative", "Absolute"),
           Line = ifelse(Variable == "SAAIndex", "SAA", "Portfolio"),
           DelCode = as.character(DelCode)) %>%
    ggplot() +
    geom_line(aes(x = Date, y = value,  linetype = Line, color = DelCode)) +
    facet_wrap(~panel, ncol = 1, scales="free_y") +
    theme_bw() +
    labs(#title = paste(delCode,"-", MAP$mgrName[MAP$DelCode == delCode]),
         y = "", x= "")
  
  return(chart)
}

