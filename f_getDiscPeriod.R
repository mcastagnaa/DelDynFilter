# delCode = as.data.frame(c('701880','701879'))
# refDate <- as.Date("2021-07-13")
# startDate <- as.Date("2020-12-31")

f_getDiscPeriod <- function(delCode, refDate, startDate) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  chart <- RETS %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    select(DelCode, Date, Port = PortIndex, SAA=SAAIndex) %>%
    mutate(DelCode = as.character(DelCode)) %>%
    pivot_longer(-c(DelCode, Date)) %>%
    arrange(Date) %>%
    group_by(DelCode, name) %>%
    tq_transmute(select     = value, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Rets") %>%
    mutate(Date = factor(format(Date, "%h-%y"), levels = reorder(format(Date, "%h-%y"), Date))) %>%
    pivot_wider(names_from = name, values_from = Rets) %>%
    mutate(ER = Port-SAA) %>%
    filter(Port!=0, SAA!=0) %>%
    pivot_longer(-c(DelCode, Date), names_to = "Object", values_to = "Rets") %>%
    mutate(panel = ifelse(Object == "ER", "Relative", "Absolute")) %>%
    ggplot() +
    geom_hline(yintercept = 0, color = "dark grey") +
    geom_col(aes(x = Date, y = Rets, fill = DelCode, alpha = Object), position  = "dodge") +
    geom_text(aes(x = Date, y = Rets, group = DelCode, label = round(Rets*100, 2)),
              position = position_dodge2(width = 0.9),      # move to center of bars
              vjust = -0.1,                               # nudge above top of bar
              size = 2) +
    scale_alpha_manual(values = c(1, 1, 0.3))+
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~panel, ncol = 1, scales="free_y") +
    theme_bw() +
    labs(title = "Monthly discrete returns",
         y = "", x= "")
  
  return(chart)
}

