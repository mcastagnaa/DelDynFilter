# delCode = as.data.frame(c('701880','701879'))
# refDate <- as.Date("2021-07-13")
# startDate <- as.Date("2020-12-31")

f_getRetsStats <- function(delCode, refDate, startDate) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)

  stats <- RETS %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(Port = PortIndex/first(PortIndex)*100,
           SAA = SAAIndex/first(SAAIndex)*100,
           DelCode = as.character(DelCode)) %>%
    select(Date, DelCode, SAA, Port) %>%
    pivot_longer(-c(Date, DelCode), names_to = "Variable") %>%
    group_by(DelCode, Variable) %>%
    tq_transmute(select     = value, 
                 mutate_fun = periodReturn, 
                 period     = "weekly", 
                 col_rename = "Rets") %>%
    pivot_wider(names_from = "Variable", values_from = "Rets") %>%
    group_by(DelCode) %>%
    tq_performance(Ra = Port, Rb = SAA, performance_fun = table.CAPM)
  
  return(stats)
}

