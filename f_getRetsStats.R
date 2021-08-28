# delCode = as.data.frame(c('701880','701879'))
# refDate <- as.Date("2021-07-13")
# startDate <- as.Date("2020-12-31")

f_getRetsStats <- function(delCode, refDate, startDate) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)

  statsData <- RETS %>%
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
    pivot_wider(names_from = "Variable", values_from = "Rets") 
  
  stats <- statsData %>%
    group_by(DelCode) %>%
    tq_performance(Ra = Port, Rb = SAA, performance_fun = table.CAPM) %>%
    select(-c(ActivePremium, `Correlationp-value`, TrackingError)) %>%
    mutate(Alpha = Alpha * 100,
           AnnualizedAlpha = AnnualizedAlpha * 100)
  
  volStats <- statsData %>%
    mutate(RR = Port - SAA) %>%
    group_by(DelCode) %>%
    summarise(Vol = sd(Port) * 100,
              VolAnn = Vol * sqrt(52),
              TE = sd(RR) * 100,
              TEAnn = TE * sqrt(52))
  
  rets <- RETS %>%
    filter(DelCode %in% delCode[,1],
           Date >= startDate,
           Date <= refDate) %>%
    arrange(Date) %>%
    mutate(DelCode = as.character(DelCode)) %>%
    group_by(DelCode) %>%
    summarise(Port = round(last(PortIndex)/first(PortIndex)-1, 4)*100,
              SAA = round(last(SAAIndex)/first(SAAIndex)-1, 4)*100,
              ER = Port-SAA) %>%
    left_join(volStats, by = "DelCode") %>%
    left_join(stats, by = "DelCode")
  
    return(rets)
}

