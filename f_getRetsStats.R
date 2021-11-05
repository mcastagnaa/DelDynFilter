delCode = as.data.frame(c('701880','701879'))
refDate <- as.Date("2021-11-04")
startDate <- as.Date("2021-10-29")

f_getRetsStats <- function(delCode, refDate, startDate) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  daysPer <- as.numeric(refDate-startDate)
  
  per <- ifelse(daysPer > 31, "weekly", "daily")
  scaleVol <- ifelse(per == "weekly", 52, 252)

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
                 period     = per, 
                 col_rename = "Rets") %>%
    pivot_wider(names_from = "Variable", values_from = "Rets") 
  
  stats <- tryCatch(
    statsData %>%
        group_by(DelCode) %>%
        tq_performance(Ra = Port, Rb = SAA, performance_fun = table.CAPM) %>%
        select(-c(ActivePremium, `Correlationp-value`, TrackingError)) %>%
        mutate(Alpha = Alpha * 100,
               AnnualizedAlpha = AnnualizedAlpha * 100),
    warning = function(w) w[1]$message,
    error = function(e) e[1]$message)
  
  sharpe <- statsData %>%
    group_by(DelCode) %>%
    tq_performance(Ra = Port, Rf = 0, performance_fun = SharpeRatio, FUN = "StdDev") %>%
    rename(Sharpe = `StdDevSharpe(Rf=0%,p=95%)`) 
  
  volStats <- statsData %>%
    mutate(RR = Port - SAA) %>%
    group_by(DelCode) %>%
    summarise(Obs = n()-1,
              Vol = sd(Port) * 100,
              VolAnn = Vol * sqrt(scaleVol),
              TE = sd(RR) * 100,
              TEAnn = TE * sqrt(scaleVol)) 
  
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
    mutate(Freq = per) %>%
    left_join(volStats, by = "DelCode") %>%
    {if(is.character(stats)) . else left_join(., stats, by = "DelCode")} %>%
    left_join(sharpe, by = "DelCode")
  
    return(rets)
}

