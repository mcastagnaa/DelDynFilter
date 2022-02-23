# dels <- data.frame(DelCode = c("642651", "702571", "701878"))
# refDate <- as.Date("2021-07-07")
# custStart <- as.Date("2020-12-31")
# chartFrame <- "YtD"
# 
# d1 <- max(RETS$Date[RETS$Date <= (refDate-1)])
# w1 <- max(RETS$Date[RETS$Date <= (refDate-7)])
# m1 <- max(RETS$Date[RETS$Date <= (refDate-months(1))])
# m3 <- max(RETS$Date[RETS$Date <= (refDate-months(3))])
# m6 <- max(RETS$Date[RETS$Date <= (refDate-months(6))])
# QtD <- max(RETS$Date[RETS$Date <= (yq(quarter(refDate, with_year = TRUE)) - days(1))])
# MtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-%m-01"))-1])
# YtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-01-01"))-1])
# 
# datesFrame <- data.frame(Label = c("1d", "1w", "1m", "3m", "6m","MtD", "YtD", "QtD"),
#                                      Date = c(d1, w1, m1, m3, m6, MtD, YtD, QtD),
#                                      stringsAsFactors = F)
# rm(d1, w1, m1, m3, m6, QtD, MtD, YtD)


f_getScatter <- function(dels, refDate, datesFrame, 
                         chartFrame, custStart, delCode) {
  
  startDate <- if(length(datesFrame$Date[datesFrame$Label %in% chartFrame]) == 0) {
    custStart
  } else datesFrame$Date[datesFrame$Label %in% chartFrame]
  
  thisDates <- c(startDate, refDate)

  thisRetsSet <- RETS %>%
    filter(DelCode %in% dels$DelCode) %>%
    group_by(DelCode) %>%
    {if (chartFrame != c("SI")) filter(., Date %in% thisDates) else filter(., Date %in% c(refDate, min(Date)))} %>%
    arrange(Date) %>%
    mutate(Del = round(last(PortIndex)/first(PortIndex)-1,4)*100,
           SAA = round(last(SAAIndex)/first(SAAIndex)-1,4)*100) %>%
    filter(Date == max(Date)) %>%
    left_join(MAP[, c("DelCode", "AssetClass")], by = "DelCode") %>%
    select(DelCode, Del, SAA, Manager = DelegateManager, AssetClass) %>%
    pivot_longer(-c(DelCode, Manager, AssetClass)) %>%
    pivot_wider(names_from = name, values_from = value)
  
  minC <- min(thisRetsSet$SAA, thisRetsSet$Del)
  maxC <- max(thisRetsSet$SAA, thisRetsSet$Del)
  
  trsup <- data.frame(x=c(minC, minC, maxC), y=c(minC, maxC, maxC))
  trinf <- data.frame(x=c(minC, maxC, maxC),y=c(minC, minC, maxC))
  
  enhDot <- thisRetsSet %>%
    filter(DelCode %in% delCode[,1])
  
  delChart <- ggplot() +
    geom_abline(slope = 1, color = "grey", linetype = 2) +
    geom_polygon(aes(x = x, y = y), data = trsup, fill = "light green", alpha = 0.1) + 
    geom_polygon(aes(x = x, y = y), data = trinf, fill = "red", alpha = 0.1) + 
    geom_point(aes(x = SAA, y = Del), data = enhDot, color = "black", size = 4, shape = 1) +
    geom_point(aes(x = SAA, y = Del, color = Manager, shape = AssetClass), size = 2, data = thisRetsSet) +
    geom_text_repel(aes(x = SAA, y = Del, label = DelCode, color = Manager), data = thisRetsSet, max.overlaps = 100) +
    theme_bw() +
    labs(x = "SAA", y = "Portfolio")
  
  rm(thisRetsSet)
  
  return(delChart)
}

