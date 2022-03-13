# groups <- c("AssetClass")
# input1 <- c("MIFL")
# input2 <- "Main"
# input3 <- "Live"
# input4 <- "Yes"
# refDate <- as.Date("2022-02-23")
# isAnnual <- T

### datesFrame
# d1 <- max(RETS$Date[RETS$Date <= (refDate-1)])
# w1 <- max(RETS$Date[RETS$Date <= (refDate-7)])
# m1 <- max(RETS$Date[RETS$Date <= refDate %m-% months(1)])
# m3 <- max(RETS$Date[RETS$Date <= refDate %m-% months(3)])
# m6 <- max(RETS$Date[RETS$Date <= refDate %m-% months(6)])
# y1 <- max(RETS$Date[RETS$Date <= (refDate-months(12))])
# QtD <- max(RETS$Date[RETS$Date <= (yq(quarter(refDate, with_year = TRUE)) - days(1))])
# MtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-%m-01"))-1])
# YtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-01-01"))-1])
# 
# datesFrame <- data.frame(Label = c("1d", "1w", "1m", "3m", "6m", "1y", "MtD", "YtD", "QtD"),
#                                      Date = c(d1, w1, m1, m3, m6, y1, MtD, YtD, QtD),
#                                      stringsAsFactors = F)
# rm(d1, w1, m1, m3, m6, y1, QtD, MtD, YtD)
# chartFrame <- "YtD"
# datesGroup <- c("1d", "1w", "MtD", "YtD", "QtD", "SI")

##### 
f_getTable <- function(groups, input1, input2, input3, input4, 
                       refDate, datesFrame, chartFrame, datesGroup, isAnnual, source) {
  
  thisMAP <- MAP %>%
    #{if (input1 == "Internal") filter(., mgrName == "MIFL") else .} %>%
    filter(mgrName %in% input1,
          hasRets) %>%
    {if (input2 == "Main") filter(., IsRepresentative) else .} %>%
    {if (input3 == "Live") filter(., is.na(EndDate)|EndDate > refDate) else .} %>%
    {if (input4 == "No") filter(., !(DelCode %in% EXCP$DelCode)) else .} %>%
    arrange(across(groups)) %>%
    group_by(across(groups)) %>%
    select(DelCode, all_of(groups), DelDispName) 
  
  datesFrame <- datesFrame %>%
    filter(Label %in% datesGroup)
  
  if(source == "RBC") mainSet <- RBCidxData else mainSet <- RETS
  
  thisRetsSet <- mainSet %>%
    #filter(DelCode %in% thisMAP$DelCode) %>%
    left_join(datesFrame, by = "Date") %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(Label = ifelse(Date == min(Date) & "SI" %in% datesGroup, "SI", Label),
           Label = ifelse(Date == refDate, "Last", Label)) %>%
    filter(!is.na(Label)) %>%
    mutate(Del = last(PortIndex)/PortIndex-1,
           SAA = last(SAAIndex)/SAAIndex-1,
           years = as.numeric(last(Date)-Date)/365.25) %>%
    mutate(Del = ifelse(isAnnual & years > 1, (1+Del)^(1/years)-1, Del),
           SAA = ifelse(isAnnual & years > 1, (1+SAA)^(1/years)-1, SAA)) %>%
    mutate(Del = round(Del, 4) * 100,
           SAA = round(SAA, 4) * 100,
           ER = round(Del-SAA, 2)) %>%
    select(-years) %>%
    filter(Label != "Last") %>%
    select(DelCode, Label, Del, SAA, ER) %>%
    mutate(Label = factor(Label, levels = c("1d", "1w", "1m", "3m", "6m", "MtD", "QtD", "YtD", "SI"))) %>%
    # mutate(ER = if_else(
    #   ER < 0, 
    #   paste0("<b style='color: red; float: left;'>", ER, "</b>"),
    #   paste0("<b style='color: green; float: left;'>", ER, "</b>")),
    #   SAA = paste0("<style='color: black; float: left;'>", SAA),
    #   Del = paste0("<style='color: black; float: left;'>", Del)) %>%
    pivot_longer(-c(DelCode, Label)) %>%
    arrange(Label) %>%
    pivot_wider(names_from = c(Label, name), values_from = value) 
  
  fullMap <- thisMAP %>%
    left_join(thisRetsSet, by = "DelCode") 
  
  allMap <- MAP %>%
    filter(DelCode %in% thisRetsSet$DelCode) %>%
    select(DelCode, mgrName, AssetClass, Region, Style, FundName) %>%
    left_join(thisRetsSet, by = "DelCode")
  
  rm(thisRetsSet)
  
  return(list(fullMap, allMap))
  #return(fullMap)
}

