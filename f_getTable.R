# groups <- c("AssetClass")
# input1 <- c("MIFL", "Wellington")
# input2 <- "Main"
# input3 <- "Live"
# input4 <- "No"
# refDate <- as.Date("2021-07-07")
# ### datesFrame <- tobeloaded
# chartFrame <- "YtD"
# datesGroup <- c("1d", "1w", "MtD", "YtD", "QtD", "SI")
# 
f_getTable <- function(groups, input1, input2, input3, input4, refDate, datesFrame, chartFrame, datesGroup) {
  
  thisMAP <- MAP %>%
    #{if (input1 == "Internal") filter(., mgrName == "MIFL") else .} %>%
    filter(mgrName %in% input1) %>%
    {if (input2 == "Main") filter(., IsRepresentative) else .} %>%
    {if (input3 == "Live") filter(., is.na(EndDate)|EndDate > refDate) else .} %>%
    {if (input4 == "No") filter(., !(DelCode %in% EXCP$DelCode)) else .} %>%
    arrange(across(groups)) %>%
    group_by(across(groups)) %>%
    select(DelCode, all_of(groups), DelDispName) 
  
  datesFrame <- datesFrame %>%
    filter(Label %in% datesGroup)
  
  thisRetsSet <- RETS %>%
    filter(DelCode %in% thisMAP$DelCode) %>%
    left_join(datesFrame, by = "Date") %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(Label = ifelse(Date == min(Date) & "SI" %in% datesGroup, "SI", Label),
           Label = ifelse(Date == refDate, "Last", Label)) %>%
    filter(!is.na(Label)) %>%
    mutate(Del = round(last(PortIndex)/PortIndex-1,4)*100,
           SAA = round(last(SAAIndex)/SAAIndex-1,4)*100,
           ER = round(Del-SAA, 2)) %>%
    filter(Label != "Last") %>%
    select(DelCode, Label, Del, SAA, ER) %>%
    mutate(Label = factor(Label, levels = c("1d", "1w", "1m", "3m", "6m", "MtD", "QtD", "YtD", "SI"))) %>%
    pivot_longer(-c(DelCode, Label)) %>%
    arrange(Label) %>%
    pivot_wider(names_from = c(Label, name), values_from = value) 
  
  fullMap <- thisMAP %>%
    left_join(thisRetsSet, by = "DelCode")
  
  rm(thisRetsSet)
  
  return(fullMap)
}

