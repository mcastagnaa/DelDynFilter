# groups <- c("AssetClass")
# input1 <- c("MIFL", "Wellington")
# input2 <- "Main"
# refDate <- as.Date("2021-07-07")
# chartFrame <- "YtD"

f_getTable <- function(groups, input1, input2, input3, refDate, datesFrame, chartFrame) {
  
  thisMAP <- MAP %>%
    #{if (input1 == "Internal") filter(., mgrName == "MIFL") else .} %>%
    filter(mgrName %in% input1) %>%
    {if (input2 == "Main") filter(., IsRepresentative) else .} %>%
    {if (input3 == "Live") filter(., is.na(EndDate)|EndDate > refDate) else .} %>%
    arrange(across(groups)) %>%
    group_by(across(groups)) %>%
    select(DelCode, all_of(groups), DelDispName) 
  
  thisRetsSet <- RETS %>%
    filter(DelCode %in% thisMAP$DelCode) %>%
    left_join(datesFrame, by = "Date") %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(Label = ifelse(Date == min(Date), "SI",Label),
           Label = ifelse(Date == refDate, "Last", Label)) %>%
    filter(!is.na(Label)) %>%
    mutate(Del = round(last(PortIndex)/PortIndex-1,4)*100,
           SAA = round(last(SAAIndex)/SAAIndex-1,4)*100,
           ER = round(Del-SAA, 2)) %>%
    filter(Label != "Last") %>%
    select(DelCode, Label, Del, SAA, ER) %>%
    #mutate(Label = factor(Label, levels = c("1 day", "1 week", "MtD", "YtD", "1y", "SI"))) %>%
    pivot_longer(-c(DelCode, Label)) 
  
  delChart <- thisRetsSet %>%
    filter(Label == chartFrame,
           name != "ER") %>%
    select(-Label) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    ggplot(aes(x = SAA, y = Del, label = DelCode)) +
    geom_abline(slope = 1, color = "grey", linetype = 2) +
    geom_point() +
    geom_text_repel() +
    theme_bw()
  
  thisRets <- thisRetsSet %>%
    pivot_wider(names_from = c(Label, name), values_from = value)
  
  fullMap <- thisMAP %>%
    left_join(thisRets, by = "DelCode")
  
  rm(thisRets, thisRetsSet)
  
  return(fullMap)
}

