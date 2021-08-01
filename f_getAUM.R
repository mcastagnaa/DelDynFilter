delCode = as.data.frame(c('701879'))
#delCode = as.data.frame(c())
mgrs <- c("MIFL", "Wellington", "AQR")
refDate <- as.Date("2021-07-13")
startDate <- as.Date("2020-12-31")

f_getAUM <- function(mgrs, delCode, refDate, startDate) {
  
  startDate <- as.Date(startDate)
  refDate <- as.Date(refDate)
  
  if(length(delCode) != 0) {
    
    stratSet <- MAP %>%
      filter(DelDispName %in% MAP$DelDispName[MAP$DelCode %in% delCode[,1]])
    
    AUMstrat <- RETS %>%
      left_join(stratSet, by = "DelCode") %>%
      filter(!is.na(DelDispName)) %>%
      group_by(Date, Strategy = DelDispName) %>%
      summarise(StrTotAUM = sum(AUM, na.rm = T)/1000000) %>%
      ggplot() +
      geom_line(aes(x = Date, y = StrTotAUM, color = Strategy)) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() +
      theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
      labs(title = "AUM (€mn) by strategy",
           y = "EUR mn", x= "")
    
    width_scale = 6
    
    AUMStrDet <- RETS %>%
      left_join(MAP, by = "DelCode") %>%
      filter(DelDispName == stratSet$DelDispName[1]) %>%
      group_by(Date, Fund = Fund_Name) %>%
      summarise(StrTotAUM = sum(AUM, na.rm = T)/1000000) %>%
      ggplot() +
      geom_area(aes(x = Date, y = StrTotAUM, fill = Fund)) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() +
      theme(legend.position="bottom", 
            legend.text=element_text(size=width_scale),
            #legend.box="vertical", 
            legend.box.margin=margin(6,6,6,6),
            legend.key.size = grid::unit(width_scale/50, "inch"),
            legend.key.width = grid::unit(width_scale/50, "inch")) +
      labs(title = paste("AUM (€mn) for", stratSet$DelDispName[1]),
           y = "EUR mn", x= "")
    
    AUMlast <- RETS %>%
      left_join(stratSet, by = "DelCode") %>%
      filter(!is.na(DelDispName), Date == refDate) %>%
      group_by(Group = DelDispName) %>%
      summarise(TotAUM = sum(AUM, na.rm = T)/1000000) %>%
      mutate(Level = "Strategy")
    
    AUMlast <- AUMlast %>%
      bind_rows(
        RETS %>%
          left_join(MAP, by = "DelCode") %>%
          filter(DelDispName %in% stratSet$DelDispName, Date == refDate) %>%
          group_by(Level = DelDispName, Group = Fund_Name) %>%
          summarise(TotAUM = sum(AUM, na.rm = T)/1000000) 
      )
        
  } else {
    AUMstrat = NULL
    AUMStrDet = NULL
    AUMlast = NULL
  }
  
  AUMmgr <- RETS %>%
    filter(DelegateManager %in% mgrs) %>%
    group_by(Date, AM = DelegateManager) %>%
    summarise(MgrTotAUM = sum(AUM, na.rm = T)/1000000) %>%
    ggplot() +
    geom_line(aes(x = Date, y = MgrTotAUM, color = AM)) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
    labs(title = "AUM (€mn) by manager",
         y = "EUR mn", x= "")
  
  AUMlast <- RETS %>%
    filter(DelegateManager %in% mgrs, Date == refDate) %>%
    group_by(Group = DelegateManager) %>%
    summarise(TotAUM = sum(AUM, na.rm = T)/1000000) %>%
    mutate(Level = "Manager") %>%
    bind_rows(AUMlast) %>%
    select(Level, Group, TotAUM)
  
  return(list(AUMmgr, AUMstrat, AUMStrDet, AUMlast))
}

