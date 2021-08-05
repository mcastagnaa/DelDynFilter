delCode <- data.frame(delcode = c("694832", "701878"))


f_getSelCorr <- function(delCode) {

  selRetsData <- RETS %>%
    filter(DelCode %in% delCode[, 1]) %>%
    left_join(MAP[, c("DelCode", "mgrName")], by = "DelCode") %>%
    group_by(Date) %>%
    select(DelCode, Date, Port = PortIndex, SAA = SAAIndex) %>%
    mutate(DelCode = paste(as.character(DelCode), "_")) %>%
    pivot_longer(-c(Date, DelCode)) %>%
    group_by(DelCode, name) %>%
    tq_transmute(select     = value, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 col_rename = "Rets") %>%
    pivot_wider(names_from = name, values_from = Rets) %>%
    mutate(RR = Port-SAA) %>%
    mutate(wday = weekdays(Date)) %>%
    adorn_totals(where = "col") %>%
    filter(!wday %in% c("Sunday", "Saturday"),
           Total != 0) %>%
    select(-c(wday, Total))
  
  selData <- selRetsData %>%
    select(DelCode, Date, Port) %>%
    pivot_wider(names_from = DelCode, values_from = Port) %>%
    select(-Date) %>%
    filter(complete.cases(.))
  
  selAbsCorr <- ggcorrplot(round(cor(selData),2), 
                        hc.order = TRUE, 
                        outline.color = "white",
                        lab = T,
                        tl.cex = 8, lab_size = 3,
                        p.mat = cor_pmat(selData))
  
  selData <- selRetsData %>%
    select(DelCode, Date, RR) %>%
    pivot_wider(names_from = DelCode, values_from = RR) %>%
    select(-Date) %>%
    filter(complete.cases(.))
  
  selRRCorr <- ggcorrplot(round(cor(selData),2), 
                        hc.order = TRUE, 
                        outline.color = "white",
                        lab = T,
                        tl.cex = 8, lab_size = 3,
                        p.mat = cor_pmat(selData))
  
  return(list(selAbsCorr, selRRCorr))
}

