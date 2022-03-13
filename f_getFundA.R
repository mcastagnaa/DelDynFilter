fundName = "CH PROVIDENT 1"

f_getFundA <- function(fundName, source) {

  width_scale = 8
  
  if(source == "RBC") mainSet <- RBCidxData else mainSet <- RETS
  
  FundWgtH <- mainSet %>%
    filter(Fund_Name == fundName) %>%
    left_join(MAP[, c("DelCode", "mgrName")], by = "DelCode") %>%
    group_by(Date) %>%
    mutate(AUM = AUM/1000000, 
           TotAUM = sum(AUM, na.rm = T),
           Wgt = AUM/TotAUM) %>%
    rename(Manager= mgrName) %>%
    ggplot() +
    geom_area(aes(x = Date, y = Wgt, fill = Manager)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(legend.position="bottom", 
          legend.text=element_text(size=width_scale),
          legend.box="vertical") +  
    labs(title = paste("Weights across delegates for", fundName),
         y = "", x= "")
    
  FundRetsData <- mainSet %>%
    filter(Fund_Name == fundName) %>%
    left_join(MAP[, c("DelCode", "mgrName")], by = "DelCode") %>%
    rename(Manager= mgrName) %>%
    group_by(Date) %>%
    select(Manager, Date, Port = PortIndex, SAA = SAAIndex, DelCode) %>%
    pivot_longer(-c(Date, Manager, DelCode)) %>%
    group_by(Manager, name, DelCode) %>%
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
  
  volStats <- FundRetsData %>%
    select(Manager, Port, RR, DelCode) %>%
    mutate(DelCode = as.character(DelCode)) %>%
    group_by(Manager, DelCode) %>%
    summarise(obs = n(),
              Vol = sd(Port) * 100,
              VolAnn = Vol * sqrt(52),
              TE = sd(RR) * 100,
              TEAnn = TE * sqrt(52))
  
  liveMgr <- mainSet %>%
    filter(Date == max(Date),
           Fund_Name == fundName) %>%
    select(DelCode)
  
  FundData <- FundRetsData %>%
    select(DelCode, Date, Port) %>%
    filter(DelCode %in% liveMgr$DelCode) %>%
    mutate(DelCode = paste0(DelCode, "_")) %>%
    pivot_wider(names_from = DelCode, values_from = Port) %>%
    select(-Date) %>%
    filter(complete.cases(.))
  
  absCorr <- ggcorrplot(round(cor(FundData),2), 
                        hc.order = TRUE, 
                        outline.color = "white",
                        lab = T,
                        tl.cex = 6, lab_size = 2,
                        p.mat = cor_pmat(FundData))
  
  FundData <- FundRetsData %>%
    select(DelCode, Date, RR) %>%
    filter(DelCode %in% liveMgr$DelCode) %>%
    mutate(DelCode = paste0(DelCode, "_")) %>%
    pivot_wider(names_from = DelCode, values_from = RR) %>%
    select(-Date) %>%
    filter(complete.cases(.))
  
  relCorr <- ggcorrplot(round(cor(FundData),2), 
                        hc.order = TRUE, 
                        outline.color = "white",
                        lab = T,
                        tl.cex = 6, lab_size = 2,
                        p.mat = cor_pmat(FundData))
  
  compCases <- nrow(FundData)
  
  return(list(FundWgtH, absCorr, relCorr, volStats, compCases))
}

