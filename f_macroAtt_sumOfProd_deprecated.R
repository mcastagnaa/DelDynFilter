startDate <- as.Date("2021-12-31")
endDate <- as.Date("2022-03-21")

thisCode <- "8824"

source = "FUSION"

f_macroAtt <- function(startDate, endDate, thisCode, source) {
  
  if(source == "RBC") mainSet <- RBCidxData else mainSet <- RETS
  
  thisDets <- filter(FUNDSFULL, ShortCode == thisCode)
  
  thisFRets <- filter(FUNDRETS, 
                      ISIN == thisDets$ISIN,
                      Date >= startDate,
                      Date <= endDate) %>%
    rename(FundIdx = Modified_GAV) %>%
    select(-ISIN)
  
  thisFAUM <- filter(FAUM, 
                     MAMLcode == thisDets$MAMLcode,
                     Date >= startDate,
                     Date <= endDate) %>%
    select(-MAMLcode)
  
  thisDelSet <- filter(mainSet,
                       DBCode == thisDets$MAMLcode,
                       Date >= startDate,
                       Date <= endDate)
  
  thisDelMap <- MAP %>%
    filter(DelCode %in% unique(thisDelSet$DelCode)) %>%
    mutate(Strategy = paste(mgrName, AssetClass, Region, Style, sep = "|"),
           Level = "Delegate") %>%
    select(Code = DelCode, Level, Strategy) %>%
    rbind(thisDets %>%
            mutate(Level = "Fund") %>%
            select(Code = ShortCode, Level, 
                   Strategy = FundName))
  
  thisSAADefs <- SAADEFS %>%
    filter(Code %in% c(unique(thisDelSet$DelCode), thisDets$ShortCode)) %>%
    group_by(Code) %>%
    filter(is.na(StartDate)| StartDate == max(StartDate)) %>%
    summarise(SAAdef = paste(paste0(round(Weight*100, 1), "%"),
                             Index, CURRENCY, PriceType, collapse = "|")) %>%
    left_join(thisDelMap, by  = "Code") %>%
    select(Level, Strategy, SAAdef) %>%
    arrange(desc(Level))
  
  retSet <- thisDelSet %>%
    select(DelCode, Date, AUM, DelIdx = PortIndex, DelBmk = SAAIndex) %>%
    left_join(thisFRets, by = "Date") %>%
    left_join(thisFAUM, by = "Date") %>%
    na.omit() %>%
    mutate(DelW = AUM/FAUM) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    mutate(DelRet = DelIdx/lag(DelIdx)-1, 
           DelBmkRet = DelBmk/lag(DelBmk)-1,
           FundRet = FundIdx/lag(FundIdx)-1,
           FundBmkRet = FSAA/lag(FSAA)-1) %>%
    na.omit() %>%
    select (-c(AUM, FAUM)) %>%
    mutate(cumFund = cumprod(1+FundRet)-1,
           cumFundSaa = cumprod(1+FundBmkRet)-1,
           cumDel = cumprod(1+DelRet)-1,
           cumDelSaa = cumprod(1+DelBmkRet)-1,
           cumDelContr = cumprod(1+DelW*DelRet)-1,
           cumDelSaaContr = cumprod(1+DelW*DelBmkRet)-1,
           cumDelERContr = cumDelContr-cumDelSaaContr,
           cumDelERSaaContr = cumDelSaaContr-(cumprod(1+DelW*FundBmkRet)-1)) 

  nperiod <- length(unique(retSet$Date))
  
  view <- rbind(retSet %>%
                  group_by(DelCode) %>%
                  mutate(avgW = sum(DelW)/nperiod) %>%
                  filter(Date == max(Date)) %>%
                  ungroup() %>%
                  summarise(Level = "TOP",
                            totAvgW = sum(avgW),
                            ptfl = first(cumFund),
                            SAA = first(cumFundSaa),
                            ptflER = ptfl-SAA,
                            ptflSAAER = NA,
                            ERDel = sum(cumDelERContr),
                            ERSAADel = sum(cumDelERSaaContr),
                            totDelER = ERDel+ERSAADel,
                            DIFF = ptflER-totDelER),
                retSet %>%
                  group_by(DelCode) %>%
                  mutate(avgW = sum(DelW)/nperiod) %>%
                  filter(Date == max(Date)) %>%
                  ungroup() %>%
                  left_join(thisDelMap, by = c("DelCode" = "Code")) %>%
                  mutate(ptflER = cumDel-cumDelSaa,
                         ptflSAAER = cumDelSaa-cumFundSaa,
                         totDelER = cumDelERContr+cumDelERSaaContr,
                         DIFF = NA) %>%
                  select(Level = Strategy,
                         totAvgW = avgW,
                         ptfl = cumDel,
                         SAA = cumDelSaa,
                         ptflER, 
                         ptflSAAER,
                         ERDel = cumDelERContr,
                         ERSAADel = cumDelERSaaContr,
                         totDelER, 
                         DIFF))
  
  retChart <- retSet %>%
    group_by(Date) %>%
    summarise(fRetDel = sum(DelW * DelRet),
              fSAADel = sum(DelW * DelBmkRet),
              fRetPU = first(FundRet),
              fSAAPU = first(FundBmkRet)) %>%
    mutate(cumFRetDel = cumprod(1+fRetDel)-1,
           cumFRetPU = cumprod(1+fRetPU)-1,
           cumFSAAPU = cumprod(1+fSAAPU)-1,
           cumFSAADel = cumprod(1+fSAADel)-1,
           cumDiffF = cumFRetPU-cumFRetDel,
           cumDiffSAA = cumFSAAPU-cumFSAADel) %>%
    select(Date, cumFRetDel, cumFRetPU, cumFSAAPU, cumFSAADel, cumDiffF, cumDiffSAA) %>%
    pivot_longer(-Date, names_to = "Version") %>%
    mutate(panel= ifelse(grepl("SAA", Version), "SAA", "Fund"),
           item = ifelse(grepl("Diff", Version), "Delta", "Absolute"),
           Version = ifelse(grepl("PU", Version), "Official", "Delegates Wavg")) %>%
    ggplot(aes(x = Date, y = value, color = Version)) +
    geom_line() + 
    facet_grid(item~panel, scales= "free_y") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    labs(title = "Par Universo vs. delegate weighted average",
         subtitle = "SAA as Wavg of delegates SAA",
         x = "", y = "Cumulative returns",
         caption = paste0("Source: ",source))
  
  ERset <- retSet %>%
    group_by(Date) %>%
    summarise(ER = first(cumFund)-first(cumFundSaa)) %>%
    select(Date, ER)
  
  contrChart <- retSet %>%
    group_by(Date) %>%
    summarise(totAvgW = sum(DelW),
              ptfl = first(cumFund),
              SAA = first(cumFundSaa),
              ptflER = ptfl-SAA,
              ER.Del = sum(cumDelERContr),
              ER.SAADel = sum(cumDelERSaaContr),
              totDelER = ER.Del+ER.SAADel,
              RESIDUAL = ptflER-totDelER) %>%
    select(Date, ER.Del, ER.SAADel, RESIDUAL, ER = ptflER) %>%
    pivot_longer(-c(Date, ER), names_to = "Contributors") %>%
    mutate(Contributors = factor(Contributors, levels = c("RESIDUAL", "ER.Del", "ER.SAADel"))) %>%
    ggplot() +
    geom_bar(aes(x = Date, y = value, fill = Contributors), stat = "identity", position = "stack") +
    geom_line(data = ERset, aes(x=Date, y = ER)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    labs(title = "Contribution to fund excess return vs. SAA",
         #subtitle = "",
         x = "", y = "Cumulative relative returns",
         caption = paste0("Source: ",source))
  
  ### TESTING !!! #################
  # retSet %>%
  #   group_by(Date) %>%
  #   summarise(Level = "TOP",
  #             totAvgW = sum(DelW),
  #             ptfl = first(cumFund),
  #             SAA = first(cumFundSaa),
  #             ptflER = ptfl-SAA,
  #             ptflSAAER = NA,
  #             ERDel = sum(cumDelERContr),
  #             ERSAADel = sum(cumDelERSaaContr),
  #             totDelER = ERDel+ERSAADel,
  #             DIFF = ptflER-totDelER) %>%
  #   select(Date, ptflER, DIFF) %>%
  #   pivot_longer(-Date) %>%
  #   ggplot(aes(x = Date, y = value, color = name)) +
  #   geom_line() +
  #   scale_y_continuous(labels = scales::percent)
  # 
  # 
  # retSet %>%
  #   group_by(DelCode, Date) %>%
  #   left_join(thisDelMap, by = c("DelCode" = "Code")) %>%
  #   mutate(ptflER = cumDel-cumDelSaa,
  #          ptflSAAER = cumDelSaa-cumFundSaa,
  #          totDelER = cumDelERContr+cumDelERSaaContr,
  #          DIFF = ptflER-totDelER) %>%
  #   ungroup() %>%
  #   select(Date,
  #          Level = Strategy,
  #          totAvgW = DelW,
  #          ptfl = cumDel,
  #          SAA = cumDelSaa,
  #          ptflER,
  #          ptflSAAER,
  #          ERDel = cumDelERContr,
  #          ERSAADel = cumDelERSaaContr,
  #          totDelER,
  #          DIFF) %>%
  #   select(Level, DIFF, Date) %>%
  #   ggplot(aes(x = Date, y = DIFF, color = Level)) +
  #   geom_line() +
  #   scale_y_continuous(labels = scales::percent)
  # 
  # TEST <- retSet %>%
  #   filter(Date %in% c(as.Date("2022-02-23"), as.Date("2022-02-24")),
  #          DelCode %in% c("707793", "705234"))

  ### END TESTING !!! #################
  
  return <- list(thisSAADefs, view, retChart, contrChart)
  
  
   
}