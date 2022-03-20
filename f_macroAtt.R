# startDate <- as.Date("2021-12-31")
# endDate <- as.Date("2022-02-28")
# 
# thisCode <- "3015"

f_macroAtt <- function(startDate, endDate, thisCode) {
  
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
                            DIFF = ptflER-totDelER) %>%
                  as.data.frame(),
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
  
  return <- list(thisSAADefs, view)
  
}