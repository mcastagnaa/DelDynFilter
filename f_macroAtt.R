# startDate <- as.Date("2021-12-31")
# endDate <- as.Date("2022-03-21")
# 
# thisCode <- "8824"
# 
# source = "FUSION"

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
    summarise(SAAdef = paste(paste0(round(Weight*100, 1), "%"),
                             Index, CURRENCY, PriceType, collapse = "|")) %>%
    left_join(thisDelMap, by  = "Code") %>%
    select(Level, Code, Strategy, SAAdef) %>%
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
    mutate(DelWRet = DelRet * DelW,
           BmkWRet = DelBmkRet * DelW,
           SAAWRet = FundBmkRet* DelW,
           count = 1) %>%
    group_by(DelCode) %>%
    mutate(cumFund = cumprod(1+FundRet)-1,
           cumFundSaa = cumprod(1+FundBmkRet)-1,
           cumDel = cumprod(1+DelRet)-1,
           cumDelSaa = cumprod(1+DelBmkRet)-1,
           cumDelWRet = cumprod(1+DelWRet)-1,
           cumBmkWRet = cumprod(1+BmkWRet)-1,
           cumSAAWRet = cumprod(1+SAAWRet)-1,
           avgW = cumsum(DelW)/cumsum(count)) %>%
    select(-count) %>%
    ungroup() %>%
    mutate(DelER = DelRet-DelBmkRet,
           FundER = FundRet-FundBmkRet,
           cumDelER = cumDel-cumDelSaa,
           cumFundER = cumFund-cumFundSaa,
           DelContrib = DelWRet - BmkWRet,
           SAAContrib = BmkWRet - SAAWRet,
           cumDelContrib = cumDelWRet - cumBmkWRet,
           cumSAAContrib = cumBmkWRet - cumSAAWRet) 

  topLevel <- retSet %>%
    arrange(Date) %>%
    group_by(Date) %>%
    summarise(totAvgW = sum(DelW),
              ptfl = first(cumFund),
              SAA = first(cumFundSaa),
              ER = first(cumFundER),
              DelRetWER = sum(DelWRet),
              BmkRetWER = sum(BmkWRet),
              SAARetWER = sum(SAAWRet)) %>%
    mutate(ERSaa = NA,
           cumDelWRet = cumprod(1+DelRetWER)-1,
           cumBmkWRet = cumprod(1+BmkRetWER)-1,
           cumSAAWRet = cumprod(1+SAARetWER)-1,
           cumDelContrib = cumDelWRet-cumBmkWRet,
           cumSAAContrib = cumBmkWRet-cumSAAWRet,
           Residual = ER-(cumDelContrib+cumSAAContrib),
           Level= "TOP") 
  
  view <- rbind(retSet %>%
                  filter(Date == max(Date)) %>%
                  mutate(ER = cumDel-cumDelSaa,
                         ERSaa = cumDelSaa-cumFundSaa,
                         Residual = (cumDelWRet-cumSAAWRet)-(cumDelContrib+cumSAAContrib)) %>%
                  select(Level = DelCode,
                         totAvgW = avgW,
                         ptfl = cumDel,
                         SAA = cumDelSaa,
                         ER, 
                         ERSaa,
                         cumDelWRet,
                         cumBmkWRet,
                         cumSAAWRet,
                         cumDelContrib,
                         cumSAAContrib,
                         Residual),
                topLevel %>%
                  filter(Date == max(Date)) %>%
                  select(-c(Date, BmkRetWER, SAARetWER, DelRetWER))) %>%
    arrange(desc(Level))
  
  view <- rename_all(view, ~gsub("cum", "", names(view)))
  
  ### CHARTS ##########################################################
  TESTS <- topLevel %>%
    select(Date, FundPU = ptfl, SAAPU = SAA, cumDelWRet, cumSAAWRet = cumBmkWRet) %>%
    mutate(cumDiffF = FundPU-cumDelWRet,
           cumDiffSAA = SAAPU-cumSAAWRet) %>%
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
  
  Cumulative <- topLevel %>%
    select(Date, ER, ER.Del = cumDelContrib, ER.SAADel = cumSAAContrib, Residual) %>%
    pivot_longer(-c(Date, ER), names_to = "Contributors") %>%
    mutate(Contributors = factor(Contributors, levels = c("Residual", "ER.Del", "ER.SAADel"))) %>%
    ggplot() +
    geom_hline(yintercept = 0, color = "black") +
    geom_bar(aes(x = Date, y = value, fill = Contributors), stat = "identity", position = "stack") +
    geom_line(aes(x=Date, y = ER)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    labs(title = "Cumulative contribution to fund excess return vs. SAA",
         #subtitle = "",
         x = "", y = "Cumulative relative returns",
         caption = paste0("Source: ",source))  
  
  Period <- retSet %>%
    select(Date, DelCode, DelContrib, SAAContrib, ER = FundER) %>%
    pivot_longer(-c(Date, DelCode, ER), names_to = "Contributors") %>%
    ggplot() +
    geom_hline(yintercept = 0, color = "black") +
    geom_bar(aes(x = Date, y = value, fill= DelCode, color = Contributors),
             stat= "identity", position = "stack") +
    # geom_bar_pattern(aes(x = Date, y = value, pattern_fill = DelCode, pattern_density = Contributors), 
    #                  stat = "identity", position = "stack", fill = 'light grey', pattern = "stripe", color = "grey") +
    #scale_pattern_density_discrete(range = c(0.3,1)) +
    scale_color_manual(values = c("black", "white")) + 
    geom_point(aes(x=Date, y = ER), size = 2) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    labs(title = "Period contribution to fund excess return vs. SAA",
         #subtitle = "",
         x = "", y = "Period relative returns",
         caption = paste0("Source: ", source))  
  
  return <- list(thisSAADefs, view, TESTS, Cumulative, Period)
}