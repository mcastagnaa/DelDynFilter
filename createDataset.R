library(openxlsx)

### SETUP ######################################
load("delMapTMP.Rda")
MIFLdel <- read.xlsx("MIFLSleeveMap.xlsx") %>%
  select(sub_Code = DelCode,
         Ptfl_Code = DBCode,
         AssetClass,
         Style,
         Region) %>%
  mutate(mgrName = "MIFL")

fundMap <- read.xlsx("mapTables.xlsx") %>%
  select(Ptfl_Code = ShortCode,
         FundName)

MIFLdel <- read.xlsx("MIFLSleeveMap.xlsx") %>%
  select(sub_Code = DelCode,
         Ptfl_Code = DBCode,
         AssetClass,
         Style,
         Region,
         MainSleeve) %>%
  mutate(mgrName = "MIFL")

delMap <- delMapAdj %>%
  mutate(AssetClass = NA,
         Region = NA, 
         Style = NA,
         MainSleeve = T) %>%
  bind_rows(MIFLdel) %>%
  left_join(fundMap, by = "Ptfl_Code") %>%
  select(-Ptfl_Code)

save(delMap, file = "dataset.Rda")