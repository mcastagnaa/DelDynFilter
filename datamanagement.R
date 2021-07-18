load("DelSet.Rda")

FUNDS <- RETS %>%
  select(DBCode, FundName = Fund_Name) %>%
  distinct() %>%
  arrange(DBCode)

MAP <- MAP %>%
  mutate(IsRepresentative = IsRepresentative == 1,
         DelDispName = paste(mgrName, AssetClass, Region, Style, sep = "|")) %>%
  filter(DelCode %in% RETS$DelCode) %>%
  left_join(FUNDS, by = "DBCode")  

