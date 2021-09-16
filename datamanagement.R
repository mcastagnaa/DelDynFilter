site <- tryCatch(get_sharepoint_site(site_id = "dc4edaeb-1257-40ab-8758-018b7b5bda5a"),
                 error = function(e) e)

if(typeof(site)== "environment") {
  docs <- site$get_drive()
  
  ## dest <- tempfile("whatever.Rda")
  # docs$download_file("Stuff/DelSet.Rda", dest=dest)
  # load(dest)
  
  docs$download_file("Stuff/DelSet.Rda", overwrite = T)
}

load("DelSet.Rda")

FUNDS <- RETS %>%
  select(DBCode, FundName = Fund_Name) %>%
  distinct() %>%
  arrange(DBCode)

MAP <- MAP %>%
  mutate(IsRepresentative = IsRepresentative == 1,
         DelDispName = paste(mgrName, AssetClass, Region, Style, sep = "|")) %>%
  filter(DelCode %in% RETS$DelCode) %>%
  left_join(FUNDS, by = "DBCode")  #%>%
  #filter(!(DelCode %in% EXCP$DelCode))

