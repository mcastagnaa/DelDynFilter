#AzureAuth::clean_token_directory()
#AzureGraph::delete_graph_login("maml.sharepoint.com")

# token <- AzureAuth::get_azure_token(tenant = "maml.sharepoint.com", 
#                                     resource = "https://maml.sharepoint.com/",
#                                     app = "8d83ba2140a51fde0b9da054f011d61a")

if(readline(prompt = "Connect to Sharepoint (Yes/anything)? ") == "Yes") {
  site <- tryCatch(get_sharepoint_site(site_id = "dc4edaeb-1257-40ab-8758-018b7b5bda5a"),
                   error = function(e) e)
  
  if(typeof(site)== "environment") {
    docs <- site$get_drive()
    
    ## dest <- tempfile("whatever.Rda")
    # docs$download_file("Stuff/DelSet.Rda", dest=dest)
    # load(dest)
    
    docs$download_file("Stuff/DelSet.Rda", overwrite = T)
  }
}

pwd <- read.csv("pwd.csv") 

user_base <- tibble::tibble(
  user = pwd$user,
  password = sapply(pwd$password, sodium::password_store),
  permissions = pwd$permission,
  name = pwd$name
)


load("DelSet.Rda")

RETS <- RETS %>%
  mutate(weekday = weekdays(Date)) %>%
  filter(!(weekday %in% c("Sunday", "Saturday"))) %>%
  select(-weekday)

FUNDS <- RETS %>%
  select(DBCode, FundName = Fund_Name) %>%
  distinct() %>%
  arrange(DBCode)

MAP <- MAP %>%
  mutate(IsRepresentative = IsRepresentative == 1,
         DelDispName = paste(mgrName, AssetClass, Region, Style, sep = "|")) %>%
  mutate(hasRets = DelCode %in% RETS$DelCode) %>%
  #filter(DelCode %in% RETS$DelCode) %>%
  left_join(FUNDS, by = "DBCode")  #%>%
  #filter(!(DelCode %in% EXCP$DelCode))

tTests <- tTests %>%
  mutate(Date = as.Date(Date),
         StatDate = as.Date(StatDate))

RANKS <- RANKS %>%
  mutate(DelCode = as.character(DelCode))


RBCidxData <- RBCidxData %>%
  mutate(weekday = weekdays(Date)) %>%
  filter(!(weekday %in% c("Sunday", "Saturday"))) %>%
  select(-weekday) %>%
  left_join(RETS[, c("DelCode", "Date", "Fund_Name", "DelegateManager", "SAAIndex", "DBCode")], by = c("DelCode","Date")) %>%
  rename(AUM = RBCAdj, PortIndex = NAV, Cashflow = NewCfl)

TURN <- TURN %>%
  select(DelCode, AM = mgrName, AssetClass, Region, Style, buy = buyTrades, sell= sellTrades, 
         Cashflows = totCfl, Turnover = UCITStnv) %>%
  arrange(desc(Turnover))

mainSet <- RBCidxData
compSet <- RETS
