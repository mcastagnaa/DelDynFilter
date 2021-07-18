library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

rm(list = ls())

### SETUP ######################################
source("datamanagement.R")
source("f_getTable.R")


d1 <- max(RETS$Date[RETS$Date <= (ydm(refDate)-1)])
w1 <- max(RETS$Date[RETS$Date <= (ydm(refDate)-7)])
y1 <- max(RETS$Date[RETS$Date <= (refDate %m-% months(12))])
MtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-%m-01"))-1])
YtD <- max(RETS$Date[RETS$Date <= as.Date(format(refDate, "%Y-01-01"))-1])

datesFrame <- data.frame(Label = c("1 day", "1 week", "MtD", "YtD", "1y"), 
                         Date = c(d1, w1, MtD, YtD, y1),
                         stringsAsFactors = F)
rm(d1, w1, y1, MtD, YtD)

groups <- c("AssetClass")
input1 <- "Internal"
input2 <- "Main"
refDate <- as.Date("2021-07-07")
chartFrame <- "YtD"

thisMAP <- MAP %>%
  {if (input1 == "Internal") filter(., mgrName == "MIFL") else .} %>%
  {if (input2 == "Main") filter(., IsRepresentative) else .} %>%
  arrange(across(all_of(groups))) %>%
  group_by(across(all_of(groups))) %>%
  select(DelCode, all_of(groups), DelDispName) 

thisRetsSet <- RETS %>%
  filter(DelCode %in% thisMAP$DelCode) %>%
  left_join(datesFrame, by = "Date") %>%
  arrange(Date) %>%
  group_by(DelCode) %>%
  mutate(Label = ifelse(Date == min(Date), "SI",Label),
         Label = ifelse(Date == refDate, "Last", Label)) %>%
  filter(!is.na(Label)) %>%
  mutate(Del = round(last(PortIndex)/PortIndex-1,4)*100,
         SAA = round(last(SAAIndex)/SAAIndex-1,4)*100,
         ER = Del-SAA) %>%
  filter(Label != "Last") %>%
  select(DelCode, Label, Del, SAA, ER) %>%
  #mutate(Label = factor(Label, levels = c("1 day", "1 week", "MtD", "YtD", "1y", "SI"))) %>%
  pivot_longer(-c(DelCode, Label)) 

chart <- thisRetsSet %>%
  filter(Label == chartFrame,
         name != "ER") %>%
  select(-Label) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  ggplot(aes(x = SAA, y = Del, label = DelCode)) +
  geom_abline(slope = 1, color = "grey", linetype = 2) +
  geom_point() +
  geom_text_repel() +
  theme_bw()

chart
