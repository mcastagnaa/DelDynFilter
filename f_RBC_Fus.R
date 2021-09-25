#delCode = '689939'

f_RBC_Fus <- function(delCode) {
  
  Hline <- data.frame(panel = c("Flows(mn)", "Diff(perc)", "Abs(mn)"), Y = c(0, 0, NA))
  
  df <- Del_recon %>%
    filter(DelCode %in% delCode) %>%
    select(Date, Fusion = Fus_AUM, RBC = RBC_AUM, Fus_Cflow, NewCfl, AUMdiffPerc) %>%
    mutate(RBCadj = RBC + NewCfl,
           Fus_Cflow = ifelse(is.na(Fus_Cflow), 0, Fus_Cflow)) %>%
    select(-c(NewCfl, RBC)) %>%
    pivot_longer(-Date, names_to = "Object") %>%
    mutate(panel = case_when(Object %in% c("Fusion", "RBC", "RBCadj") ~ "Abs(mn)",
                             Object %in% c("AUMdiffPerc") ~ "Diff(perc)",
                             Object %in% c("Fus_Cflow") ~ "Flows(mn)"),
           value = ifelse(Object == "AUMdiffPerc", value * 100, value/1000000))
  
  chart <- ggplot() +
    geom_hline(data = Hline, aes(yintercept = Y), color = "dark grey", linetype = "dashed") +
    geom_line(data = filter(df, panel != "Flows(mn)"), aes(x = Date, y = value, color = Object)) +
    geom_bar(data = filter(df, panel == "Flows(mn)"), stat = "identity", aes(x = Date, y = value), color = "black", fill = NA) +
    facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    theme_bw() + 
    #theme(legend.position='bottom') +
    labs(x = "", y = ""
         , title = paste(delCode,
                         "-",
                         MAP$DelDispName[MAP$DelCode == delCode], "for",
                         FUNDS$FundName[FUNDS$DBCode == MAP$DBCode[MAP$DelCode == delCode]],
                         "- IsFund =", MAP$IsFund[MAP$DelCode == delCode]))
  
  return(chart)
}
