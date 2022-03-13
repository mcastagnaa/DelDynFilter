#delCode = as.data.frame(c('689939','684491'))

f_delComp <- function(delCode, source) {
  
  Hline <- data.frame(panel = c("Diff", "Abs"), Y = c(0,100))
  
  if(source == "RBC") mainSet <- RBCidxData else mainSet <- RETS
  
  chart <- mainSet %>%
    filter(DelCode %in% delCode[,1]) %>%
    select(DelCode, Date, MIO = PortIndex) %>%
    #mutate(DelCode = as.character(DelCode)) %>%
    arrange(Date) %>%
    group_by(DelCode) %>%
    tq_transmute(select     = MIO, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "MIO") %>%
    mutate(DateYM = format(Date, "%y%m")) %>%
    #filter(Date != as.Date("2019-12-31")) %>%
    select(-Date) %>%
    left_join(delData, by = c("DelCode", "DateYM")) %>%
    rename(Delegate = DelData) %>%
    select(-Date) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    pivot_longer(-c(DelCode, DateYM)) %>%
    arrange(DateYM) %>%
    group_by(DelCode) %>%
    mutate(value = ifelse(DateYM == "1912", 0, value),
           Cumulative = cumprod(1+value)*100) %>%
    select(-value) %>%
    pivot_wider(names_from = name, values_from = Cumulative) %>%
    mutate(Delta = MIO-Delegate) %>%
    pivot_longer(-c(DelCode, DateYM)) %>%
    mutate(panel = ifelse(name == "Delta", "Diff", "Abs"),
           DelCode = as.character(DelCode)) %>%
    ggplot(aes(x = as.Date(paste0(DateYM, "01"), "%y%m%d"), y = value, color = name)) +
    geom_hline(data = Hline, aes(yintercept = Y), color = "dark grey", linetype = "dashed") +
    geom_line() +
    facet_wrap(panel~DelCode, scales = "free_y", nrow = 2) +
    theme_bw() + 
    theme(legend.position='bottom') +
    labs(x = "", y = "")
  
  return(chart)
}
