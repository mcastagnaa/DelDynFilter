#delCode <- data.frame(delcode = c("697132", "701878"))

f_getRanks <- function(delCode) {

PGs <- MAP %>%
  filter(DelCode %in% delCode[, 1]) %>%
  mutate(DelCode = as.character(DelCode)) %>%
  select(DelCode, DelDispName, MStarPG)


chart <- RANKS %>%
  filter(DelCode %in% delCode[, 1]) %>%
  select(DelCode, Date, starts_with("Rnk")) %>%
  pivot_longer(-c(Date, DelCode), names_to = "Frame") %>%
  mutate(Frame = gsub("Rnk", "", Frame),
         DelCode = as.character(DelCode),
         Frame = factor(Frame, levels = c("1m", "3m", "6m", "YtD", "1y", "3y", "5y", "SI"))) %>%
  filter(!is.na(value)) %>%
  ggplot() +
  geom_hline(yintercept = 25, color = "Green", linetype = "dashed") +
  geom_hline(yintercept = 50, color = "Orange", linetype = "dashed") +
  geom_hline(yintercept = 75, color = "Red", linetype = "dashed") +
  geom_line(aes(x = Date, y = value, color = DelCode)) +
  facet_wrap(~Frame, nrow = 2) +
  scale_y_reverse(limits = c(100, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "", y = "Percentile (lower is better)")
  
  return(list(PGs, chart))
}

