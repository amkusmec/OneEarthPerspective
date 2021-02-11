library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)


# Analysis of average US corn yields --------------------------------------
d_yield <- read_csv("USDA_yields.csv") %>%
  select(Year, Value) %>%
  arrange(Year) %>%
  mutate(Era = if_else(Year < 1930, "Open-pollinated", 
                       if_else(Year < 1960, "Double Cross", 
                               if_else(Year < 1995, "Single Cross", "Biotech/GMO"))))

trends <- d_yield %>%
  group_by(Era) %>%
  group_modify(~ {
    model <- lm(Value ~ Year, data = .x)
    tibble(YearStart = min(.x$Year), 
           YearEnd = max(.x$Year), 
           ValueStart = predict(model, data.frame(Year = YearStart)), 
           ValueEnd = predict(model, data.frame(Year = YearEnd)), 
           Rate = coef(model)[2])
  }) %>%
  ungroup() %>%
  mutate(Rate = paste0("b=", round(Rate, 2)),
         YearMid = (YearStart + YearEnd)/2 + c(5, -5, 0, -10), 
         Y = c(110, 50, 40, 110))

pA <- ggplot(d_yield, aes(x = Year, y = Value)) + theme_bw() + 
  geom_point(aes(colour = Era), size = 2) + 
  geom_segment(aes(x = YearStart, xend = YearEnd, y = ValueStart, yend = ValueEnd), 
               data = trends, linetype = 2, size = 1) + 
  geom_text(aes(x = YearMid, y = Y, label = Rate), data = trends) + 
  scale_colour_brewer(type = "qual", palette = "Paired") + 
  scale_x_continuous(breaks = seq(1860, 2020, 10)) + 
  scale_y_continuous(breaks = seq(0, 200, 25), 
                     sec.axis = sec_axis(trans = ~ .*62.77, 
                                         name = "Average Corn Yield (kg/ha)", 
                                         breaks = seq(0, 11000, 1000))) + 
  labs(x = "Year", y = "Average Corn Yield (bu/a)") + 
  guides(colour = guide_legend(direction = "horizontal", nrow = 2)) + 
  theme(legend.position = c(0.3, 0.87), 
        legend.title = element_blank(), 
        # legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))


# Summary of agricultural R&D spending ------------------------------------
d_spend <- read_xls("agresearchfunding2015.xls", skip = 1L) %>%
  select(Year, `Public Ag R&D (billion 2013$)`, `Private Food & Ag  R&D (billion 2013$)`) %>%
  rename(Public = `Public Ag R&D (billion 2013$)`, 
         Private = `Private Food & Ag  R&D (billion 2013$)`) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Year)) %>%
  pivot_longer(-Year, names_to = "Sector", values_to = "Spending")

pB <- ggplot(d_spend, aes(x = Year, y = Spending)) + theme_bw() + 
  geom_line(aes(colour = Sector), size = 1.5) + 
  scale_y_continuous(breaks = seq(2.5, 12.5, 2.5), limits = c(2.5, 12.5)) + 
  scale_colour_manual(values = c("Public" = "blue", "Private" = "red")) + 
  labs(x = "Year", y = "Billion US$ (2013)") + 
  theme(legend.position = c(0.2, 0.87), 
        legend.title = element_blank(), 
        # legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"))


# Assemble the final figure -----------------------------------------------
lay <- matrix(c(1, 1, 2), nrow = 1)
grob1 <- grobTree(ggplotGrob(pA), 
                  textGrob("A", x = unit(0.03, "npc"), y = unit(0.975, "npc"), 
                           hjust = "left", vjust = "top", 
                           gp = gpar(fontface = "bold", fontsize = 14)))
grob2 <- grobTree(ggplotGrob(pB), 
                  textGrob("B", x = unit(0.03, "npc"), y = unit(0.975, "npc"), 
                           hjust = "left", vjust = "top", 
                           gp = gpar(fontface = "bold", fontsize = 14)))
g <- arrangeGrob(grob1, grob2, layout_matrix = lay)
ggsave("figure1.pdf", g, width = 9, height = 4, units = "in", dpi = 300)
