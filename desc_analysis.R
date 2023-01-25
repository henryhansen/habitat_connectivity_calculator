
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(viridis)

# read data ---------------------------------------------------------------

lhd <- read.csv("extracted_data/lifeHistoryDistances_VG_G.csv")

stages <- c("Larval",
            "Juvenile",
            "Summer",
            "Winter",
            "Spawning")

# plot distances by survivors ---------------------------------------------

#number of individuals flushed
lhd %>% 
  group_by(discharge) %>%
  mutate(flushed = if_else(distance > 54, 1, 0, NA_real_)) %>% 
  group_by(discharge, flushed) %>% 
  summarise(n = n()/5) %>% 
  spread(flushed, n)-> lhd_flush

write.csv(lhd_flush, "Tables/flushed.csv")


lhd %>% 
  filter(distance < 55) %>% 
  group_by(life.history,discharge) %>% 
  # distinct(rkm,habitat_type, .keep_all = TRUE) %>%
  summarise(n = n(),
            md = mean(distance),
            sdd =sd(distance)) %>% 
  mutate(se = sdd/sqrt(n)) -> overview

overview$life.history <- factor(overview$life.history,
                                levels=c("Larval","Juvenile","Summer","Winter","Spawning"), 
                                ordered = T)

# list averages per life history stage
overview %>% 
  group_by(life.history) %>% 
  summarise(mean = mean(md),
            sd = sd(md))

overview_plot <- ggplot(overview, aes(x = life.history, y = md, fill = factor(discharge))) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = md - se, ymax = md + se, ),
                width=.2,
                position=position_dodge(.9)) +
  labs(x = "Life History Stages", 
       y = "Mean Minimum Distance (km)",
       fill = expression(paste("Discharge ","m"^"3"))) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0,12)) +
  theme_minimal()

overview_plot

tiff(filename = "Figures/overview_plot.tif",
     width = 200,
     height = 100,
     units = "mm",
     res = 300)
overview_plot
dev.off()


# other data exploration --------------------------------------------------


by(lhd, lhd$discharge, summary)
by(lhd, lhd$life.history, summary)


# no difference between life histories but increasing distance with discharge
ggplot(lhd,aes(x = discharge, y = distance, color = life.history)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_minimal()


# no difference between life histories but increasing distance with discharge
ggplot(lhd,aes(x = discharge, y = distance, color = life.history)) +
  geom_point() +
  geom_smooth() +
  geom_jitter() +
  theme_minimal()


