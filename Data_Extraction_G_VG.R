
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)
library(viridis)

# add rkm index -----------------------------------------------------------

rkm <- c(0.0,0.20,0.40,0.60,0.80,1.00,1.20,1.40,1.60,1.80,2.00,2.20,2.40,2.60,2.80,3.00,3.20,3.40,3.60,3.80,4.00,4.20,4.40,4.60,4.80,5.00,5.20,5.40,5.60,5.80,6.00,6.20,6.40,6.60,6.80,7.00,7.20,7.40,7.60,7.80,8.00,8.20,8.40,8.60,8.80,9.00,9.20,9.40,9.60,9.80,10.00,10.20,10.40,10.60,10.80,11.00,11.20,11.40,11.60,11.80,12.00,12.20,12.40,12.60,12.80,13.00,13.20,13.40,13.60,13.80,14.00,14.20,14.40,14.60,14.80,15.00,15.20,15.40,15.60,15.80,16.00,16.20,16.40,16.60,16.80,17.00,17.20,17.40,17.60,17.80,18.00,18.20,18.40,18.60,18.80,19.00,19.20,19.40,19.60,19.80,20.00,20.20,20.40,20.60,20.80,21.00,21.20,21.40,21.60,21.80,22.00,22.20,22.40,22.60,22.80,23.00,23.20,23.40,23.60,23.80,24.00,24.20,24.40,24.60,24.80,25.00,25.20,25.40,25.60,25.80,26.00,26.20,26.40,26.60,26.80,27.00,27.20,27.40,27.60,27.80,28.00,28.20,28.40,28.60,28.80,29.00,29.20,29.40,29.60,29.80,30.00,30.20,30.40,30.60,30.80,31.00,31.10,31.20,31.40,31.60,31.80,32.00,32.20,32.40,32.60,32.80,33.00,33.20,33.40,33.60,33.80,34.00,34.20,34.40,34.60,34.80,35.00,35.20,35.40,35.60,35.80,36.00,36.20,36.40,36.60,36.80,37.00,37.20,37.40,37.60,37.80,38.00,38.20,38.40,38.60,38.80,39.00,39.20,39.30,39.40,39.60,39.80,40.00,40.20,40.40,40.60,40.80,41.00,41.20,41.40,41.60,41.80,42.00,42.20,42.40,42.60,42.80,43.00,43.20,43.40,43.60,43.80,44.00,44.20,44.40,44.60,44.80,45.00,45.10,45.20,45.40,45.60,45.80,46.00,46.20,46.40,46.60,46.80,46.90,47.00,47.20,47.40,47.60,47.80,48.00,48.20,48.40,48.60,48.80,49.00,49.20,49.40,49.60,49.80,50.00,50.20,50.40,50.60,50.80,51.00,51.20,51.40,51.60,51.80,52.00,52.20,52.40,52.60,52.80,52.90,53.00)

# Prepare dat file for query ----------------------------------------------
path <- "2022_07_04_IllerHenry/LongitudinalSections/StatusQuo/Data"

file_list <- list.files(path) # list files

for (file in file_list){
temp_dataset <- read.csv(paste(path,"/",file,sep = ""), header=TRUE) # read dat files

temp_dataset <- temp_dataset[,c("X0.4.0.6","X0.6.0.8","X0.8.1.0")]

temp_dataset$type <- paste(file) # add name from file as type col

temp_dataset$rkm <- rkm #add rkm col

if (exists("dataset"))
  dataset <- rbind(dataset, temp_dataset)


if (!exists("dataset"))
  dataset <- temp_dataset

}

rm(temp_dataset)

dataset %>%  # replace file names with type and add velocities column
  mutate(habitat_type = case_when(
    grepl("larva", type, fixed = T) ~ "Type 1",
    grepl("juv", type, fixed = T) ~ "Type 2",
    grepl("Aesche_sp", type, fixed = T) ~ "Type 3",
    grepl("Nase_sp", type, fixed = T) ~ "Type 4",
    grepl("Sommer", type, fixed = T) ~ "Type 5",
    grepl("Winter", type, fixed = T) ~ "Type 6")) %>% 
  mutate(discharge = case_when(
    grepl("3.0", type, fixed = T) ~ 3.0,
    grepl("_6.0", type, fixed = T) ~ 6.0,
    grepl("9.0", type, fixed = T) ~ 9.0,
    grepl("12.0", type, fixed = T) ~ 12.0,
    grepl("18.0", type, fixed = T) ~ 18.0,
    grepl("27.0", type, fixed = T) ~ 27.0,
    grepl("36.0", type, fixed = T) ~ 36.0,
    grepl("47.0", type, fixed = T) ~ 47.0,
    grepl("57.0", type, fixed = T) ~ 57.0,
    grepl("70.0", type, fixed = T) ~ 70.0
  )) %>% 
  rename("Fair" =X0.4.0.6, "Good" = X0.6.0.8, "Very Good" = X0.8.1.0) %>% 
  select(-type) -> dataset

# Extract Spawning Sites -------------------------------------------------

# Specify spawning reaches (must have both type 3 and 4 together at the same rkm)

temp_spawning3 <- dataset[(dataset$Fair > Inf | dataset$Good > 0 | dataset$`Very Good` > 0) & (dataset$habitat_type == "Type 3" ), ]
temp_spawning4 <- dataset[(dataset$Fair > Inf | dataset$Good > 0 | dataset$`Very Good` > 0) & (dataset$habitat_type == "Type 4" ), ]

temp_spawning <- rbind(temp_spawning3,temp_spawning4)

temp_spawning %>% 
  group_by(discharge,rkm) %>% 
  mutate(spawning_site = case_when(
    length(habitat_type) == 2 ~ 1,
    length(habitat_type) == 1 ~ 0)) %>% 
  filter(spawning_site == 1) -> spawning_sites

# quick plot of spawning sites
quick_spawn <- gather(spawning_sites, key = status, value = area, -c(4:6))

spawning_plot<- ggplot(data = quick_spawn, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(x = "river kilometer (0 = mouth, 53 = headwaters)", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)
# 
# png(filename = "Figures/spawning_plot_VG_G.png",
#     width = 1000,
#     units = "px")
# spawning_plot
# dev.off()


# Extract Distances -------------------------------------------------------

# Generate spawning site rkms for each discharge
spawning_sites %>% 
  group_by(discharge) %>% 
  distinct(rkm) -> spawning_index


# Create a larval distance function ---------------------------------------

larval_movement <- function(index, dataset, habitat, direction) {
  rkm <- index$rkm
  dis <- index$discharge
  temp_0 <- dataset[dataset$habitat_type == habitat & dataset$discharge == dis,]
  temp <- temp_0[which(rowSums(temp_0[,2:3]) > 0),  ]
  if (direction == "downstream") {
    if (any(temp$rkm - rkm <= 0)) {
      out <- temp[max(which(temp$rkm - rkm <= 0)),]
      out$distance <- abs(out$rkm - rkm)
      out$old_rkm <- rkm
    } else  {
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
    }
    return(out)

  }  else  {
    print("error")
  }
}

#test
# larval_movement(spawning_index[30,], dataset, habitat = "Type 1", "downstream")

# find distance to nearest larval site ------------------------------------
full_larval <- NULL
for (i in 1:nrow(spawning_index)) {
  near <- larval_movement(spawning_index[i,],
                  dataset,
                  habitat = "Type 1",
                  "downstream")
  neardf <- data.frame(near)
  full_larval <- rbind(full_larval,neardf)
}

ggplot(data = full_larval, aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~discharge)+
  labs(title = "Larval")

# quick plot of larval sites
quick_larval <- gather(full_larval[,2:8], key = status, value = area, -c(3:7))

ggplot(data = quick_larval, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  labs(x = "rkm", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types",
       title = "Larval Sites") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)

# create a juvenile year 0-10 cm function ---------------------------------------

juvenile_movement_010cm <- function(index, dataset, habitat, direction) {
  rkm <- index$rkm
  dis <- index$discharge
  temp_0 <- dataset[dataset$habitat_type == habitat & dataset$discharge == dis,]
  temp <- temp_0[which(rowSums(temp_0[,2:3]) > 0), ]
  if (direction == "downstream") {
    tryCatch(expr = {
      any(temp$rkm - rkm <= 0)
      out <- temp[max(which(temp$rkm - rkm <= 0)),]
      out$distance <- abs(out$rkm - rkm)
      out$old_rkm <- rkm
      return(out)
    }, warning = function(w){
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
      return(out)
    }, error = function(e){
      print("error")
    }
    )
    
  }  else  {
    print("error")
  }
}

# create larval index -----------------------------------------------------

larval_index <- full_larval[,c("rkm","discharge")]

# find distance to juvenile year 0 site -----------------------------------

full_juv0 <- NULL
for (i in 1:nrow(larval_index)) {
  near <- juvenile_movement_010cm(larval_index[i,],
                          dataset,
                          habitat = "Type 2",
                          "downstream")
  neardf <- data.frame(near)
  full_juv0 <- rbind(full_juv0,neardf)
}

ggplot(data = full_juv0, aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~discharge)+
  labs(title = "Juvenile 0-10cm")


# quick plot of juv 010 sites
quick_juv0 <- gather(full_juv0[,2:8], key = status, value = area, -c(3:7))

ggplot(data = quick_juv0, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000)) +
  labs(x = "rkm", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types",
       title = "Juvenile 0-10 cm Sites") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)

# Create summer adult function ----------------------------------------------

seasonal_adult_movement <- function(index, dataset, habitat, direction) {
  rkm <- index$rkm
  dis <- index$discharge
  temp_0 <- dataset[dataset$habitat_type == habitat & dataset$discharge == dis,]
  temp <- temp_0[which(rowSums(temp_0[,2:3]) > 0), ]
  
  if (direction == "either") {
    tryCatch(expr = {
      any(abs(temp$rkm - rkm) <= 53)
      potential <- temp[which(abs(temp$rkm - rkm) <= 53),]
      out <- potential[which.min(abs(potential$rkm - rkm)),]
      out$distance <- abs(out$rkm - rkm)
      out$old_rkm <- rkm
      return(out)
    }, warning = function(w){
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
      return(out)
    }, error = function(e){
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
      return(out)
    }
    )
    
  }  else  {
    print("error")
  }
}

# create juvenile 0 index -----------------------------------------------------

juv0_index <- full_juv0[,c("rkm","discharge")]


# find distance to summer sites ----------------------------------------------


full_summer <- NULL
for (i in 1:nrow(juv0_index)) {
  near <- seasonal_adult_movement(juv0_index[i,],
                                dataset,
                                habitat = "Type 5",
                                "either")
  neardf <- data.frame(near)
  full_summer <- rbind(full_summer,neardf)
}

ggplot(data = full_summer, aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~discharge) +
  labs(title = "Summer Adult")

# quick plot of summer sites
quick_summer <- gather(full_summer[,2:8], key = status, value = area, -c(3:7))

ggplot(data = quick_summer, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50000)) +
  labs(x = "rkm", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types",
       title = "Summer Adult Sites") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)



# use seasonal function for winter ----------------------------------------

# create summer index -----------------------------------------------------

summer_index <- full_summer[,c("rkm","discharge")]


# find distance to winter sites ----------------------------------------------


full_winter <- NULL
for (i in 1:nrow(summer_index)) {
  near <- seasonal_adult_movement(summer_index[i,],
                                  dataset,
                                  habitat = "Type 6",
                                  "either")
  neardf <- data.frame(near)
  full_winter <- rbind(full_winter, neardf)
}

ggplot(data = full_winter, aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~discharge) +
  labs(title = "Winter Adult")

# quick plot of winter sites
quick_winter <- gather(full_winter[2:8], key = status, value = area, -c(3:7))


ggplot(data = quick_winter, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50000)) +
  labs(x = "rkm", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types",
       title = "winter Adult Sites") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)


# Create a spawning adult function ----------------------------------------


spawning_movement <- function(index, dataset, habitat1, habitat2, direction) {
  rkm <- index$rkm
  dis <- index$discharge
  temp_0 <- dataset[(dataset$habitat_type == habitat1 | dataset$habitat_type == habitat2 ) & (dataset$discharge == dis),]
  temp <- temp_0[which(rowSums(temp_0[,2:3]) > 0), ]
  
  if (direction == "either") {
    tryCatch(expr = {
      any(abs(temp$rkm - rkm) <= 53)
      potential <- temp[which(abs(temp$rkm - rkm) <= 53),]
      out <- potential[which.min(abs(potential$rkm - rkm)),]
      out$distance <- abs(out$rkm - rkm)
      out$old_rkm <- rkm
      return(out)
    }, warning = function(w){
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
      return(out)
    }, error = function(e){
      out <- temp_0[1,]
      out$rkm <- NA
      out$old_rkm <- rkm
      out$distance <- 55
      out$Fair <- 0
      out$Good <- 0
      out$`Very Good` <- 0
      return(out)
    }
    )
    
  }  else  {
    print("error")
  }
}

# create winter index -----------------------------------------------------

winter_index <- full_winter[,c("rkm","discharge")]


# find distance to spawning site ----------------------------------------

full_spawn <- NULL
for (i in 1:nrow(winter_index)) {
  near <- spawning_movement(winter_index[i,],
                                dataset,
                                habitat1 = "Type 3",
                                habitat2 = "Type 4",
                                "either")
  neardf <- data.frame(near)
  full_spawn <- rbind(full_spawn,neardf)
}

ggplot(data = full_spawn, aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~discharge) +
  labs(title = "Spawning")

# quick plot of spawn sites
quick_spawn <- gather(full_spawn[2:8], key = status, value = area, -c(3:7))

ggplot(data = quick_spawn, aes(x = rkm, y = area, fill = habitat_type)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50000)) +
  labs(x = "rkm", 
       y = expression(paste("area ","m"^"2")),
       fill = "Habitat Types",
       title = "Spawning Sites") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  facet_wrap(~ discharge,nrow = 5)

# Export final dataset ----------------------------------------------------

full_lh <- rbind.data.frame(full_larval,
                 full_juv0,
                 full_summer,
                 full_winter,
                 full_spawn)

full_lh$life.history <- c(rep("Larval",141),
                          rep("Juvenile",141),
                          rep("Summer",141),
                          rep("Winter",141),
                          rep("Spawning",141))

write.csv(full_lh,"extracted_data/lifeHistoryDistances_VG_G.csv")

# create facets labeller for habitat plots --------------------------------

# create labels for plot facets
discharge_levels <- list(
  '3'=expression(Discharge ~3 ~ m^3 ~s^-1),
  '6'=expression(Discharge ~6 ~ m^3 ~s^-1),
  '9'=expression(Discharge ~9 ~ m^3 ~s^-1),
  '12'=expression(Discharge ~12 ~ m^3 ~s^-1),
  '18'=expression(Discharge ~18 ~ m^3 ~s^-1),
  '27'=expression(Discharge ~27 ~ m^3 ~s^-1),
  '36'=expression(Discharge ~36 ~ m^3 ~s^-1),
  '47'=expression(Discharge ~47 ~ m^3 ~s^-1),
  '57'=expression(Discharge ~57 ~ m^3 ~s^-1),
  '70'=expression(Discharge ~70 ~ m^3 ~s^-1)
)

discharge_labeller <- function(variable,value){
  return(discharge_levels[value])
}



# full_lh <- read.csv("extracted_data/lifeHistoryDistances_VG_G.csv")
# Plot habitat overviews with barriers-----------------------------------------

#read in barrier data

barriers <- read.csv("2022_07_04_IllerHenry/IllerBarriers.txt", sep = "\t")

#rearrange habitat data
quick_full <- gather(full_lh[2:9], key = status, value = area, -c(3:8))

# plotting order for life histories data
stages <- c("Larval",
            "Juvenile",
            "Summer",
            "Winter",
            "Spawning")

quick_full %>% 
  group_by(life.history, discharge) %>% 
  distinct(rkm,habitat_type, status, .keep_all = TRUE) -> quick_full
  

vcolor <- viridis(5)

lh_plot <- ggplot(data = quick_full, aes(x = rkm, y = area)) +
  geom_col(aes(fill = factor(life.history)), width = 0.2) +
  geom_rug(data = barriers, 
           aes(x = RKM, colour = "red"), 
           inherit.aes = F, 
           outside = T, 
           sides = "b", 
           size = 1,
           linetype = "dotted",
           show.legend = T) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(name = "Habitat Types", values = c("Larval" = "#440154FF", 
                                                       "Juvenile" = "#3B528BFF", 
                                                       "Summer" = "#21908CFF", 
                                                       "Winter" = "#5DC863FF",
                                                       "Spawning" = "#FDE725FF")) +
  # scale_fill_viridis_d(limits = stages) +
  labs(x = "river kilometer (0 = mouth, 53 = headwaters)",
       y = expression(paste("Area ","m"^"2"))) +
       # title = expression(paste("Habitats Used Throughout Lifecycle By Discharge m")^"3"/"s")) +
  scale_color_manual(name = "", labels = "Barriers", values ="red") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(colour = NA))) +
  facet_wrap(~ discharge,nrow = 5, labeller = discharge_labeller)

lh_plot

# png(filename = "Figures/lifehistory_plot_VG_G.png",
#     width = 1000,
#     units = "px")
# lh_plot
# dev.off()

tiff(filename = "Figures/lifehistory_plot_VG_G.tif",
     width = 200,
     height = 280,
     units = "mm",
     res = 300)
lh_plot
dev.off()



# calculate total areas by discharge --------------------------------------

quick_full %>% 
  group_by(discharge, life.history) %>% 
  summarise(sum = sum(area)) %>% 
  spread(life.history,sum) %>% 
  mutate(Total = Juvenile + Larval + Spawning + Summer + Winter) %>% 
  mutate_if(is.numeric, round,digits = 2)-> areas

write.csv(areas, "Tables/areas.csv")

# Plot of original habitat ------------------------------------------------
colnames(dataset) <- c("Fair","Good","Very_Good","rkm","habitat_type","discharge")

dataset %>% 
  mutate(life.history = recode(dataset$habitat_type,
                               "Type 1" = "Larval",
                               "Type 2" = "Juvenile",
                               "Type 3" = "Spawning",
                               "Type 4" = "Spawning",
                               "Type 5" = "Summer",
                               "Type 6" = "Winter")) %>% 
  select(2:7) %>% 
  mutate(habitat_area = dataset$Good + dataset$Very_Good) -> original

# plot original data
stages <- c("Larval",
            "Juvenile",
            "Summer",
            "Winter",
            "Spawning")


  
orig_plot <- ggplot(data = original, aes(x = rkm, y = habitat_area, fill = factor(life.history))) +
  geom_col(aes(fill = factor(life.history)), width = 0.2) +
  geom_rug(data = barriers, 
           aes(x = RKM, colour = "red"), 
           inherit.aes = F, 
           outside = T, 
           sides = "b", 
           size = 1,
           linetype = "dotted",
           show.legend = T) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(name = "Habitat Types", values = c("Larval" = "#440154FF", 
                                                       "Juvenile" = "#3B528BFF", 
                                                       "Summer" = "#21908CFF", 
                                                       "Winter" = "#5DC863FF",
                                                       "Spawning" = "#FDE725FF")) +
  # scale_fill_viridis_d(limits = stages) +
  labs(x = "river kilometer (0 = mouth, 53 = headwaters)",
       y = expression(paste("Area ","m"^"2"))) +
       # title = expression(paste("Original Habitats Assessed Throughout River By Discharge m")^"3"/"s")) +
  scale_color_manual(name = "", labels = "Barriers", values ="red") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour = "gray")) +
  theme(legend.position="bottom") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(colour = NA))) +
  facet_wrap(~ discharge,nrow = 5,labeller = discharge_labeller)

orig_plot

tiff(filename = "Figures/lifehistory_plot_orig.tif",
    width = 200,
    height = 280,
    units = "mm",
    res = 300)
orig_plot
dev.off()

