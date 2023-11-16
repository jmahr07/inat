library(tidyverse)
data <- read.csv("data.csv")

#DATA MANAGEMENT

##Birds
cd_birds <- data %>% 
  filter(taxon_class_name == "Aves") %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  count(year, month, day) %>%
  filter(year >= 2015)

cd_birds$ymd <- paste(cd_birds$year, cd_birds$month, cd_birds$day, sep = "/") 
cd_birds$ymd <- parse_date(cd_birds$ymd, "%Y/%m/%d")

##Mammals
cd_mammals <- data %>% 
  filter(taxon_class_name == "Mammalia") %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  count(year, month, day) %>% 
  filter(year >= 2015)

cd_mammals$ymd <- paste(cd_mammals$year, cd_mammals$month, cd_mammals$day, sep = "/") 
cd_mammals$ymd <- parse_date(cd_mammals$ymd, "%Y/%m/%d")

##Reptiles
cd_reptiles <- data %>% 
  filter(taxon_class_name == "Reptilia") %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  count(year, month, day) %>% 
  filter(year >= 2015)

cd_reptiles$ymd <- paste(cd_reptiles$year, cd_reptiles$month, cd_reptiles$day, sep = "/") 
cd_reptiles$ymd <- parse_date(cd_reptiles$ymd, "%Y/%m/%d")

##Insects
cd_insects <- data %>% 
  filter(taxon_class_name == "Insecta") %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  count(year, month, day) %>% 
  filter(year >= 2015)

cd_insects$ymd <- paste(cd_insects$year, cd_insects$month, cd_insects$day, sep = "/") 
cd_insects$ymd <- parse_date(cd_insects$ymd, "%Y/%m/%d")

##Fish
cd_fish <- data %>% 
  filter(taxon_class_name == "Actinopterygii") %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  count(year, month, day) %>% 
  filter(year >= 2015)

cd_fish$ymd <- paste(cd_fish$year, cd_fish$month, cd_fish$day, sep = "/") 
cd_fish$ymd <- parse_date(cd_fish$ymd, "%Y/%m/%d")

##PLOT
ggplot() +
  
  ###X-Axes
  geom_hline(aes(yintercept = seq(-750, 500, by = 10)),
             size = 0.1,
             color = "white") +
  geom_hline(aes(yintercept = seq(-750, 500, by = 50)),
             size = 0.1,
             color = "#aec3b0") +
  geom_hline(aes(yintercept = -500),
             size = 0.2,
             color = "#598392") +
  geom_hline(aes(yintercept = -250),
             size = 0.2,
             color = "#436875") +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             color = "#2D4D58") +
  geom_hline(aes(yintercept = 250),
             size = 0.2,
             color = "#17313B") +
  geom_hline(aes(yintercept = 500),
             size = 0.2,
             color = "#01161E") +
  
  ###Birds
  geom_text(data = cd_birds, aes(x = as.Date("2015/01/01"), y = -550,
                                 label= "Birds"),
            family = "OCR A Std", color = "#598392", hjust = 0) +
  geom_segment(data = cd_birds, 
               aes(x = ymd, 
                   xend = ymd, 
                   y = -n/2-500, 
                   yend = n/2-500),
               size = 0.2,
               color = "#598392") +
  
  ###Mammals
  geom_text(data = cd_mammals, aes(x = as.Date("2015/01/01"), y = -300,
                                 label= "Mammals"),
            family = "OCR A Std", color = "#436875", hjust = 0) +
  geom_segment(data = cd_mammals, 
               aes(x = ymd, 
                   xend = ymd, 
                   y = -n/2-250, 
                   yend = n/2-250),
               size = 0.2,
               color = "#436875") +
  
  ###Reptiles
  geom_text(data = cd_reptiles, aes(x = as.Date("2015/01/01"), y = -50,
                                   label= "Reptiles"),
            family = "OCR A Std", color = "#2D4D58", hjust = 0) +
  geom_segment(data = cd_reptiles, 
               aes(x = ymd, 
                   xend = ymd, 
                   y = -n/2, 
                   yend = n/2),
               size = 0.2,
               color = "#2D4D58") +
  
  ###Insects
  geom_text(data = cd_insects, aes(x = as.Date("2015/01/01"), y = 200,
                                    label= "Insects"),
            family = "OCR A Std", color = "#17313B", hjust = 0) +
  geom_segment(data = cd_insects, 
               aes(x = ymd, 
                   xend = ymd, 
                   y = -n/2+250, 
                   yend = n/2+250),
               size = 0.2,
               color = "#17313B") +
  
  ###Fish
  geom_text(data = cd_fish, aes(x = as.Date("2015/01/01"), y = 450,
                                   label= "Fish"),
            family = "OCR A Std", color = "#01161E", hjust = 0) +
  geom_segment(data = cd_fish, 
               aes(x = ymd, 
                   xend = ymd, 
                   y = -n/2+500, 
                   yend = n/2+500),
               size = 0.2,
               color = "#01161E") +
  
  ###Labels
  geom_segment(aes(x = as.Date("2015/01/01"), xend = as.Date("2015/01/01"),
                   y = -652, yend = -698),
               size = 0.2,
               arrow = arrow(length = unit(0.02, "in"), ends = "both"),
               color = "#aec3b0") +
  geom_text(aes(x = as.Date("2015/01/10"), y = -675, label = "=50 observations"),
            size = 1.5, family = "OCR A Std", color = "#aec3b0", hjust = 0) +
  labs(title = "Animal Observations Over Time, By Class",
       subtitle = "# of observations reported each day since 2015-01-01") +
  
  ###Theme
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#eff6e0"),
        panel.background = element_rect(fill = "#eff6e0"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2, color = "#aec3b0"),
        panel.grid.minor.x = element_line(size = 0.1, color = "white"),
        text = element_text(family = "OCR A Std", color = "#17313B"),
        plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "in"))

#SAVE
ggsave("classcount.png", width = 12, height = 6, units = "in", dpi = 900)
                     