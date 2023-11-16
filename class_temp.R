library(tidyverse)

#DATA MANAGEMENT
data <- read.csv("data.csv")
weather <- read.csv("weather.csv")

##MEAN WEATHER/DAY
weather_day <- weather %>% 
  group_by(DATE) %>% 
  summarize_at(vars(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN), list(mean = mean), na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  separate(DATE, sep = "-", c("year", "month", "day"))

weather_day$ymd <- paste(weather_day$year, weather_day$month, weather_day$day, sep = "/") 
weather_day$ymd <- parse_date(weather_day$ymd, "%Y/%m/%d")

##BIRDS
birds <- data %>% 
  filter(taxon_class_name == "Aves") %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month, day) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

birds$ymd <- paste(birds$year, birds$month, birds$day, sep = "/") 
birds$ymd <- parse_date(birds$ymd, "%Y/%m/%d")

birds <- merge(birds, weather_day, by = "ymd")

bird_temp <- birds %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  group_by(TAVG_mean) %>% 
  summarize_at(vars(mean), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

##MAMMALS
mammals <- data %>% 
  filter(taxon_class_name == "Mammalia") %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month, day) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

mammals$ymd <- paste(mammals$year, mammals$month, mammals$day, sep = "/") 
mammals$ymd <- parse_date(mammals$ymd, "%Y/%m/%d")

mammals <- merge(mammals, weather_day, by = "ymd")

mammal_temp <- mammals %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  group_by(TAVG_mean) %>% 
  summarize_at(vars(mean), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

##INSECTS
insects <- data %>% 
  filter(taxon_class_name == "Insecta") %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month, day) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

insects$ymd <- paste(insects$year, insects$month, insects$day, sep = "/") 
insects$ymd <- parse_date(insects$ymd, "%Y/%m/%d")

insects <- merge(insects, weather_day, by = "ymd")

insect_temp <- insects %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  group_by(TAVG_mean) %>% 
  summarize_at(vars(mean), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

##REPTILES
reptiles <- data %>% 
  filter(taxon_class_name == "Reptilia") %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month, day) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

reptiles$ymd <- paste(reptiles$year, reptiles$month, reptiles$day, sep = "/") 
reptiles$ymd <- parse_date(reptiles$ymd, "%Y/%m/%d")

reptiles <- merge(reptiles, weather_day, by = "ymd")

reptile_temp <- reptiles %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  group_by(TAVG_mean) %>% 
  summarize_at(vars(mean), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

##AMPHIBIANS
amphibians <- data %>% 
  filter(taxon_class_name == "Amphibia") %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month, day) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

amphibians$ymd <- paste(amphibians$year, amphibians$month, amphibians$day, sep = "/") 
amphibians$ymd <- parse_date(amphibians$ymd, "%Y/%m/%d")

amphibians <- merge(amphibians, weather_day, by = "ymd")

amphibian_temp <- amphibians %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  group_by(TAVG_mean) %>% 
  summarize_at(vars(mean), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#PLOT
ggplot() +
  geom_area(data = bird_temp,
             aes(TAVG_mean, mean),
             color = "#50A2A7", fill = "#50A2A7", alpha = 0.2, linetype = 1, size = 0.4) +
  geom_area(data = insect_temp,
            aes(TAVG_mean, mean),
            color = "#E4D6A7", fill = "#E4D6A7", alpha = 0.2, linetype = 2, size = 0.4) +
  geom_area(data = mammal_temp,
             aes(TAVG_mean, mean),
             color = "#E9B44C", fill = "#E9B44C", alpha = 0.2, linetype = 3, size = 0.4) +
  geom_area(data = amphibian_temp,
            aes(TAVG_mean, mean),
            color = "#9B2915", fill = "#9B2915", alpha = 0.2, linetype = 4, size = 0.4) +
  geom_area(data = reptile_temp,
             aes(TAVG_mean, mean),
             color = "#1C110A", fill = "#1C110A", alpha = 0.2, linetype = 5, size = 0.4) +
  geom_rect(aes(xmin = -13, xmax = -2, ymin = 19, ymax = 31), 
            fill = "#1C110A") +
  geom_rect(aes(xmin = -13, xmax = -2, ymin = 19, ymax = 31), 
            fill = "#E4D6A7", alpha = 0.15, color = "#E4D6A7") +
  geom_text(aes(x = -8, y = c(21, 23, 25, 27, 29), 
                label = c("Reptiles", "Amphibians", "Mammals", "Insects", "Birds")),
            color = c("#1C110A", "#9B2915", "#E9B44C", "#E4D6A7", "#50A2A7"),
            hjust = 0,
            family = "Candara",
            size = 3) +
  geom_segment(aes(x = -11.5, xend = -8.5, y = c(21, 23, 25, 27, 29), yend = c(21, 23, 25, 27, 29)),
               color = c("#1C110A", "#9B2915", "#E9B44C", "#E4D6A7", "#50A2A7"),
               linetype = 5:1) +
  labs(title = "Animal Observations as a Funtion of Temperature",
       subtitle = "Mean number of observations per class",
       x = "Temperature (°C)",
       y = "Avg. # Observations") +
  theme(text = element_text(color = "#E9B44C", family = "Candara"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.text = element_text(color = "#E4D6A7"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#1C110A"),
        panel.background = element_rect(fill = "#1C110A"),
        panel.grid = element_line(color = "#E4D6A7", size = 0.05),
        plot.margin =margin(0.2, 0.2, 0.2, 0.2, unit = "in"))

ggsave("class_temp.png", width = 7.4, height = 4.4, units = "in", dpi = 600)
