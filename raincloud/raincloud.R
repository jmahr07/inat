library(tidyverse)

#DATA MANAGEMENT
data <- read.csv("data.csv")
weather <- read.csv("weather.csv")

weather_avg <- weather %>% 
  group_by(DATE) %>% 
  summarize_at(vars(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN), list(mean = mean), na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, digits = 2)

weather_aug <- weather_avg %>% 
  separate(DATE, sep = "-", c("year", "month", "day")) %>% 
  filter(year == "2020" & month == "08") 

data_aug <- data %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  filter(year == "2020" & month == "08") %>% 
  count(day)

#PLOT
ggplot() +
  
#OBSERVATION COLUMNS 
  geom_col(data = data_aug, 
           aes(1:31, n/10),
           fill = "#023047") +
  
#RAINFALL SEGMENTS
  geom_segment(data = weather_aug, 
               aes(x = 1:31, xend = 1:31, y = 30, yend = 30-PRCP_mean),
               linetype = 3, size = 0.25, color = "#219EBC") +
  
#CLOUD  
  geom_point(aes(x = c(10, 23), y = 35.8),
             size = 33, color = "darkgrey", alpha = 0.8) +
  geom_point(aes(x = 13, y = 39.5),
             size = 50, color = "grey") +
  geom_point(aes(x = c(11, 28), y = c(39.5, 38.5)),
             size = c(45, 50), color = "lightgrey") +
  geom_point(aes(x = c(5.5, 20.5), y = 36.5),
             size = 30, color = "grey") +
  geom_point(aes(x = c(03, 18), y = 38.5),
             size = 50, color = "lightgrey") +
  
#DAY LABELS  
  geom_text(data = data_aug, aes(x = 1:31, y = -0.5, label = day),
            color = "#219EBC", size = 1, family = "Book Antiqua", fontface = "bold") +
  
#OBSERVATION LABELS  
  geom_text(data = data_aug, aes(x = 1:31, y = n/20, label = n),
            color = "white", size = 1, family = "Book Antiqua") +
  
#RAINFALL LABELS  
  geom_text(data = weather_aug, aes(x = 1:31, y = 29.75-PRCP_mean, label = PRCP_mean),
            color = "#023047", size = 1, family = "Book Antiqua") +
  
#TITLE & SUBTITLE 
  geom_text(aes(x = 1, y = 39, label = "Does Rain Affect Daily Number of iNaturalist Observations?"),
            hjust = 0, color = "#023047", size = 2.5, family = "Book Antiqua", fontface = "bold") +
  geom_text(aes(x = 1, y = 37.6, label = "A look at August 2020"),
            hjust = 0, color = "#023047", size = 2, family = "Book Antiqua", fontface = "italic") +
  
#AXES LABEL  
  geom_text(aes(x = 0, y = 5, label = "# of Reported Observations"),
            color = "#023047", size = 1.5, family = "Book Antiqua", angle = 90) +
  geom_text(aes(x = 1.5, y = 20, label = "Daily Rainfall (cm)"),
            color = "#219EBC", size = 1.5, family = "Book Antiqua", angle = 90) +
  
#THEME  
  theme_void() +
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, face = "italic"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        panel.background = element_rect(fill = "#8ECAE6", color = NA),
        plot.background = element_rect(fill = "#FFB703"),
        panel.grid = element_blank(),
        axis.text = element_blank())


ggsave("PRCP_aug.png", width = 4, height = 4, units = "in", dpi = 900)
