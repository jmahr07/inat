#Libraries
library(tidyverse)

#Data
data <- read.csv("data.csv")
weather <- read.csv("weather.csv")

#Data Management
data_19_21 <- data %>% 
  filter(observed_on >= 2019 & observed_on < 2022) %>% 
  count(observed_on) %>% 
  separate(observed_on, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month) %>% 
  summarize_at(vars(n), list(mean = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

weather_avg <- weather %>% 
  separate(DATE, sep = "-", c("year", "month", "day")) %>% 
  group_by(year, month) %>% 
  summarize_at(vars(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN), list(mean = mean), na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, digits = 2)

all_data <- merge(data_19_21, weather_avg, by = c("year", "month"))
all_data$ym <- paste(all_data$year, all_data$month, sep = "/") 
all_data$ym <- parse_date(all_data$ym, "%Y/%m")

#Plot
ggplot(all_data) +
  geom_col(aes(ym, mean, fill = year), show.legend = FALSE) +
  scale_fill_manual(values = c("#E1E6DB", "#A5B394","#687755")) +
  geom_point(data = ~ filter(all_data, SNOW_mean > 1), 
             aes(ym, SNOW_mean*5), 
             shape = 8, color = "#F8FAF0", size = 0.5) +
  geom_segment(data = ~ filter(all_data, SNOW_mean > 1),
               aes(ym, 0, xend = ym, yend = SNOW_mean*5), 
               color = "#F8FAF0",
               linetype = "dotted",
               size = 0.1) +
  geom_point(aes(ym, PRCP_mean*5), 
             color = "#7191C1", size = 0.5) +
  geom_area(aes(ym, TAVG_mean*5), 
            color = "#DC6641", fill = "#DC6641", alpha = 0.1, size = 0.2) +
  geom_text(aes(ym, -5, label = month), 
            size = 1.5, 
            family = "Letter Gothic Std", 
            color = "#EDDDD4", 
            fontface = "bold") +
  geom_text(aes(ym, mean, label = mean),
            size = 1.5, 
            vjust = -0.4,
            hjust = -0.3,
            angle = 90,
            family = "Letter Gothic Std", 
            color = "#EDDDD4", 
            fontface = "bold") +
  geom_text(aes(ym, -65, label = TAVG_mean), 
            size = 1, 
            color = "#DC6641", 
            family = "Letter Gothic Std") +
  geom_text(aes(ym, -75, label = PRCP_mean),
            size = 1, 
            color = "#7191C1", 
            family = "Letter Gothic Std") +
  geom_text(aes(ym, -85, label = SNOW_mean),
            size = 1, 
            color = "#F8FAF0",
            family = "Letter Gothic Std") +
  geom_text(aes(as.Date("2019-02-01"), 190, label = "Temperature"), 
            color = "#DC6641", family = "Letter Gothic Std", hjust = 0, size = 3) +
  geom_text(aes(as.Date("2019-02-01"), 175, label = "Precipitation"), 
            color = "#7191C1", family = "Letter Gothic Std", hjust = 0, size = 3) +
  geom_text(aes(as.Date("2019-02-01"), 160, label = "Snowfall"), 
            color = "#F8FAF0", family = "Letter Gothic Std", hjust = 0, size = 3) +
  scale_y_continuous("Avg. # of Observations", 
                     sec.axis = sec_axis(~./5, name = "°C / cm")) +
  xlab(NULL) +
  labs(title = "iNaturalist Observations in Montreal",
       subtitle = "Plotted against weather data",
       caption = "Sources: iNaturalist, NOAA") +
  theme(text = element_text(family = "Letter Gothic Std", color = "#EDDDD4"),
        panel.background = element_rect(fill = "#27233A"),
        plot.background = element_rect(fill = "#27233A"),
        axis.text = element_text(color = "#EDDDD4", size = 5),
        axis.ticks = element_blank(),
        panel.grid = element_line(size = 0.05, color = "#EDDDD4"))

#Save
ggsave("weather.png", width = 6.5, height = 4.5, units = "in", dpi = 320)
