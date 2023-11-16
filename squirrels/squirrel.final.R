library(tidyverse)
library(patchwork)

#Read Data and Separate Date
data <- read.csv("data.csv")
data <- data %>% 
  separate(observed_on, sep = "-", c("year","month","day"))

#Find Squirrels
squirrels <- data %>% 
  select(common_name, year, month, latitude, longitude) %>% 
  filter(common_name == "Eastern Gray Squirrel" | common_name == "American Red Squirrel")

egs <- data %>% 
  select(common_name, year, month, latitude, longitude) %>% 
  filter(common_name == "Eastern Gray Squirrel")

ars <- data %>% 
  select(common_name, year, month, latitude, longitude) %>% 
  filter(common_name == "American Red Squirrel")

sq_avg <- squirrels %>% 
  group_by(common_name, year, month) %>% 
  count(year, month) %>% 
  group_by(common_name, month) %>% 
  summarize_at(vars(n), list(n = mean)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#Map
latlong <- ggplot(data, aes(longitude, latitude)) +
  geom_point(color = "gray", 
             shape = 20,
             size = 0.5,
             alpha = 0.5,
             position = "jitter") +
  coord_quickmap() +
  geom_point(data = subset(data, common_name == "Eastern Gray Squirrel"),
             shape = 21,
             fill = "#0076BF",
             color = "white") +
  geom_point(data = subset(data, common_name == "American Red Squirrel"), 
             shape = 21,
             fill = "#B80000",
             color = "white") +
  labs(title = "Squirrel Observations in Montreal") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(family = "Century Gothic"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "#E7E7E7"),
        panel.grid.major = element_line(size = 0.2),
        panel.grid.minor = element_line(linetype = "dashed"),
        axis.ticks = element_line(FALSE))

#Column
bar <- ggplot(sq_avg, aes(month, n)) +
  geom_col(aes(fill = common_name), 
           position = "dodge", 
           show.legend = FALSE) +
  scale_fill_manual(values = c("#B80000", "#0076BF")) +
  xlab("Month") +
  ylab("Avg. # of Observations") +
  theme(text = element_text(family = "Century Gothic"),
        plot.background = element_rect(fill = "white",
                                       color = "gray"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "#E7E7E7"),
        panel.grid.major = element_line(size = 0.2),
        panel.grid.minor = element_line(FALSE),
        axis.ticks = element_line(FALSE),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7)) +
  annotate("text", x = "01", y = 38,
           label = "Eastern Gray Squirrel",
           color = "#0076BF",
           hjust = 0,
           family = "Century Gothic",
           fontface = 2,
           size = 2.5) +
  annotate("text", x = "01", y = 33,
           label = "American Red Squirrel",
           color = "#B80000",
           hjust = 0,
           family = "Century Gothic",
           fontface = 2,
           size = 2.5)

#Patch & Save
latlong + inset_element(bar, 0.05,0.53,0.6,0.955)
ggsave("squirrels.png", width = 5, height = 5, units = "in", dpi = 320)
