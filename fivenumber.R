library(tidyverse)
data <- read.csv("data.csv")

#DATA MANAGEMENT
data_day_raw <- data %>% 
  separate(observed_on, into = c("year", "month", "day"), sep = "-") %>% 
  count(year, month, day) %>% 
  group_by(month, day)

data_day_raw$date <- paste(data_day_raw$month, data_day_raw$day, sep = "/")

data_month <- data %>% 
  separate(observed_on, into = c("year", "month", "day"), sep = "-") %>%
  count(year, month, day) %>% 
  group_by(month) %>% 
  summarize(min = fivenum(n)[1],
            Q1 = fivenum(n)[2],
            median = fivenum(n)[3],
            mean = mean(n),
            Q3 = fivenum(n)[4],
            max = fivenum(n)[5]) %>% 
  mutate_if(is.numeric, round, digits = 1)

#PLOT
ggplot() +
  
#BOXPLOT
  geom_boxplot(data = data_day_raw, aes(month, n), 
               color = "#233D4D",
               fill = "#619B8A",
               outlier.shape = NA) +

#MEAN POINT
  geom_point(data = data_month, aes(month, mean),
             shape = 21,
             color = "#233D4D",
             fill = "#FCCA46",
             size = 2) +

#MIN LABEL
  geom_text(data = data_month, aes(month, c(Q3+5), label = paste("Min=", min, sep = "")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +
  
#Q1 LABEL
  geom_text(data = data_month, aes(month, c(Q3+10), label = paste("Q1 =", Q1, sep = " ")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +

#MEDIAN LABEL
  geom_text(data = data_month, aes(month, c(Q3+15), label = paste("Mdn =", median, sep = " ")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +
  
#MEAN LABEL
  geom_text(data = data_month, aes(month, c(Q3+20), label = paste("Mean =", mean, sep = " ")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +
  
#Q3 LABEL
  geom_text(data = data_month, aes(month, c(Q3+25), label = paste("Q3 =", Q3, sep = " ")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +
  
#MAX LABEL  
  geom_text(data = data_month, aes(month, c(Q3+30), label = paste("Max =", max, sep = " ")),
            hjust = 0,
            nudge_x = 0.05,
            size = 2,
            color = "#233D4D",
            family = "SimSun") +
  
#COORD ADJUSTMENT TO IGNORE OUTLIERS
  coord_cartesian(ylim = c(0, 250)) +

  
#LABS  
  labs(title = "iNaturalist Observations Reported Each Month",
       subtitle = "Accumulated data since 1965",
       x = "Month",
       y = "# of Observations") +
  
#THEME  
  theme(panel.background = element_rect(fill = "#CCDDBB"),
        plot.background = element_rect(fill = "#CCDDBB"),
        panel.grid = element_line(color = "#619B8A",
                                  size = 0.1),
        text = element_text(color = "#233D4D",
                            family = "SimSun"),
        axis.text = element_text(size = 5),
        axis.ticks = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "in"))

ggsave("stats_obs.png", width = 10.4, height = 5.4, units = "in", dpi = 900)
        