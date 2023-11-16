library(tidyverse)
birds <- data %>% filter(taxon_class_name == "Aves")

#DATA MANAGEMENT
top_orders <- birds %>% 
  count(taxon_order_name) %>% 
  top_n(5)

top_family <- birds %>% 
  filter(taxon_order_name %in% top_orders$taxon_order_name) %>% 
  count(taxon_order_name, taxon_family_name) %>%
  group_by(taxon_order_name) %>% 
  top_n(5)

top_genus <- birds %>% 
  filter(taxon_family_name %in% top_family$taxon_family_name) %>% 
  count(taxon_order_name, taxon_family_name, taxon_genus_name) %>%
  group_by(taxon_order_name) %>% 
  top_n(5)

top_species <- birds %>% 
  filter(taxon_genus_name %in% top_genus$taxon_genus_name) %>% 
  count(taxon_order_name, taxon_family_name, taxon_genus_name, taxon_species_name, common_name) %>%
  group_by(taxon_order_name) %>% 
  top_n(5)

##LIMITS
###ORDER
xmax_o <- c(sum(top_species$n[1:5]),
            sum(top_species$n[6:10]),
            sum(top_species$n[11:15]),
            sum(top_species$n[16:20]),
            sum(top_species$n[21:25]))

top_species$xmax_o <- c(rep(sum(top_species$n[1:5]),5),
                        rep(sum(top_species$n[6:10]),5),
                        rep(sum(top_species$n[11:15]),5),
                        rep(sum(top_species$n[16:20]),5),
                        rep(sum(top_species$n[21:25]),5))

###SPECIES
top_species$xmin_s <- c(0, top_species$n[1], sum(top_species$n[1:2]), sum(top_species$n[1:3]), sum(top_species$n[1:4]),
                        0, top_species$n[6], sum(top_species$n[6:7]), sum(top_species$n[6:8]), sum(top_species$n[6:9]),
                        0, top_species$n[11], sum(top_species$n[11:12]), sum(top_species$n[11:13]), sum(top_species$n[11:14]),
                        0, top_species$n[16], sum(top_species$n[16:17]), sum(top_species$n[16:18]), sum(top_species$n[16:19]),
                        0, top_species$n[21], sum(top_species$n[21:22]), sum(top_species$n[21:23]), sum(top_species$n[21:24]))

top_species$xmax_s <- c(top_species$n[1], sum(top_species$n[1:2]), sum(top_species$n[1:3]), sum(top_species$n[1:4]), sum(top_species$n[1:5]),
                        top_species$n[6], sum(top_species$n[6:7]), sum(top_species$n[6:8]), sum(top_species$n[6:9]), sum(top_species$n[6:10]),
                        top_species$n[11], sum(top_species$n[11:12]), sum(top_species$n[11:13]), sum(top_species$n[11:14]), sum(top_species$n[11:15]),
                        top_species$n[16], sum(top_species$n[16:17]), sum(top_species$n[16:18]), sum(top_species$n[16:19]), sum(top_species$n[16:20]),
                        top_species$n[21], sum(top_species$n[21:22]), sum(top_species$n[21:23]), sum(top_species$n[21:24]), sum(top_species$n[21:25]))

###GENUS
x_g <- c(0, top_species$n[1], sum(top_species$n[1:2]), sum(top_species$n[1:3]), sum(top_species$n[1:4]), sum(top_species$n[1:5]),
         0, top_species$n[6], sum(top_species$n[6:7]), sum(top_species$n[6:8]), sum(top_species$n[6:9]), sum(top_species$n[6:10]),
         0, top_species$n[11], sum(top_species$n[11:12]), sum(top_species$n[11:13]), sum(top_species$n[11:14]), sum(top_species$n[11:15]),
         0, sum(top_species$n[16:17]), sum(top_species$n[16:18]), sum(top_species$n[16:19]), sum(top_species$n[16:20]),
         0, top_species$n[21], sum(top_species$n[21:23]), sum(top_species$n[21:24]), sum(top_species$n[21:25]))

g_labels <- c(0, top_species$n[1], sum(top_species$n[1:2]), sum(top_species$n[1:3]), sum(top_species$n[1:4]),
              0, top_species$n[6], sum(top_species$n[6:7]), sum(top_species$n[6:8]), sum(top_species$n[6:9]),
              0, top_species$n[11], sum(top_species$n[11:12]), sum(top_species$n[11:13]), sum(top_species$n[11:14]),
              0, sum(top_species$n[16:17]), sum(top_species$n[16:18]), sum(top_species$n[16:19]),
              0, top_species$n[21], sum(top_species$n[21:23]), sum(top_species$n[21:24]))

###FAMILY
x_f <- c(0, 
         0, top_species$xmin_s[7], top_species$xmin_s[9],
         0, top_species$xmin_s[12:15],
         0,
         0)

x_fend <- c(top_species$xmax_s[5], 
            top_species$xmax_s[6], top_species$xmax_s[8], top_species$xmax_s[10],
            top_species$xmax_s[11:15],
            top_species$xmax_s[20],
            top_species$xmax_s[25])

#COLORS
species_fill <- c("#E8C0C0", "#D19090", "#BA6060", "#A33030", "#8C0000",
                  "#F0D3C0", "#E2B190", "#D38E60", "#C56C30", "#B64900",
                  "#CFE5C0", "#A8CB90", "#80B160", "#599730", "#327D00",
                  "#C0DCEF", "#90BFDF", "#60A2CE", "#3085BE", "#0068AE",
                  "#E1C8F7", "#CAA1EE", "#B379E6", "#9C52DD", "#852AD5")

#LABELS
family_list <- top_species %>% count(taxon_family_name)
genus_list <- top_species %>% count(taxon_genus_name)

#PLOT
ggplot() +
  
##ORDER COLUMNS  
  geom_rect(aes(xmin = 0, 
               xmax = xmax_o,  
               ymin = c(0, 10, 20, 30, 40), 
               ymax = c(5, 15, 25, 35, 45)),
            fill = c("#FFF0F0", "#FFF6F0", "#F6FFF0", "#F0F9FF", "#F8F0FF")) +
  
##ORDER LABELS
  geom_text(data = top_orders,
            aes(x = -30, y = c(2.5, 12.5, 22.5, 32.5, 42.5), label = taxon_order_name),
            size = 4, angle = 90,
            color = c("#8C0000", "#B64900", "#327D00", "#0068AE", "#852AD5"),
            family = "Cambria", fontface = "bold") +
  
##SPECIES LABELS  
  geom_text(data = top_species, 
            aes(x = top_species$xmax_o+10, 
                y = c(0.5:4.5, 10.5:14.5, 20.5:24.5, 30.5:34.5, 40.5:44.5), 
                label = paste(common_name,",", n)), 
            hjust = 0, size = 3.5, color = species_fill,
            family = "Cambria", fontface = "bold") +
  
##SPECIES COLUMNS  
  geom_rect(data = top_species, 
          aes(xmin = xmin_s, 
              xmax = xmax_s,
              ymin = c(0.25:4.25, 10.25:14.25, 20.25:24.25, 30.25:34.25, 40.25:44.25), 
              ymax = c(0.75:4.75, 10.75:14.75, 20.75:24.75, 30.75:34.75, 40.75:44.75)), 
          fill = species_fill) +
  
##GENUS SEGMENTS  
  geom_segment(aes(x = x_g,
                   xend = x_g,
                   y = c(rep(0,6), rep(10,6), rep(20,6), rep(30,5), rep(40,5)),
                   yend = c(rep(5.2,3), 6.2, 5.2, 5.2, 16.2, 15.2, 17.2, 16.2, 15.2, 15.2, rep(25.2,6), 35.2, 37.2, 36.2, 35.2, 35.2, rep(45.2,5))), 
               color = "gray") +

##GENUS LABELS
  geom_text(data = genus_list,
          aes(x = g_labels + 6, 
              y = c(rep(5.5,3), 6.5, 5.5, 16.5, 15.5, 17.5, 16.5, 15.5, rep(25.5,5), 35.5, 37.5, 36.5, 35.5, rep(45.5,4)),
              label = taxon_genus_name), size = 3, hjust = 0, color = "white",
          family = "Cambria") +

##FAMILY SEGMENTS
  geom_segment(aes(x = x_f+10,
                   xend = x_fend-10,
                   y = c(-1, rep(9,3), rep(19,5), 29, 39),
                   yend = c(-1, rep(9,3), rep(19,5), 29, 39)),
               color = "white") +
  
##FAMILY LABELS
  geom_text(data = family_list,
            aes(x = x_f+10,
                y = c(-0.5, 8.5, 9.5, 9.5, rep(19.5,5), 29.5, 39.5),
                label = taxon_family_name),
            size = 2, hjust = 0, color = "white",
            family = "Cambria") +
  
##KEY
  geom_rect(aes(xmin = 2150, xmax = 3250, ymin = 30, ymax = 45),
            fill = "black", color = "white") +
  geom_rect(aes(xmin = 2250, xmax = 2750, ymin = 35, ymax = 40),
            fill = "white") +
  geom_rect(aes(xmin = 2400, xmax = 2750, ymin = 39.25, ymax = 39.75),
            fill = "gray47") +
  geom_rect(aes(xmin = 2250, xmax = 2400, ymin = 38.25, ymax = 38.75),
            fill = "gray80") +
  geom_segment(aes(x = 2260, xend = 2740, y = 34, yend = 34),
            color = "white") +
  geom_segment(aes(x = 2400, xend = 2400, y = 35, yend = 40.2),
               color = "gray") +
  geom_text(aes(x = 2270, y = 34.5, label = "Family"),
            color = "white", hjust = 0, size = 2, family = "Cambria") +
  geom_text(aes(x = 2406, y = 40.5, label = "Genus"),
            color = "white", hjust = 0, size = 3, family = "Cambria") +
  geom_text(aes(x = 2760, y = 39.5, label = "Common Name, # of Observations"),
            color = "gray47", size = 3, hjust = 0, family = "Cambria", fontface = "bold") +
  geom_text(aes(x = 2230, y = 37.5, label = "Order"),
            color = "white", size = 4, angle = 90, family = "Cambria", fontface = "bold") +

##PLOT ADJUSTMENTS
  scale_x_continuous(limit = c(-30, 3300)) +
  scale_y_continuous(limit = c(-2, 50)) +
  labs(title = "5 Most Observed Bird Species of the 5 Most Observed Orders in Montreal",
       x = "Total Number of Observations",
       y = NULL) +
  theme(plot.background = element_rect(fill = "black"),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "in"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white", family = "Cambria"),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "gray50", size = 0.1))

#SAVE
ggsave("topbirds.png", width = 16, height = 8, units = "in", dpi = 640)