
# Alba Aguion, (alba.aguion@duke.edu)
# October 1, 2024
# National archetypes from LCCA global model (Kenya, Philippines and Madagascar)

# package load
library(dplyr)
library(ggplot2)
library(svglite)
library(scales)

# read dataset
national_archetypes <- read.csv("Fig_national_archetypes.csv")

# relabel archetypes names
national_archetypes$archetype[which(national_archetypes$archetype == "1")] = "Arch. I"  
national_archetypes$archetype[which(national_archetypes$archetype == "2")] = "Arch. II"
national_archetypes$archetype[which(national_archetypes$archetype == "4")] = "Arch. IV"  
national_archetypes$archetype[which(national_archetypes$archetype == "5")] = "Arch. V" 

# set factors
national_archetypes <- national_archetypes %>% mutate_at(vars( "variable", "archetype", "country"), factor) 
national_archetypes$archetype <- factor(national_archetypes$archetype, levels = c("Arch. I", "Arch. II", "Arch. IV", "Arch. V"))

## Figure panel a (Kenya) ##

# Calculate the total catch for each ecosystem type
kenya <- subset (national_archetypes, country == "Kenya") %>%
  group_by(variable) %>%
  summarise(total_catch = sum(catch), .groups = 'drop')

kenya <- merge(subset (national_archetypes, country == "Kenya"), kenya, by = "variable")

# Create a variable to define the fraction for each ecosystem type
ken_donut <- kenya %>%
  group_by(variable) %>%
  mutate(fraction = catch / total_catch)

# Create a cumulative sum for the y values to help create the stacked donut
ken_donut <- ken_donut %>%
  arrange(variable, archetype) %>%
  mutate(ymax = cumsum(fraction),
         ymin = lag(ymax, default = 0))

# Adjust the center positioning for the label
kenya <- kenya %>% mutate(x_center = 2, y_center = 0) 

ken_donut$variable <- factor(ken_donut$variable, levels = c("Nearshore coastal", "Reef", "Benthic", "Pelagic"))

# Start the SVG device
# svglite("kenya_plot.svg", width = 5.02, height = 3.47)  # Adjust width and height as needed

ggplot(ken_donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = archetype)) +
  geom_rect() +
  geom_text(data = kenya,
            aes(x = x_center, y = y_center, label = comma(round(total_catch, 0))),
            size = 5, color = "black", inherit.aes = FALSE) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) + 
  facet_wrap(~variable, ncol = 2) +  
  theme_void() +  
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # Remove panel background
    plot.background = element_rect(fill = "transparent", color = NA),   # Remove plot background
    legend.position = "right", 
    strip.text = element_text(size = 12, face="bold")  # Adjust legend and facet label size
  ) +
  labs(title = "", fill = "Variable") +
  scale_fill_manual(values=c("#cae1ff", "#ffb3ba", "#baffc9","#ffa54f")) 

# Close the device to finalize the file
# dev.off()


## Figure panel b (Madagascar) ##

# Calculate the total catch for each province
madagascar <- subset (national_archetypes, country == "Madagascar") %>%
  group_by(variable) %>%
  summarise(total_catch = sum(catch), .groups = 'drop')

madagascar <- merge(subset (national_archetypes, country == "Madagascar"), madagascar, by = "variable")

# Create a variable to define the fraction for each province
mad_donut <- madagascar %>%
  group_by(variable) %>%
  mutate(fraction = catch / total_catch)

# Create a cumulative sum for the y values to help create the stacked donut
mad_donut <- mad_donut %>%
  arrange(variable, archetype) %>%
  mutate(ymax = cumsum(fraction),
         ymin = lag(ymax, default = 0))

# Adjust the center positioning for the label
madagascar <- madagascar %>% mutate(x_center = 2, y_center = 0) 

# Start the SVG device
# svglite("madagascar_plot.svg", width = 5.02, height = 3.47)  # Adjust width and height as needed

# Plotting using ggplot2
ggplot(mad_donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = archetype)) +
  geom_rect() +
  geom_text(data = madagascar,
            aes(x = x_center, y = y_center, label = comma(round(total_catch, 0))),
            size = 4, color = "black", inherit.aes = FALSE) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +  
  facet_wrap(~variable, ncol = 2) + 
  theme_void() +  
  theme(legend.position = "right", 
        strip.text = element_text(size = 12, face = "bold")) +  # Adjust legend and facet label size
  labs(title = "", fill = "Variable") +
  scale_fill_manual(values = c("#cae1ff", "#ffb3ba")) 

# dev.off()


## Figure panel c (Philippines) ##
philippines <- subset (national_archetypes, country == "Philippines")
philippines$variable <- factor(philippines$variable, levels = c("Male", "Female"))

# Calculate the total catch per gender
philippines1 <- philippines %>%
  group_by(variable) %>%
  summarise(total_catch = sum(catch), .groups = 'drop')


max_total_catch <- max(philippines1$total_catch)

philippines <- philippines  %>%
  group_by(variable) %>%
  mutate(scaled_harvest = catch / sum(catch) * max_total_catch)

# Start the SVG device
# svglite("philippines_plot.svg", width = 5.02, height = 3.47)  # Adjust width and height as needed

ggplot(philippines, aes(x = factor(1), y = scaled_harvest, fill = archetype)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~variable, ncol=1) +
  theme_void() +
  theme(legend.position = "right", strip.text = element_text(size = 12, face="bold")) +
  scale_fill_manual(values = c("#cae1ff", "#baffc9","#ffa54f")) +  # Customize colors as needed
  geom_text(aes(x = 0, y = 0, label = comma(round(philippines1$total_catch, 0))), size = 5, data = philippines[!duplicated(philippines$variable),]) +
  labs(title = "")

# dev.off()