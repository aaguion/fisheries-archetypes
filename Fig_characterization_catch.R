
# Alba Aguion, (alba.aguion@duke.edu)
# September 30, 2024
# Paper figure ssf catch distribution along the characterization score at global, regional and national levels

# package load
library(dplyr)
library(ggplot2)
library(forcats)

# disable scientific notation
options(scipen = 999)

# read dataset
catch_char_country <- read.csv("Fig_characterization_catch.csv")

# set factors
catch_char_country <- catch_char_country %>% mutate_at(vars("country", "region", "total_marine_char"), factor) 

## Cumulative catch at the global level (panel a)

# Summing catch per year by characterization score
catch_calculations1 <- catch_char_country %>%
  group_by(total_marine_char) %>%
  summarise(across(starts_with("characterized_catch"), sum, na.rm = TRUE))

catch_calculations1 <- catch_calculations1 %>% mutate(across(-1, ~ na_if(., 0)))
catch_calculations1 <- catch_calculations1 %>% arrange(total_marine_char)

# Calculate the annual average catches per characterization score
catch_calculations1 <- catch_calculations1 %>%
  mutate(annual_average_catch = rowMeans(select(., starts_with("characterized_catch")), na.rm = TRUE))

# Calculate the cumulative catch and the percentage of cumulative catch per characterization score
cum_catch_global <- catch_calculations1 %>%
  mutate(cumulative_catch = cumsum(annual_average_catch),
         cumulative_catch_percentage = (cumulative_catch / sum(annual_average_catch)) * 100)

cum_catch_global$geog_resolution   <- "global"
cum_catch_global$lowest_resolution <- "Global"

## Cumulative catch at the global level (panel b)

# Summing catch per year by characterization score
catch_calculations2 <- catch_char_country %>%
  group_by(total_marine_char, region) %>%
  filter(!region %in% c("Europe", "Oceania")) %>%
  summarise(across(starts_with("characterized_catch"), sum, na.rm = TRUE)) 

catch_calculations2 <- catch_calculations2 %>% mutate(across(-1, ~ na_if(., 0)))
catch_calculations2 <- catch_calculations2 %>% arrange(total_marine_char)

# Calculate the annual average catches per characterization score
catch_calculations2 <- catch_calculations2 %>% ungroup() %>% 
  mutate(annual_average_catch = rowMeans(select(., starts_with("characterized_catch")), na.rm = TRUE))

# Calculate the cumulative catch and the percentage of cumulative catch per characterization score
cum_catch_regional <- catch_calculations2 %>%  group_by(region) %>%
  mutate(cumulative_catch = cumsum(annual_average_catch),
         cumulative_catch_percentage = (cumulative_catch / sum(annual_average_catch)) * 100)

cum_catch_regional <- cum_catch_regional %>% arrange(region)
cum_catch_regional$geog_resolution   <- "regional"
colnames(cum_catch_regional)[colnames(cum_catch_regional) == "region"] <- "lowest_resolution"

## Cumulative catch at the country level (panel c)

# Calculate the annual average catches per characterization score
catch_calculations3 <- catch_char_country %>% 
  mutate(annual_average_catch = rowMeans(select(., starts_with("characterized_catch")), na.rm = TRUE))

# Calculate the cumulative catch and the percentage of cumulative catch per characterization score
cum_catch_country <- catch_calculations3 %>%  group_by (country) %>%
  mutate(cumulative_catch = cumsum(annual_average_catch),
         cumulative_catch_percentage = (cumulative_catch / sum(annual_average_catch)) * 100)

cum_catch_country <- cum_catch_country %>% arrange(country)
cum_catch_country$geog_resolution   <- "country"
colnames(cum_catch_country)[colnames(cum_catch_country) == "country"] <- "lowest_resolution"
cum_catch_country <- select(cum_catch_country, -region)

# Exclude countries with insufficient data at the country level
cum_catch_country <- cum_catch_country %>% filter(!lowest_resolution %in% c("Gabon", "Congo", "Congo DR", "Liberia"))


## Cumulative catch dataset ##
cum_catch_data <- rbind (cum_catch_global, cum_catch_regional, cum_catch_country)
cum_catch_data  <- cum_catch_data  %>% arrange(lowest_resolution)
rm(list = setdiff(ls(), "cum_catch_data"))


## Figure ##

# Organize the order from global, regional to national levels for the figure
cum_catch_data$lowest_resolution <- as.factor(cum_catch_data$lowest_resolution)
cum_catch_data$total_marine_char <- as.numeric(as.character(cum_catch_data$total_marine_char))

cum_catch_data$lowest_resolution <- factor(cum_catch_data$lowest_resolution, 
                                           
                levels = c( "Global", "Asia", "Americas", "Africa", # global (a) and regional (b)
                            
                            # national (c) ordered by region
                            "Philippines", "India", "Maldives","Turkey", "Sri Lanka","China","Bangladesh","Thailand","Indonesia","Vietnam", "Iran", 
                            "Argentina","Peru","Barbados","St. Lucia","St. Vincent","Mexico","Chile","St. Kitts","Greenland",  
                            
                            "South Africa", "Kenya", "Seychelles","Madagascar","Tanzania", "Mozambique","Mauritania","Nigeria","Morocco",
                            "Egypt", "Gambia", "Guinea","Sierra Leone","Senegal","Guinea Bissau", 
                                                     
                             "Fiji", "Norway", "Spain", "United Kingdom"))


# Find the closest cum_catch_per value to 50 for each country
closest_to_50 <- cum_catch_data %>%
  group_by(lowest_resolution) %>%
  filter(abs(cumulative_catch_percentage - 50) == min(abs(cumulative_catch_percentage - 50))) %>%
  ungroup()


# Plot with discrete points and circles for values closest to 50%
ggplot(cum_catch_data) +
  
  # Add discrete points for each country
  geom_point(aes(x = total_marine_char, y = fct_rev(lowest_resolution),
                 color = cumulative_catch_percentage), size = 4, alpha = 0.8) +
  
  # Add a circle at the closest cum_catch_per to 50 for each country
  geom_point(data = closest_to_50,
             aes(x = total_marine_char, y = fct_rev(lowest_resolution)),
             color = "black", shape = 21, size = 4, stroke = 1, fill = NA) +
  
  # Customize color scale for the points
  scale_color_gradient2(low = "#4575b4", mid = "#cccccc", high = "#d73027", 
                        midpoint = 50, 
                        limits = c(0, 100), 
                        na.value = "transparent") +
  
  scale_x_continuous(breaks= c(0, 9, 19, 29, 39), expand =c(0.02,0.02), limits =c(0,39)) +
  
  labs(x = "Total Characterization Score", y = "",
       color = "Cumulative\nSSF marine catch (%)") +
  
  theme_classic() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10, r = -4, b = 0, l = 0)),
        legend.position = "right",
        legend.text = element_text(size = 10))

# ggsave(filename = "characterization_catch_dot50.png", width = 7.79, height = 9.04)
