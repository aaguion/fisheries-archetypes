
# Alba Aguion, (alba.aguion@duke.edu)
# October 1, 2024
# LCCA marine small-scale fisheries archetypes model

# load packages
library(dplyr)
library(glca)
library(cluster)
library(stats)
library(dendextend)
library(reshape2)
library(forcats)
library(gplots)
library(ggplot2)


# read dataset
data <- read.csv("Char_fishery_units.csv")

# set factors
data <- data %>% mutate_at(vars("fishery_unit_id", "country", "region", "total_char"), factor) 

# matrix attributes as ordered factors
data$gear          <- factor(data$gear, order = TRUE, levels = c("0", "1", "2", "3"))
data$mechanization <- factor(data$mechanization, order = TRUE, levels = c("0", "1", "2", "3"))
data$size          <- factor(data$size, order = TRUE, levels = c("0", "1", "2", "3"))
data$motorization  <- factor(data$motorization, order = TRUE, levels = c("0", "1", "2", "3"))
data$trip          <- factor(data$trip, order = TRUE, levels = c("0", "1", "2", "3"))
data$distance      <- factor(data$distance, order = TRUE, levels = c("0", "1", "2", "3"))
data$refrigeration <- factor(data$refrigeration, order = TRUE, levels = c("0", "1", "2", "3"))
data$labour        <- factor(data$labour, order = TRUE, levels = c("0", "1", "2", "3"))
data$ownership     <- factor(data$ownership, order = TRUE, levels = c("0", "1", "2", "3"))
data$commitment    <- factor(data$commitment, order = TRUE, levels = c("0", "1", "2", "3"))
data$disposal      <- factor(data$disposal, order = TRUE, levels = c("0", "1", "2", "3"))
data$utilization   <- factor(data$utilization, order = TRUE, levels = c("0", "1", "2", "3"))
data$economy       <- factor(data$economy, order = TRUE, levels = c("0", "1", "2", "3"))

str(data)
summary(data)

# model formula
formula <- item(gear, mechanization, size, motorization, trip, distance, refrigeration,
                  labour, ownership, commitment, disposal, utilization, economy) ~ 1 

# model with different numbers of clusters, adding region as covariate
lca3  <- glca(formula, data = data[3:16], group= region, nclass = 3,  seed = 1, verbose = FALSE)
lca4  <- glca(formula, data = data[3:16], group= region, nclass = 4,  seed = 1, verbose = FALSE)
lca5  <- glca(formula, data = data[3:16], group= region, nclass = 5,  seed = 1, verbose = FALSE)
lca6  <- glca(formula, data = data[3:16], group= region, nclass = 6,  seed = 1, verbose = FALSE)
lca7  <- glca(formula, data = data[3:16], group= region, nclass = 7,  seed = 1, verbose = FALSE)
lca8  <- glca(formula, data = data[3:16], group= region, nclass = 8,  seed = 1, verbose = FALSE)
lca9  <- glca(formula, data = data[3:16], group= region, nclass = 9,  seed = 1, verbose = FALSE)
lca10 <- glca(formula, data = data[3:16], group= region, nclass = 10, seed = 1, verbose = FALSE)
lca11 <- glca(formula, data = data[3:16], group= region, nclass = 11, seed = 1, verbose = FALSE)

# compare model fit
gofglca(lca3, lca4,lca5,lca6,lca7,lca8,lca9,lca10,lca11, test="chisq") 
gc()

# function to assess cluster separation
tab.class<-function(post=lca5$posterior){
  n.group<-length(post)
  n.class<-ncol(post[[1]])
  tabl<-matrix(0,nrow=(n.group+1),ncol=n.class)
  group<-names(post)
  rownames(tabl)<-c(group,"Total")
  colnames(tabl)<-paste0("class",1:n.class)
  sw.mat<-NULL
  neg.vec<-sw.vec<-numeric(n.group+1)
  for (i in 1:n.group){
    x<-data[data$region==group[i],4:15]
    for (j in 1:ncol(x)){
      x[,j]<-factor(x[,j],labels=0:3,levels=0:3,ordered=TRUE)
    }
    prob.data <- as.data.frame(post[[i]])
    c.max<-apply(prob.data,1,max)
    prob.data<-sweep(prob.data,MAR=1,STAT=c.max,FUN="==")
    prob.data<-sweep(prob.data,MAR=2,STAT=(1:ncol(prob.data)),FUN="*")
    temp<-table(latent.class<-apply(prob.data,1,sum))
    tabl[i,as.numeric(names(temp))]<-temp
    s.out<-silhouette(latent.class, dist = daisy(x))
    sw<-as.data.frame(s.out)
    neg.vec[i]<-sum(sw$sil_width<0)
    sw.vec[i]<-mean(sw$sil_width)
    sw.mat<-rbind(sw.mat,sw)
  }
  neg.vec[n.group+1]<-sum(sw.mat$sil_width<0)
  sw.vec[n.group+1]<-mean(sw.mat$sil_width)
  tabl[n.group+1,]<-apply(tabl,2,sum)
  tabl<-cbind(tabl,sw.vec,neg.vec)
  colnames(tabl)[(n.class+1):(n.class+2)]<-c("SW","n.NegSW")
  ##
  return(list(class.table=tabl,sw=sw.mat))
}

# assess cluster separation (Total ASW)
tab.class(post=lca5$posterior)$class.table

names(lca5$posterior) 
summary(lca5)
glca::gofglca(lca5)

# clean environment
rm(list = ls()[!ls() %in% c("data", "lca5")])

# latent classes have been determined,
# lets assigns the latent class (from the posterior probabilities) to each fishery unit within the dataset
prob.data_region = data.table::rbindlist(lca5[["posterior"]])

# assign latent class with the higher posterior probability
prob.data_region$latent_class = colnames(prob.data_region)[max.col(prob.data_region, ties.method="first")]

# assign labels based on the logic order from artisanal-like to industrial-like fleets
prob.data_region$latent_class[which(prob.data_region$latent_class=="Class 1")] <- "5" 
prob.data_region$latent_class[which(prob.data_region$latent_class=="Class 2")] <- "2" 
prob.data_region$latent_class[which(prob.data_region$latent_class=="Class 3")] <- "1"
prob.data_region$latent_class[which(prob.data_region$latent_class=="Class 4")] <- "3"
prob.data_region$latent_class[which(prob.data_region$latent_class=="Class 5")] <- "4"

# to add assigned latent class 
prob.data_region1 <- as.data.frame(cbind(data, prob.data_region$latent_class))
colnames(prob.data_region1)[18] <- "latent_class"

# Calculate the proportion of fishery units in each latent class
archetype_weights <- prob.data_region1 %>%
  group_by(latent_class) %>%
  summarise(count = n()) %>%
  mutate(weight = count / sum(count))

# from wide to long format
cust.long1 <- melt(data.frame(lapply(prob.data_region1, as.character), stringsAsFactors=FALSE), 
                  id = c("fishery_unit_id", "latent_class"), factorsAsStrings=T)

cust.long1 <- subset(cust.long1, !variable %in% c("region", "total_char", "country"))

cust.long_matrix1       <- subset (cust.long1, cust.long1$value %in%  c(0, 1, 2, 3))
cust.long_matrix1$value <- as.numeric(as.character(cust.long_matrix1$value))
cust.long_matrix1       <- cust.long_matrix1[,-1]

# assign correct categories to each of the attributes from the matrix
cust.long_matrix1 <- cust.long_matrix1 %>%
  mutate(category = case_when(
    variable %in% c("trip", "distance", "commitment", "labour")  ~ "Operational",
    variable %in% c("economy", "disposal", "ownership")          ~ "Economic",
    variable == "utilization"                                    ~ "Post-harvest use",
    TRUE                                                         ~ "Technological"
  ))

# calculate the mean and sd for each combination of latent_class (archetype), category and attribute
cust.long.ave_matrix1 = cust.long_matrix1 %>% group_by(latent_class, category, variable) %>%
  dplyr::summarise(mean = mean(value), sd = sd(value))

# change to factors with correct order
cust.long.ave_matrix1$latent_class <- factor(cust.long.ave_matrix1$latent_class, levels = c("1", "2", "3", "4", "5"))
cust.long.ave_matrix1$category <- factor(cust.long.ave_matrix1$category, levels = c("Operational", "Post-harvest use", "Economic", "Technological"))

# Figure (dendogram and heatmap with mean value per attribute and archetype)
cust.long.ave_matrix_wide1 = dcast (cust.long.ave_matrix1, latent_class ~ variable, value.var = "mean")
rownames(cust.long.ave_matrix_wide1) = cust.long.ave_matrix_wide1[,1]
cust.long.ave_matrix_wide1[,1] = NULL
cust.long.ave_matrix_wide1 = t(cust.long.ave_matrix_wide1)

# perform hierarchical clustering to create the dendogram
distance <- dist(t(cust.long.ave_matrix_wide1), method ="euclidean")
hcluster <- hclust(distance, method ="ward.D")
dend1    <- as.dendrogram(hcluster)

# set colors
cols_branches <- c("#cae1ff", "#ffb3ba", "#b39eb5", "#baffc9","#ffa54f")
my_palette    <- colorRampPalette(c("#F8F9FAFF", "gray79", "#6F8BA0FF", "#181D20FF"))(100)

# create dendogram
dend1 <- color_branches(dend1, k = 5, col = cols_branches)

col_labels <- get_leaves_branches_col(dend1)
col_labels <- col_labels[order(order.dendrogram(dend1))]

heatmap.2(cust.long.ave_matrix_wide1,   trace="none",
          Rowv=FALSE, Colv = dend1,
          dendrogram="col", na.rm = TRUE,
          margins =c(5,10),
          col=my_palette,
          rowsep = c(4,5,8),
          sepwidth=c(0.01, 0.01, 0.01),
          sepcolor='black', key = FALSE,
          ColSideColors = col_labels # to add nice colored strips
)

# clean environment
rm(list = ls()[!ls() %in% c("data", "lca5", "cust.long.ave_matrix1", "archetype_weights")])

# Figure (mean and sd per attribute and archetype)
clus_names <- as_labeller( c(`1` = "Archetype I\n",`2` = "Archetype II\n",`3` = "Archetype III\n", 
                             `4` = "Archetype IV\n", `5` = "Archetype V\n")) # labels for the archetypes

ggplot(cust.long.ave_matrix1 , aes(x=fct_rev(variable), y=mean, color=category)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, size=0.5) +
  scale_y_continuous () + coord_flip() +
  scale_color_manual (values=c("#8c564b","#eec900","grey79","#ee5c42")) +
  labs(title='', y='Characterization Attribute Scores', x='', color="")  +
  geom_point(group = 'character', size=2) + theme_bw() + theme(axis.text.y = element_text(size=12),
                                                               axis.title.x = element_text(size=12, face="bold", margin = margin(t = 10, r = -4, b = 0, l = 0)),
                                                               legend.position = "right", panel.grid.minor = element_blank(),
                                                               strip.text = element_text(size=12, face="bold"),
                                                               legend.text = element_text(size=11),
                                                               strip.background =element_rect(fill="grey90")) +
  facet_wrap (~latent_class, labeller = clus_names, nrow=1)



### CALCULATE Fractions of probabilities for the different categories ##

# Extract posterior probabilities for each unit and each class
posterior_probs <- lca5$posterior  
# Combine all list elements into one unified matrix
posterior_matrix <- do.call(rbind, posterior_probs)
# Check the resulting matrix
print(dim(posterior_matrix))  # This should show the total number of observations and classes

# Find the maximum posterior probability for each unit, ignoring NAs
max_probs <- apply(posterior_matrix, 1, max, na.rm = TRUE)

# Calculate the fraction of units with probabilities > 0.5, > 0.75, > 0.95, > 0.99
fraction_above_0.5  <- mean(max_probs > 0.5, na.rm = TRUE)
fraction_above_0.75 <- mean(max_probs > 0.75, na.rm = TRUE)
fraction_above_0.95 <- mean(max_probs > 0.95, na.rm = TRUE)
fraction_above_0.99 <- mean(max_probs > 0.99, na.rm = TRUE)

# Create a data frame with the results
results <- data.frame(Threshold = c(">0.5", ">0.75", ">0.95", ">0.99"),
                      Fraction = c(fraction_above_0.5, fraction_above_0.75, fraction_above_0.95, fraction_above_0.99))

print(results)
rm(results, fraction_above_0.5, fraction_above_0.75, fraction_above_0.95, fraction_above_0.99, posterior_matrix, posterior_probs)


### Probabilities for individual observations
# Function to process each region
process_region <- function(region_name, lca_posterior, marine_data) {
  individual_region <- lca_posterior[[region_name]]
  individual_region$region <- region_name
  country_region <- select(subset(marine_data, region == region_name), fishery_unit_id, country, region)
  individual_region <- as.data.frame(cbind(individual_region, country_region))
  return(individual_region)
}

# Apply the function for each region and combine the results
regions <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
individual_list <- lapply(regions, process_region, lca_posterior = lca5$posterior, marine_data = data)

# Combine all individual regions into one data frame
individual_probabilities <- do.call(rbind, individual_list)
individual_probabilities <- individual_probabilities[,-9]
individual_probabilities <- melt(individual_probabilities)

# Filter probabilities and select maximum class
individual_probabilities_s <- individual_probabilities %>%
  group_by(fishery_unit_id, country, region) %>%
  filter(value >= 0.48) %>%
  slice_max(order_by = value, n = 1)


## Calculate summary statistics
# Calculate counts for each group (country, variable, region)
individual_probabilities_s1 <- individual_probabilities_s %>% 
  group_by(country, variable, region) %>% 
  dplyr::summarise(counts = n())

# Calculate total counts for each country
individual_probabilities_s2 <- individual_probabilities_s %>% 
  group_by(country) %>% 
  dplyr::summarise(country_counts = n())

# Merge the two datasets
individual_probabilities_s3 <- full_join(individual_probabilities_s1, individual_probabilities_s2)

# Remove intermediate datasets
rm(individual_probabilities_s1, individual_probabilities_s2)

# Calculate the z-score for 95% confidence interval
z <- qnorm(0.975)

# Calculate proportions, standard errors, and confidence intervals for counts only
individual_probabilities_prop <- individual_probabilities_s3 %>% 
  group_by(country, variable, region) %>% 
  dplyr::summarise(
    # Proportion of counts
    pro_counts = (counts / country_counts),
    # Standard error of counts
    se_counts = sqrt((counts / country_counts) * (1 - (counts / country_counts)) / country_counts),
    # Confidence intervals for counts
    ci_lower_counts = (counts / country_counts) - z * se_counts,
    ci_upper_counts = (counts / country_counts) + z * se_counts
  )

colnames(individual_probabilities_prop)[2] <- "archetype"

# assign correct labels 
individual_probabilities_prop$archetype <- as.character(individual_probabilities_prop$archetype)
individual_probabilities_prop$archetype[which(individual_probabilities_prop$archetype=="Class 1")] <- "5" 
individual_probabilities_prop$archetype[which(individual_probabilities_prop$archetype=="Class 2")] <- "2" 
individual_probabilities_prop$archetype[which(individual_probabilities_prop$archetype=="Class 3")] <- "1"
individual_probabilities_prop$archetype[which(individual_probabilities_prop$archetype=="Class 4")] <- "3"
individual_probabilities_prop$archetype[which(individual_probabilities_prop$archetype=="Class 5")] <- "4"
individual_probabilities_prop$archetype <- as.factor(individual_probabilities_prop$archetype)

# order by country 
extract_number <- function(country) {
  as.numeric(gsub("[^0-9]", "", country)) 
}

individual_probabilities_prop <- individual_probabilities_prop[order(extract_number(individual_probabilities_prop$country)), ]

# save data
# write.csv(cust.long.ave_matrix_wide1,     "summary_attributes.csv") # mean, sd per attribute and latent class
# write.csv(archetype_weights,              "archetype_weights.csv")  # archetype weights
# write.csv(individual_probabilities_prop,  "individual_probabilities_prop.csv") # individual probabilities per country from LCCA global model
