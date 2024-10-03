
# Alba Aguion, (alba.aguion@duke.edu)
# September 30, 2024
# Paper figure ssf definition. Catch included as SSF depending on the criteria to use

# package load
library(fmsb)

# read dataset
definition_ssf <- read.csv("Fig_ssf_definition.csv")

# function to create radar chart
create_radarchart <- function(data, vlabels = colnames(data), vlcex = 1,
                              caxislabels = NULL, title = NULL, ...){
                              radarchart(data, axistype = 1,
                                          cglcol = "darkgrey",  cglwd = 0.3,
                                          axislabcol = "darkgrey",
                                          vlcex = vlcex, vlabels = vlabels, 
                                          caxislabels = caxislabels, title = title, ...)}

# reduce plot margin 
op <- par(mar = c(0.25, 0.25, 0.25, 0.25))

# first col to row names
rownames(definition_ssf) <- definition_ssf[[1]]
definition_ssf <- definition_ssf[ , -1]

# Create radar chart
create_radarchart(
  data = definition_ssf, vlcex = 0.9,  plwd = 2.5, title = "",
  vlabels = c("Vessel length\n<12 m",  "Labour-intensive or\npassive gear", "Individuals or\ncooperatives as labour", 
              "Direct household\nconsumption or sale\nat landing site", "Owner is\nthe operator"),
  pcol = c("#f78c85", "#82a4d3", "#9dd498", "#ceaac4", "#feb673", "#e5e500"),
  plty = rep(6,6),
  pty = c(16, 16, 16, 16, 16, 16))


legend(
  x = "right", legend = rownames(definition_ssf[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 ,
  col = c("#f78c85", "#82a4d3", "#9dd498", "#ceaac4", "#feb673", "#e5e500"),
  text.col = "black", cex = 1, pt.cex = 1.5)
