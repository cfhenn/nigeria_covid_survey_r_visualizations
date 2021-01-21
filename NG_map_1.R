#clear workspace
rm(list = ls())

#must run install.packages("naijR") before this works
library(naijR)
library(ggplot2)

#Set the working directory
setwd("/Users/conor/Documents/PUBPOL543/NGA_2020_NLPS_v06_M_CSV")
nigeria_df <- as.data.frame(read.csv("r1_sect_1.csv")) 

nigeria_df$urbanization[nigeria_df$sector == "1. Urban"] <- 1
nigeria_df$urbanization[nigeria_df$sector == "2. Rural"] <- 0

nigeria_df <- aggregate(urbanization ~ state, nigeria_df, mean)

map_ng()
ss = states()
# Create variables
nn <- nigeria_df$urbanization
bb <- c(0, 0.4, 0.6, 1)

#map_ng(region = ss, x = nn, breaks = bb, col = 'YlOrRd')

map_ng(
  region = ss,
  x = nn,
  breaks = bb,
  categories = c("Mostly Rural", "Mixed", "Mostly Urban"),
  col = 3L
)
