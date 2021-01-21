#clear workspace
rm(list = ls())

#must run install.packages("naijR") before this works
library(naijR)
library(ggplot2)

#Set the working directory
setwd("/Users/conor/Documents/PUBPOL543/NGA_2020_NLPS_v06_M_CSV")
nigeria_df <- as.data.frame(read.csv("r1_sect_a_3_4_5_6_8_9_12.csv")) 

#satisfied with gov response
nigeria_df$s3q4[nigeria_df$s3q4 == "1. YES"] <- 1
nigeria_df$s3q4[nigeria_df$s3q4 == "2. NO" ] <- 0
nigeria_df$satisfied <- strtoi(nigeria_df$s3q4)

nigeria_df <- aggregate(satisfied ~ state, nigeria_df, mean)

map_ng()
ss = states()
# Create variables
nn <- nigeria_df$satisfied
bb <- c(0, 0.4, 0.6, 1)

#map_ng(region = ss, x = nn, breaks = bb, col = 'YlOrRd')

map_ng(
  region = ss,
  x = nn,
  breaks = bb,
  categories = c("Mostly Unsatisfied", "Mixed", "Mostly Satisfied"),
  col = 3L
)
