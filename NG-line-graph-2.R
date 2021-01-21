#clear workspace
rm(list = ls())

#include library for creating graphs
library(ggplot2)

#Set the working directory
setwd("/Users/conor/Documents/PUBPOL543/NGA_2020_NLPS_v06_M_CSV")

nigeria_df_1 <- as.data.frame(read.csv("r1_sect_11.csv")) 
nigeria_df_1$received_assistance <- 0
nigeria_df_1$received_assistance[nigeria_df_1$s11q1 == "1. YES" ] <- 1
nigeria_df_1 <- aggregate(received_assistance ~ hhid, nigeria_df_1, mean)

nigeria_df_2 <- as.data.frame(read.csv("r1_sect_a_3_4_5_6_8_9_12.csv")) 
nigeria_df <- merge(nigeria_df_1, nigeria_df_2, by="hhid")

#satisfied with gov response
nigeria_df$s3q4[nigeria_df$s3q4 == "1. YES"] <- 1
nigeria_df$s3q4[nigeria_df$s3q4 == "2. NO" ] <- 0
nigeria_df$s3q4 <- strtoi(nigeria_df$s3q4)


#weight data
nigeria_df$lost_job  <- nigeria_df$received_assistance * nigeria_df$wt_baseline
nigeria_df$satisfied <- nigeria_df$s3q4 * nigeria_df$wt_baseline

#get state totals
nigeria_df <- aggregate(cbind(received_assistance,satisfied,wt_baseline) ~ state, nigeria_df, sum)

#rename population column
names(nigeria_df)[names(nigeria_df) == "wt_baseline"] <- "population"

#get state percentages
nigeria_df$pct_satisfied <- (nigeria_df$satisfied/nigeria_df$population) * 100
nigeria_df$pct_received_assistance  <- (nigeria_df$received_assistance/nigeria_df$population)  * 100

base = ggplot(data = nigeria_df, aes(x = pct_received_assistance, y = pct_satisfied)) 

plot1 = base +  geom_line() + geom_point()

