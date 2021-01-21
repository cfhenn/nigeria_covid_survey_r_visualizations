#clear workspace
rm(list = ls())

#include library for creating graphs
library(ggplot2)

#Set the working directory
setwd("/Users/conor/Documents/PUBPOL543/NGA_2020_NLPS_v06_M_CSV")

#read in one of the Nigeria data files as a dataframe
nigeria_df <- as.data.frame(read.csv("r1_sect_a_3_4_5_6_8_9_12.csv")) 

#satisfied with gov response
nigeria_df$s3q4[nigeria_df$s3q4 == "1. YES"] <- 1
nigeria_df$s3q4[nigeria_df$s3q4 == "2. NO" ] <- 0
nigeria_df$s3q4 <- strtoi(nigeria_df$s3q4)

#lost job
nigeria_df$s6q1[nigeria_df$s6q1 == "1. YES"] <- 1
nigeria_df$s6q1[nigeria_df$s6q1 == "2. NO" ] <- 0
nigeria_df$s6q1 <- strtoi(nigeria_df$s6q1)
nigeria_df <- nigeria_df[!(is.na(nigeria_df$s6q1)),]
nigeria_df$s6q2[nigeria_df$s5q4a == "1. YES"] <- 1
nigeria_df$s6q2[nigeria_df$s5q4a == "2. NO" ] <- 0
nigeria_df$s6q2 <- strtoi(nigeria_df$s6q2)
nigeria_df$lost_job <- 0
nigeria_df$lost_job[(nigeria_df$s6q1 == 0) & (nigeria_df$s6q2 == 1)] <- 1

#weight data
nigeria_df$lost_job  <- nigeria_df$lost_job * nigeria_df$wt_baseline
nigeria_df$satisfied <- nigeria_df$s3q4 * nigeria_df$wt_baseline

#get state totals
nigeria_df <- aggregate(cbind(lost_job,satisfied,wt_baseline) ~ state, nigeria_df, sum)

#rename population column
names(nigeria_df)[names(nigeria_df) == "wt_baseline"] <- "population"

#get state percentages
nigeria_df$pct_satisfied <- (nigeria_df$satisfied/nigeria_df$population) * 100
nigeria_df$pct_lost_job  <- (nigeria_df$lost_job/nigeria_df$population)  * 100

base = ggplot(data = nigeria_df, aes(x = pct_lost_job, y = pct_satisfied)) 

plot1 = base +  geom_line() + geom_point()
