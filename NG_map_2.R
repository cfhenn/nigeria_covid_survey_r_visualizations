#clear workspace
rm(list = ls())

#must run install.packages("naijR") before this works
library(naijR)
library(ggplot2)

#get data file
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_10.csv?raw=true"
nigeria_df1 <- as.data.frame(read.csv(file = url(link))) 

nigeria_df1$lost_job <- 0
nigeria_df1[((nigeria_df1$shock_cd == "5. Job loss") & (nigeria_df1$s10q1 == "1. YES")), ] <- 1
nigeria_df1[((nigeria_df1$shock_cd == "5. Job loss") & (nigeria_df1$s10q1 == "1. YES")), ]
nigeria_df1[is.na(nigeria_df1$s10q3__14),] <- 0
nigeria_df1$reduced_food <- nigeria_df1$s10q3__14
nigeria_df1$urban <- 0
nigeria_df1$urban[(nigeria_df1$sector == "1. Urban")] <-  1

nigeria_df1 <- aggregate(cbind(lost_job, reduced_food, urban) ~ hhid, nigeria_df1, max)

# collecting the data
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df2 <- as.data.frame(read.csv(file = url(link)))
nigeria_df2 <- nigeria_df2[,c("hhid", "wt_baseline", "state")]

nigeria_df  <- merge(nigeria_df1, nigeria_df2, by="hhid")
nigeria_df$lost_job     <- nigeria_df$lost_job*nigeria_df$wt_baseline
nigeria_df$urban        <- nigeria_df$urban*nigeria_df$wt_baseline
nigeria_df$reduced_food <- nigeria_df$reduced_food*nigeria_df$wt_baseline

nigeria_df <- aggregate(cbind(lost_job, reduced_food, urban, wt_baseline) ~ state, nigeria_df, sum)
nigeria_df$lost_job     <- nigeria_df$lost_job/nigeria_df$wt_baseline
nigeria_df$urban        <- nigeria_df$urban/nigeria_df$wt_baseline
nigeria_df$reduced_food <- nigeria_df$reduced_food/nigeria_df$wt_baseline

map_ng()
ss = states()
# Create variables
nn <- nigeria_df$lost_job
bb <- c(0, 0.4, 0.6, 1)

#map_ng(region = ss, x = nn, breaks = bb, col = 'YlOrRd')

map_ng(
  region = ss,
  x = nn,
  breaks = bb,
  categories = c("Mostly Rural", "Mixed", "Mostly Urban"),
  col = 3L
)
