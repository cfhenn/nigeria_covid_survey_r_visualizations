rm(list=ls())
library(ggplot2)
library(questionr)
library(foreign)

# collecting the data
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/ng_w4_land_ownership.dta?raw=true"

nigeria_lsms_df <- as.data.frame(read.dta(file = url(link)))
