#clear workspace
rm(list = ls())

#include library for creating graphs
library(ggplot2)

#Set the working directory
setwd("/Users/conor/Documents/PUBPOL543/NGA_2020_NLPS_v06_M_CSV")

#read in one of the Nigeria data files as a dataframe
nigeria_df <- as.data.frame(read.csv("r1_sect_a_3_4_5_6_8_9_12.csv")) 

#convert "yes"/"no" survey responses to 1/0 integer variables that can be summed/averaged
nigeria_df$s9q2[nigeria_df$s9q2 == "1. A substantial threat"] <- 1
nigeria_df$s9q2[nigeria_df$s9q2 == "2. A moderate threat" ] <- 1
nigeria_df$s9q2[nigeria_df$s9q2 == "3. Not much of a threat"] <- 0
nigeria_df$s9q2[nigeria_df$s9q2 == "4. Not a threat at all" ] <- 0
nigeria_df$s9q2 <- strtoi(nigeria_df$s9q2)

#sort respondents in to industries
nigeria_df$industry[nigeria_df$s6q4 ==  "1. AGRICULTURE, HUNTING, FISHING"] <- "Agriculture"
nigeria_df$industry[(nigeria_df$s6q4 == "2. MINING, MANUFACTURING") | (nigeria_df$s6q4 == "3. ELECTRICITY, GAS, WATER SUPPLY") | (nigeria_df$s6q4 =="4. CONSTRUCTION") ] <- "Labor"
nigeria_df$industry[(nigeria_df$s6q4 == "7. PROFESSIONAL ACTIVITIES: FINANCE, LEGAL, ANALYSIS, COMPUTER, REAL ESTATE") | (nigeria_df$s6q4 == "8. PUBLIC ADMINISTRATION")] <- "Professional"
nigeria_df$industry[(nigeria_df$s6q4 == "5. BUYING &amp; SELLING GOODS, REPAIR OF GOODS, HOTELS &amp; RESTAURANTS") | (nigeria_df$s6q4 == "9. PERSONAL SERVICES, EDUCATION, HEALTH, CULTURE, SPORT, DOMESTIC WORK, OTHER")] <- "Other"


nigeria_df$s9q2 <- nigeria_df$s9q2 * nigeria_df$wt_baseline
nigeria_df <- merge(aggregate(s9q2 ~ industry, nigeria_df, sum), aggregate(wt_baseline ~ industry, nigeria_df, sum))
names(nigeria_df)[names(nigeria_df) == "wt_baseline"] <- "population"
nigeria_df$pct_finances_threatened <- (nigeria_df$s9q2/nigeria_df$population)*100

#create a plot
base = ggplot(data = nigeria_df, aes(x = industry, y = pct_finances_threatened)) 

#create labels for the plot
plot1 = base + geom_bar(fill ="blue", stat = 'identity')  + xlab("State") + ylab("% stopped school")

titleText='Percentage of Nigerian housholds that are financially threatened by COVID-19, by industry'
sourceText='Source: World Bank'

plot1 = plot1 + labs(title=titleText, x =NULL, y = NULL, caption = sourceText)

plot1