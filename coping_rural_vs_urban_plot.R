rm(list=ls())
library(ggplot2)
library(questionr)
library(reshape)

# collecting data on which households are  rural/urban
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_sec_df <- as.data.frame(read.csv(file = url(link)))
nigeria_sec_df <- nigeria_sec_df[,c("sector", "wt_baseline", "hhid")]
nigeria_sec_df$sector[(nigeria_sec_df$sector == "1. Urban")] <- "Urban"
nigeria_sec_df$sector[(nigeria_sec_df$sector == "2. Rural")] <- "Rural"


#get data on what shocks each household has experienced, and how they have been coping
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_10.csv?raw=true"
nigeria_shock_df <- as.data.frame(read.csv(file = url(link)))
coping_qs <- c("s10q3__1", "s10q3__6", "s10q3__7", "s10q3__8", "s10q3__9", "s10q3__11", "s10q3__12", "s10q3__13", "s10q3__14", "s10q3__15", "s10q3__16", "s10q3__17", "s10q3__18", "s10q3__19", "s10q3__20", "s10q3__21", "s10q3__96")
nigeria_shock_df <- nigeria_shock_df[,c("hhid", coping_qs)]
nigeria_shock_df <- melt(nigeria_shock_df, "hhid")
nigeria_shock_df$value[(is.na(nigeria_shock_df$value))] <- 0
nigeria_shock_df <- nigeria_shock_df[!duplicated(nigeria_shock_df[c("hhid","variable")]), ]

coping_descriptions <- c("Sold assets", "Earned additional income", "Got help from friends or family", "Received loan from friends or family", "Took out loan from bank", "Made purchases on credit", "Delayed payment obligations", "Sold harvest early", "Reduced food consumption", "Reduced non-food consumption", "Relied on savings", "Recieved aid from NGO", "Took advance from employer", "Received aid from government", "Relied on insurancne coverage", "Did nothing", "Other")
nigeria_shock_df$covid_shock_coping_action <- "No shock"
for (i in 1:length(coping_qs)) {
  nigeria_shock_df$covid_shock_coping_action[((nigeria_shock_df$variable==coping_qs[[i]]) & (nigeria_shock_df$value==1)) ]<- coping_descriptions[[i]]
}
nigeria_shock_df <- nigeria_shock_df[,c("hhid", "covid_shock_coping_action")]
nigeria_shock_df <- nigeria_shock_df[!duplicated(nigeria_shock_df[c("hhid","covid_shock_coping_action")]), ]

nigeria_df <- merge(nigeria_shock_df, nigeria_sec_df, by="hhid")
remove(nigeria_sec_df, nigeria_shock_df)
nigeria_df <- nigeria_df[(nigeria_df$sector != ""),]
nigeria_df <- nigeria_df[(nigeria_df$covid_shock_coping_action != "No shock"), ]
nigeria_df <- nigeria_df[(nigeria_df$covid_shock_coping_action != "Other"), ]

coping_table <- wtd.table(nigeria_df$covid_shock_coping_action, nigeria_df$sector, weights = nigeria_df$wt_baseline)

coping_table <- prop.table(coping_table, margin = 2)


coping_df <- as.data.frame(coping_table)
names(coping_df) <- c("copingmethod","sector", "pct")
sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19'

# similar to base4
base  = ggplot(coping_df, aes(x = copingmethod ,  y = pct ) ) 
barsIO = base + geom_bar( stat = "identity" )
barsIO = barsIO + facet_grid( ~ sector) 
barsIO = barsIO + coord_flip()
barsIO <- barsIO + xlab("Methods of coping with income loss") # for the y axis label
barsIO <- barsIO + ylab("% of households") # for the y axis label
barsIO <- barIO+ labs( x =NULL, y = NULL, caption = sourceText)
barsIO

barsIO
