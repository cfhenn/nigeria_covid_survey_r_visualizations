rm(list=ls())
library(ggplot2)
library(data.table)
library(reshape)


# collecting the data
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/ng_lsms_w4_land_ownership.csv"
nigeria_lsms_df <- as.data.frame(read.csv(file = url(link)))

link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_lsms_weights <- as.data.frame(read.csv(file = url(link)))
nigeria_lsms_df <- merge(nigeria_lsms_weights, nigeria_lsms_df, by="hhid")
nigeria_lsms_df <-nigeria_lsms_df[c("hhid", "owns_land_farmed", "wt_baseline")]
remove(nigeria_lsms_weights)
  
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_10.csv?raw=true"
nigeria_covid_hf_df <- as.data.frame(read.csv(file = url(link)))
coping_qs <- c("s10q3__1", "s10q3__6", "s10q3__7", "s10q3__8", "s10q3__9", "s10q3__11", "s10q3__12", "s10q3__13", "s10q3__14", "s10q3__15", "s10q3__16", "s10q3__17", "s10q3__18", "s10q3__19", "s10q3__20", "s10q3__21", "s10q3__96")
nigeria_covid_hf_df <- nigeria_covid_hf_df[,c("hhid", coping_qs)]
nigeria_covid_hf_df <- melt(nigeria_covid_hf_df, "hhid")
nigeria_covid_hf_df$value[(is.na(nigeria_covid_hf_df$value))] <- 0
nigeria_covid_hf_df <- nigeria_covid_hf_df[!duplicated(nigeria_covid_hf_df[c("hhid","variable")]), ]

coping_descriptions <- c("Sold assets", "Earned additional income", "Got help from friends or family", "Received loan from friends or family", "Took out loan from bank", "Made purchases on credit", "Delayed payment obligations", "Sold harvest early", "Reduced food consumption", "Reduced non-food consumption", "Relied on savings", "Recieved aid from NGO", "Took advance from employer", "Received aid from government", "Relied on insurancne coverage", "Did nothing", "Other")
nigeria_covid_hf_df$covid_shock_coping_action <- "No shock"
for (i in 1:length(coping_qs)) {
  #print(coping_descriptions[[i]])
  nigeria_covid_hf_df$covid_shock_coping_action[((nigeria_covid_hf_df$variable==coping_qs[[i]]) & (nigeria_covid_hf_df$value==1)) ]<- coping_descriptions[[i]]
}
nigeria_covid_hf_df <- nigeria_covid_hf_df[,c("hhid", "covid_shock_coping_action")]
nigeria_covid_hf_df <- nigeria_covid_hf_df[!duplicated(nigeria_covid_hf_df[c("hhid","covid_shock_coping_action")]), ]

nigeria_df <- merge(nigeria_lsms_df, nigeria_covid_hf_df, by="hhid")
nigeria_df$owns_land_farmed[(nigeria_df$owns_land_farmed == 1)  ] <- "Household owns the land they farm"
nigeria_df$owns_land_farmed[(nigeria_df$owns_land_farmed == 0)  ] <- "Houshoeld does not own the land they farm"


remove(nigeria_covid_hf_df, nigeria_lsms_df)

nigeria_df <- nigeria_df[(nigeria_df$covid_shock_coping_action != "No shock"), ]



coping_table <- wtd.table(nigeria_df$covid_shock_coping_action, nigeria_df$owns_land_farmed,weights = nigeria_df$wt_baseline)

coping_table <- prop.table(coping_table,margin = 2)
#adding marginal
coping_df <- as.data.frame(coping_table)
names(coping_df) <- c("copingmethod","ownland", "pct")

# similar to base4
base  = ggplot(coping_df, aes(x = copingmethod ,  y = pct ) ) 
barsIO = base + geom_bar( stat = "identity" )
barsIO = barsIO + facet_grid( ~ ownland) 
barsIO = barsIO + coord_flip()
barsIO <- barsIO + xlab("Methods of coping with income loss") # for the y axis label
barsIO <- barsIO + ylab("% of Nigerian farmers") # for the y axis label



barsIO
