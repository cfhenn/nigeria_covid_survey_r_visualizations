rm(list=ls())
library(ggplot2)
library(questionr)
library(reshape)
library(dplyr)

# collecting data on which households are  rural/urban
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_sec_df <- as.data.frame(read.csv(file = url(link)))
nigeria_sec_df <- nigeria_sec_df[,c("sector", "wt_baseline", "hhid")]
nigeria_sec_df$sector <- recode(nigeria_sec_df$sector, "1. Urban" = "Urban", 
                                                       "2. Rural" = "Rural")


#get data on what shocks each household has experienced, and how they have been coping
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_10.csv?raw=true"
nigeria_shocks_and_coping_df <- as.data.frame(read.csv(file = url(link)))
nigeria_shocks_df <- nigeria_shocks_and_coping_df[,c("hhid", "shock_cd", "s10q1")]
nigeria_shocks_df <- nigeria_shocks_df[(nigeria_shocks_df$s10q1 != "2. NO"), ]
nigeria_shocks_df <- nigeria_shocks_df[,c("hhid", "shock_cd")]

#give shocks more readable descriptions
nigeria_shocks_df$shock_cd <- recode(nigeria_shocks_df$shock_cd, 
                                   "1. Illness, injury, or death of income earning member of household" = "Illness or death of income earner", 
                                   "5. Job loss" = "Job Loss",
                                   "6. Nonfarm business closure" = "Nonfarm business closure",
                                   "7. Theft/looting of cash and other property" = "Theft",
                                   "8. Disruption of farming, livestock, fishing activities" = "Disruption of farm activities",
                                   "10. Increase in price of farming/business inputs" =  "Increased cost of doing business or farming",
                                   "11. Fall in the price of farming/business output" = "Decreased price of products sold",
                                   "12. Increase in price of major food items consumed" = "Increased price of food",
                                   "96. Other (Specify)" = "Other")

#combine with rural urban data, drop uninteresting cases (very few)
nigeria_shocks_df <- merge(nigeria_shocks_df, nigeria_sec_df, by="hhid")
nigeria_shocks_df <- nigeria_shocks_df[(nigeria_shocks_df$shock_cd != "Other"), ]


#get frequencies of each shock  type by rural/urban
shock_table <- wtd.table(nigeria_shocks_df$shock_cd, nigeria_shocks_df$sector, weights = nigeria_shocks_df$wt_baseline)
shock_table <- prop.table(shock_table, margin = 2)
shock_df <- as.data.frame(shock_table)
names(shock_df) <- c("shock","Sector", "pct")

#shock data is prepared, now move on  to coping strategies
#keep data on coping strategies, format  data long, get rid of duplicates/NAs
#because not interested in what shock each coping strategy was a response to, just if a household used it
coping_qs <- c("s10q3__1", "s10q3__6", "s10q3__7", "s10q3__8", "s10q3__9", "s10q3__11", 
               "s10q3__12", "s10q3__13", "s10q3__14", "s10q3__15", "s10q3__16", "s10q3__17",
               "s10q3__18", "s10q3__19", "s10q3__20", "s10q3__21", "s10q3__96")
nigeria_coping_df <- nigeria_shocks_and_coping_df[,c("hhid", coping_qs)]
nigeria_coping_df <- melt(nigeria_coping_df, "hhid")
nigeria_coping_df$value[(is.na(nigeria_coping_df$value))] <- 0
nigeria_coping_df <- nigeria_coping_df[!duplicated(nigeria_coping_df[c("hhid","variable")]), ]

#generate single variable with  readable description of coping strategy
coping_descriptions <- c("Sold assets", "Earned additional income", "Received aid from friends or family", "Received loan from friends or family", "Took out loan from bank", "Made purchases on credit", "Delayed payment obligations", "Sold harvest early", "Reduced food consumption", "Reduced non-food consumption", "Relied on savings", "Recieved aid from NGO", "Took advance from employer", "Received aid from government", "Relied on insurancne coverage", "Did nothing", "Other")
nigeria_coping_df$covid_shock_coping_action <- "No shock"
for (i in 1:length(coping_qs)) {
  nigeria_coping_df$covid_shock_coping_action[( (nigeria_coping_df$variable==coping_qs[[i]]) 
                                             &  (nigeria_coping_df$value==1)) ] <- coping_descriptions[[i]]
}
nigeria_coping_df <- nigeria_coping_df[,c("hhid", "covid_shock_coping_action")]

#get rid of uninteresting cases (very  few)
nigeria_coping_df <- merge(nigeria_coping_df, nigeria_sec_df, by="hhid")
nigeria_coping_df <- nigeria_coping_df[(nigeria_coping_df$sector != ""),]
nigeria_coping_df <- nigeria_coping_df[(nigeria_coping_df$covid_shock_coping_action != "No shock"), ]
nigeria_coping_df <- nigeria_coping_df[(nigeria_coping_df$covid_shock_coping_action != "Other"), ]

#get frequencies of each strategy  by  rural/urban
coping_table <- wtd.table(nigeria_coping_df$covid_shock_coping_action, nigeria_coping_df$sector, weights = nigeria_coping_df$wt_baseline)
coping_table <- prop.table(coping_table, margin = 2)
coping_df <- as.data.frame(coping_table)
names(coping_df) <- c("copingmethod","Sector", "pct")

#MAKE PLOTS!

sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19'

dual_bar_plot_shocks <- ggplot(shock_df, aes(x = shock,  y = pct, fill = Sector ) ) + 
  geom_bar( stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values=c("#9999CC", "#66CC99")) +
  facet_grid( ~ Sector) +
  coord_flip() +
  xlab("Methods of coping with income loss") +
  ylab("% of households") +
  labs( x =NULL, y = NULL, caption = sourceText)
dual_bar_plot_shocks

dual_bar_plot_coping <- ggplot(coping_df, aes(x = copingmethod ,  y = pct, fill = Sector ) ) + 
  geom_bar( stat = "identity", show.legend = FALSE ) +
  scale_fill_manual(values=c("#9999CC", "#66CC99")) +
  facet_grid( ~ Sector) + 
  coord_flip() +
  xlab("Methods of coping with income loss") +
  ylab("% of households") +
  labs( x =NULL, y = NULL, caption = sourceText)
dual_bar_plot_coping 

