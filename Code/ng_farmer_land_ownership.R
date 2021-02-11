rm(list=ls())
library(ggplot2)
library(questionr)

# collecting the data
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/ng_lsms_w4_land_ownership.csv"
nigeria_lsms_df <- as.data.frame(read.csv(file = url(link)))
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_covid_df <- as.data.frame(read.csv(file = url(link)))
nigeria_covid_df <- nigeria_covid_df[,(names(nigeria_covid_df) %in% c("hhid","s9q2","wt_baseline"))]
nigeria_df <- merge(nigeria_covid_df, nigeria_lsms_df, by="hhid")
nigeria_df <- nigeria_df[complete.cases(nigeria_df), ] #may skew the results if incomplete cases are a nonrandom sample
names(nigeria_df)[names(nigeria_df) == "s9q2"] <- "financial_threat"
remove(nigeria_lsms_df, nigeria_covid_df, link)

# get percentage of farmers who own / do not own land
ownership_plot_df <- aggregate(wt_baseline ~ owns_land_farmed, nigeria_df, sum)
names(ownership_plot_df)[names(ownership_plot_df) == "wt_baseline"] <- "pct"
ownership_plot_df$pct <- (ownership_plot_df$pct/sum(ownership_plot_df$pct))*100
ownership_plot_df$owns_land_farmed[(ownership_plot_df$owns_land_farmed == 1)] <- "Household owns the land they farm"
ownership_plot_df$owns_land_farmed[(ownership_plot_df$owns_land_farmed == 0)] <- "Houshoeld does not own the land they farm"
ownership_plot_df

#visualize the ownership data
plot= ggplot(data = ownership_plot_df, 
             aes(x = owns_land_farmed,
                 y = pct)) 

plot <- plot + ylab("% of Nigerian Farmers") # for the y axis label

plot <- plot + geom_bar(fill ="blue",
                        stat = 'identity') 

plot <- plot + theme( axis.text.x = element_text(angle = 90, hjust = 1, size=10 ))

sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19 and Nigeria General Household Survey'
titleText='Ownership of Land by Nigerian Farmers'

plot <- plot + labs(title=titleText,
                    x =NULL, 
                    caption = sourceText)

plot
