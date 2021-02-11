rm(list=ls())
library(ggplot2)
library(questionr)
library(scales)

# collecting the data
link = "https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/ng_lsms_w4_land_ownership.csv"
nigeria_lsms_df <- as.data.frame(read.csv(file = url(link)))
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_covid_df <- as.data.frame(read.csv(file = url(link)))
nigeria_covid_df <- nigeria_covid_df[,(names(nigeria_covid_df) %in% c("hhid","s9q2","wt_baseline"))]
nigeria_df <- merge(nigeria_covid_df, nigeria_lsms_df, by="hhid")
nigeria_df <- nigeria_df[complete.cases(nigeria_df), ] #may skew the results if incomplete cases are a nonrandom sample
remove(nigeria_lsms_df, nigeria_covid_df, link)

#get the weighted number of people who feel various levels of threatened by COVID19 as columns
nigeria_df$threat[nigeria_df$s9q2  == "1. A substantial threat"] <- "Severe threat"
nigeria_df$threat[nigeria_df$s9q2  == "2. A moderate threat"] <- "Medium threat"
nigeria_df$threat[(nigeria_df$s9q2 == "3. Not much of a threat")|(nigeria_df$s9q2 == "4. Not a threat at all" )] <- "Little or no threat"


ownership_threat=wtd.table(nigeria_df$threat, nigeria_df$owns_land_farmed, weights = nigeria_df$wt_baseline)
#ownership_threat <- ownership_threat[-c(1),] 
ownership_threat_df=as.data.frame(ownership_threat)

names(ownership_threat_df) <- c("threat","owns_farm","counts")
ownership_threat_df$owns_farm <- as.integer(ownership_threat_df$owns_farm)
ownership_threat_df$owns_farm[ownership_threat_df$owns_farm == 1] <- "Does not own farm"
ownership_threat_df$owns_farm[ownership_threat_df$owns_farm == 2] <- "Owns farm"


#marginal
ownership_threat_mg_col <- prop.table(ownership_threat,margin = 2)
#adding marginal
ownership_threat_df$pct_col <- as.data.frame(ownership_threat_mg_col)[,3]

base=ggplot(data <- ownership_threat_df,  aes(x=reorder(owns_farm, counts), y=counts, fill=threat))

conditionColor <- ifelse(ownership_threat_df$threat%in%c("Minor Threat",'No Threat'),'grey80','grey50')
bar_stacked <- base + geom_bar(stat = "identity", position = 'fill')

bar_stacked <- bar_stacked + theme( axis.text.x = element_text(angle = 90, hjust = 1, size=10 ))
titleText='Number of Nigerian housholds that are financially threatened by COVID-19, by industry'
sourceText='Source: LSMS-ISA'

bar_stacked <- bar_stacked + xlab("Industry") + ylab("Number of Households")
bar_stacked <- bar_stacked + labs(title=titleText, x =NULL, y = NULL, caption = sourceText)
bar_stacked <- bar_stacked + guides(fill=guide_legend(title=""))


bar_stacked


