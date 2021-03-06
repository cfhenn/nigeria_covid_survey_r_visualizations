rm(list=ls())
library(ggplot2)
library(questionr)

# collecting the data
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df <- as.data.frame(read.csv(file = url(link)))
nigeria_df <- nigeria_df[,(names(nigeria_df) %in% c("s9q2","s6q4","wt_baseline"))]
nigeria_df <- nigeria_df[complete.cases(nigeria_df), ] #may skew the results if incomplete cases are a nonrandom sample


#get the weighted number of people who feel various levels of threatened by COVID19 as columns
nigeria_df$threat[nigeria_df$s9q2 == "1. A substantial threat"] <- "Severe threat"
nigeria_df$threat [nigeria_df$s9q2 == "2. A moderate threat"] <- "Medium threat"
nigeria_df$threat [(nigeria_df$s9q2 == "3. Not much of a threat")|(nigeria_df$s9q2 == "4. Not a threat at all" )] <- "Little or no threat"

#sort respondents in to industries
nigeria_df$industry[nigeria_df$s6q4 ==  "1. AGRICULTURE, HUNTING, FISHING"] <- "Agriculture"
nigeria_df$industry[(nigeria_df$s6q4 == "2. MINING, MANUFACTURING")] <- "Mining & Manufacturing"
nigeria_df$industry[(nigeria_df$s6q4 == "3. ELECTRICITY, GAS, WATER SUPPLY")] <- "Utilities"
nigeria_df$industry[(nigeria_df$s6q4 == "4. CONSTRUCTION")] <- "Construction"
nigeria_df$industry[(nigeria_df$s6q4 == "7. PROFESSIONAL ACTIVITIES: FINANCE, LEGAL, ANALYSIS, COMPUTER, REAL ESTATE")] <- "Finance, Law, & Tech"
nigeria_df$industry[(nigeria_df$s6q4 == "8. PUBLIC ADMINISTRATION")] <- "Public Administrationn"
nigeria_df$industry[(nigeria_df$s6q4 == "5. BUYING &amp; SELLING GOODS, REPAIR OF GOODS, HOTELS &amp; RESTAURANTS")] <- "Service & Hospitality"
nigeria_df$industry[(nigeria_df$s6q4 == "9. PERSONAL SERVICES, EDUCATION, HEALTH, CULTURE, SPORT, DOMESTIC WORK, OTHER")] <- "Other"


industry_threat=wtd.table(nigeria_df$industry, nigeria_df$threat, weights = nigeria_df$wt_baseline)
industry_threat_df=as.data.frame(industry_threat)
names(industry_threat_df) <- c("industry","threat","counts")

#marginal
industry_threat_mg_col <- prop.table(industry_threat,margin = 2)
#adding marginal
industry_threat_df$pct_col <- as.data.frame(industry_threat_mg_col)[,3]

base=ggplot(data <- industry_threat_df,  aes(x=reorder(industry, counts), y=counts, fill=threat))

conditionColor <- ifelse(industry_threat_df$threat%in%c("Minor Threat",'No Threat'),'grey80','grey50')
bar_stacked <- base + geom_bar(stat = "identity", position = 'stack', color = "black")

bar_stacked <- bar_stacked 
titleText='Number of Nigerian Housholds That Are Financially\nThreatened By COVID-19, By Industry'
subtitleText='As of May 2020'
sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19'

bar_stacked <- bar_stacked + labs(x = NULL, y = 'Number of Households', title=titleText, subtitle=subtitleText, x =NULL, y = NULL, caption = sourceText)
bar_stacked <- bar_stacked + guides(fill=guide_legend(title="")) + scale_fill_brewer(palette = "Blues") + theme_bw()
bar_stacked <- bar_stacked + theme( axis.text.x = element_text(angle = 30, hjust = 1, size=10 ))


bar_stacked