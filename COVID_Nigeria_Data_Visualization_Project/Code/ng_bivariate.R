rm(list=ls())
library(ggplot2)

# collecting the data
link="https://github.com/adam-porton/PubPol543/raw/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv"
nigeria_df <- as.data.frame(read.csv(file = url(link)))

#convert "yes"/"no" survey responses to 1/0 integer variables that can be summed/averaged
nigeria_df$threat[nigeria_df$s9q2 == "1. A substantial threat"] <- "Substantial Threat"
nigeria_df$threat[nigeria_df$s9q2 == "2. A moderate threat" ] <- "Moderate Threat"
nigeria_df$threat[(nigeria_df$s9q2 == "3. Not much of a threat") | (nigeria_df$s9q2 == "4. Not a threat at all" )] <- "Little or No Threat"

#sort respondents in to industries
nigeria_df$industry[nigeria_df$s6q4 ==  "1. AGRICULTURE, HUNTING, FISHING"] <- "Agriculture"
nigeria_df$industry[(nigeria_df$s6q4 == "2. MINING, MANUFACTURING")] <- "Mining & Manufacturing"
nigeria_df$industry[(nigeria_df$s6q4 == "3. ELECTRICITY, GAS, WATER SUPPLY")] <- "Utilities"
nigeria_df$industry[(nigeria_df$s6q4 == "4. CONSTRUCTION")] <- "Construction"
nigeria_df$industry[(nigeria_df$s6q4 == "7. PROFESSIONAL ACTIVITIES: FINANCE, LEGAL, ANALYSIS, COMPUTER, REAL ESTATE")] <- "Finance, Law, Tech, & Real Estate"
nigeria_df$industry[(nigeria_df$s6q4 == "8. PUBLIC ADMINISTRATION")] <- "Public Administrationn"
nigeria_df$industry[(nigeria_df$s6q4 == "5. BUYING &amp; SELLING GOODS, REPAIR OF GOODS, HOTELS &amp; RESTAURANTS")] <- "Service & Hospitality"
nigeria_df$industry[(nigeria_df$s6q4 == "9. PERSONAL SERVICES, EDUCATION, HEALTH, CULTURE, SPORT, DOMESTIC WORK, OTHER")] <- "Other"


##################################################################################
##################################################################################
# Relevant section here 

industry_threat=table(nigeria_df$industry, nigeria_df$threat)
industry_threat_df=as.data.frame(industry_threat)
names(industry_threat_df) <- c("industry","threat","counts")

#marginal
industry_threat_mg_col <- prop.table(industry_threat,margin = 2)
#adding marginal
industry_threat_df$pct_col <- as.data.frame(industry_threat_mg_col)[,3]

##################################################################################
##################################################################################
base=ggplot(data <- industry_threat_df,  aes(x=reorder(industry, counts), y=counts, fill=threat))

conditionColor <- ifelse(industry_threat_df$threat%in%c("Minor Threat",'No Threat'),'grey80','grey50')
bar_stacked <- base + geom_bar(stat = "identity", position = 'stack')

bar_stacked <- bar_stacked + theme( axis.text.x = element_text(angle = 90, hjust = 1, size=8 ))
titleText='Number of Nigerian housholds that are financially threatened by COVID-19, by industry'
sourceText='Source: World Bank'

bar_stacked <- bar_stacked + xlab("Industry") + ylab("Number of Households")
bar_stacked <- bar_stacked + labs(title=titleText, x =NULL, y = NULL, caption = sourceText)
bar_stacked <- bar_stacked + guides(fill=guide_legend(title=""))

bar_stacked
dev.copy(png,'myplot.png')
dev.off()