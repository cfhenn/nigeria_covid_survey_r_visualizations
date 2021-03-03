link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df_states <- as.data.frame(read.csv(file = url(link)))
nigeria_df_states <- nigeria_df_states[,c("wt_baseline", "state", "hhid")]

link="https://github.com/adam-porton/PubPol543/blob/main/Data/r1_sect_7.csv?raw=true"
nigeria_df_income <- as.data.frame(read.csv(file = url(link)))
nigeria_df_income$lost_income <- as.integer(nigeria_df_income$s7q2 == "3. Reduced")
nigeria_df_income <- aggregate(lost_income ~ hhid, nigeria_df_income, max)
nigeria_df_income <- merge(nigeria_df_states, nigeria_df_income, by="hhid")
remove(nigeria_df_states)
nigeria_df_income$lost_income <- nigeria_df_income$lost_income*nigeria_df_income$wt_baseline
nigeria_df_income <- aggregate(cbind(lost_income, wt_baseline)~state, nigeria_df_income, sum)
nigeria_df_income$lost_income <- 100*(nigeria_df_income$lost_income/nigeria_df_income$wt_baseline)
nigeria_df_income <- nigeria_df_income[,c("state", "lost_income")]

nigeria_df_income$state[(substring(nigeria_df_income$state,2,2) == ".")] <- substr(nigeria_df_income$state[(substring(nigeria_df_income$state,2,2) == ".")] , 4, length(nigeria_df_income$state))
nigeria_df_income$state[(substring(nigeria_df_income$state,3,3) == ".")] <- substr(nigeria_df_income$state[(substring(nigeria_df_income$state,3,3) == ".")] , 5, length(nigeria_df_income$state))
nigeria_df_income$state[(nigeria_df_income$state == "FCT")] <- "Fct, Abuja"

linkMap="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/nigeria_geojson.geojson" 

map_ng=read_sf(linkMap)

map_ng_vars=merge(map_ng, nigeria_df_income, by='state') 

titletext <- "Pecentage of Nigerians who have experienced a decrease in income since March 2020, by state"
sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19'


map=ggplot(data=map_ng) + geom_sf(fill='grey90',color=NA) + 
  theme_classic() +
  geom_sf(data=map_ng_vars, aes(fill=lost_income), color=NA) + 
  scale_fill_gradient(low = 'blue', high= 'red') +
  guides(fill=guide_legend(title="Percentage of people")) +
  ggtitle(titletext) +
  labs( caption = sourceText)
map
