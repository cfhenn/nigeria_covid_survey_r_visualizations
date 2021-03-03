link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df_states <- as.data.frame(read.csv(file = url(link)))

link="https://github.com/adam-porton/PubPol543/blob/main/Data/r1_sect_7.csv?raw=true"
nigeria_df_income <- as.data.frame(read.csv(file = url(link)))

nigeria_df_income <- merg(nigeria_df_states)

nigeria_df$state[(substring(nigeria_df$state,2,2) == ".")] <- substr(nigeria_df$state[(substring(nigeria_df$state,2,2) == ".")] , 4, length(nigeria_df$state))
nigeria_df$state[(substring(nigeria_df$state,3,3) == ".")] <- substr(nigeria_df$state[(substring(nigeria_df$state,3,3) == ".")] , 5, length(nigeria_df$state))
nigeria_df$state[(nigeria_df$state == "FCT")] <- "Fct, Abuja"

linkMap="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/nigeria_geojson.geojson" 

map_ng=read_sf(linkMap)

map_ng_vars=merge(map_ng, #map first
                  nigeria_df,
                  by='state') 

