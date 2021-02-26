linkCSV='https://github.com/Visual-Analytics-Project-UW-EvansSchool/mergedData/raw/main/merged_data.csv'

dataCSV=read.csv(linkCSV)

str(dataCSV)

linkMap="https://github.com/Visual-Analytics-Project-UW-EvansSchool/ourMap/raw/main/worldMap.geojson" # link desde github
library(sf)

mapWorld=read_sf(linkMap)

head(mapWorld)

mapWorldVars=merge(mapWorld, #map first
                   dataCSV, 
                   by='ISO3') 

library(ggplot2)
# plot original map
base=ggplot(data=mapWorld) + geom_sf(fill='grey90',
                                     color=NA) + theme_classic()

base #base map



#ggplot(data=mapWorldVars) + geom_boxplot(aes(y=DemoIndex))

#ggplot(data=mapWorldVars) + geom_boxplot(aes(y=HDI))

#ggplot(data=mapWorldVars) + geom_boxplot(aes(y=mil_expend))

mapWorldVars$HDI_S=as.vector(scale(mapWorldVars$HDI))
mapWorldVars$DEM_S=as.vector(scale(mapWorldVars$DemoIndex))
mapWorldVars$MIL_S=as.vector(scale(mapWorldVars$mil_expend))

#data to cluster
library(cluster)
vars=c('HDI_S','DEM_S','MIL_S')

set.seed(123)
distvars=mapWorldVars[,vars]%>%
  st_drop_geometry() %>%
  cluster::daisy()

res.hier=hclust(distvars,"ward.D2")

library(ggdendro)
ggdendrogram(res.hier)

mapWorldVars$clustH=cutree(res.hier,k=5)%>%as.factor()

vars2=c('HDI_S','DEM_S','MIL_S','clustH')
aggregate(.~clustH,
          data=mapWorldVars[,vars2]%>%
            st_drop_geometry(),
          FUN=median)

mapWorldVars$clustH=factor(mapWorldVars$clustH,
                           levels=c(5,1,2,3,4),
                           labels=c(1,2,3,4,5), 
                           ordered=T)




library(rio)

linkCity="https://github.com/Visual-Analytics-Project-UW-EvansSchool/mergedData/raw/main/otherData/citiesloc.xlsx"


idxcity=import(linkCity)

str(idxcity)

idxcity_sf = st_as_sf(idxcity, 
                      coords = c("lng", "lat"),
                      crs = st_crs(mapWorldVars))

head(idxcity_sf)

base + geom_sf(data=idxcity_sf)

varsidx=c("DIGITAL","HEALTH","INFRASTRUCTURE","PERSONAL")

set.seed(123)
distvars2=idxcity_sf[,varsidx]%>%
  st_drop_geometry() %>%
  cluster::daisy()

res.hier2=hclust(distvars2,"ward.D2")

library(ggdendro)
ggdendrogram(res.hier2)

# getClusters

idxcity_sf$clustH=cutree(res.hier2,k=4)%>%as.factor()

#check order:

varsidx=c(varsidx,'clustH')
aggregate(.~clustH,
          data=idxcity_sf[,varsidx]%>%
            st_drop_geometry(),
          FUN=median)

idxcity_sf$clustH=factor(idxcity_sf$clustH,
                         levels=c(4,3,2,1),
                         labels =c("bad","middle",'good','very good'),
                         ordered=T)

#previously
fillLabels=c('1_worst',2,3,'4_best')
fillTitle="Cities safety"
nameSize="Population \n(in millions)"

points= base + geom_sf(data=idxcity_sf,
                       aes(fill=clustH,size=population/1000000),shape=21) 
#now
points = points + scale_fill_brewer(palette ='YlOrRd',
                                    direction = -1,
                                    labels=fillLabels,
                                    name=fillTitle)
points =points + scale_size(name=nameSize,
                            range = c(0.2,5))
points
