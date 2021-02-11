# collecting the data
link="https://github.com/EvansDataScience/data/raw/master/crime.RData"
load(file = url(link))

# seeing the variable names
names(crime)

# checking data types
str(crime,width = 50,strict.width='cut')

# contingency table of counts
(PrecintDaytime=table(crime$Precinct,crime$Occurred.DayTime))

# computing marginal percent (per column) from contingency table
library(magrittr)
(PrecDayti_mgCol=prop.table(PrecintDaytime,
                            margin = 2)%>%round(.,3))

#making a data frame from contingency table

PrecDaytiDF=as.data.frame(PrecintDaytime)
names(PrecDaytiDF)=c("precint","daytime","counts")

#adding marginal percents:
PrecDaytiDF$pctCol=as.data.frame(PrecDayti_mgCol)[,3]

# head of data frame representing contingency table and marginals
head(PrecDaytiDF)

library(ggplot2)
base1=ggplot(data=PrecDaytiDF, 
             aes(x=daytime, y=counts,
                 fill=precint)) # this 'aes' in legend

barDodge= base1 +  geom_bar(stat="identity",
                            position ='dodge') 
barDodge

barDodge + geom_text(position = position_dodge(),
                     angle=90,#angle!!
                     hjust=1,
                     aes(label=counts)) # AES!

barDodge=barDodge + geom_text(position = position_dodge(0.9),
                              angle=90,#angle!!
                              hjust=1,
                              aes(label=counts)) # AES!
barDodge

barDodge + scale_fill_brewer(palette="Paired")

PrecDaytiDF$precint=factor(PrecDaytiDF$precint,
                           levels=c("NORTH","WEST","EAST", "SOUTH","SOUTHWEST"))

base1=ggplot(data=PrecDaytiDF, 
             aes(x=daytime, y=counts,
                 fill=precint)) # this 'aes' in legend

barDodge= base1 +  geom_bar(stat="identity",
                            position ='dodge') 
barDodge=barDodge + geom_text(position = position_dodge(0.9),
                              angle=90,#angle!!
                              hjust=1, size=4,
                              fontface='bold',
                              aes(label=counts)) # AES!
# palette with ordering
barDodge + scale_fill_brewer(name="PRECINCT",
                             palette="BuPu",#for order
                             direction = -1) 

# Stacked bar plot
conditionColor=ifelse(PrecDaytiDF$precint%in%c("NORTH",'WEST'),'grey80','grey50')

barStacked = base1 + geom_bar(stat = "identity",
                              position = 'stack')#default
barStacked = barStacked + geom_text(size = 5,
                                    fontface='bold',
                                    position = position_stack(vjust = 0.5),
                                    color=conditionColor,
                                    aes(label=counts))# its own AES!
barStacked + scale_fill_brewer(palette="GnBu",
                               direction = -1)

#stacked percent
# they show the marginal table above: "PrecDayti_mgCol"
library(scales) # notice in 'percent()" below"

base2=ggplot(data=PrecDaytiDF, 
             aes(fill=precint,y=counts,x=daytime)) # changes

barStackPct= base1 +  geom_bar(stat = "identity",
                               position="fill") # you need this

barStackPct= barStackPct + geom_text(size = 5,# check below:
                                     position = position_fill(vjust = 0.5),
                                     aes(label=percent(pctCol,accuracy = 0.1)))

barStackPct

# contingency table with many levels:

(CrimeDay=table(crime$crimecat,crime$Occurred.DayTime))

#making a data frame from contingency table

CrimeDayDF=as.data.frame(CrimeDay)
#renaming:
names(CrimeDayDF)=c("crime","daytime","counts")
#marginal
CrimeDay_mgCol=prop.table(CrimeDay,margin = 2)
#adding marginal
CrimeDayDF$pctCol=as.data.frame(CrimeDay_mgCol)[,3]

# result for ggplot:
head(CrimeDayDF)

# bad idea
base2=ggplot(data=CrimeDayDF,
             aes(x=daytime,y=counts,fill=crime))
base2 + geom_bar(stat = "identity") + 
  geom_text(size = 3, 
            position = position_stack(vjust = 0.5),
            aes(label=counts))

# plotting a representation of contingency table:

library(ggplot2)                           
base3 = ggplot(CrimeDayDF, aes(x=daytime,y=crime)) 
# plot value as point, size by value of percent
tablePlot = base3 + geom_point(aes(size = pctCol*100)) 
# add value of Percent as label
tablePlot = tablePlot + geom_text(aes(label = percent(pctCol)),
                                  nudge_x = 0.2,
                                  size=3)
tablePlot

# improving previous plot

tablePlot = tablePlot + theme_minimal() # less ink
tablePlot = tablePlot + theme(legend.position="none") # no legend
tablePlot

# as usual for barplot (less info than base1)
base4 = ggplot(CrimeDayDF, aes(x = crime, y = counts ) ) 

#the bars
bars  = base4 + geom_bar( stat = "identity" ) + theme_minimal()

# bar per day time with 'facet'
barsFa = bars + facet_grid(~ daytime) 

barsFa

# change the minimal theme

barsFa = barsFa + theme( axis.text.x = element_text(angle = 90,
                                                    hjust = 1,
                                                    size=3 ))
barsFa

# similar to base4
base5  = ggplot(CrimeDayDF, aes(x = crime,  y = pctCol ) ) 
barsIO = base5 + geom_bar( stat = "identity" )
barsIO = barsIO + facet_grid( ~ daytime) 
barsIO = barsIO + coord_flip()

barsIO