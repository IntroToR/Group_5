## Loading in the global crop yield data from tidytuesday ##

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields
View(key_crop_yields)

## Looking at the structure of the data frame ##

class(key_crop_yields)  #it's a data frame

dim(key_crop_yields)  #dimensions of the data frame, 14 columns 13075 rows

summary(key_crop_yields) # summary of all variables

## summary stats on the different yields all countries and years##
sumStats<-function(X,...){
  xbar<-mean(X,...)
  sd<-sd(X,...)
  q1<-quantile(X,p=0.25,...)
  med<-median(X,...)
  q3<-quantile(X,p=0.75,...)
  c(mean =xbar, sd = sd, Q1 = q1, median = med, Q3 = q3)
}
apply(key_crop_yields[,-c(1:3)],2,FUN=sumStats, na.rm=T)

## changing Year variable to a factor
key_crop_yields$Year<-as.factor(key_crop_yields$Year) 

##Mean yields by country##
key_crop_yields %>% 
  group_by(Entity) %>% 
  summarise_if(is.double,mean, na.rm=T)

##Mean yields by year##
key_crop_yields %>%
  group_by(Year) %>%
  summarise_if(is.double,mean, na.rm=T)

##############################################################################
#This is where the useful coding begins, determining number of countries that 
#produce each crop and make a barplot#

###Average yields each crop by country
yieldavgs<-key_crop_yields %>% 
  group_by(Entity) %>% 
  summarise_if(is.double,mean, na.rm=T) 

###Combine all crop variables into one column named "crop" and the average yield
###values into "avgyield"
yieldavgs<-pivot_longer(yieldavgs,c(2:12),names_to="crop",values_to = "avgyield")

### Removes all NaN observations for countries with no production of each of the
###crops
yieldavgs1<-na.omit(yieldavgs)

### generates number of countries that produce each crop
cropcounts<-yieldavgs1%>%
  group_by(crop)%>%
  tally(name="Number of producing entities")

### plot showing number of countries that produce each crop
###Forming the barplot
barplot<- cropcounts%>%
  mutate(crop=fct_reorder(crop,`Number of producing entities`))%>%
  ggplot(aes(x=crop,y=`Number of producing entities`),show.legend=FALSE)+
  geom_bar(colour="black",stat="identity",aes(fill=as.factor(crop)),width=0.7,show.legend=FALSE)+
  coord_flip()+
  xlab("Crop")+
  ylab("Number of Countries Producing Crop")+
  expand_limits(y=c(0,225))

###Adjusting labels and creating colors to fill bars
croplabs<- c("Cocoa beans","Cassava","Peas","Soybeans","Barley","Rice","Wheat","Bananas","Beans","Potatoes","Maize")
numcol<- 11
colors<-colorRampPalette(brewer.pal(9,"YlGn"))(numcol)

###Formatting barplot with axis labels, title, colors, and theme. Also added data labels to show value next to bars
barplot+scale_x_discrete(labels=croplabs)+
  ggtitle("Global Production of Major Crops 1961-2018")+
  scale_fill_manual(values=colors)+
  theme_classic()+
  theme(panel.background = element_rect(fill="gray96"),axis.text = element_text(size=12),axis.title=element_text(size=13.5))+
  geom_text(aes(label=cropcounts$`Number of producing entities`),color="gray40",hjust=-.2)



