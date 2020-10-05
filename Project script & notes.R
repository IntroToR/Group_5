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


