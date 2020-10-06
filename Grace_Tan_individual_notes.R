## Loading in the global crop yield data from tidytuesday ##

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields
View(key_crop_yields)

library(tidyverse)
library(dplyr)
library(ggplot2)

unique_entities <- unique(key_crop_yields$Entity) # 249 countries/entities in this table
entities_df <-data.frame(unique_entities)
length(entities_df)

range(key_crop_yields$Year) # shows crop yields from 1961 to 2018

for (i in length(entities_df)) {
key_crop_yields %>%
  filter(Entity == entities_df[i, ])
}



