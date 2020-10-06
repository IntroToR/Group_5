## Loading in the global crop yield data from tidytuesday ##

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields
range(key_crop_yields$Year) # shows data from 1961-2018

# change year to factor so that numeric calculations will only act upon crop yield data
key_crop_yields$Year<-as.factor(key_crop_yields$Year) 

library(tidyverse)
library(dplyr)
library(ggplot2)

# I am going to try to average all the crops' yields over the entire 1961-2018 period
# Then I will select what each countries' most product product (on average) is over those years
# and assemble the country name, max historical average yield, and the name of the crop in a dataframe

unique_entities <- unique(key_crop_yields$Entity) # 249 countries/entities in this table
entities_df <-data.frame(unique_entities)
length(entities_df$unique_entities)

historical_avg_max <- matrix(NA, nrow = 249, ncol=3)
colnames(historical_avg_max) <- c("Entity", "max_yield", "max_crop")


i <- 1
for (i in 1:length(entities_df$unique_entities)) {
  entity_name <- entities_df[i,1]
  historical_avg_max[i,1] <- entity_name
  
  entity_i <- filter(key_crop_yields, Entity == entity_name)
  historical_avgs <- summarise_if(entity_i, is.numeric, mean)
  max_yield <- max(historical_avgs, na.rm = TRUE)
  historical_avg_max[i,2] <- max_yield
  
  max_col <- which(historical_avgs == max_yield)
  max_crop <- colnames(historical_avgs[max_col])
  historical_avg_max[i,3] <- max_crop
  
  i <- i + 1
}


# turned the final result into a data frame and coerced the yield to a numeric
historical_avg_max_df <- data.frame(historical_avg_max)
historical_avg_max_df$max_yield <- as.numeric(historical_avg_max_df$max_yield) 

unique(historical_avg_max_df$max_crop)


# plotting the data
# 249 is a lot of entities to deal with, so maybe ill focus on one crop

potato_entities <- historical_avg_max_df %>%
  filter(max_crop == "Potatoes (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(potato_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Potato Yield", "1961-2018")

cassava_entities <- historical_avg_max_df %>%
  filter(max_crop == "Cassava (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(cassava_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Cassava Yield", "1961-2018")

banana_entities <- historical_avg_max_df %>%
  filter(max_crop == "Bananas (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(banana_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Banana Yield", "1961-2018")

maize_entities <- historical_avg_max_df %>%
  filter(max_crop == "Maize (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(maize_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Maize Yield", "1961-2018")

rice_entities <- historical_avg_max_df %>%
  filter(max_crop == "Rice (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(rice_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Rice Yield", "1961-2018")

barley_entities <- historical_avg_max_df %>%
  filter(max_crop == "Barley (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(barley_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Barley Yield", "1961-2018")


# this one's kinda dumb because Namibia is the only country where wheat was the most produced in that time
wheat_entities <- historical_avg_max_df %>%
  filter(max_crop == "Wheat (tonnes per hectare)") %>%
  arrange(max_yield)
ggplot(wheat_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_col() +
  ggtitle("Average Wheat Yield", "1961-2018")
