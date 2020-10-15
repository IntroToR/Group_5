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

historical_avg_max <- matrix(NA, nrow = 249, ncol=4)
colnames(historical_avg_max) <- c("Entity", "max_yield", "max_crop", "sd")


i <- 1
for (i in 1:length(entities_df$unique_entities)) {
  entity_name <- entities_df[i,1]
  historical_avg_max[i,1] <- entity_name 
  
  entity_i <- filter(key_crop_yields, Entity == entity_name)
  historical_avgs_sd <- summarise_if(entity_i, is.numeric, list(mean, sd))
  historical_avgs <- select(historical_avgs_sd, contains("fn1"))
  sd_avg_yield <- select(historical_avgs_sd, contains("fn2"))
  max_yield <- max(historical_avgs, na.rm = TRUE)
  historical_avg_max[i,2] <- max_yield 
  
  max_col <- which(historical_avgs == max_yield)
  max_crop <- colnames(historical_avgs[max_col])
  historical_avg_max[i,3] <- max_crop 
  
  max_crop <- chartr(old = "fn1", new = "fn2", max_crop)
  max_crop_sd <- (sd_avg_yield[1,max_crop])
  num_sd <- as.numeric(max_crop_sd[1,1])
  historical_avg_max[i,4] <- num_sd
  
  i <- i + 1
}


# turned the final result into a data frame and coerced the yield to a numeric
historical_avg_max_df <- data.frame(historical_avg_max)
historical_avg_max_df$max_yield <- as.numeric(historical_avg_max_df$max_yield) 
historical_avg_max_df$sd <- as.numeric(historical_avg_max_df$sd) 

unique(historical_avg_max_df$max_crop)

# plotting the data
# 249 is a lot of entities to deal with, so maybe ill focus on one crop


# The potato is the crop that the most entities produce as their highest yield from 1961-2018
potato_entities <- historical_avg_max_df %>%
  filter(max_crop == "Potatoes (tonnes per hectare)_fn1") %>%
  arrange(max_yield)
ggplot(potato_entities, aes(x=reorder(Entity, max_yield), y=max_yield)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=max_yield-sd, ymax=max_yield+sd), width=.2, position=position_dodge(.9)) +
  ggtitle("Average Potato Yield", "1961-2018") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 4)) +
  labs(x = "Country", y = "Historical Average Yield (tonnes per hectare)")



# But Hannah's graph shows that maize is the most commonly produced crop, so we will graph that too
avg_maize <- matrix(NA, nrow = 249, ncol=3)
colnames(avg_maize) <- c("Entity", "avg_yield", "sd")

i <- 1
for (i in 1:length(entities_df$unique_entities)) {
  entity_name <- entities_df[i,1]
  avg_maize[i,1] <- entity_name
  
  entity_i <- filter(key_crop_yields, Entity == entity_name)
  avg_maize_yield_i <- mean(entity_i$`Maize (tonnes per hectare)`)
  avg_maize[i,2] <- avg_maize_yield_i
  maize_sd <- sd(entity_i$`Maize (tonnes per hectare)`)
  avg_maize[i,3] <- maize_sd
  
  i <- i + 1
}

# turned the final result into a data frame and coerced the yield to a numeric
avg_maize_df <- data.frame(avg_maize)
avg_maize_df$avg_yield <- as.numeric(avg_maize_df$avg_yield) 
avg_maize_df$sd <- as.numeric(avg_maize_df$sd) 


# remove NA entities and order from highest maize yield to lowest
avg_maize_df2 <- avg_maize_df %>% 
  drop_na() %>%
  arrange(desc(avg_yield))
ggplot(avg_maize_df2, aes(x=reorder(Entity, avg_yield), y=avg_yield)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=avg_yield-sd, ymax=avg_yield+sd), width=.2, position=position_dodge(.9)) +
  ggtitle("Average Maize Yield", "1961-2018") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 4)) +
  labs(x = "Country", y = "Historical Average Yield (tonnes per hectare)")














# more bar graphs for entities that produce each crop on average the highest
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
