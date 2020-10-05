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

