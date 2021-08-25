library(tidymodels)
library(skimr)
library(tidyverse)
library(scales)
library(tidyverse)
library(silgelib)
install.packages("ranger")
library(ranger)
theme_set(theme_plex())

volcano_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
skim(volcano_raw)
volcano_raw%>%filter(last_eruption_year!="Unknown")
volcano_raw%>%count(primary_volcano_type, sort=TRUE)
#rewrite our variable we use transmute

volcano_df <- volcano_raw %>%
  transmute(volcano_type = case_when(str_detect(primary_volcano_type, "Stratovolcano") ~ "Stratovolcano",
                                     str_detect(primary_volcano_type, "Shield") ~ "Shield",
                                     TRUE ~ "Other"),
            volcano_number, latitude, longitude, elevation, 
            tectonic_settings, major_rock_1) %>%
  mutate_if(is.character, factor)
volcano_df%>%count(volcano_type)

#data split & recipe
set.seed(123)
volcano_df_split<-initial_split(volcano_df)
#training vs testing partitioning
volcano_train<-training(volcano_df_split)
volcano_test<-testing(volcano_df_split)

volcano_train%>%count(volcano_type)
volcano_test%>%count(volcano_type)


volcano_train%>%count(volcano_type)
volcano_test%>%count(volcano_type)

#bootstrapping
volcano_boot<-bootstraps(volcano_df)

#recipe

volcano_df%>%count(volcano_number)
class(volcano_df$volcano_number)

volcano_rec<-recipe(volcano_type~., data =volcano_df)%>%
  update_role(volcano_number, new_role="Id")%>%
  
  
  volcano_df %>% count(tectonic_settings, sort = TRUE)

  
volcano_prep<-(volcano_rec)

#extract the dataset from the one you using in the recipe 
juice(volcano_prep)
#if you need to apply the recipe into the new data (which is not in the recipe settings), you need to use the bake function()



#modeling

install.packages("ranger")
library(ranger)

rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

