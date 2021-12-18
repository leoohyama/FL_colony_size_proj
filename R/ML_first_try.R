#Load colony data

colony<-read.csv("Data/FL_colony_size_data.csv", encoding = "UTF-8",
                    na.strings = "")

#load libraries
library(tidyverse)
library(tidymodels)
library(ranger)
library(randomForest)

#look at column types
as.numeric(colony$colony_size)
colony_df<-as_tibble(colony)

colony_df<-colony_df %>%
  mutate(colony_size = as.numeric(colony_size))


#remove major and minor for now
colony_df %>%
  filter(!str_detect(Genus.Species, "major")) %>%
  filter(!is.na(colony_size)) %>%
  group_by(Subfamily, Genus.Species, diet, status) %>%
  summarise(median_size = median(colony_size, na.rm = T)) %>%
  ggplot(aes(x = status, y = median_size)) +
  geom_bar(stat = "identity")  +
  theme(axis.text = element_text(angle = 45))


colony_df_f<-colony_df %>%
  filter(!str_detect(Genus.Species, "major")) %>%
  filter(!is.na(colony_size)) %>%
  filter(!is.na(diet)) %>%
  filter(!is.na(nesting_strata)) %>%
  filter(!is.na(status)) %>%
  group_by(Subfamily, Genus.Species, diet, nesting_strata, morphism ,  status) %>%
  summarise(median_size = median(colony_size, na.rm = T))


#split data
set.seed(123)
colony_split <- initial_split(colony_df_f, prop = 0.75)
colony_split

#assign testing and training
colony_train <- training(colony_split)
colony_test <- testing(colony_split)


#resample data because we don't have enough data
set.seed(234)
colony_folds <- bootstraps(colony_train, strata = median_size)
colony_folds

rf_spec <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger",
             importance = "permutation")
colony_wf <- workflow() %>%
  add_formula(median_size~ diet + nesting_strata + morphism + status)


rf_rs <- colony_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = colony_folds,
    control = control_resamples(save_pred = TRUE)
  )

collect_metrics(rf_rs)
rf_rs$.metrics

