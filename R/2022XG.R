library(tidyverse)      # data manipulation
library(ggplot2)        # data visualization
library(hrbrthemes)     # customization of plot theme
library(ROSE)           # over and under balancing data 
library(forester)       # training tree-based models (from GitHub: https://github.com/ModelOriented/forester)
library(DALEX)          # using XAI tools
library(imager)         #load images 
library(ingredients)    # creating CP and AP 
library(worldfootballR) # scraping shot data  (from GitHub: https://github.com/JaseZiv/worldfootballR)



shot_stats <- shots_dataset %>% filter(result != "OwnGoal") %>%
  mutate(status = ifelse(result == "Goal", 1, 0)) %>%
  mutate(distanceToGoal = sqrt((105 - (X * 105)) ^ 2 + (34 - (Y * 68)) ^ 2)) %>%
  mutate(angleToGoal = abs(atan((7.32 * (105 - (X * 105))) / ((105 - (X * 105))^2 + (34 - (Y * 68)) ^ 2 - (7.32 / 2) ^ 2)) * 180 / pi)) %>%
  mutate(h_a = factor(h_a),
         situation = factor(situation),
         shotType = factor(shotType),
         lastAction = factor(lastAction),
         minute = as.numeric(minute)) %>%
  select(status, minute, h_a, situation, shotType, lastAction, 
         distanceToGoal, angleToGoal, season, match_id, result, player_id)
distangle <- load.image("shot_dist.png")




# preparing train set of original dataset
train_data <- shot_stats %>%
  select(status, minute, h_a, situation, shotType, lastAction, 
         distanceToGoal, angleToGoal)

# preparing train set of under-sampled dataset
set.seed(123)
under_train_data <- ovun.sample(status ~ ., data = train_data, method = "under")

# preparing train set of over-sampled dataset
set.seed(123)
over_train_data <- ovun.sample(status ~ ., data = train_data, method = "over")




source("evaluate.R")
source("forester.R")
source("make_ranger.R")
source("make_xgboost.R")
source("make_lightgbm.R")
source("make_catboost.R")
source("model_performancex.R")



# training tree-based models on original dataset
set.seed(123)
original_model <- forester(data   = train_data,
                           target = "status",
                           type   = "classification")
# training tree-based models on under-sampled dataset
set.seed(123)
under_model <- forester(data    = under_train_data$data,
                        target  = "status",
                        type    = "classification",
                        refclass = "")
# training tree-based models on over-sampled dataset
set.seed(123)
over_model <- forester(data   = over_train_data$data,
                       target = "status",
                       type   = "classification")


# Table III: Performance of trained xG models
# performance of random forest model
# on over-sampled data
model_performancex(over_model$model3)



# on under-sampled data
model_performancex(under_model$model3)



# on original data
model_performancex(original_model$model3)



# performance of catboost model
# on over-sampled data
model_performancex(over_model$model1)



# on under-sampled data
model_performancex(under_model$model1)



# on original data
model_performancex(original_model$model1)




# performance of xgboost model
# on over-sampled data
model_performancex(over_model$model2)



# on under-sampled data
model_performancex(under_model$model2)



# on original data
model_performancex(original_model$model2)




# performance of lightgbm model
# on over-sampled data
model_performancex(over_model$model4)



# on under-sampled data
model_performancex(under_model$model4)



# on original data
model_performancex(original_model$model4)




# Figure 1: The distribution of angle to goal and distance to goal of shots regarding goal status in the last seven seasons of top-five European football leagues


shot_vis <- data.frame(sta = as.factor(rep(shot_stats$status, 2)),
                       obs = c(shot_stats$distanceToGoal, 
                               shot_stats$angleToGoal),
                       vty = as.factor(c(rep("Distance to goal", length(shot_stats$distanceToGoal)),
                                         rep("Angle to goal", length(shot_stats$angleToGoal)))),
                       lea = rep(shot_stats$league, 2),
                       sea = rep(shot_stats$season, 2))
shot_vis$lea[shot_vis$lea == "La_liga"] <- "La Liga"
shot_vis$lea[shot_vis$lea == "Ligue_1"] <- "Ligue 1"
shot_vis$lea[shot_vis$lea == "Serie_A"] <- "Serie A"
ggplot(shot_vis, 
       aes(x = obs, 
           group = sta, 
           fill = sta)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set1",
                    name = "Goal Status", 
                    labels = c("No goal", "Goal")) + 
  theme(legend.position = "bottom") +
  labs(x = "Value (meter or angle)",
       y = "Density") + 
  facet_grid(lea ~ vty, 
             scales="free_x") 
plot(distangle,axes=FALSE)


#Saving the Model
saveRDS(model,file='D:\Kiran\Engg\SEM 5\SPT\Explainable_xG_model_paper-main\model.rda')


#load the Model
model_old=readRDS("D:\Kiran\Engg\SEM 5\SPT\Explainable_xG_model_paper-main")
