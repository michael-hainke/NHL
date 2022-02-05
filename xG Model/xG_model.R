
# Libraries
library(tidyverse)   # data wrangling
library(tidymodels)  # tidy machine learning functions
library(Information) # WOE and IV functions
library(corrplot)    # correlation plots
library(themis)      # sample balancing
library(glmnet)      # linear algorithms
library(vip)         # model summary - importance scores
library(probably)    # threshold tuning
library(xgboost)     # xgboost algorithm

# Functions

# Use this avoid some parallel computing errors I get when trying to tune model
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# Set Parameters

size <- 100000     # Train & Test data sample size
algorithm <- "LR"  # Algorithm (LR, RF, XGB)
revision <- 1      # Revision Number

# Output Folder
output_folder <- "~/GitHub/NHL/xG Model/Outputs"

predictor_names <- c("secondaryType",
                     "dist",
                     "angle",
                     "last_shot")
outcome_name <- "Event"
id_names <- NULL
col_names <- c(id_names, predictor_names, outcome_name)

# Read Data
play_data <- c("plays_2011.rds",
               "plays_2012.rds",
               "plays_2013.rds",
               "plays_2014.rds",
               "plays_2015.rds",
               "plays_2016.rds",
               "plays_2017.rds",
               "plays_2018.rds",
               "plays_2019.rds",
               "plays_2020.rds")

setwd("../Data")

df_raw <- NULL

for (filename in play_data) {
  
  tmp <- readRDS(filename) %>%
         select(event,
                eventCode,
                eventTypeId,
                description,
                secondaryType,
                gameWinningGoal,
                emptyNet,
                penaltySeverity,
                penaltyMinutes,
                eventIdx,
                eventId,
                period,
                periodType,
                ordinalNum,
                periodTime,
                periodTimeRemaining,
                dateTime,
                x,
                y,
                id,
                name,
                link,
                triCode,
                row.Names,
                Assist1,
                Assist2,
                Blocker,
                DrewBy,
                Goalie,
                Hittee,
                Hitter,
                Loser,
                PenaltyOn,
                PlayerID,
                Scorer,
                Shooter,
                Winner,
                gamePk,
                gameType,
                away_team,
                home_team,
                strength.code,
                strength.name,
                goals.away,
                goals.home,
                ServedBy)
  
  if (is.null(df_raw)) { df_raw <- tmp } else { df_raw <- rbind(df_raw,tmp) }
  
}

###############################
# Filter and Pre-Process Data #
###############################

# Goal Line at x = 89 feet
# Goal Width 3 ft (use 2.9999 ft to avoid NaN arctan result)
# For shots from behind net (x > 89) assume shot from goal line x = 89


# Pre-process Data
# Remove Period = 5 and time = 00:00 data as those are shootout events
xg_data <- df_raw %>%
           filter(eventTypeId %in% c("GOAL","SHOT"),
                  !is.na(x),
                  !is.na(y),
                  !(period == 5 & periodTime == '00:00')) %>%
           mutate(time_s = as.integer(substr(periodTime,1,2))*60+as.integer(substr(periodTime,4,5)),
                  dist = sqrt(y^2 + (89 - abs(x))^2),
                  angle = case_when(abs(x) <= 89 ~ atan((abs(y)-2.999)/(89 - abs(x)))*180/pi,
                                    TRUE ~ atan((abs(y)-2.9999)/(0))*180/pi)) %>%
           group_by(gamePk, period, name) %>%
           arrange(time_s) %>%
           mutate(last_shot = time_s - lag(time_s, 1)) %>%
           ungroup() %>%
           mutate(Event = case_when(eventTypeId == "GOAL" ~ 1,
                                    TRUE ~ 0)) %>%
           select(one_of(col_names))

######################
# WOE transformation #
######################

woe_table <- create_infotables(data = xg_data, y="Event")

# Define Features to Use
col_vector <- xg_data %>%
              select(one_of(predictor_names)) %>%
              imap_dfr(~ tibble(colname = .y, class = class(.x) %>% str_c(collapse = ", ")))

# Loop Through Each Variable and Create a New WOE Transformed Column
col_vector <- col_vector %>% filter(colname %in% woe_table$Summary$Variable)

for (i in 1:length(col_vector$colname)) {
  if (col_vector$class[i] == 'numeric') {
    # extract woe table and split min and max bin
    df <- woe_table[["Tables"]][[col_vector$colname[i]]] %>%
          mutate(id = as.character(row_number())) %>%
          separate(col_vector$colname[i], c("min_bin","max_bin"), ",") %>%
          mutate(min_bin = str_replace(min_bin, "\\[",""),
                 max_bin = as.numeric(str_replace(max_bin, "\\]","")))
    
    # Create a Vector With Bins
    bin_cuts <- df %>%
                select(id, max_bin) %>%
                mutate_all(~replace(., is.na(.),-100000000))
    
    # Split Original Data Into Some Bins
    cut_vector <- as.character(cut(xg_data[[col_vector$colname[i]]], breaks = bin_cuts$max, labels = bin_cuts$id[2:length(bin_cuts$id)]))
    
    # Create New Column with WOE Data (train)
    tmp <- data.frame(xg_data[[col_vector$colname[i]]], cut_vector, stringsAsFactors = FALSE)
    names(tmp) <- c(col_vector$colname[i],"id")
    tmp <- mutate(tmp, id = case_when(is.na(id) ~ "1",
                                      TRUE ~ id)) %>%
           left_join(df, by = c("id")) %>%
           select(WOE)
    names(tmp) <- c(paste0(col_vector$colname[i],"_woe"))
  } else if (col_vector$class[i] == 'character') {
    df <- woe_table[["Tables"]][[col_vector$colname[i]]]
    tmp <- data.frame(xg_data[,col_vector$colname[i]]) 
    names(tmp) <- c(col_vector$colname[i])
    tmp <- tmp %>%
           left_join(df, by = c(col_vector$colname[i])) %>%
           select(WOE)
    names(tmp) <- c(paste0(col_vector$colname[i],"_woe"))
  }
  # Bind Onto Existing Dataframe (Train)
  xg_data <- cbind.data.frame(xg_data, tmp)
  
}

# Select Only WOE Columns
xg_data <- xg_data %>% select(Event,
                              one_of(id_names),
                              ends_with("woe")) %>%
           mutate(Event = as.factor(Event))


# Correlation Analysis
df_corr <- select(xg_data,-Event,-one_of(id_names))
corr_matrix <- cor(df_corr)
corrplot(corr_matrix)

# Identify Low Predictive Power Columns (IV < 0.02)
low_IV <- woe_table$Summary %>% filter(IV < 0.02)
low_IV


###############
# Train Model #
###############

# Train / Test split
sample_df <- sample_n(xg_data, size)

data_split <- initial_split(sample_df, prop = 3/4, strata = Event)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Cross Fold validation split
folds <- vfold_cv(train_data, strata = Event)

# Build Model Pipeline - Set Model Parameters
if (algorithm == "LR") {
  mod <- logistic_reg(penalty = tune(), mixture = 1) %>%
         set_engine("glmnet")
  mod_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
} else if (algorithm == 'RF') {
  mod <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
         set_engine("ranger", importance = "permutation") %>% 
         set_mode("classification")
  mod_grid <- mod %>%
              parameters() %>%
              finalize(select(sample_df,-Event,-one_of(id_names))) %>%
              grid_max_entropy(size = 10)
} else if (algorithm == 'XGB') {
  mod <- boost_tree(
         trees = 4,
         tree_depth = tune(), min_n = tune(),
         loss_reduction = tune(),                     ## first three: model complexity
         sample_size = tune(), mtry = tune(),         ## randomness
         learn_rate = tune(),                         ## step size
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  mod_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 30
  )
}

# Build Model Pipeline - Define Recipe
pred_rec <- recipe(Event ~ ., data = xg_data) %>% 
            update_role(one_of(id_names), new_role = "ID") %>%
            step_zv(all_predictors()) %>%
            step_rose(Event)

# Build Model Pipeline - Define Workflow
pred_wflow <- workflow() %>% 
              add_recipe(pred_rec)  %>% 
              add_model(mod)

# Tune Model
metric_list <- metric_set(roc_auc, accuracy, sens, spec)

unregister_dopar()

res <- pred_wflow %>%
       tune_grid(resamples = folds,
                 grid = mod_grid,
                 control = control_grid(save_pred = TRUE, event_level = "second"),
                 metrics = metric_list)

best_tune <- res %>% select_best("roc_auc")

######################
# Final Fit and Test #
######################

# Finalize workflow
final_wflow <- pred_wflow %>% 
               finalize_workflow(best_tune)

# Train and Test Model
final_fit <- final_wflow %>%
             last_fit(data_split, metrics = metric_list)

#####################
# Get Model Metrics #
#####################

# Sample Size and Event Frequency
dim(sample_df)[1]
table(sample_df$Event)

# Summary metrics
test_metrics <- final_fit %>% collect_metrics()
test_metrics

# ROC curve
test_results <- final_fit %>% collect_predictions()
test_roc <- test_results %>% roc_curve(Event, .pred_0) %>% autoplot()
test_roc

# Cross Validation metrics
cv_metrics <- collect_metrics(res)
cv_metrics

# Variable importance
imp_var <- final_fit %>% pull(.workflow) %>% pluck(1) %>% pull_workflow_fit() %>% vip()
imp_var

# LR summary
if (algorithm == 'LR') { summary <- final_fit %>% pull(.workflow) %>% pluck(1) %>% tidy() %>% arrange(desc(estimate)) } else { summary <- ''}
summary

# Confusion matrix
test_matrix <- final_fit %>% collect_predictions() %>% conf_mat(Event, .pred_class)
test_matrix$table

# Correlation Matrix
corrplot(corr_matrix)

# WOE Table Summary
woe_table[["Summary"]]

# WOE Tables
for (i in 1:length(woe_table$Summary$Variable)) {
  print(as.data.frame(woe_table[["Tables"]][woe_table$Summary$Variable[i]]))
}

####################
# Save Final Model #
####################

setwd(output_folder)

# Save Model as .rds
final_model <- final_fit$.workflow[[1]]
saveRDS(final_model, file = paste0(Sys.Date(),"_NHL_XGMODEL_",algorithm,"_v",revision,".rds"))

# Save WOE tables as .rds
saveRDS(woe_table, file = paste0(Sys.Date(),"_NHL_XGMODEL_WOE.rds"))


