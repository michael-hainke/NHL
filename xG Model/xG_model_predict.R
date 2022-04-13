library(tidyverse)  # data manipulation
library(lubridate)  # date parsing
library(tidymodels) # tidy machine learning functions


# Set Parameters

model_folder <- "~/GitHub/NHL/xG Model/Outputs"
model_name <- "2022-02-04_NHL_XGMODEL_LR_v1.rds"
woe_name <- "2022-02-04_NHL_XGMODEL_WOE.rds"

id_names <- c("dateTime",
              "name",
              "Shooter",
              "gamePk",
              "away_team",
              "home_team")
event <- "Event"
predictor_names <- c("dist",
                     "angle",
                     "last_shot")
col_names <- c(id_names,event,predictor_names)

### Load & Prep Data ###
########################

df <- readRDS('../Data/2022-02-03_plays_2021-2022.rds')

# Pre-process Data
# Remove Period = 5 and time = 00:00 data as those are shootout events
xg_data <- df %>%
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

### Load Models ###
###################

# Load WOE Table & Model
woe_table <- readRDS(paste0(model_folder,"/",woe_name))
cur_model <- readRDS(paste0(model_folder,"/",model_name))

# Variable Info
var_info <- cur_model$pre$mold$blueprint$recipe$var_info
predictor_names <- var_info %>%
                   filter(role == 'predictor') %>%
                   select(variable) %>%
                   mutate(variable = gsub("_woe","",variable))
col_names <- c(id_names, predictor_names$variable)

# Filter Data
df_pred <-  xg_data %>% select(one_of(col_names))

# WOE Transformation
# Loop Through Each Variable and Create a New WOE Transformed Column

# Define Features to Use
col_vector <- xg_data %>%
              select(one_of(predictor_names$variable)) %>%
              imap_dfr(~ tibble(colname = .y, class = class(.x) %>% str_c(collapse = ", ")))

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
    cut_vector <- as.character(cut(df_pred[[col_vector$colname[i]]], breaks = bin_cuts$max, labels = bin_cuts$id[2:length(bin_cuts$id)]))
    
    # Create New Column with WOE Data (train)
    tmp <- data.frame(df_pred[[col_vector$colname[i]]], cut_vector, stringsAsFactors = FALSE)
    names(tmp) <- c(col_vector$colname[i],"id")
    tmp <- mutate(tmp, id = case_when(is.na(id) ~ "1",
                                      TRUE ~ id)) %>%
            left_join(df, by = c("id")) %>%
            select(WOE) %>%
            mutate(WOE = replace_na(WOE, 0))
    names(tmp) <- c(paste0(col_vector$colname[i],"_woe"))
  } else if (col_vector$class[i] == 'character') {
    df <- woe_table[["Tables"]][[col_vector$colname[i]]]
    tmp <- data.frame(df_pred[,col_vector$colname[i]]) 
    names(tmp) <- c(col_vector$colname[i])
    tmp <- tmp %>%
          left_join(df, by = c(col_vector$colname[i])) %>%
          select(WOE) %>%
          mutate(WOE = replace_na(WOE, 0))
    names(tmp) <- c(paste0(col_vector$colname[i],"_woe"))
  }
  # Bind Onto Existing Dataframe (Train)
  df_pred <- cbind.data.frame(df_pred, tmp)
  
}

# Select Only WOE Columns
df_total <- df_pred %>% select(one_of(id_names),
                               ends_with("woe"))

# Make Predictions and join to data
preds <- cur_model %>% predict(df_total, type = "prob")

df_total <- cbind(xg_data, preds)




