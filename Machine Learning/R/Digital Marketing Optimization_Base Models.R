#------------------------------------------------------------------------------#
# Header ####
#------------------------------------------------------------------------------#
# Robyn Code for Base Models
# Last Modified: 04-20-2025
# Team: Digital Marketing Optimization

#------------------------------------------------------------------------------#
# Load Libraries ####
#------------------------------------------------------------------------------#
library(Robyn)
library(reticulate)
library(dplyr)
library(tidyr)
library(parallel)
library(readxl)
library(skimr)
library(ISOweek)
library(readr)
detectCores()

#------------------------------------------------------------------------------#
# Setup Environment ####
#------------------------------------------------------------------------------#
# Enable multi-core processing 
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

# Use virtual environment
use_virtualenv("r-reticulate", required = TRUE)

# Test installation of nevergrad
# ng <- import("nevergrad")
# print(ng)

# Set up folder structure
create_files <- TRUE
robyn_directory <- "~/Robyn"
path.expand('~')

#------------------------------------------------------------------------------#
# Import Data and Pre-Process ####
#------------------------------------------------------------------------------#
# Import prophet for holidays
data("dt_prophet_holidays")

data <- read_csv("TopNetworks_2023_01_31_2024_06_30.csv",
                 col_types = c("nncccncnnnnnnnnnn"))

### PRE-PREPROCESSING STEPS ###

# Convert categorical columns to lower, replace special characters, 
# replace spaces with underscores,and replace double underscores with single
for (col in names(data)) {
  if(is.character(data[[col]])) {
    data[[col]] <- tolower(data[[col]])
    data[[col]] <- gsub("[^a-z0-9 ]","", data[[col]])
    data[[col]] <- gsub(" ","_", data[[col]])
    data[[col]] <- gsub("__", "_", data[[col]])
  }
}

# Convert 53rd week to 52
data <- data %>%
  mutate(
    week = ifelse(week > 52, 52, week)
  )

# Filter for brand 1
brand_1 <- data %>%
  filter(Context == "brand_1_spend") %>%
  arrange(year, week)

colSums(is.na(brand_1))

# Create new grouping
brand_1 <- brand_1 %>%
  mutate(
    group = paste(supply_type, platform, sep = "_")
  )

n_distinct(brand_1$group)

options(scipen = 999)

brand_1_spend_summary <- brand_1 %>%
  group_by(group) %>%
  summarize(total = sum(spend_usd)) %>%
  arrange(desc(total))

brand_1_spend_summary <- brand_1_spend_summary %>%
  group_by(group) %>%
  summarise(
    total_spend = sum(total, na.rm = TRUE),
    percentage_spend = total_spend / sum(brand_1_spend_summary$total) * 100
  ) %>%
  arrange(desc(percentage_spend)) %>%
  mutate(cumulative_percent = cumsum(percentage_spend))

top_10 <- brand_1_spend_summary %>%
  head(10) %>%
  pull(group)

# Create spend and impression vars
paid_media_spends <- paste0(top_10, "_S")
paid_media_vars <- paste0(top_10, "_I")

# Select spend vars
brand_1_spend_vars <- brand_1 %>%
  group_by(year, week, group) %>%
  summarize(total = sum(spend_usd))

# Select impression vars
brand_1_impression_vars <- brand_1 %>%
  group_by(year, week, group) %>%
  summarize(total = sum(impressions))

# Select value field
brand_1_value <- brand_1 %>%
  group_by(year, week) %>%
  summarize(value_from_conversions = sum(value_from_conversions))

# Extract Competitor Sales (Brand 2 for Brand 1 and vice versa)
brand_2_value <- data %>%
  filter(Context == "brand_2_spend") %>%
  group_by(year, week) %>%
  summarize(brand_2_value = sum(value_from_conversions)) %>%
  arrange(year, week)

# Select conversions field
brand_1_conversions <- brand_1 %>%
  group_by(year, week) %>%
  summarize(conversions = sum(conversions))

sum(brand_1_spend_vars$total) == sum(brand_1$spend_usd)
sum(brand_1_impression_vars$total) == sum(brand_1$impressions)
sum(brand_1_value$value_from_conversions) == sum(brand_1$value_from_conversions)
sum(brand_1_conversions$conversions) == sum(brand_1$conversions)

# Pivot the spend vars
brand_1_pivot_spend <- brand_1_spend_vars %>%
  pivot_wider(
    names_from = group,
    values_from = total
  ) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  rename_with(
    ~ if_else(. %in% c("year", "week"), ., paste0(., "_S"))
  )

# Pivot impressions
brand_1_pivot_impressions <- brand_1_impression_vars %>%
  pivot_wider(
    names_from = group,
    values_from = total
  ) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  rename_with(
    ~ if_else(. %in% c("year", "week"), ., paste0(., "_I"))
  )

# Combine data frames 
data_clean <- brand_1_pivot_spend %>%
  full_join(brand_1_pivot_impressions, by = c("year", "week")) %>%
  full_join(brand_1_conversions, by = c("year", "week")) %>%
  full_join(brand_1_value, by = c("year", "week")) %>%
  left_join(brand_2_value, by = c("year", "week")) %>%
  mutate(date = paste(year, week, sep = "-")) %>%
  ungroup() %>%
  select(-year, -week)

data_clean <- data_clean %>%
  mutate(
    date = as.Date(paste(date, 1, sep = "-"), format = "%Y-%U-%u")
  ) %>%
  select(date, value_from_conversions, conversions, everything())

# Replance NAs with 0s
colSums(is.na(data_clean))

data_clean$brand_2_value[is.na(data_clean$brand_2_value)] <- 0

colSums(is.na(data_clean))

### END PRE-PROCESSING ###

#------------------------------------------------------------------------------#
# Define Model Variables ####
#------------------------------------------------------------------------------#
# Define the inputs 
# Placeholder for now without data
InputCollect <- robyn_inputs(
  dt_input = data_clean,
  date_var = "date",
  dep_var = "conversions", # value_from_conversions or conversions
  dep_var_type = "conversion", # revenue or conversion
  prophet_vars = c("trend", "season", "holiday"),
  prophet_country = "US",
  paid_media_spends = paid_media_spends,
  paid_media_vars = paid_media_vars,
  context_vars = "brand_2_value",
  adstock = "geometric",
)

print(InputCollect)

# Run this to get the names of the hyper parameters to be used in the model
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

# Flip to TRUE to see the plot in R
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

# Run this to check maximum upper and lower bounds by range
hyper_limits()

# List of paid media spend variables
paid_media_spends = paid_media_spends

# Function to generate hyper parameters
generate_hyperparameters <- function(spend_vars) {
  hyperparameters <- list()
  for (var in spend_vars) {
    hyperparameters[[paste0(var, "_alphas")]] <- c(0.5, 3)
    hyperparameters[[paste0(var, "_gammas")]] <- c(0.3, 1)
    hyperparameters[[paste0(var, "_thetas")]] <- c(0.1, 0.4)
  }
  hyperparameters$train_size <- c(0.5, 0.8)
  return(hyperparameters)
}

# Generate the hyper parameters list
hyperparameters <- generate_hyperparameters(paid_media_spends)

# Print the result
print(hyperparameters)

# Add the hyper parameters into the Robyn inputs
InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)

# Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

#------------------------------------------------------------------------------#
# Calibration ####
#------------------------------------------------------------------------------#
# Used for experimentation purposes only -- NOT TO BE USED WITHOUT FURTHER TESTING
# calibration_input <- data.frame(
#   channel = c("website_desktop_S"),  # Channel being calibrated
#   liftStartDate = as.Date("2023-06-01"),  # Start date of calibration period
#   liftEndDate = as.Date("2023-06-30"),  # End date of calibration period
#   liftAbs = c(30600),  # Estimated incremental revenue
#   spend = c(17000),  # Actual spend during the period
#   confidence = c(0.7),  # Confidence in the estimate (scale 0-1)
#   metric = c("conversions"),
#   calibration_scope = c("immediate")
# )
# 
# InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)

#------------------------------------------------------------------------------#
# Build the Models ####
#------------------------------------------------------------------------------#
OutputModels <- robyn_run(
  InputCollect = InputCollect,
  cores = 6, # decrease number of cores for faster processing
  iterations = 4000, # try an increase in iterations (4000)
  trials = 5, # try an increase in trials (7-8)
  ts_validation = TRUE, 
  add_penalty_factor = FALSE
)

print(OutputModels)

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

if (OutputModels$ts_validation) OutputModels$ts_validation_plot

# Reset max dimension for exporting plots if plotting errors
# options(ragg.max_dim = 50000)

# Calculate Pareto fronts, cluster and export results and plots
OutputCollect <- robyn_outputs(
  InputCollect,
  OutputModels,
  pareto_fronts = "auto",
  csv_out = "pareto",
  clusters = TRUE, 
  export = create_files,
  plot_folder = robyn_directory,
  plot_pareto = create_files 
)

print(OutputCollect)

#------------------------------------------------------------------------------#
# Print and Save Best Model ####
#------------------------------------------------------------------------------#
# Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)

# Select best performing model(s) to save specifically
select_model <- c("1_580_1","5_563_6","5_564_5","5_614_5","5_631_2","5_635_4","5_645_4","5_646_6","5_661_1")

# Loop to write multiple models
for (model in select_model) {
  robyn_write(InputCollect, OutputCollect, model, export = create_files)
}

# Save an individual selected model
select_model <- "5_614_5"

ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = TRUE)

# View one-pager plots
myOnePager[[select_model]]$patches$plots[[1]][[1]]
myOnePager[[select_model]]$patches$plots[[1]][[2]]
myOnePager[[select_model]]$patches$plots[[2]][[1]]
myOnePager[[select_model]]$patches$plots[[2]][[2]]
myOnePager[[select_model]]$patches$plots[[3]][[1]]
myOnePager[[select_model]]$patches$plots[[4]][[1]]

#------------------------------------------------------------------------------#
# Budget Allocation ####
#------------------------------------------------------------------------------#
# Set model(s) or use assignment from above
select_model <- c("X_XXX_XX", "X_XXX_XX")

# Output and save multiple models
for (model in select_model) {
  robyn_allocator(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = model,
    date_range = "all",
    total_budget = NULL,
    channel_constr_low = 0.5, # default if blank
    channel_constr_up = 2, # default if blank
    channel_constr_multiplier = 3, # default if blank
    scenario = "max_response",
    export = create_files
  )
}

# Output results of an individual model
select_model <- "5_164_5"

# max_response scenario 
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "all",
  total_budget = NULL,
  channel_constr_low = 0.5, 
  channel_constr_up = 2,
  channel_constr_multiplier = 3,
  scenario = "max_response",
  export = create_files
)

# target_efficiency scenario 
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "all",
  total_budget = NULL,
  channel_constr_low = 0.5, 
  channel_constr_up = 2,
  channel_constr_multiplier = 3,
  scenario = "target_efficiency",
  target_value = 80,
  export = create_files
)

# Print & plot allocator's output
print(AllocatorCollect)
plot(AllocatorCollect)
AllocatorCollect$dt_optimOut

# Print individual allocator plots
AllocatorCollect$plots$p1
AllocatorCollect$plots$p2
AllocatorCollect$plots$p3

#------------------------------------------------------------------------------#
# Calculate Marginal ROAS ####
#------------------------------------------------------------------------------#
# recreate response curve(s)
Response <- robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  select_model = select_model,
  metric_name = "spend_usd_desktop_network_1"
)

Response$plot

ReSpend1 <- 20000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "app_publisher_desktop_S",
  metric_value = Spend1, # total budget for date_range
)
Response1$plot

Spend2 <- Spend1 + 100
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "app_publisher_desktop_S",
  metric_value = Spend2,
)
# ROAS for the 100$ from Spend1 level
(Response2$response_total - Response1$response_total) / (Spend2 - Spend1)

#------------------------------------------------------------------------------#
# Recreate a Saved Model ####
#------------------------------------------------------------------------------#
# Input location of saved model
RobynRecreated <- robyn_recreate(
  json_file = "~/Robyn/Robyn_202503021731_init/RobynModel-2_1987_1.json",
  dt_input = final_data, # Make sure to switch to correct data set!
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)

InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

print(OutputCollectX)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX,
                               OutputCollectX,
                               export = create_files,
                               plot_folder = "~/Robyn/")

# Print one-pager plots
myModelPlot[[1]][[1]][[1]]
myModelPlot[[1]][[2]][[1]]
myModelPlot[[1]][[2]][[2]]
myModelPlot[[1]][[4]][[1]]

select_model <- "2_1987_1"

# Re-run Budget Allocator
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  select_model = select_model,
  date_range = "all",
  total_budget = NULL,
  channel_constr_low = 0.5,
  channel_constr_up = 2,
  channel_constr_multiplier = 3,
  scenario = "target_efficiency",
  target_value = 80,
  export = create_files,
  plot_folder = "~/Robyn/"
)

print(AllocatorCollect)

# Print individual allocator plots
AllocatorCollect$plots$p1
AllocatorCollect$plots$p2
AllocatorCollect$plots$p3

#------------------------------------------------------------------------------#
# Refresh a Saved Model ####
#------------------------------------------------------------------------------#
# Import saved model (update path and file name of model)
json_file <- "~/Robyn/Robyn_202412221914_init/RobynModel-1_75_5.json"

# Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Run refresh with new data
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = data_clean, # update with new data
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 13,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)
