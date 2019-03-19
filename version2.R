library(tidyverse)
library(foreach)
library(caret)



# raw data
## if Raphey send's a .zst file decompress in temrinal with: unzstd filename
raw_data <- read.table(file = "gc_task_completion_20181026_augmented_single_website.tsv",
                       sep = '\t', header = TRUE, stringsAsFactors = F)

## tidyup long names
corrected_names <- str_replace_all(names(raw_data), "gc_task_completion.", "")
names(raw_data) <- corrected_names



# Pre processing
pdata <- raw_data %>% mutate_at(vars(c(npages:nwidgets, 
                                       add_subscribe:add_membership, 
                                       days2convert, days2publish:gemsend_flg)),
                                funs(str_replace(., "NULL", NA_character_) %>% as.numeric)) # %>% 
  
# actually Raphey said to include these after all
  #select(-c(custom_domain_task_completed:ols_task_completed)) # these are multicolinear with the new target vars

## replace NULLs with NA to pass to preprocess in caret
pdata[pdata == "NULL"] <- NA

## hot encode categoricals
## no need to separate categoricals from numerics, dummyVars takes care of that
training_data <- select(pdata, -c(shopper_id:orion_id, user_provided_vertical)) # user_provided_vertical has too many levels to 1, would need word embeddings
cats <- names(training_data[sapply(training_data, is.character)])

## replace low frequency levels with "Other"
## works but spits out to console for some reason
foreach(c = cats) %do% {
  print(c)
  freqs <- table(training_data[c])
  low_freqs <- names(which(freqs < 20000))
  training_data[c][[1]] <- ifelse(training_data[c][[1]] %in% low_freqs, "Other", training_data[c][[1]])
  return(NULL) # otherwise spits out the whole thing
}

dummy <- caret::dummyVars(~ ., data = training_data, fullRank = TRUE, sep = ".")
training_data <- predict(dummy, training_data) %>% as.data.frame()

## create separatley preprocessed data frames for training
median_training_data <- predict(preProcess(training_data, method = c("medianImpute")), training_data)

## scale everything to be between 0 and 1 (for using varImp() function after fitting)
### non binaries
non_binaries <- c("npages", "nwidgets", "days2convert", "website_age", "days_since_last_active", "number_publishes",
                  "number_visitors", "number_bookings", "number_orders", "days2publish", "days2activate" )
stdize = function(x) {(x - min(x)) / (max(x) - min(x))}
median_training_data <- median_training_data %>% mutate_at(non_binaries, stdize)

## save training data and remove from RAM
rm(list = setdiff(ls(), "median_training_data"))

## get list of target vars
# targets <- select(median_training_data, matches("task_completed_delta")) %>% names()   
targets <- readRDS("targets.rds")


# training and cross validation
## train control
set.seed(123)

## custom evaluation metric function
my_summary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

# train control
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  verboseIter = TRUE,
  summaryFunction = my_summary,
  savePredictions = TRUE,
  allowParallel = TRUE
)

## tuning grid
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

## xgb
XGBs <- foreach(i = targets) %do% {
  target <- median_training_data[i][[1]] %>% make.names() %>% factor(levels = c("X1", "X0")) # found out the hard way that the true class must be the first level
  print(table(target)) # to see how "X1" and "X0" are distributed, expectation that "X1" is the true class with smaller frequency than "X0"
  xgb <- train(x = select(median_training_data, -matches("task_completed_delta")), 
               y = target,
               method = "xgbTree",
               metric = "AUC", # This is actually prAUC, ROC for regular AUC
               trControl = train_control,
               tuneGrid = tune_grid,
               tuneLength = 10)
  saveRDS(xgb, file = paste0(i, ".rds"))
  print(xgb)
  return(1) # make loop not store in memory
}

## recombine rds files
rm(median_training_data) # clear up ram
xgbs_done <- foreach(i = targets) %do% {
  readRDS(paste0(i, ".rds"))
}
names(xgbs_done) <- targets

## Results and comparison between initial algorithms
results <- resamples(xgbs_done)
summary(results)
dotplot(results)
lapply(xgbs_done, varImp)

## prAUC curve
library(yardstick)
pr_curves_data <- map(xgbs_done, function(x) {
  # gets the data for autoplot
  xgb_pr_curve <- x$pred %>%
    group_by(Resample) %>%
    pr_curve(obs, X1) %>%
    na.omit()
  })

## save thresholds to csvs for Raphey
imap(pr_curves_data, ~write.csv(x = .x,file = paste0(.y, ".csv"), row.names = F))