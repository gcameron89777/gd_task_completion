parent_dir <- "/home/rstudio/Raphey"

# get shopperml models into right format for plotting
models_dir <- "shopperml_pr_points"
models <- dir(parent_dir, full.names = T) %>% setdiff("shopperml_pr_points/holdout")
rapheys_df_list <- map(models, function(path){
  files <- dir(path, full.names = T, pattern="\\.csv$")
  temp_list <- imap(files, ~read.table(file = .x, stringsAsFactors = F, sep = ",", header = T) %>% select(-X) %>% 
                      mutate(resample = paste0("Fold ", .y),
                             model = "ShopperML") %>% 
                      select(resample, recall, precision, model))
  return(do.call(bind_rows, temp_list))
})
names(rapheys_df_list) <- (dir(models_dir) %>% setdiff("holdout"))


# now xgb models
XGB_models_Prep <- map(pr_curves_data, ~select(.x, -.threshold) %>% 
                    mutate(Model = "XGB") %>% 
                    rename(resample = Resample,
                           model = Model) %>% 
                    na.omit())
clean_names <- str_replace_all(names(XGB_models_Prep), "_task_completed_delta", "")
names(XGB_models_Prep) <- clean_names

# sort your list into same order
XGB_models_Prep <- XGB_models_Prep[names(rapheys_df_list)]


# join together
combined <- map2(rapheys_df_list, XGB_models_Prep, bind_rows)


# make and save the plots
prauc_plots <- imap(.x = combined, .f = function(.x, .y) {
  plot <- ggplot(.x, aes(x = recall, y = precision, group = resample, color = model)) + geom_line() + ggtitle(.y)
  ggsave(plot = plot, filename = paste0(.y, ".png"), device = "png", path = "plots")
})