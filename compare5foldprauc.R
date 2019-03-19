models_dir <- "shopperml_pr_points"
models <- dir(parent_dir, full.names = T)

df_list <- lapply(models, function(path){
  files <- dir(path, full.names = T, pattern="\\.csv$")
  temp_list <- imap(files, ~read.table(file = .x, stringsAsFactors = F, sep = ",", header = T) %>% select(-X) %>% 
                      mutate(Fold = paste0("Fold ", .y)))
  return(do.call(bind_rows, temp_list))
})