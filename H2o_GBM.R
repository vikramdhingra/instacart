rm(list = ls())  # Removing all current Object
x <- c('data.table','dplyr','caret') 
lapply(x,require,character.only=TRUE) # loading packages
memory.size(max = TRUE) # Setting memory to max
options(scipen = 99,digits = 10) 
df <- readRDS('E://MMA2020//8 MMA 831 Marketing Analytics//Final_Project//user_product_metrics3.rds')

# Data Cleaning
df$up_mean_days_since_last_order[is.na(df$up_mean_days_since_last_order)] <- 999
df$days_since_last_order[is.na(df$days_since_last_order)] <- 999


# test_1 <- df %>% dplyr::filter(order_id==17)

# Training Data Cleaning
insta_df <- df %>%
  filter(eval_set == "train") %>% 
  as.data.frame() %>%
  select(-c(eval_set, user_id, product_id, order_id))

insta_df$reordered[is.na(insta_df$reordered)] <- 0
insta_df$reordered <- as.factor(insta_df$reordered)

# Testing Data Cleaning
test <- df %>%
  filter(eval_set == "test") %>%
  as.data.frame() %>%
  select(-c(eval_set, reordered))

# Sampling
# Sample 50% of the insta_df data to work with 
# subtrain <- insta_df %>% sample_frac(0.1)
subtrain <- insta_df
set.seed(1000) #set a random number generation seed to ensure that the split is the same everytime
# 

# Detaching the H2O for CLEAN LOAD
detach("package:h2o", unload = TRUE)
library(h2o)
# Initiallizing H2O with 2 threads and 10 gigs of memory
h2o.shutdown()
h2o.init(nthreads = 2,max_mem_size = "10g")

# Loading data frame to H2o frame
df_train_h20 <- as.h2o(as.data.frame(subtrain))
rm(df,subtrain)
gc()
# df_test_h20 <- as.h2o(as.data.frame(test))

# Spliting the the dataset in 80 TRAIN - 20 VALIDATION
df.splits <- h2o.splitFrame(data =  df_train_h20, ratios = .8)
train <- df.splits[[1]]
valid <- df.splits[[2]]

# Making list of all the predictors
colnames(df_train_h20)
predictors <- colnames(df_train_h20)[1:44]
predictors
# sort(predictors)

# Setting Y variable
response <- "reordered"


# interact_list <- c("SEX","MARRIAGE")
insta_gbm <- h2o.gbm(x = predictors, y = response, training_frame = train,
                     validation_frame = valid,seed = 1,nfolds=3,distribution = 'multinomial',
                     max_depth = 7
                     )
                     #ntrees = 10000, learn_rate = 0.001,
                     #stopping_rounds = 5,stopping_tolerance = 1e-4,stopping_metric = "AUC")
# ,interaction_pairs =list(
# c("SEX","MARRIAGE") )
# ,interactions = interact_list
# ignore_const_cols = FALSE
# insta_gbm <- readRDS("E:/MMA2020/8 MMA 831 Marketing Analytics/Final_Project/kaggle_submissions/insta_GBM.rds")
# insta_gbm
h2o.saveModel(insta_gbm,"E:/MMA2020/8 MMA 831 Marketing Analytics/Final_Project/kaggle_submissions/")
# insta_gbm <- h2o.loadModel(insta_gbm)

## Checking AUC
print(h2o.auc(insta_gbm))
print(h2o.auc(insta_gbm, valid = TRUE))


h20_test <- as.h2o(as.data.frame(test))
pred <- as.data.frame(h2o.predict(insta_gbm, h20_test))

test$reordered <- ifelse(pred$p0 > 0.80,0,1)

print(paste(Sys.time()," - - - - -7 "))
submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )
print(paste(Sys.time()," - - - - -8 "))
missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)
print(paste(Sys.time()," - - - - -9 "))
submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
print(paste(Sys.time()," - - - - -10 "))
write.csv(submission, file = "C:/Users/Vikram Dhingra/Desktop/submit22.csv", row.names = F)
write.csv(trevor_dta, file = "C:/Users/Vikram Dhingra/Desktop/top_100_order.csv", row.names = F)

saveRDS(insta_gbm,"C:/Users/Vikram Dhingra/Desktop/insta_GBM.rds")
