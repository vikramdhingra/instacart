set.seed(100)  #Setting Seed to 100
rm(list = ls())  # Removing all current Object
x <- c('data.table','dplyr','caret') 
lapply(x,require,character.only=TRUE) # loading packages
memory.size(max = TRUE) # Setting memory to max
options(scipen = 99,digits = 10) 
df <- readRDS('E://MMA2020//8 MMA 831 Marketing Analytics//Final_Project//user_product_metrics3.rds')

# Data Cleaning
# unique(df$eval_set)
# dplyr::n_distinct(df$user_id)
df$up_mean_days_since_last_order[is.na(df$up_mean_days_since_last_order)] <- 999
df$days_since_last_order[is.na(df$days_since_last_order)] <- 999

# Training Data Cleaning
insta_df <- df %>%
  filter(eval_set == "train") %>% 
  as.data.frame() %>%
  select(-c(eval_set, user_id, product_id, order_id))

insta_df$reordered[is.na(insta_df$reordered)] <- 0
insta_df$reordered <- as.factor(insta_df$reordered)
# head(insta_df)

# Testing Data Cleaning
test <- df %>%
  filter(eval_set == "test") %>%
  as.data.frame() %>%
  select(-c(eval_set, user_id, reordered))

# Sampling
# Sample 50% of the insta_df data to work with 
subtrain <- insta_df %>% sample_frac(0.5)
# subtrain <- insta_df
set.seed(1000) #set a random number generation seed to ensure that the split is the same everytime
# inTrain <- createDataPartition(y = subtrain$reordered,
#                                p = 0.8, list = FALSE)
# train <- subtrain[ inTrain,]
# valid <- subtrain[ -inTrain,]
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

insta_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                       validation_frame = valid,alpha = 0,nfolds = 3,lambda_search = T,seed = 1,
                       family = 'binomial',link='logit'
                       #,lambda_search = TRUE
                       # ,interaction_pairs =list(
                       # c("MSSubClass","MSZoning","SaleCondition","OverallCond") )
                       # ,interactions = interact_list
                       # ignore_const_cols = FALSE
)


insta_glm
## Checking AUC
print(h2o.auc(insta_glm))
print(h2o.auc(insta_glm, valid = TRUE))
# Checking Accuracy on the validation frame
h2o.confusionMatrix(insta_glm)
h2o.confusionMatrix(insta_glm,valid=T)
h2o.confusionMatrix(insta_glm,valid=T,thresholds=0.2)
# h2o.confusionMatrix(mlcup_glm,thresholds=0.22,valid=T)
# Plotting the coefficients of variables
h2o.std_coef_plot(insta_glm,num_of_features=50)
h2o.varimp_plot(insta_glm,num_of_features=50)
x <- h2o.varimp(insta_glm)

h2o.performance(insta_glm)



pred <- as.data.frame(h2o.predict(insta_glm, as.h2o(as.data.frame(test))))
submit <- data.frame('Id'=df_test$Id,'Predicted'=pred$predict,'Po'=pred$p1) 
write.csv(submit,"C:/Users/Vikram Dhingra/Desktop/p1.csv",row.names = F)

####################################################
library(dplyr)
print(paste(Sys.time()," - - - - -5 "))
test$reordered = predict(lgb.model, data = as.matrix(test %>% select(-order_id, -product_id)) , n = lgb.model$best_iter)
print(paste(Sys.time()," - - - - -6 "))
pred <- as.data.frame(h2o.predict(insta_glm, as.h2o(as.data.frame(test))))
test$reordered <- ifelse(pred$p0 > 0.81,0,1)
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
write.csv(submission, file = "C:/Users/Vikram Dhingra/Desktop/submit12.csv", row.names = F)
