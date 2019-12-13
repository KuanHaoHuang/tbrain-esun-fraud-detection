library(mlr)
library(tidyverse)

# Load Data ---------------------------------------------------------------

USE_PCA <- FALSE
processed_data_dir <- "./processed_data/"

rm(list = ls() %>% setdiff(c("USE_PCA", "processed_data_dir")))
warmstart_ind <- read_rds(str_c(processed_data_dir, "warmstart_split.rds"))
data_list <- read_rds(str_c(processed_data_dir, "processed_data.rds"))
train_X <- data_list$train_X
train_y <- data_list$train_y
train_X <- train_X %>% bind_cols(data_list$train_warmstart_entry_X)
test_X <- data_list$test_X
test_X <- test_X %>% bind_cols(data_list$test_warmstart_entry_X)
if (USE_PCA) {
    train_X <- train_X %>% bind_cols(data_list$train_pca_X)
    test_X <- test_X %>% bind_cols(data_list$test_pca_X)
} else {
    train_X <- train_X %>% bind_cols(data_list$train_entry_X)
    test_X <- test_X %>% bind_cols(data_list$test_entry_X)
}
train_X$dataset <- "null"
train_X$dataset[warmstart_ind$train_ind] <- "train"
train_X$dataset[warmstart_ind$valid_ind] <- "valid"
train_y <- train_y[which(train_X$dataset != "null")]
train_X <- train_X[which(train_X$dataset != "null"), ]
valid_ind <- which(train_X$dataset == "valid")
train_ind <- 1:nrow(train_X) %>% setdiff(valid_ind)
train_X$dataset <- NULL
test_X <- test_X[warmstart_ind$test_ind, ]
rm(data_list)

# Config ------------------------------------------------------------------

set.seed(42)
FEAT_SEL <- TRUE
TUNING <-  TRUE

result_dir <- "./result/"
source("./src/get_learner.R")

# Feature Selection -------------------------------------------------------

if (FEAT_SEL) {
    lrn <- get_learner("xgb")
    rm_feats <- list()
    perf_track <- list()
    
    const_feats <- c()
    ts_train_ind <- sample(train_ind, 
                           size = floor(length(train_ind) * 0.5),
                           replace = FALSE)
    for (c in colnames(train_X)) {
        if (max(train_X[ts_train_ind, c]) == min(train_X[ts_train_ind, c])) {
            if (c == "fraud_ind") next
            print(c)
            const_feats <- union(const_feats, c)
        }
    }
    ts_train_task <- makeClassifTask(data = train_X[ts_train_ind, ] %>% 
                                         mutate(fraud_ind = train_y[ts_train_ind]) %>% 
                                         select(-one_of(const_feats)) %>% 
                                         as.data.frame(), 
                                     target = "fraud_ind", 
                                     positive = "1",
                                     id = "my_train_task")
    ts_valid_task <- makeClassifTask(data = train_X[valid_ind, ] %>% 
                                         mutate(fraud_ind = train_y[valid_ind]) %>% 
                                         as.data.frame() %>% 
                                         select(-one_of(const_feats)), 
                                     target = "fraud_ind", 
                                     positive = "1",
                                     id = "my_valid_task")
    for (i in 1:5) {
        print(i)
        t0 <- Sys.time()
        if (i > 1) {
            const_feats <- c()
            ts_train_ind <- sample(train_ind, 
                                   size = floor(length(train_ind) * 0.5),
                                   replace = FALSE)
            for (colm in colnames(train_X)) {
                if (max(train_X[ts_train_ind, colm]) == min(train_X[ts_train_ind, colm])) {
                    if (colm == "fraud_ind") next
                    print(colm)
                    const_feats <- union(const_feats, colm)
                }
            }
            if (is.null(const_feats)) {const_feats <- c(" ")}
            rm(ts_train_task, ts_valid_task)
            ts_train_task <- makeClassifTask(data = train_X[ts_train_ind, ] %>% 
                                                 mutate(fraud_ind = train_y[ts_train_ind]) %>% 
                                                 select(-one_of(unlist(rm_feats)),
                                                        -one_of(const_feats)) %>% 
                                                 as.data.frame(), 
                                             target = "fraud_ind", 
                                             positive = "1",
                                             id = "my_train_task")
            ts_valid_task <- makeClassifTask(data = train_X[valid_ind, ] %>% 
                                                 mutate(fraud_ind = train_y[valid_ind]) %>% 
                                                 select(-one_of(unlist(rm_feats)),
                                                        -one_of(const_feats)) %>% 
                                                 as.data.frame(), 
                                             target = "fraud_ind", 
                                             positive = "1",
                                             id = "my_valid_task")
        }
        ts_mod <- train(lrn, ts_train_task)
        valid_pred <- predict(ts_mod, task = ts_valid_task)
        valid_score <- performance(valid_pred, measures = list(f1))
        perf_track[[i]] <- valid_score
        imp <- getFeatureImportance(ts_mod)
        imp <- tibble(feat = names(imp$res), value = (imp$res %>% t)[, 1])
        tmp_rm_feats <- imp %>% arrange(desc(value)) %>% tail(round(nrow(imp) * 0.3)) %>% pull(feat)
        rm_feats[[i]] <- tmp_rm_feats %>% union(const_feats)
        print(Sys.time() - t0)
    }
    write_rds(list(rm_feats = rm_feats, valid_f1 = perf_track), str_c(processed_data_dir, "warmstart_rm_feats.rds"))
}

rm_feats <- read_rds(str_c(processed_data_dir, "warmstart_rm_feats.rds"))
all_rm_feats <- unlist(rm_feats$rm_feats) %>% setdiff(" ")
train_task <- makeClassifTask(data = train_X %>% mutate(fraud_ind = train_y) %>% data.frame(), 
                              target = "fraud_ind", 
                              positive = "1")
train_task <- dropFeatures(train_task, all_rm_feats)

# Tuning ------------------------------------------------------------------

tune_result_file_name <- str_c("warmstart_xgb",
                               ifelse(USE_PCA, "_PCA", ""),
                               "_tune_result.rds")
if (TUNING || !(tune_result_file_name %in% list.files(result_dir))) {
    lrn <- get_learner("xgb")
    ps <- makeParamSet(
        makeIntegerParam("nrounds", lower = 250, upper = 700),
        makeNumericParam("eta", lower = 0.05, upper = 0.35),
        makeNumericParam("gamma", lower = 0, upper = 4),
        makeNumericParam("scale_pos_weight", lower = 1, upper = 20),
        makeIntegerParam("max_depth", lower = 2, upper = 10),
        makeIntegerParam("min_child_weight", lower = 1, upper = 10)
    )
    ctrl <- makeTuneControlRandom(maxit = 8L)
    rdesc <- makeFixedHoldoutInstance(
        train.inds = train_ind, 
        test.inds = valid_ind,
        size = getTaskSize(train_task)
    )
    msr_list <- list("f1" = f1, "auc" = auc, "acc" = acc)
    tune_result <- tuneParams(lrn, 
                              task = train_task, 
                              resampling = rdesc,
                              par.set = ps, 
                              control = ctrl,
                              measure = msr_list)
    saveRDS(tune_result, str_c(result_dir, tune_result_file_name))
} else {
    tune_result <- readRDS(str_c(result_dir, tune_result_file_name))
}

as.data.frame(tune_result$opt.path)
plotOptPath(tune_result$opt.path)

# Train Model -------------------------------------------------------------

lrn <- get_learner("xgb")
lrn <- setHyperPars(lrn, par.vals = tune_result$x)
mod <- train(lrn, train_task)
# write_rds(mod, paste0("warmstart_trained_xgb",
#                       ifelse(USE_PCA, "_PCA", ""),
#                       "_model.rds"))
print("single warmstart xgb model finished training.")

## Feature Importance
imp <- getFeatureImportance(mod)
imp <- imp$res %>% sort(decreasing = T) %>% head(30) %>% {tibble(x = colnames(.), y = t(.)[, 1])}
imp %>% 
    ggplot(aes(reorder(x, y), y)) + 
    geom_bar(stat = "identity") +
    coord_flip()

## Have a look at the results on training_data
pred_train <- predict(mod, 
                      task = train_task)
data.frame(pred = pred_train$data$response, truth = train_y) %>%
    table

pred_test <- predict(mod, 
                     newdata = test_X %>% 
                         mutate(fraud_ind = "0") %>% 
                         select(-one_of(all_rm_feats)) %>% 
                         as.data.frame)

# Submission --------------------------------------------------------------

subm <- read_csv(str_c(result_dir, "my_xgb",
                       ifelse(USE_PCA, "_PCA", ""),
                       "_submission.csv"),
                 col_types = cols(txkey = col_character()))
warmstart_pred <- (pred_test$data %>% pull(response)) %>% as.character %>% as.integer()

## How the warmstart model makes the original coldstart model different?
print(table(warmstart_pred, subm$fraud_ind[warmstart_ind$test_ind]))

subm$fraud_ind[warmstart_ind$test_ind] <- warmstart_pred
write_csv(subm, str_c(result_dir, "my_warmstart_xgb",
                      ifelse(USE_PCA, "_PCA", ""),
                      "_submission.csv"))