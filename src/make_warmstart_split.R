library(tidyverse)

data_dir <- "./sample_data/"
processed_data_dir <- "./processed_data/"

train_dat <- read_csv(str_c(data_dir, "train.csv")) %>% 
    mutate(dataset = "train", row_num = row_number())
test_dat <- read_csv(str_c(data_dir, "test.csv")) %>% 
    mutate(dataset = "test", row_num = row_number())
dat <- bind_rows(train_dat, test_dat)

# Test Ind ----------------------------------------------------------------
## x 只選擇 train / test 都有數據 且 (train 內數據 >= 3 或 曾有任何 fraud 紀錄) 
## v 只選擇 train / test 都有數據 且 (train 內數據 >= 4)

warmstart_bacno <- dat %>% 
    left_join((dat %>% 
                   group_by(bacno) %>% 
                   summarise(dataset_count = n_distinct(dataset))),
              by = "bacno") %>% 
    filter(dataset_count == 2) %>% 
    pull(bacno) %>% 
    unique()

warmstart_dat <- dat %>% 
    filter(bacno %in% warmstart_bacno)

test_id <- warmstart_dat %>% 
    group_by(bacno) %>% 
    summarize(prop_train = round(sum(dataset == "train") / n(), 4),
              prop_test = round(sum(dataset == "test") / n(), 4),
              fraud_count = sum(fraud_ind == 1),
              count_train = sum(dataset == "train"),
              count_test = sum(dataset == "test"),
              n = n()) %>% 
    ungroup() %>% 
    filter(count_train > 3) %>% 
    pull(bacno)

test_ind <- warmstart_dat %>% 
    filter(dataset == "test", 
           bacno %in% test_id) %>% 
    pull(row_num) %>% 
    sort()


# Train / Valid Ind -------------------------------------------------------
## training 只選擇數據長度 >= 5 的 bacno
## 且保留 max(round(#entry * 0.2), 1) 長度、最大 locdt 的幾筆資料，作為 valid set

train_id <- train_dat %>% 
    group_by(bacno) %>%
    count() %>%
    ungroup() %>% 
    filter(n >= 5) %>% 
    pull(bacno)

train_dat_sub <- train_dat %>% 
    filter(bacno %in% train_id) %>% 
    select(bacno, locdt, row_num)

train_dat_sub <- train_dat_sub %>% 
    mutate(dataset = "train") %>% 
    group_by(bacno) %>% 
    nest() %>% 
    mutate(data = map(data, function(df) {
        val_sz <- max(round(nrow(df) * 0.2), 1)
        tmp <- df %>% mutate(row_num_sub = row_number()) %>% arrange(locdt)
        df$dataset[tmp$row_num_sub[(nrow(tmp) - val_sz + 1):nrow(tmp)]] <- "valid"
        return(df)
    })) %>% 
    unnest() %>% 
    ungroup() %>% 
    arrange(bacno, locdt)

train_ind <- train_dat_sub %>% filter(dataset == "train") %>% pull(row_num) %>% sort()
valid_ind <- train_dat_sub %>% filter(dataset == "valid") %>% pull(row_num) %>% sort()


write_rds(list(train_ind = train_ind, valid_ind = valid_ind, test_ind = test_ind),
          str_c(processed_data_dir, "warmstart_split.rds"))