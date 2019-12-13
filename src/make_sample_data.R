library(tidyverse)

## 說明 sample data 的來歷，與實際分析過程完全無關

data_dir <- "C:/Users/user_name/Downloads/dataset/tbrain_2019fall/" ## original data
save_dir <- "./sample_data/"

subm <- read_csv(str_c(data_dir, "submission_test.csv"),
                 col_types = cols(txkey = col_character()))
train_dat <- read_csv(str_c(data_dir, "train.csv"), 
                      col_types = cols(
                          bacno = col_character(),
                          txkey = col_character(),
                          locdt = col_integer(),
                          loctm = col_character(),
                          cano = col_character(),
                          contp = col_character(),
                          etymd = col_character(),
                          mchno = col_character(),
                          acqic = col_character(),
                          mcc = col_character(),
                          conam = col_double(),
                          ecfg = col_character(),
                          insfg = col_character(),
                          iterm = col_integer(),
                          stocn = col_character(),
                          scity = col_character(),
                          stscd = col_character(),
                          ovrlt = col_character(),
                          flbmk = col_character(),
                          hcefg = col_character(),
                          csmcu = col_character(),
                          flg_3dsmk = col_character(),
                          fraud_ind = col_character()
                      )) %>% 
    mutate(dataset = "train")

test_dat <- read_csv(str_c(data_dir, "test.csv"), 
                     col_types = cols(
                         bacno = col_character(),
                         txkey = col_character(),
                         locdt = col_integer(),
                         loctm = col_character(),
                         cano = col_character(),
                         contp = col_character(),
                         etymd = col_character(),
                         mchno = col_character(),
                         acqic = col_character(),
                         mcc = col_character(),
                         conam = col_double(),
                         ecfg = col_character(),
                         insfg = col_character(),
                         iterm = col_integer(),
                         stocn = col_character(),
                         scity = col_character(),
                         stscd = col_character(),
                         ovrlt = col_character(),
                         flbmk = col_character(),
                         hcefg = col_character(),
                         csmcu = col_character(),
                         flg_3dsmk = col_character()
                     )) %>% mutate(dataset = "test")

dat <- bind_rows(train_dat, test_dat)

## 純 train 抽 100 人, 純 test 抽 100 人, warmstart 抽 50 人
## 篩選刷卡次數超過一次的人
# dat %>% 
#     group_by(bacno) %>% 
#     summarise(dataset_count = n_distinct(dataset))
d <- dat %>% 
    left_join((dat %>% 
                   group_by(bacno) %>% 
                   summarise(dataset_count = n_distinct(dataset),
                             all_row_count = n())),
              by = "bacno") %>% 
    select(bacno, dataset, dataset_count, all_row_count) %>% 
    filter(all_row_count >= 2)

set.seed(42)
bacno_overlap <- d %>% 
    filter(dataset_count == 2) %>% 
    pull(bacno) %>% 
    unique %>% 
    sample(60)
bacno_train <- d %>% 
    filter(dataset_count == 1,
           dataset == "train") %>% 
    pull(bacno) %>% 
    unique %>% 
    sample(150)
bacno_test <- d %>% 
    filter(dataset_count == 1,
           dataset == "test") %>% 
    pull(bacno) %>% 
    unique %>% 
    sample(150)

sample_train_dat <- train_dat %>% 
    filter(bacno %in% union(bacno_overlap, bacno_train)) %>% 
    select(-dataset)
sample_test_dat <- test_dat %>% 
    filter(bacno %in% union(bacno_overlap, bacno_test)) %>% 
    select(-dataset) %>% 
    arrange(txkey)
sample_subm <- subm %>% 
    filter(txkey %in% sample_test_dat$txkey) %>% 
    arrange(txkey)

write_csv(sample_train_dat, str_c(save_dir, "train.csv"))
write_csv(sample_test_dat, str_c(save_dir, "test.csv"))
write_csv(sample_subm, str_c(save_dir, "submission_test.csv"))
