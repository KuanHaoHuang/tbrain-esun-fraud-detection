library(tidyverse)
library(magrittr)
library(lubridate)

source("./src/get_col_types.R")
data_dir <- "./sample_data/"
mid_data_dir <- "./intermediate_data/"

my_cols <- get_col_types(is_test = FALSE)
my_cols$cols$cano <- col_character()
train_dat <- read_csv(str_c(data_dir, "train.csv"), 
                      col_types = my_cols) %>% 
    mutate(dataset = "train",
           loctm_chr = str_pad(loctm %>% as.integer %>% as.character, width = 6, pad = "0"),
           loctm_hh = loctm_chr %>% substr(1, 2) %>% as.integer(),
           loctm_mm = loctm_chr %>% substr(3, 4) %>% as.integer(),
           loctm_ss = loctm_chr %>% substr(5, 6) %>% as.integer())
my_cols$cols$fraud_ind <- NULL
test_dat <- read_csv(str_c(data_dir, "test.csv"), 
                     col_types = my_cols) %>% 
    mutate(dataset = "test",
           loctm_chr = str_pad(loctm %>% as.integer %>% as.character, width = 6, pad = "0"),
           loctm_hh = loctm_chr %>% substr(1, 2) %>% as.integer(),
           loctm_mm = loctm_chr %>% substr(3, 4) %>% as.integer(),
           loctm_ss = loctm_chr %>% substr(5, 6) %>% as.integer())

dat <- bind_rows(train_dat, test_dat)

my_dat <- dat %>% 
    mutate(row_num = row_number()) %>% 
    replace_na(replace = list(fraud_ind = "NA")) %>% 
    select(row_num, 
           bacno, locdt,
           cano, etymd, csmcu, stscd, mchno, ecfg,
           contp, stocn, conam, fraud_ind) %>% 
    arrange(bacno, locdt)

arr_fraud_acc_cano <- vector("integer", nrow(my_dat))
arr_fraud_acc_etymd <- vector("integer", nrow(my_dat))
arr_fraud_acc_csmcu <- vector("integer", nrow(my_dat))
arr_fraud_acc_stscd <- vector("integer", nrow(my_dat))
arr_fraud_acc_mchno <- vector("integer", nrow(my_dat))
arr_fraud_acc_ecfg <- vector("integer", nrow(my_dat))
arr_fraud_acc_contp <- vector("integer", nrow(my_dat))
arr_fraud_acc_stocn <- vector("integer", nrow(my_dat))
arr_fraud_acc_conam <- vector("integer", nrow(my_dat)) # 過去這個金額 (相差 < 50) 發生過 fraud 的次數


t0 <- Sys.time()
progress_bar <- nrow(my_dat) %/% 100 * 3
for (i in 1:nrow(my_dat)) {
    # progress_bar
    if ((i >= progress_bar) && (i %% progress_bar == 0)) {
        print(paste0(
            round(i / nrow(my_dat) * 100),
            "% done"
        ))
        print(Sys.time() - t0)
    }
    
    this_bacno <- my_dat$bacno[i]
    this_locdt <- my_dat$locdt[i]
    this_cano <- my_dat$cano[i]
    this_etymd <- my_dat$etymd[i]
    this_csmcu <- my_dat$csmcu[i]
    this_stscd <- my_dat$stscd[i]
    this_mchno <- my_dat$mchno[i]
    this_ecfg <- my_dat$ecfg[i]
    this_contp <- my_dat$contp[i]
    this_stocn <- my_dat$stocn[i]
    this_conam <- my_dat$conam[i]
    
    # go up
    j <- i - 1
    l <- this_locdt
    while (j > 0) {
        if (my_dat$bacno[j] != this_bacno) break
        if ((this_locdt != my_dat$locdt[j]) && (this_locdt - my_dat$locdt[j] > 1)) { # 非當日，全部歷史紀錄
            arr_fraud_acc_cano[[i]] <- arr_fraud_acc_cano[[i]] + ifelse(my_dat$cano[j] == this_cano && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_etymd[[i]] <- arr_fraud_acc_etymd[[i]] + ifelse(my_dat$etymd[j] == this_etymd && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_csmcu[[i]] <- arr_fraud_acc_csmcu[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_stscd[[i]] <- arr_fraud_acc_stscd[[i]] + ifelse(my_dat$stscd[j] == this_stscd && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_mchno[[i]] <- arr_fraud_acc_mchno[[i]] + ifelse(my_dat$mchno[j] == this_mchno && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_ecfg[[i]] <- arr_fraud_acc_ecfg[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_contp[[i]] <- arr_fraud_acc_contp[[i]] + ifelse(my_dat$contp[j] == this_contp && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_stocn[[i]] <- arr_fraud_acc_stocn[[i]] + ifelse(my_dat$stocn[j] == this_stocn && my_dat$fraud_ind[j] == "1", 1, 0)
            arr_fraud_acc_conam[[i]] <- arr_fraud_acc_conam[[i]] + ifelse(abs(my_dat$conam[j] - this_conam) < 50 && my_dat$fraud_ind[j] == "1", 1, 0)
        }
        j <- j - 1
    }
    
    # do not go down
}

my_dat$warmstart_entry_fraud_acc_cano <- arr_fraud_acc_cano
my_dat$warmstart_entry_fraud_acc_etymd <- arr_fraud_acc_etymd
my_dat$warmstart_entry_fraud_acc_csmcu <- arr_fraud_acc_csmcu
my_dat$warmstart_entry_fraud_acc_stscd <- arr_fraud_acc_stscd
my_dat$warmstart_entry_fraud_acc_mchno <- arr_fraud_acc_mchno
my_dat$warmstart_entry_fraud_acc_ecfg <- arr_fraud_acc_ecfg
my_dat$warmstart_entry_fraud_acc_contp <- arr_fraud_acc_contp
my_dat$warmstart_entry_fraud_acc_stocn <- arr_fraud_acc_stocn
my_dat$warmstart_entry_fraud_acc_conam <- arr_fraud_acc_conam

write_rds(my_dat %>% 
              arrange(row_num) %>% 
              select(row_num, 
                     bacno, locdt, 
                     starts_with("warmstart_entry_")), 
          str_c(mid_data_dir, "warmstart_entry_count.rds"))