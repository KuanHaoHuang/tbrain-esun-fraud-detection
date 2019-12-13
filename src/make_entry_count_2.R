library(tidyverse)

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

# 計算 entry count
# (除去今天) 前後 30 天出現過相同, 前後第 4 ~ 30 天出現過相同, 歷史曾經出現過相同
#     cano, etymd, csmcu, acqic, stscd, mchno, mcc, ecfg
# 例如: cano 過去出現過，未來不再出現 -> 因為 fraud 而剪卡
my_dat <- dat %>% 
    mutate(row_num = row_number()) %>% 
    select(row_num, 
           bacno, locdt, loctm_hh, loctm_mm, loctm_ss, 
           cano, csmcu, stscd, mchno, ecfg,
           stocn, conam) %>% 
    arrange(bacno, locdt, loctm_hh, loctm_mm, loctm_ss)

arr_same_cano_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_cano_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_csmcu_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_csmcu_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_stscd_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_stscd_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_mchno_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_mchno_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_ecfg_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_ecfg_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_stocn_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_stocn_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_conam_lt_1_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_conam_lt_1_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_conam_delta_lt_50_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_conam_delta_lt_50_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_loctm_hh_from_4_to_30 <- vector("integer", nrow(my_dat))
arr_same_loctm_hh_from_0_to_3 <- vector("integer", nrow(my_dat))
arr_same_cano_in_1hr <- vector("integer", nrow(my_dat))
arr_same_csmcu_in_1hr <- vector("integer", nrow(my_dat))
arr_same_stscd_in_1hr <- vector("integer", nrow(my_dat))
arr_same_mchno_in_1hr <- vector("integer", nrow(my_dat))
arr_same_ecfg_in_1hr <- vector("integer", nrow(my_dat))
arr_same_stocn_in_1hr <- vector("integer", nrow(my_dat))
arr_same_cano_in_3mins <- vector("integer", nrow(my_dat))
arr_same_csmcu_in_3mins <- vector("integer", nrow(my_dat))
arr_same_stscd_in_3mins <- vector("integer", nrow(my_dat))
arr_same_mchno_in_3mins <- vector("integer", nrow(my_dat))
arr_same_ecfg_in_3mins <- vector("integer", nrow(my_dat))
arr_same_stocn_in_3mins <- vector("integer", nrow(my_dat))
arr_last_conam_diff_lt_50 <- vector("integer", nrow(my_dat))
arr_last_conam_is_0 <- vector("integer", nrow(my_dat))
arr_is_max_in_7days <- vector("integer", nrow(my_dat))
arr_is_min_in_7days <- vector("integer", nrow(my_dat))
arr_acc_cano <- vector("integer", nrow(my_dat))
arr_acc_csmcu <- vector("integer", nrow(my_dat))
arr_acc_stscd <- vector("integer", nrow(my_dat))
arr_acc_mchno <- vector("integer", nrow(my_dat))
arr_acc_ecfg <- vector("integer", nrow(my_dat))
arr_acc_stocn <- vector("integer", nrow(my_dat))
arr_acc_loctm_hh <- vector("integer", nrow(my_dat))

arr_entry_diff <- vector("integer", nrow(my_dat))
arr_entry_cano_diff <- vector("integer", nrow(my_dat))
arr_entry_csmcu_diff <- vector("integer", nrow(my_dat))
arr_entry_stscd_diff <- vector("integer", nrow(my_dat))
arr_entry_mchno_diff <- vector("integer", nrow(my_dat))
arr_entry_ecfg_diff <- vector("integer", nrow(my_dat))
arr_entry_stocn_diff <- vector("integer", nrow(my_dat))
arr_conam_diff <- vector("double", nrow(my_dat))
arr_conam_cano_diff <- vector("double", nrow(my_dat))
arr_conam_csmcu_diff <- vector("double", nrow(my_dat))
arr_conam_stscd_diff <- vector("double", nrow(my_dat))
arr_conam_mchno_diff <- vector("double", nrow(my_dat))
arr_conam_ecfg_diff <- vector("double", nrow(my_dat))
arr_conam_stocn_diff <- vector("double", nrow(my_dat))
arr_conam_acc_diff <- vector("double", nrow(my_dat))
arr_conam_cano_acc_diff <- vector("double", nrow(my_dat))
arr_conam_csmcu_acc_diff <- vector("double", nrow(my_dat))
arr_conam_stscd_acc_diff <- vector("double", nrow(my_dat))
arr_conam_mchno_acc_diff <- vector("double", nrow(my_dat))
arr_conam_ecfg_acc_diff <- vector("double", nrow(my_dat))
arr_conam_stocn_acc_diff <- vector("double", nrow(my_dat))

t0 <- Sys.time()
progress_bar <- nrow(my_dat) %/% 100 * 3
for (i in 1:nrow(my_dat)) {
    # progress bar
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
    this_csmcu <- my_dat$csmcu[i]
    this_stscd <- my_dat$stscd[i]
    this_mchno <- my_dat$mchno[i]
    this_ecfg <- my_dat$ecfg[i]
    this_stocn <- my_dat$stocn[i]
    this_conam <- my_dat$conam[i]
    this_loctm_hh <- my_dat$loctm_hh[i]
    this_loctm_mm <- my_dat$loctm_mm[i]
    
    arr_is_max_in_7days[[i]] <- 1
    arr_is_min_in_7days[[i]] <- 1
    this_conam_max <- this_conam
    this_conam_min <- this_conam
    
    arr_conam_lt_1_from_4_to_30[[i]] <- 0
    arr_conam_delta_lt_50_from_4_to_30[[i]] <- 0
    
    this_entry_today_n <- 1
    this_entry_cano_today_n <- 1
    this_entry_csmcu_today_n <- 1
    this_entry_stscd_today_n <- 1
    this_entry_mchno_today_n <- 1
    this_entry_ecfg_today_n <- 1
    this_entry_stocn_today_n <- 1
    this_entry_history_days <- -1
    this_entry_cano_history_days <- -1
    this_entry_csmcu_history_days <- -1
    this_entry_stscd_history_days <- -1
    this_entry_mchno_history_days <- -1
    this_entry_ecfg_history_days <- -1
    this_entry_stocn_history_days <- -1
    this_entry_history_n <- 0
    this_entry_cano_history_n <- 0
    this_entry_csmcu_history_n <- 0
    this_entry_stscd_history_n <- 0
    this_entry_mchno_history_n <- 0
    this_entry_ecfg_history_n <- 0
    this_entry_stocn_history_n <- 0
    this_conam_history_sum <- 0
    this_conam_cano_history_sum <- 0
    this_conam_csmcu_history_sum <- 0
    this_conam_stscd_history_sum <- 0
    this_conam_mchno_history_sum <- 0
    this_conam_ecfg_history_sum <- 0
    this_conam_stocn_history_sum <- 0
    this_conam_history_count <- 0
    this_conam_cano_history_count <- 0
    this_conam_csmcu_history_count <- 0
    this_conam_stscd_history_count <- 0
    this_conam_mchno_history_count <- 0
    this_conam_ecfg_history_count <- 0
    this_conam_stocn_history_count <- 0
    this_conam_history_acc_sum <- 0
    this_conam_cano_history_acc_sum <- 0
    this_conam_csmcu_history_acc_sum <- 0
    this_conam_stscd_history_acc_sum <- 0
    this_conam_mchno_history_acc_sum <- 0
    this_conam_ecfg_history_acc_sum <- 0
    this_conam_stocn_history_acc_sum <- 0
    this_conam_history_acc_count <- 0
    this_conam_cano_history_acc_count <- 0
    this_conam_csmcu_history_acc_count <- 0
    this_conam_stscd_history_acc_count <- 0
    this_conam_mchno_history_acc_count <- 0
    this_conam_ecfg_history_acc_count <- 0
    this_conam_stocn_history_acc_count <- 0
    
    
    # go up
    j <- i - 1
    l <- this_locdt
    while (j > 0) {
        if (my_dat$bacno[j] != this_bacno) break
        if (i - j == 1) {
            # 上一筆資料
            if (abs(my_dat$conam[j] - this_conam) <= 50) {
                arr_last_conam_diff_lt_50[[i]] <- arr_last_conam_diff_lt_50[[i]] + 1
            }
            if (my_dat$conam[j] < 1) {
                arr_last_conam_is_0[[i]] <- 1
            }
        }
        if ((this_locdt != my_dat$locdt[j]) && (this_locdt - my_dat$locdt[j] <= 7) && (this_locdt - my_dat$locdt[j] > 1)) { # 非當天
            # 更新 max / min
            if ((my_dat$conam[j] > this_conam_max) && (my_dat$conam[j] >= 1)) {
                this_conam_max <- my_dat$conam[j]
            } else if ((my_dat$conam[j] < this_conam_min) && (my_dat$conam[j] >= 1)) {
                this_conam_min <- my_dat$conam[j]
            }
        }
        if ((this_locdt != my_dat$locdt[j]) && (this_locdt - my_dat$locdt[j] > 1)) { # 非當天
            arr_acc_cano[[i]] <- arr_acc_cano[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            arr_acc_csmcu[[i]] <- arr_acc_csmcu[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            arr_acc_stscd[[i]] <- arr_acc_stscd[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            arr_acc_mchno[[i]] <- arr_acc_mchno[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            arr_acc_ecfg[[i]] <- arr_acc_ecfg[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            arr_acc_stocn[[i]] <- arr_acc_stocn[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            arr_acc_loctm_hh[[i]] <- arr_acc_loctm_hh[[i]] + ifelse(my_dat$loctm_hh[j] == this_loctm_hh, 1, 0)
        }
        if (this_locdt == my_dat$locdt[j]) { # 當天
            this_entry_today_n <- this_entry_today_n + 1
            this_entry_cano_today_n <- this_entry_cano_today_n + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            this_entry_csmcu_today_n <- this_entry_csmcu_today_n + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            this_entry_stscd_today_n <- this_entry_stscd_today_n + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            this_entry_mchno_today_n <- this_entry_mchno_today_n + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            this_entry_ecfg_today_n <- this_entry_ecfg_today_n + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            this_entry_stocn_today_n <- this_entry_stocn_today_n + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
        } else if ((this_locdt - my_dat$locdt[j] <= 11) && (this_locdt - my_dat$locdt[j] > 1)) { # 非當天
            this_entry_history_days <- union(this_entry_history_days, my_dat$locdt[j])
            this_entry_cano_history_days <- ifelse(my_dat$cano[j] == this_cano, union(this_entry_cano_history_days, my_dat$locdt[j]), this_entry_cano_history_days)
            this_entry_csmcu_history_days <- ifelse(my_dat$csmcu[j] == this_csmcu, union(this_entry_csmcu_history_days, my_dat$locdt[j]), this_entry_csmcu_history_days)
            this_entry_stscd_history_days <- ifelse(my_dat$stscd[j] == this_stscd, union(this_entry_stscd_history_days, my_dat$locdt[j]), this_entry_stscd_history_days)
            this_entry_mchno_history_days <- ifelse(my_dat$mchno[j] == this_mchno, union(this_entry_mchno_history_days, my_dat$locdt[j]), this_entry_mchno_history_days)
            this_entry_ecfg_history_days <- ifelse(my_dat$ecfg[j] == this_ecfg, union(this_entry_ecfg_history_days, my_dat$locdt[j]), this_entry_ecfg_history_days)
            this_entry_stocn_history_days <- ifelse(my_dat$stocn[j] == this_stocn, union(this_entry_stocn_history_days, my_dat$locdt[j]), this_entry_stocn_history_days)
            this_entry_history_n <- this_entry_history_n + 1
            this_entry_cano_history_n <- this_entry_cano_history_n + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            this_entry_csmcu_history_n <- this_entry_csmcu_history_n + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            this_entry_stscd_history_n <- this_entry_stscd_history_n + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            this_entry_mchno_history_n <- this_entry_mchno_history_n + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            this_entry_ecfg_history_n <- this_entry_ecfg_history_n + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            this_entry_stocn_history_n <- this_entry_stocn_history_n + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            if (my_dat$conam[j] > 1) {
                this_conam_cano_history_sum <- this_conam_cano_history_sum + ifelse(my_dat$cano[j] == this_cano, my_dat$conam[j], 0)
                this_conam_csmcu_history_sum <- this_conam_csmcu_history_sum + ifelse(my_dat$csmcu[j] == this_csmcu, my_dat$conam[j], 0)
                this_conam_stscd_history_sum <- this_conam_stscd_history_sum + ifelse(my_dat$stscd[j] == this_stscd, my_dat$conam[j], 0)
                this_conam_mchno_history_sum <- this_conam_mchno_history_sum + ifelse(my_dat$mchno[j] == this_mchno, my_dat$conam[j], 0)
                this_conam_ecfg_history_sum <- this_conam_ecfg_history_sum + ifelse(my_dat$ecfg[j] == this_ecfg, my_dat$conam[j], 0)
                this_conam_stocn_history_sum <- this_conam_stocn_history_sum + ifelse(my_dat$stocn[j] == this_stocn, my_dat$conam[j], 0)
                this_conam_history_count <- this_conam_history_count + 1
                this_conam_cano_history_count <- this_conam_cano_history_count + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                this_conam_csmcu_history_count <- this_conam_csmcu_history_count + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                this_conam_stscd_history_count <- this_conam_stscd_history_count + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                this_conam_mchno_history_count <- this_conam_mchno_history_count + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                this_conam_ecfg_history_count <- this_conam_ecfg_history_count + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                this_conam_stocn_history_count <- this_conam_stocn_history_count + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            }
        }
        if ((this_locdt != my_dat$locdt[j]) && (this_locdt - my_dat$locdt[j] > 1)) { # 非當日，全部歷史紀錄
            if (my_dat$conam[j] > 1) {
                this_conam_history_acc_sum <- this_conam_history_acc_sum + my_dat$conam[j]
                this_conam_cano_history_acc_sum <- this_conam_cano_history_acc_sum + ifelse(my_dat$cano[j] == this_cano, my_dat$conam[j], 0)
                this_conam_csmcu_history_acc_sum <- this_conam_csmcu_history_acc_sum + ifelse(my_dat$csmcu[j] == this_csmcu, my_dat$conam[j], 0)
                this_conam_stscd_history_acc_sum <- this_conam_stscd_history_acc_sum + ifelse(my_dat$stscd[j] == this_stscd, my_dat$conam[j], 0)
                this_conam_mchno_history_acc_sum <- this_conam_mchno_history_acc_sum + ifelse(my_dat$mchno[j] == this_mchno, my_dat$conam[j], 0)
                this_conam_ecfg_history_acc_sum <- this_conam_ecfg_history_acc_sum + ifelse(my_dat$ecfg[j] == this_ecfg, my_dat$conam[j], 0)
                this_conam_stocn_history_acc_sum <- this_conam_stocn_history_acc_sum + ifelse(my_dat$stocn[j] == this_stocn, my_dat$conam[j], 0)
                this_conam_history_acc_count <- this_conam_history_acc_count + 1
                this_conam_cano_history_acc_count <- this_conam_cano_history_acc_count + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                this_conam_csmcu_history_acc_count <- this_conam_csmcu_history_acc_count + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                this_conam_stscd_history_acc_count <- this_conam_stscd_history_acc_count + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                this_conam_mchno_history_acc_count <- this_conam_mchno_history_acc_count + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                this_conam_ecfg_history_acc_count <- this_conam_ecfg_history_acc_count + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                this_conam_stocn_history_acc_count <- this_conam_stocn_history_acc_count + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            }
        }
        if (this_locdt - my_dat$locdt[j] <= 3) { # from_0_to_3
            if (this_locdt == my_dat$locdt[j]) { # 當天
                if (abs(this_loctm_hh - my_dat$loctm_hh[j]) <= 1) {
                    # 一小時內
                    arr_same_cano_in_1hr[[i]] <- arr_same_cano_in_1hr[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                    arr_same_csmcu_in_1hr[[i]] <- arr_same_csmcu_in_1hr[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                    arr_same_stscd_in_1hr[[i]] <- arr_same_stscd_in_1hr[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                    arr_same_mchno_in_1hr[[i]] <- arr_same_mchno_in_1hr[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                    arr_same_ecfg_in_1hr[[i]] <- arr_same_ecfg_in_1hr[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                    arr_same_stocn_in_1hr[[i]] <- arr_same_stocn_in_1hr[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
                    if (abs(this_loctm_mm - my_dat$loctm_mm[j]) <= 3) {
                        # 三分鐘內
                        arr_same_cano_in_3mins[[i]] <- arr_same_cano_in_3mins[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                        arr_same_csmcu_in_3mins[[i]] <- arr_same_csmcu_in_3mins[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                        arr_same_stscd_in_3mins[[i]] <- arr_same_stscd_in_3mins[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                        arr_same_mchno_in_3mins[[i]] <- arr_same_mchno_in_3mins[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                        arr_same_ecfg_in_3mins[[i]] <- arr_same_ecfg_in_3mins[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                        arr_same_stocn_in_3mins[[i]] <- arr_same_stocn_in_3mins[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
                    }
                }
            }
            arr_same_cano_from_0_to_3[[i]] <- arr_same_cano_from_0_to_3[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            arr_same_csmcu_from_0_to_3[[i]] <- arr_same_csmcu_from_0_to_3[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            arr_same_stscd_from_0_to_3[[i]] <- arr_same_stscd_from_0_to_3[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            arr_same_mchno_from_0_to_3[[i]] <- arr_same_mchno_from_0_to_3[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            arr_same_ecfg_from_0_to_3[[i]] <- arr_same_ecfg_from_0_to_3[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            arr_same_stocn_from_0_to_3[[i]] <- arr_same_stocn_from_0_to_3[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            arr_conam_lt_1_from_0_to_3[[i]] <- arr_conam_lt_1_from_0_to_3[[i]] + ifelse(my_dat$conam[j] < 1, 1, 0)
            arr_conam_delta_lt_50_from_0_to_3[[i]] <- arr_conam_delta_lt_50_from_0_to_3[[i]] + ifelse(abs(my_dat$conam[j] - this_conam) < 50, 1, 0)
            arr_same_loctm_hh_from_0_to_3[[i]] <- arr_same_loctm_hh_from_0_to_3[[i]] + ifelse(my_dat$loctm_hh[j] == this_loctm_hh, 1, 0)
        } else if (this_locdt - my_dat$locdt[j] <= 30) { # from_4_to_30
            arr_same_cano_from_4_to_30[[i]] <- arr_same_cano_from_4_to_30[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            arr_same_csmcu_from_4_to_30[[i]] <- arr_same_csmcu_from_4_to_30[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            arr_same_stscd_from_4_to_30[[i]] <- arr_same_stscd_from_4_to_30[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            arr_same_mchno_from_4_to_30[[i]] <- arr_same_mchno_from_4_to_30[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            arr_same_ecfg_from_4_to_30[[i]] <- arr_same_ecfg_from_4_to_30[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            arr_same_stocn_from_4_to_30[[i]] <- arr_same_stocn_from_4_to_30[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            arr_conam_lt_1_from_4_to_30[[i]] <- arr_conam_lt_1_from_4_to_30[[i]] + ifelse(my_dat$conam[j] < 1, 1, 0)
            arr_conam_delta_lt_50_from_4_to_30[[i]] <- arr_conam_delta_lt_50_from_4_to_30[[i]] + ifelse(abs(my_dat$conam[j] - this_conam) < 50, 1, 0)
            arr_same_loctm_hh_from_4_to_30[[i]] <- arr_same_loctm_hh_from_4_to_30[[i]] + ifelse(my_dat$loctm_hh[j] == this_loctm_hh, 1, 0)
        }
        j <- j - 1
    }
    
    # go down
    j <- i + 1
    l <- this_locdt
    while (j <= nrow(my_dat)) {
        if (my_dat$bacno[j] != this_bacno) break
        if ((this_locdt != my_dat$locdt[j]) && (my_dat$locdt[j] - this_locdt <= 2) && (my_dat$locdt[j] - this_locdt > 1)) { # 非當天
            # 更新 max / min
            if ((my_dat$conam[j] > this_conam_max) && (my_dat$conam[j] >= 1)) {
                this_conam_max <- my_dat$conam[j]
            } else if ((my_dat$conam[j] < this_conam_min) && (my_dat$conam[j] >= 1)) {
                this_conam_min <- my_dat$conam[j]
            }
        }
        if (this_locdt == my_dat$locdt[j]) { # 當天
            this_entry_today_n <- this_entry_today_n + 1
            this_entry_cano_today_n <- this_entry_cano_today_n + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            this_entry_csmcu_today_n <- this_entry_csmcu_today_n + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            this_entry_stscd_today_n <- this_entry_stscd_today_n + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            this_entry_mchno_today_n <- this_entry_mchno_today_n + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            this_entry_ecfg_today_n <- this_entry_ecfg_today_n + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            this_entry_stocn_today_n <- this_entry_stocn_today_n + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
        } else if ((my_dat$locdt[j] - this_locdt <= 3) && (my_dat$locdt[j] - this_locdt > 1)) { # 非當天
            this_entry_history_days <- union(this_entry_history_days, my_dat$locdt[j])
            this_entry_cano_history_days <- ifelse(my_dat$cano[j] == this_cano, union(this_entry_cano_history_days, my_dat$locdt[j]), this_entry_cano_history_days)
            this_entry_csmcu_history_days <- ifelse(my_dat$csmcu[j] == this_csmcu, union(this_entry_csmcu_history_days, my_dat$locdt[j]), this_entry_csmcu_history_days)
            this_entry_stscd_history_days <- ifelse(my_dat$stscd[j] == this_stscd, union(this_entry_stscd_history_days, my_dat$locdt[j]), this_entry_stscd_history_days)
            this_entry_mchno_history_days <- ifelse(my_dat$mchno[j] == this_mchno, union(this_entry_mchno_history_days, my_dat$locdt[j]), this_entry_mchno_history_days)
            this_entry_ecfg_history_days <- ifelse(my_dat$ecfg[j] == this_ecfg, union(this_entry_ecfg_history_days, my_dat$locdt[j]), this_entry_ecfg_history_days)
            this_entry_stocn_history_days <- ifelse(my_dat$stocn[j] == this_stocn, union(this_entry_stocn_history_days, my_dat$locdt[j]), this_entry_stocn_history_days)
            this_entry_history_n <- this_entry_history_n + 1
            this_entry_cano_history_n <- this_entry_cano_history_n + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            this_entry_csmcu_history_n <- this_entry_csmcu_history_n + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            this_entry_stscd_history_n <- this_entry_stscd_history_n + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            this_entry_mchno_history_n <- this_entry_mchno_history_n + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            this_entry_ecfg_history_n <- this_entry_ecfg_history_n + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            this_entry_stocn_history_n <- this_entry_stocn_history_n + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            if (my_dat$conam[j] > 1) {
                this_conam_cano_history_sum <- this_conam_cano_history_sum + ifelse(my_dat$cano[j] == this_cano, my_dat$conam[j], 0)
                this_conam_csmcu_history_sum <- this_conam_csmcu_history_sum + ifelse(my_dat$csmcu[j] == this_csmcu, my_dat$conam[j], 0)
                this_conam_stscd_history_sum <- this_conam_stscd_history_sum + ifelse(my_dat$stscd[j] == this_stscd, my_dat$conam[j], 0)
                this_conam_mchno_history_sum <- this_conam_mchno_history_sum + ifelse(my_dat$mchno[j] == this_mchno, my_dat$conam[j], 0)
                this_conam_ecfg_history_sum <- this_conam_ecfg_history_sum + ifelse(my_dat$ecfg[j] == this_ecfg, my_dat$conam[j], 0)
                this_conam_stocn_history_sum <- this_conam_stocn_history_sum + ifelse(my_dat$stocn[j] == this_stocn, my_dat$conam[j], 0)
                this_conam_history_count <- this_conam_history_count + 1
                this_conam_cano_history_count <- this_conam_cano_history_count + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                this_conam_csmcu_history_count <- this_conam_csmcu_history_count + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                this_conam_stscd_history_count <- this_conam_stscd_history_count + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                this_conam_mchno_history_count <- this_conam_mchno_history_count + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                this_conam_ecfg_history_count <- this_conam_ecfg_history_count + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                this_conam_stocn_history_count <- this_conam_stocn_history_count + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            }
        }
        if (my_dat$locdt[j] - this_locdt <= 3) { # from_0_to_3
            if (this_locdt == my_dat$locdt[j]) { # 當天
                if (abs(this_loctm_hh - my_dat$loctm_hh[j]) <= 1) {
                    # 一小時內
                    arr_same_cano_in_1hr[[i]] <- arr_same_cano_in_1hr[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                    arr_same_csmcu_in_1hr[[i]] <- arr_same_csmcu_in_1hr[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                    arr_same_stscd_in_1hr[[i]] <- arr_same_stscd_in_1hr[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                    arr_same_mchno_in_1hr[[i]] <- arr_same_mchno_in_1hr[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                    arr_same_ecfg_in_1hr[[i]] <- arr_same_ecfg_in_1hr[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                    arr_same_stocn_in_1hr[[i]] <- arr_same_stocn_in_1hr[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
                    if (abs(this_loctm_mm - my_dat$loctm_mm[j]) <= 3) {
                        # 三分鐘內
                        arr_same_cano_in_3mins[[i]] <- arr_same_cano_in_3mins[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
                        arr_same_csmcu_in_3mins[[i]] <- arr_same_csmcu_in_3mins[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
                        arr_same_stscd_in_3mins[[i]] <- arr_same_stscd_in_3mins[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
                        arr_same_mchno_in_3mins[[i]] <- arr_same_mchno_in_3mins[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
                        arr_same_ecfg_in_3mins[[i]] <- arr_same_ecfg_in_3mins[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
                        arr_same_stocn_in_3mins[[i]] <- arr_same_stocn_in_3mins[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
                    }
                }
            }
            arr_same_cano_from_0_to_3[[i]] <- arr_same_cano_from_0_to_3[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            arr_same_csmcu_from_0_to_3[[i]] <- arr_same_csmcu_from_0_to_3[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            arr_same_stscd_from_0_to_3[[i]] <- arr_same_stscd_from_0_to_3[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            arr_same_mchno_from_0_to_3[[i]] <- arr_same_mchno_from_0_to_3[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            arr_same_ecfg_from_0_to_3[[i]] <- arr_same_ecfg_from_0_to_3[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            arr_same_stocn_from_0_to_3[[i]] <- arr_same_stocn_from_0_to_3[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            arr_conam_lt_1_from_0_to_3[[i]] <- arr_conam_lt_1_from_0_to_3[[i]] + ifelse(my_dat$conam[j] < 1, 1, 0)
            arr_conam_delta_lt_50_from_0_to_3[[i]] <- arr_conam_delta_lt_50_from_0_to_3[[i]] + ifelse(abs(my_dat$conam[j] - this_conam) < 50, 1, 0)
            arr_same_loctm_hh_from_0_to_3[[i]] <- arr_same_loctm_hh_from_0_to_3[[i]] + ifelse(my_dat$loctm_hh[j] == this_loctm_hh, 1, 0)
        } else if (my_dat$locdt[j] - this_locdt <= 12) { # from_4_to_30, but actually not too far "future"
            arr_same_cano_from_4_to_30[[i]] <- arr_same_cano_from_4_to_30[[i]] + ifelse(my_dat$cano[j] == this_cano, 1, 0)
            arr_same_csmcu_from_4_to_30[[i]] <- arr_same_csmcu_from_4_to_30[[i]] + ifelse(my_dat$csmcu[j] == this_csmcu, 1, 0)
            arr_same_stscd_from_4_to_30[[i]] <- arr_same_stscd_from_4_to_30[[i]] + ifelse(my_dat$stscd[j] == this_stscd, 1, 0)
            arr_same_mchno_from_4_to_30[[i]] <- arr_same_mchno_from_4_to_30[[i]] + ifelse(my_dat$mchno[j] == this_mchno, 1, 0)
            arr_same_ecfg_from_4_to_30[[i]] <- arr_same_ecfg_from_4_to_30[[i]] + ifelse(my_dat$ecfg[j] == this_ecfg, 1, 0)
            arr_same_stocn_from_4_to_30[[i]] <- arr_same_stocn_from_4_to_30[[i]] + ifelse(my_dat$stocn[j] == this_stocn, 1, 0)
            arr_conam_lt_1_from_4_to_30[[i]] <- arr_conam_lt_1_from_4_to_30[[i]] + ifelse(my_dat$conam[j] < 1, 1, 0)
            arr_conam_delta_lt_50_from_4_to_30[[i]] <- arr_conam_delta_lt_50_from_4_to_30[[i]] + ifelse(abs(my_dat$conam[j] - this_conam) < 50, 1, 0)
            arr_same_loctm_hh_from_4_to_30[[i]] <- arr_same_loctm_hh_from_4_to_30[[i]] + ifelse(my_dat$loctm_hh[j] == this_loctm_hh, 1, 0)
        }
        if (my_dat$locdt[j] - this_locdt > 30) {
            break
        }
        j <- j + 1
    }
    # 確認 max / min
    if (this_conam >= 1) {
        if (this_conam_max - this_conam > 50) {
            arr_is_max_in_7days[[i]] <- 0
        } 
        if (this_conam - this_conam_min > 50) {
            arr_is_min_in_7days[[i]] <- 0
        }
    } else {
        arr_is_max_in_7days[[i]] <- 0
        arr_is_min_in_7days[[i]] <- 0
    }
    # 確認 entry
    arr_entry_diff[[i]] <- ifelse(sum(this_entry_history_days >= 0) > 0, this_entry_today_n - (this_entry_history_n / sum(this_entry_history_days >= 0)), 0)
    arr_entry_cano_diff[[i]] <- ifelse(sum(this_entry_cano_history_days >= 0) > 0, this_entry_cano_today_n - (this_entry_cano_history_n / sum(this_entry_cano_history_days >= 0)), 0)
    arr_entry_csmcu_diff[[i]] <- ifelse(sum(this_entry_csmcu_history_days >= 0) > 0, this_entry_csmcu_today_n - (this_entry_csmcu_history_n / sum(this_entry_csmcu_history_days >= 0)), 0)
    arr_entry_stscd_diff[[i]] <- ifelse(sum(this_entry_stscd_history_days >= 0) > 0, this_entry_stscd_today_n - (this_entry_stscd_history_n / sum(this_entry_stscd_history_days >= 0)), 0)
    arr_entry_mchno_diff[[i]] <- ifelse(sum(this_entry_mchno_history_days >= 0) > 0, this_entry_mchno_today_n - (this_entry_mchno_history_n / sum(this_entry_mchno_history_days >= 0)), 0)
    arr_entry_ecfg_diff[[i]] <- ifelse(sum(this_entry_ecfg_history_days >= 0) > 0, this_entry_ecfg_today_n - (this_entry_ecfg_history_n / sum(this_entry_ecfg_history_days >= 0)), 0)
    arr_entry_stocn_diff[[i]] <- ifelse(sum(this_entry_stocn_history_days >= 0) > 0, this_entry_stocn_today_n - (this_entry_stocn_history_n / sum(this_entry_stocn_history_days >= 0)), 0)
    # 確認 conam
    arr_conam_diff[[i]] <- ifelse(this_conam_history_count > 0, this_conam - (this_conam_history_sum / this_conam_history_count), 0)
    arr_conam_cano_diff[[i]] <- ifelse(this_conam_cano_history_count > 0, this_conam - (this_conam_cano_history_sum / this_conam_cano_history_count), 0)
    arr_conam_csmcu_diff[[i]] <- ifelse(this_conam_csmcu_history_count > 0, this_conam - (this_conam_csmcu_history_sum / this_conam_csmcu_history_count), 0)
    arr_conam_stscd_diff[[i]] <- ifelse(this_conam_stscd_history_count > 0, this_conam - (this_conam_stscd_history_sum / this_conam_stscd_history_count), 0)
    arr_conam_mchno_diff[[i]] <- ifelse(this_conam_mchno_history_count > 0, this_conam - (this_conam_mchno_history_sum / this_conam_mchno_history_count), 0)
    arr_conam_ecfg_diff[[i]] <- ifelse(this_conam_ecfg_history_count > 0, this_conam - (this_conam_ecfg_history_sum / this_conam_ecfg_history_count), 0)
    arr_conam_stocn_diff[[i]] <- ifelse(this_conam_stocn_history_count > 0, this_conam - (this_conam_stocn_history_sum / this_conam_stocn_history_count), 0)
    arr_conam_acc_diff[[i]] <- ifelse(this_conam_history_acc_count > 0, this_conam - (this_conam_history_acc_sum / this_conam_history_acc_count), 0)
    arr_conam_cano_acc_diff[[i]] <- ifelse(this_conam_cano_history_acc_count > 0, this_conam - (this_conam_cano_history_acc_sum / this_conam_cano_history_acc_count), 0)
    arr_conam_csmcu_acc_diff[[i]] <- ifelse(this_conam_csmcu_history_acc_count > 0, this_conam - (this_conam_csmcu_history_acc_sum / this_conam_csmcu_history_acc_count), 0)
    arr_conam_stscd_acc_diff[[i]] <- ifelse(this_conam_stscd_history_acc_count > 0, this_conam - (this_conam_stscd_history_acc_sum / this_conam_stscd_history_acc_count), 0)
    arr_conam_mchno_acc_diff[[i]] <- ifelse(this_conam_mchno_history_acc_count > 0, this_conam - (this_conam_mchno_history_acc_sum / this_conam_mchno_history_acc_count), 0)
    arr_conam_ecfg_acc_diff[[i]] <- ifelse(this_conam_ecfg_history_acc_count > 0, this_conam - (this_conam_ecfg_history_acc_sum / this_conam_ecfg_history_acc_count), 0)
    arr_conam_stocn_acc_diff[[i]] <- ifelse(this_conam_stocn_history_acc_count > 0, this_conam - (this_conam_stocn_history_acc_sum / this_conam_stocn_history_acc_count), 0)
}

my_dat$entry_count_same_cano_from_0_to_3 <- arr_same_cano_from_0_to_3
my_dat$entry_count_same_csmcu_from_0_to_3 <- arr_same_csmcu_from_0_to_3
my_dat$entry_count_same_stscd_from_0_to_3 <- arr_same_stscd_from_0_to_3
my_dat$entry_count_same_mchno_from_0_to_3 <- arr_same_mchno_from_0_to_3
my_dat$entry_count_same_ecfg_from_0_to_3 <- arr_same_ecfg_from_0_to_3
my_dat$entry_count_same_stocn_from_0_to_3 <- arr_same_stocn_from_0_to_3
my_dat$entry_count_conam_lt_1_from_0_to_3 <- arr_conam_lt_1_from_0_to_3
my_dat$entry_count_conam_delta_lt_50_from_0_to_3 <- arr_conam_delta_lt_50_from_0_to_3
my_dat$entry_count_same_loctm_hh_from_0_to_3 <- arr_same_loctm_hh_from_0_to_3

my_dat$entry_count_same_cano_from_4_to_30 <- arr_same_cano_from_4_to_30
my_dat$entry_count_same_csmcu_from_4_to_30 <- arr_same_csmcu_from_4_to_30
my_dat$entry_count_same_stscd_from_4_to_30 <- arr_same_stscd_from_4_to_30
my_dat$entry_count_same_mchno_from_4_to_30 <- arr_same_mchno_from_4_to_30
my_dat$entry_count_same_ecfg_from_4_to_30 <- arr_same_ecfg_from_4_to_30
my_dat$entry_count_same_stocn_from_4_to_30 <- arr_same_stocn_from_4_to_30
my_dat$entry_count_conam_lt_1_from_4_to_30 <- arr_conam_lt_1_from_4_to_30
my_dat$entry_count_conam_delta_lt_50_from_4_to_30 <- arr_conam_delta_lt_50_from_4_to_30
my_dat$entry_count_same_loctm_hh_from_4_to_30 <- arr_same_loctm_hh_from_4_to_30

my_dat$entry_count_same_cano_in_1hr <- arr_same_cano_in_1hr
my_dat$entry_count_same_csmcu_in_1hr <- arr_same_csmcu_in_1hr
my_dat$entry_count_same_stscd_in_1hr <- arr_same_stscd_in_1hr
my_dat$entry_count_same_mchno_in_1hr <- arr_same_mchno_in_1hr
my_dat$entry_count_same_ecfg_in_1hr <- arr_same_ecfg_in_1hr
my_dat$entry_count_same_stocn_in_1hr <- arr_same_stocn_in_1hr

my_dat$entry_count_same_cano_in_3mins <- arr_same_cano_in_3mins
my_dat$entry_count_same_csmcu_in_3mins <- arr_same_csmcu_in_3mins
my_dat$entry_count_same_stscd_in_3mins <- arr_same_stscd_in_3mins
my_dat$entry_count_same_mchno_in_3mins <- arr_same_mchno_in_3mins
my_dat$entry_count_same_ecfg_in_3mins <- arr_same_ecfg_in_3mins
my_dat$entry_count_same_stocn_in_3mins <- arr_same_stocn_in_3mins

my_dat$entry_count_last_conam_diff_lt_50 <- arr_last_conam_diff_lt_50
my_dat$entry_count_last_conam_is_0 <- arr_last_conam_is_0
my_dat$entry_count_is_max_in_7days <- arr_is_max_in_7days
my_dat$entry_count_is_min_in_7days <- arr_is_min_in_7days

my_dat$entry_count_acc_cano <- arr_acc_cano 
my_dat$entry_count_acc_csmcu <- arr_acc_csmcu 
my_dat$entry_count_acc_stscd <- arr_acc_stscd 
my_dat$entry_count_acc_mchno <- arr_acc_mchno 
my_dat$entry_count_acc_ecfg <- arr_acc_ecfg 
my_dat$entry_count_acc_stocn <- arr_acc_stocn 
my_dat$entry_count_acc_loctm_hh <- arr_acc_loctm_hh 

my_dat$entry_entry_diff <- arr_entry_diff
my_dat$entry_entry_cano_diff <- arr_entry_cano_diff
my_dat$entry_entry_csmcu_diff <- arr_entry_csmcu_diff
my_dat$entry_entry_stscd_diff <- arr_entry_stscd_diff
my_dat$entry_entry_mchno_diff <- arr_entry_mchno_diff
my_dat$entry_entry_ecfg_diff <- arr_entry_ecfg_diff
my_dat$entry_entry_stocn_diff <- arr_entry_stocn_diff

my_dat$entry_conam_diff <- arr_conam_diff
my_dat$entry_conam_cano_diff <- arr_conam_cano_diff
my_dat$entry_conam_csmcu_diff <- arr_conam_csmcu_diff
my_dat$entry_conam_stscd_diff <- arr_conam_stscd_diff
my_dat$entry_conam_mchno_diff <- arr_conam_mchno_diff
my_dat$entry_conam_ecfg_diff <- arr_conam_ecfg_diff
my_dat$entry_conam_stocn_diff <- arr_conam_stocn_diff
my_dat$entry_conam_acc_diff <- arr_conam_acc_diff
my_dat$entry_conam_cano_acc_diff <- arr_conam_cano_acc_diff
my_dat$entry_conam_csmcu_acc_diff <- arr_conam_csmcu_acc_diff
my_dat$entry_conam_stscd_acc_diff <- arr_conam_stscd_acc_diff
my_dat$entry_conam_mchno_acc_diff <- arr_conam_mchno_acc_diff
my_dat$entry_conam_ecfg_acc_diff <- arr_conam_ecfg_acc_diff
my_dat$entry_conam_stocn_acc_diff <- arr_conam_stocn_acc_diff

# debugging: 
# my_dat %>%
#     filter(bacno == sample(my_dat$bacno, 1)) %>%
#     select(locdt, bacno, conam, loctm_hh, contains("ecfg")) %>%
#     arrange(locdt, loctm_hh) %>%
#     view()

write_rds(my_dat %>%
              arrange(row_num) %>%
              select(row_num,
                     bacno, locdt,
                     starts_with("entry_")),
          str_c(mid_data_dir, "entry_count_2.rds"))
