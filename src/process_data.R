library(tidyverse)
# library(magrittr)
library(lubridate)
library(recipes)

source("./src/get_col_types.R")
data_dir <- "./sample_data/"
mid_data_dir <- "./intermediate_data/"
processed_data_dir <- "./processed_data/"

# Load Data
train_dat <- read_csv(str_c(data_dir, "train.csv"), 
                      col_types = get_col_types(is_test = FALSE)) %>% 
    mutate(dataset = "train")
test_dat <- read_csv(str_c(data_dir, "test.csv"), 
                     col_types = get_col_types(is_test = TRUE)) %>% 
    mutate(dataset = "test")
dat <- bind_rows(train_dat, test_dat)

# 部分 categorical features 用 "名單" 方式手動篩選、後續接 one-hot
# filter_lists <- make_filter_lists()
make_filter_lists <- function() {
    # mchno
    mchno_list <- train_dat %>% 
        group_by(mchno) %>% 
        summarize(n = n(),
                  fraud_count = sum(fraud_ind == "1"),
                  fraud_prob = (fraud_count / n) %>% round(4)) %>% 
        filter(fraud_count > 0) %>% 
        arrange(desc(fraud_count)) %>% 
        left_join(test_dat %>% 
                      group_by(mchno) %>% 
                      summarize(n_test = n()), 
                  by = "mchno") %>% 
        filter(
            n_test > 0,
            (n > 1 & fraud_prob >= 0.5) | (n > 15 & fraud_prob >= 0.3) | (n > 30 & fraud_prob >= 0.1)
        ) %>% 
        pull(mchno)
    
    # acqic
    acqic_list <- train_dat %>%
        group_by(acqic) %>%
        summarize(n = n(),
                  fraud_count = sum(fraud_ind == "1"),
                  fraud_prob = (fraud_count / n) %>% round(4)) %>%
        filter(fraud_count > 0) %>%
        arrange(desc(fraud_count)) %>%
        left_join(test_dat %>%
                      group_by(acqic) %>%
                      summarize(n_test = n()),
                  by = "acqic") %>%
        filter(
            n_test > 0,
            (n > 1 & fraud_prob >= 0.5) | (n > 15 & fraud_prob >= 0.3) | (n > 30 & fraud_prob >= 0.1)
        ) %>%
        pull(acqic)
    
    # mcc
    mcc_list <- train_dat %>%
        group_by(mcc) %>%
        summarize(n = n(),
                  fraud_count = sum(fraud_ind == "1"),
                  fraud_prob = (fraud_count / n) %>% round(4)) %>%
        filter(fraud_count > 0) %>%
        arrange(desc(fraud_count)) %>%
        left_join(test_dat %>%
                      group_by(mcc) %>%
                      summarize(n_test = n()),
                  by = "mcc") %>%
        filter(
            n_test > 0,
            (n > 1 & fraud_prob >= 0.5) | (n > 15 & fraud_prob >= 0.3) | (n > 30 & fraud_prob >= 0.1)
        ) %>%
        pull(mcc)
    
    # stocn
    stocn_list <- dat %>%
        group_by(stocn) %>%
        summarise(train_cnt = sum(dataset == "train"),
                  test_cnt = sum(dataset == "test"),
                  fraud_ratio = round(sum(fraud_ind == "1", na.rm = TRUE) / sum(dataset == "train"), 3)) %>%
        arrange(desc(train_cnt)) %>%
        filter(test_cnt > 0 & train_cnt > 0 & fraud_ratio > 0) %>%
        filter((fraud_ratio >= 0.4 & train_cnt >= 10) |
                   (fraud_ratio >= 0.1 & train_cnt >= 30 & test_cnt >= 3) |
                   (train_cnt >= 40 & test_cnt >= 5)) %>%
        pull(stocn)
    
    # scity
    scity_list <- dat %>%
        group_by(scity) %>%
        summarise(train_cnt = sum(dataset == "train"),
                  test_cnt = sum(dataset == "test"),
                  fraud_ratio = round(sum(fraud_ind == "1", na.rm = TRUE) / sum(dataset == "train"), 3)) %>%
        arrange(desc(train_cnt)) %>%
        filter(test_cnt > 0 & train_cnt > 0 & fraud_ratio > 0) %>%
        filter((fraud_ratio >= 0.4 & train_cnt >= 10) |
                   (fraud_ratio >= 0.1 & train_cnt >= 30 & test_cnt >= 3) |
                   (train_cnt >= 40 & test_cnt >= 5)) %>%
        pull(scity)
    
    # csmcu
    csmcu_list <- dat %>%
        group_by(csmcu) %>%
        summarise(train_cnt = sum(dataset == "train"),
                  test_cnt = sum(dataset == "test"),
                  fraud_ratio = round(sum(fraud_ind == "1", na.rm = TRUE) / sum(dataset == "train"), 3)) %>%
        arrange(desc(train_cnt)) %>%
        filter(test_cnt > 0 & train_cnt > 0 & fraud_ratio > 0) %>%
        filter((fraud_ratio >= 0.4 & train_cnt >= 10) |
                   (fraud_ratio >= 0.1 & train_cnt >= 30 & test_cnt >= 3) |
                   (train_cnt >= 40 & test_cnt >= 5)) %>%
        pull(csmcu)
    
    # make list
    l <- list()
    for (v in ls()[endsWith(ls(), "_list")]) {
        l[v] <- list(get(v))
    }
    return(l)
}

# 創造 list(train_X=..., train_y=..., test_X=...)
get_data <- function(use_pca = TRUE) {
    filter_lists <- make_filter_lists()
    
    # 加入 entry 系列數據
    entry_count_dat <- read_rds(str_c(mid_data_dir, "entry_count.rds")) # 這個需要 join
    entry_count_2_dat <- read_rds(str_c(mid_data_dir, "entry_count_2.rds"))
    dat <- dat %>% 
        bind_cols(entry_count_2_dat %>% 
                      select(starts_with("entry_")))
    
    train_y <- train_dat$fraud_ind
    my_dat <- dat %>% 
        select(-fraud_ind) %>% 
        replace_na(replace = list(flbmk = "NA", flg_3dsmk = "NA")) %>% 
        mutate(
            loctm_chr = str_pad(loctm %>% as.integer %>% as.character, width = 6, pad = "0"),
            loctm_hh = loctm_chr %>% substr(1, 2) %>% as.integer(),
            loctm_hh_chr = loctm_chr %>% substr(1, 2),
            # loctm_hh_max_20 = ifelse(loctm_hh >= 20, loctm_hh - 24, loctm_hh),
            locdt_mod_7 = as.character(locdt %% 7),
        ) %>% 
        left_join(entry_count_dat, by = c("bacno", "locdt", "loctm_hh")) 
    
    ## just some renaming, not really meaningful
    should_be_renamed_cols <- c("entry_count_last_conam_diff_lt_50",
                                "entry_count_last_conam_is_0",
                                "entry_count_is_max_in_7days",
                                "entry_count_is_min_in_7days")
    for (e in should_be_renamed_cols) {
        if (e %in% names(my_dat)) {
            names(my_dat)[names(my_dat) == e] <- gsub(x = e, pattern = "entry_count_", replacement = "")
        }
    }
    
    # # debugging:
    # my_dat %>%
    #     filter(bacno == sample(my_dat$bacno, 1)) %>% 
    #     select(locdt, bacno, conam, loctm_hh, contains("mchno")) %>% 
    #     arrange(locdt, loctm_hh) %>% 
    #     view()
    
    my_dat <- my_dat %>% 
        mutate(
            contp = fct_other(contp, drop = c("0", "1", "3")),
            mchno = fct_other(mchno, keep = filter_lists$mchno_list),
            acqic = fct_other(acqic, keep = filter_lists$acqic_list),
            mcc = fct_other(mcc, keep = filter_lists$mcc_list),
            conam_log = log1p(conam),
            conam_log_cat = cut(conam_log, breaks = seq(0, ceiling(max(conam_log)), 1), include.lowest = TRUE),
            conam_zero = as.integer(conam < 1),
            ecfg_Y = as.integer(ecfg == "Y"),
            insfg_N = as.integer(insfg == "N"),
            iterm = fct_other(as.character(iterm), keep = c("0", "2")),
            stocn = fct_other(stocn, keep = filter_lists$stocn_list),
            scity = fct_other(scity, keep = filter_lists$scity_list),
            stscd = fct_other(stscd, keep = c("0", "1", "2")),
            ovrlt_Y = as.integer(ovrlt == "Y"),
            hcefg = fct_other(hcefg, keep = c("1", "5", "6", "7")),
            csmcu = fct_other(csmcu, keep = filter_lists$csmcu_list)
        )
    
    # PCA 如果要執行，只執行在 entry 系列數據上
    if (use_pca) {
        my_dat_entry <- my_dat %>% select(starts_with("entry_"))
        my_dat_non_entry <- my_dat %>% select(-starts_with("entry_"))
        zero_cols <- colSums(my_dat_entry) %>% {names(.[. == 0])}
        my_dat_entry <- my_dat_entry %>% select(-one_of(zero_cols))
        
        rec <- recipe( ~ ., data = my_dat_entry)
        pca_trans <- rec %>%
            step_center(all_numeric()) %>%
            step_scale(all_numeric()) %>%
            step_pca(all_numeric(), threshold = .92)
        pca_estimates <- pca_trans %>% prep(training = my_dat_entry)
        my_dat_pca <- pca_estimates %>% bake(my_dat_entry)
        my_dat <- my_dat_non_entry
        rm(my_dat_non_entry, zero_cols, rec, pca_trans, pca_estimates)
    }
    
    my_dat <- my_dat %>% 
        mutate(is_valid = ((locdt >= 61) & (dataset == "train"))) %>% 
        select(-bacno, -locdt, -loctm, -loctm_chr, -loctm_hh, -ecfg, -insfg, -ovrlt, -conam_log)
    
    rec_obj <- recipe(~ ., 
                      dat = my_dat %>% filter(dataset == "train") %>% select(-dataset, -is_valid)) %>% 
        # step_other(all_nominal(), threshold = 1e-06, other = "others") %>% 
        step_dummy(contp, etymd, mchno, acqic, mcc, stocn, scity, stscd, flbmk, hcefg, csmcu, flg_3dsmk,
                   conam_log_cat, iterm, loctm_hh_chr, locdt_mod_7,
                   one_hot = TRUE)
    
    rec_obj <- rec_obj %>% 
        prep(training = my_dat %>% filter(dataset == "train") %>% select(-dataset, -is_valid), 
             retain = FALSE, 
             strings_as_factors = TRUE)
    
    valid_ind <- which(my_dat$is_valid)
    # write_rds(valid_ind, str_c(processed_data_dir, "valid_ind.rds"))
    train_ind <- which(my_dat$dataset == "train")
    test_ind <- which(my_dat$dataset == "test")
    
    entry_count_warmstart_dat <- read_rds(str_c(mid_data_dir, "warmstart_entry_count.rds")) %>% select(starts_with("warmstart_entry_"))
    train_X <- rec_obj %>% 
        bake(new_data = my_dat %>% filter(dataset == "train") %>% select(-dataset, -is_valid))
    train_entry_X <- my_dat_entry[train_ind, ]
    train_warmstart_entry_X <- entry_count_warmstart_dat[train_ind, ]
    train_pca_X <- my_dat_pca[train_ind, ]
    test_X <- rec_obj %>% 
        bake(new_data = my_dat %>% filter(dataset == "test") %>% select(-dataset, -is_valid))
    test_entry_X <- my_dat_entry[test_ind, ]
    test_warmstart_entry_X <- entry_count_warmstart_dat[test_ind, ]
    test_pca_X <- my_dat_pca[test_ind, ]
    
    write_rds(
        list(
            train_X = train_X,
            train_entry_X = train_entry_X,
            train_warmstart_entry_X = train_warmstart_entry_X,
            train_pca_X = train_pca_X,
            train_y = train_y,
            valid_ind = valid_ind,
            test_X = test_X,
            test_entry_X = test_entry_X,
            test_warmstart_entry_X = test_warmstart_entry_X,
            test_pca_X = test_pca_X
        ),
        str_c(processed_data_dir, "processed_data.rds")
    )
    return(NULL)
}

## main
get_data()

