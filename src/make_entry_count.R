library(tidyverse)

source("./src/get_col_types.R")
data_dir <- "./sample_data/"
mid_data_dir <- "./intermediate_data/"

# Load and Process Data ---------------------------------------------------

train_dat <- read_csv(str_c(data_dir, "train.csv"), 
                      col_types = get_col_types(is_test = FALSE)) %>% 
    mutate(dataset = "train",
           loctm_chr = str_pad(loctm %>% as.integer %>% as.character, width = 6, pad = "0"),
           loctm_hh = loctm_chr %>% substr(1, 2) %>% as.integer(),
           loctm_mm = loctm_chr %>% substr(3, 4) %>% as.integer(),
           loctm_ss = loctm_chr %>% substr(5, 6) %>% as.integer())
test_dat <- read_csv(str_c(data_dir, "test.csv"), 
                     col_types = get_col_types(is_test = TRUE)) %>% 
    mutate(dataset = "test",
           loctm_chr = str_pad(loctm %>% as.integer %>% as.character, width = 6, pad = "0"),
           loctm_hh = loctm_chr %>% substr(1, 2) %>% as.integer(),
           loctm_mm = loctm_chr %>% substr(3, 4) %>% as.integer(),
           loctm_ss = loctm_chr %>% substr(5, 6) %>% as.integer())

dat <- bind_rows(train_dat, test_dat)


# Make Features -----------------------------------------------------------

## 相對於該 entry 當下時間
# +/- 3 小時 entry 數
# +/- 3 天 #entry
# +/- 3 天 有 entry 天數
# 過去 30 天 entry 數
# 過去 30 天 有 entry 天數

# Grouped by hour
grp_dat <- dat %>% 
    group_by(bacno, locdt, loctm_hh) %>% 
    summarize(entry_count = n()) %>% 
    arrange(bacno, locdt, loctm_hh)

# initialize arrays
arr_3hr <- vector("integer", nrow(grp_dat))
arr_3d <- vector("integer", nrow(grp_dat))
arr_day_3d <- vector("integer", nrow(grp_dat))
arr_last_30d <- vector("integer", nrow(grp_dat))
arr_day_last_30d <- vector("integer", nrow(grp_dat))

t0 <- Sys.time()
progress_bar <- nrow(grp_dat) %/% 100 * 3
for (i in 1:nrow(grp_dat)) {
    # progress bar
    if ((i >= progress_bar) && (i %% progress_bar == 0)) {
        print(paste0(
            round(i / nrow(grp_dat) * 100),
            "% done"
        ))
        print(Sys.time() - t0)
    }
    
    # initialize values
    this_bacno <- grp_dat$bacno[i]
    this_locdt <- grp_dat$locdt[i]
    this_loctm_hh <- grp_dat$loctm_hh[i]
    arr_3hr[[i]] <- grp_dat$entry_count[i]
    arr_3d[[i]] <- grp_dat$entry_count[i]
    arr_day_3d[[i]] <- 1
    arr_last_30d[[i]] <- grp_dat$entry_count[i]
    arr_day_last_30d[[i]] <- 1
    
    # look up the "past" of this bacno
    j <- i - 1
    l <- this_locdt # variable for recording day count
    while (j > 0) {
        if (grp_dat$bacno[j] != this_bacno) break
        if (this_locdt - grp_dat$locdt[j] <= 30) {
            if (grp_dat$locdt[j] != l) {
                arr_day_last_30d[[i]] <- arr_day_last_30d[[i]] + 1
                l <- grp_dat$locdt[j]
            }
            arr_last_30d[[i]] <- arr_last_30d[[i]] + grp_dat$entry_count[j]
        } else {
            break
        }
        if (this_locdt - grp_dat$locdt[j] <= 3) {
            if (grp_dat$locdt[j] != l) {
                arr_day_3d[[i]] <- arr_day_3d[[i]] + 1
                l <- grp_dat$locdt[j]
            }
            arr_3d[[i]] <- arr_3d[[i]] + grp_dat$entry_count[j]
            if ((this_locdt - grp_dat$locdt[j] == 0) && (this_loctm_hh - grp_dat$loctm_hh[j] <= 3)) {
                arr_3hr[[i]] <- arr_3hr[[i]] + grp_dat$entry_count[j]
            } else if ((this_locdt - grp_dat$locdt[j] == 1) && (this_loctm_hh + 24 - grp_dat$loctm_hh[j] <= 3)){
                arr_3hr[[i]] <- arr_3hr[[i]] + grp_dat$entry_count[j]
            }
        }
        j <- j - 1
    }
    
    # look up the "future" of this bacno
    j <- i + 1
    l <- this_locdt
    while (j <= nrow(grp_dat)) {
        if (grp_dat$bacno[j] != this_bacno) break
        if (grp_dat$locdt[j] - this_locdt <= 3) {
            if (grp_dat$locdt[j] != l) {
                arr_day_3d[[i]] <- arr_day_3d[[i]] + 1
                l <- grp_dat$locdt[j]
            }
            arr_3d[[i]] <- arr_3d[[i]] + grp_dat$entry_count[j]
            if ((grp_dat$locdt[j] - this_locdt == 0) && (grp_dat$loctm_hh[j] - this_loctm_hh <= 1)) {
                arr_3hr[[i]] <- arr_3hr[[i]] + grp_dat$entry_count[j]
            } else if ((grp_dat$locdt[j] - this_locdt == 1) && (grp_dat$loctm_hh[j] + 24 - this_loctm_hh <= 3)){
                arr_3hr[[i]] <- arr_3hr[[i]] + grp_dat$entry_count[j]
            }
        } else {
            break
        }
        j <- j + 1
    }
}
grp_dat$entry_count_in_3hr <- arr_3hr
grp_dat$entry_count_in_3d <- arr_3d
grp_dat$entry_day_count_in_3d <- arr_day_3d
grp_dat$entry_count_last_30d <- arr_last_30d
grp_dat$entry_day_count_last_30d <- arr_day_last_30d

grp_dat <- grp_dat %>% 
    group_by(bacno) %>% 
    nest() %>% 
    mutate(data = map(data, function(x) {
        x %>% 
            mutate(entry_count_hourly_acc = cumsum(entry_count) - entry_count)})
    ) %>% 
    unnest() %>% 
    select(-entry_count)

# debugging:
# grp_dat %>% print(n = 30)

write_rds(grp_dat, str_c(mid_data_dir, "entry_count.rds"))