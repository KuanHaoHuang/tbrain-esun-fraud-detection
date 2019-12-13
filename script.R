CLEAN_START <- TRUE

if (CLEAN_START) {
    for (dr in c("result", "processed_data", "intermediate_data")) {
        for (f in list.files(dr, full.names = TRUE)) file.remove(f)
    }
    
}

source("./src/make_entry_count.R")
source("./src/make_entry_count_2.R")
source("./src/make_warmstart_entry_count.R")
source("./src/make_warmstart_split.R")
source("./src/process_data.R")
source("./src/train_initial_single_model.R")
source("./src/train_warmstart_single_model.R")