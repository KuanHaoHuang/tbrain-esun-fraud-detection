library(tidyverse)

# helper funtion for loading csv
get_col_types <- function(is_test = FALSE) {
    this_cols <- cols(
        bacno = col_character(),
        txkey = col_skip(),
        locdt = col_integer(),
        loctm = col_character(),
        cano = col_skip(),
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
    )
    if (is_test) {
        this_cols$cols$fraud_ind <- NULL
    }
    return(this_cols)
}