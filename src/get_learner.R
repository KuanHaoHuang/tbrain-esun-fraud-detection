library(mlr)

# helper function for mlr classifier creation
get_learner <- function(kw) {
    if (kw == "xgb" || kw == "xgb_tuned") {
        lrn <- suppressWarnings(
            makeLearner(
                "classif.xgboost",
                predict.type = "prob",
                par.vals = list(
                    objective = "binary:logistic",
                    eval_metric = "auc"
                )
            )
        )
        lrn$par.set <- c(
            lrn$par.set,
            suppressWarnings(
                makeParamSet(makeNumericLearnerParam("scale_pos_weight"))
            )
        )
    } else if (kw == "ranger") {
        lrn <- makeLearner("classif.ranger", predict.type = "prob")
    } else {
        print("unknown model keyword")
        return (NA)
    } 
    return(lrn)
}