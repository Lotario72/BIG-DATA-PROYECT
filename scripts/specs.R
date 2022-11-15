library("tidymodels")
library("ranger")
library("xgboost")

specs <- function(model) {
    # Linear - Regression
    if (model == "lm") {
        spec <- linear_reg() %>%
            set_engine("lm")
    }

    # Elastic - Regression
    if (model == "elastic") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")
    }

    # Ridge - Regression
    if (model == "ridge") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = 0
        ) %>%
            set_engine("glmnet")
    }

    # Lasso - Regression
    if (model == "lasso") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = 1
        ) %>%
            set_engine("glmnet")
    }


    # RF - Regression
    if (model == "rf") {
        spec <- rand_forest(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
        ) %>%
            set_engine(
                "ranger",
                importance = "impurity",
                verbose = TRUE,
                num.threads = 3
            ) %>%
            set_mode("regression")
    }

    # XGB - Regression
    if (model == "xgb") {
        spec <- boost_tree(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
            sample_size = tune(),
            stop_iter = tune(),
            tree_depth = tune(),
            learn_rate = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost", nthread = 3) %>%
            set_mode("regression")
    }


    spec
}
