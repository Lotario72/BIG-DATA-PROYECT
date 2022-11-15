library("tidymodels")
source("../scripts/specs.R")
grids <- function(model) {
    # Lasso parameters
    penalty_lasso <- seq(0.018, 0.02, length.out = 50)

    # Ridge/elastic parameters
    penalty_ridge <- seq(0.27, 0.33, length.out = 50)


    # Elastic parameters
    penalty_elastic <- seq(0.023, 0.029, length.out = 50)
    mixture <- seq(0.8, 0.91, length.out = 50)

    # RF parameters
    mtry <- floor(seq(5, 15, length.out = 5))
    min_n <- floor(seq(5, 15, length.out = 5))
    trees <- floor(seq(250, 1000, length.out = 5))


    # XGB parameters
    sample_size <- c(0.72, 0.92)
    mtry_xgb <- c(4, 12)
    min_n_xgb <- c(5, 15)
    tree_depth <- c(6, 10)
    learn_rate <- c(0.003, 0.007)
    loss_reduction <- c(0.00000001, 0.0001)
    trees_xgb <- c(1000, 1600)
    stop_iter <- c(3, 5)


    # Lasso - Regression
    if (model == "lasso") {
        grid <- expand.grid(
            penalty = penalty_lasso
        )
    }

    # Ridge - Regression
    if (model == "ridge") {
        grid <- expand.grid(
            penalty = penalty_ridge
        )
    }

    # Elastic - Regression
    if (model == "elastic") {
        grid <- expand.grid(
            penalty = penalty_elastic,
            mixture = mixture
        )
    }


    # RF - Regression
    if (model == "rf") {
        grid <- expand.grid(
            mtry = mtry,
            min_n = min_n,
            trees = trees
        )
    }


    # XGB - Regression
    if (model == "xgb") {
        spec <- specs("xgb")
        params <- extract_parameter_set_dials(spec)
        params <- params %>% update(
            mtry = mtry_xgb,
            min_n = min_n_xgb,
            sample_size = sample_size,
            tree_depth = tree_depth,
            learn_rate = learn_rate,
            loss_reduction = loss_reduction,
            trees = trees_xgb,
            stop_iter = stop_iter
        )
        # grid <- expand.grid(
        #     mtry = mtry_xgb,
        #     min_n = min_n_xgb,
        #     sample_size = sample_size,
        #     tree_depth = tree_depth,
        #     learn_rate = learn_rate,
        #     loss_reduction = loss_reduction,
        #     trees = trees_xgb,
        #     stop_iter = stop_iter
        # )
    }


    grid
}
