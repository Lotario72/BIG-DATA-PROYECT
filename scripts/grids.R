grids <- function(model) {
    # Lasso/ridge/elastic parameters
    penalty <- seq(0.0001, 0.001, length.out = 100)
    mixture <- seq(0.1, 0.9, length.out = 4)

    # RF parameters
    mtry <- c(5, 8, 12)
    min_n <- c(10, 20, 30)
    trees <- c(200, 300, 400)


    # XGB parameters
    sample_size <- c(1)
    mtry_xgb <- c(8)
    min_n_xgb_reg <- c(20, 40, 100)
    tree_depth <- c(6, 8, 10)
    learn_rate <- c(0.00001, 0.001, 0.01)
    loss_reduction <- c(0.00001, 0.001, 0.01)


    # Lasso - Regression
    if (model == "lasso") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 1
        )
    }

    # Ridge - Regression
    if (model == "ridge") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 0
        )
    }

    # Elastic - Regression
    if (model == "elastic") {
        grid <- expand.grid(
            penalty = penalty,
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
        grid <- expand.grid(
            mtry = mtry_xgb,
            min_n = min_n_xgb_reg,
            sample_size = sample_size,
            tree_depth = tree_depth,
            learn_rate = learn_rate,
            loss_reduction = loss_reduction
        )
    }


    grid
}
