library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()

tuning <- function(object,
                   #    grid,
                   #    params,
                   resamples,
                   model,
                   ...) {
    if (model == "lm") {
        tune <- tune::tune_grid(
            object = object,
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "ridge") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "lasso") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "elastic") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                mixture(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "rf") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(10, 11)),
                min_n(c(5, 6)),
                trees(c(1250, 1500)),
                size = 10
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "xgb") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(8, 12)),
                min_n(c(3, 5)),
                sample_prop(c(0.85, 0.9)),
                tree_depth(c(11, 13)),
                learn_rate(c(0.003, 0.01)),
                loss_reduction(c(0.00000005, 0.01)),
                trees(c(100, 300)),
                stop_iter(c(2, 5)),
                size = 20
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    tune
}
