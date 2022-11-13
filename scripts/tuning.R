library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()

tuning <- function(object,
                   grid,
                   resamples,
                   model = "other",
                   ...) {
    if (model == "lm") {
        tune <- tune::tune_grid(
            object = object,
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    } else {
        tune <- tune::tune_grid(
            object = object,
            grid = 100,
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    tune
}
