source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/grids.R")
source("../scripts/tuning.R")

wf <- workflows("ridge")
grid <- grids("ridge")
result <- wf %>% tuning(grid, validation_split)
result %>% collect_metrics()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)

# Check coefficients
final_wf %>%
    fit(validation) %>%
    tidy() %>%
    print(n = Inf)

# Fit on training, predict on test, and report performance
lf <- last_fit(final_wf, data_split)
# Performance metric on test set
metric <- rmse(
    data.frame(
        test["Ingpcug"],
        lf %>% extract_workflow() %>% predict(test)
    ),
    Ingpcug,
    .pred
)$.estimate

# Final report for this model
report <- data.frame(
    Problema = "Reg.", Modelo = "Ridge",
    Penalidad = "N/A", Mixtura = "0",
    result %>% show_best(n = 1) %>% mutate(mean = metric)
)

saveRDS(report, file = "../stores/ridge_reg.rds")
