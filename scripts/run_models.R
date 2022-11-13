system.time({
    source("../scripts/lasso_reg.R")
    source("../scripts/ridge_reg.R")
    source("../scripts/elastic_reg.R")

    source("../scripts/rf_reg.R")
    autoplot(result)
    ggsave("../stores/rf_reg.png")

    source("../scripts/xgb_reg.R")
    autoplot(result)
    ggsave("../stores/xgb_reg.png")
})

worst_vars_lin <- function(wf, thresh) {
    # Check coefficients
    wf %>%
        fit(validation) %>%
        tidy() %>%
        .[which(abs(.$estimate) < thresh), "term"]
}

worst_vars_tree <- function(wf, thresh) {
    # See variables arranged by importance
    wf %>%
        fit(validation) %>%
        extract_fit_parsnip() %>%
        vip::vi() %>%
        .[which(abs(.$Importance) < thresh), "Variable"]
}

res_ridge <- readRDS("../stores/bestwf_ridge.Rds")
res_lasso <- readRDS("../stores/bestwf_lasso.Rds")
res_elastic <- readRDS("../stores/bestwf_elastic.Rds")
res_rf <- readRDS("../stores/bestwf_rf.Rds")
res_xgb <- readRDS("../stores/bestwf_xgb.Rds")

worst_ridge <- worst_vars_lin(res_ridge, 0.1)
worst_lasso <- worst_vars_lin(res_lasso, 0.1)
worst_elastic <- worst_vars_lin(res_elastic, 0.1)
worst_rf <- worst_vars_tree(res_rf, 2)
worst_xgb <- worst_vars_tree(res_xgb, 0.02)

# Load worst variables from previous iteration
previous_worst <- readRDS("../stores/worst_vars.Rds")

# Update worst variables using variables from current iteration
worst_vars <- c(
    previous_worst,
    intersect(
        intersect(
            intersect(
                intersect(
                    worst_ridge$term,
                    worst_lasso$term
                ),
                worst_elastic$term
            ),
            worst_rf$Variable
        ),
        worst_xgb$Variable
    )
)

worst_vars <- unique(worst_vars)

saveRDS(worst_vars, "../stores/worst_vars.Rds")
