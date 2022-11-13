library("stacks")

lm <- readRDS("../stores/result_lm.Rds")
ridge <- readRDS("../stores/result_ridge.Rds")
lasso <- readRDS("../stores/result_lasso.Rds")
elastic <- readRDS("../stores/result_elastic.Rds")
rf <- readRDS("../stores/result_rf.Rds")
xgb <- readRDS("../stores/result_xgb.Rds")

final_st <-
    # initialize the stack
    stacks() %>%
    # add each of the models
    add_candidates(lm) %>%
    add_candidates(ridge) %>%
    add_candidates(lasso) %>%
    add_candidates(elastic) %>%
    add_candidates(rf) %>%
    add_candidates(xgb) %>%
    blend_predictions() %>% # evaluate candidate models
    fit_members()

final_pred <- predict(final_st, validation) %>%
    bind_cols(validation)
data.frame((final_pred$price * 899818886) + 869755897 - (final_pred$.pred * 30000000) + 869755897)
