library("stacks")

lm <- readRDS("../stores/bestwf_lm.R")
ridge <- readRDS("../stores/bestwf_ridge.R")
lasso <- readRDS("../stores/bestwf_lasso.R")
elastic <- readRDS("../stores/bestwf_elastic.R")
rf <- readRDS("../stores/bestwf_rf.R")
xgb <- readRDS("../stores/bestwf_xgb.R")

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
