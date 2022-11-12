library("stacks")

ridge <- readRDS("../stores/bestwf_ridge.R")
lasso <- readRDS("../stores/bestwf_lasso.R")
elastic <- readRDS("../stores/bestwf_elastic.R")
rf <- readRDS("../stores/bestwf_rf.R")
xgb <- readRDS("../stores/bestwf_xgb.R")

final_st <-
    # initialize the stack
    stacks() %>%
    # add each of the models
    add_candidates(ridge) %>%
    add_candidates(lasso) %>%
    add_candidates(elastic) %>%
    add_candidates(rf) %>%
    add_candidates(xgb) %>%
    blend_predictions() %>% # evaluate candidate models
    fit_members()

final_pred <- predict(final, test) %>%
    bind_cols(test)
