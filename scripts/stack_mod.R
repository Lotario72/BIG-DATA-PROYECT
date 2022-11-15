library("stacks")
source("../scripts/recipes.R")


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

houses_cal <- readRDS("../stores/lum_dist_vars_imputed_cal.Rds")


houses_cal <- houses_cal %>% mutate(
  across(
    c(
      city,
      house,
      sala_com,
      upgrade_in,
      upgrade_out,
      garage,
      light,
      estrato
    ),
    as.factor
  )
)


houses_cal <- recipe(~., data = houses_cal) %>%
  step_rm(property_id, city) %>%
  update_role(price, new_role = "outcome") %>%
  update_role(all_of(!!predictors), new_role = "predictor") %>%
  update_role(all_numeric_predictors(), new_role = "num_pred") %>%
  step_impute_mean(
    surface_total, lum_val) %>%
  step_impute_median(bathrooms) %>%
  step_impute_mode(   
    sala_com, upgrade_in, upgrade_out, garage, light, estrato) %>%
  step_dummy(
    # city,
    house,
    sala_com,
    upgrade_in,
    upgrade_out,
    garage,
    light,
    estrato
  ) %>%
  prep() %>%
  bake(new_data = NULL)


final_pred <- predict(final_st, houses_cal) %>%
    bind_cols(houses_cal)

valor = 601842533/sd(final_pred$.pred)
final_pred$price = final_pred$.pred*valor

valor_2 = 555314430 - mean(final_pred$price)

final_pred$price = final_pred$price+valor_2

 ggplot(formulario, aes(x = price)) +
  geom_histogram(bins = 50,color = "grey30", fill = "red") +
  ggtitle("price") +
  labs(x = "NA", y = "NA") +
  theme_bw()


formulario = read_delim("../stores/submission_template.csv")

formulario$price = final_pred$price
formulario = mutate(formulario, price = ifelse(
  price<0,price*-1,price
))


write.csv(formulario,"../stores/predictions_bonilla_santofimio_velasquez.csv", row.names = FALSE)
