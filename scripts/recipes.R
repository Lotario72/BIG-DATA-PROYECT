source("../scripts/get_mode.R")
library("tidyverse")
library("tidymodels")

houses_bog <- readRDS("../stores/lum_dist_vars_imputed_bog.Rds")
houses_med <- readRDS("../stores/lum_dist_vars_imputed_med.Rds")

std_scaler <- sd(houses_med$price) / sd(houses_bog$price)
houses_bog$price <- houses_bog$price * std_scaler
mean_transformer <- mean(houses_med$price) - mean(houses_bog$price)
houses_bog$price <- houses_bog$price + mean_transformer

data <- rbind(houses_bog, houses_med)
set.seed(10)
data <- data %>%
    group_by(city) %>%
    slice_sample(n = 500) %>%
    ungroup()

predictors <- data %>%
    select(-price) %>%
    names()

# Create recipe

data <- recipe(~., data = data) %>%
    update_role(price, new_role = "outcome") %>%
    update_role(all_of(!!predictors), new_role = "predictor") %>%
    # step_dummy(
    #     city,
    #     house,
    #     sala_com,
    #     upgrade_in,
    #     upgrade_out,
    #     garage,
    #     light
    # ) %>%
    prep() %>%
    bake(new_data = NULL)

# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.8)
train <- data_split %>% training()
test <- data_split %>% testing()

set.seed(10)
validation_split <- vfold_cv(train, v = 5)

# Recipes to prepare data for classification
rec_reg <- recipe(price ~ ., data = train) %>%
    step_impute_mean(surface_total, lum_val) %>%
    step_mutate_at(
        all_of(
            c(
                "bedrooms",
                "bathrooms",
                "sala_com",
                "upgrade_in",
                "upgrade_out",
                "garage",
                "light"
            )
        ),
        fn = get_mode
    ) %>%
    step_poly(
        surface_total,
        lum_val,
        starts_with("less"), starts_with("closest"),
        degree = 3
    ) %>%
    step_interact(
        terms = ~ house:all_predictors() +
            city:all_predictors()
    )
