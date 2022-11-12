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
data <- data %>% mutate(
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

price_std <- (data$price - mean(data$price)) / sd(data$price)
outliers <- which(abs(price_std) > 3)
data <- data[-outliers]


levels(data$city) <- c("Bogota", "Medellin")

# set.seed(10)
# data <- data %>%
#     group_by(city) %>%
#     slice_sample(n = 500) %>%
#     ungroup()

predictors <- data %>%
    select(-price) %>%
    names()

# Create recipe to perform initial transformation

data <- recipe(~., data = data) %>%
    update_role(price, new_role = "outcome") %>%
    update_role(all_of(!!predictors), new_role = "predictor") %>%
    update_role(all_numeric_predictors(), new_role = "num_pred") %>%
    step_dummy(
        city,
        house,
        sala_com,
        upgrade_in,
        upgrade_out,
        garage,
        light, estrato
    ) %>%
    prep() %>%
    bake(new_data = NULL)

# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.85)
train <- data_split %>% training()
test <- data_split %>% testing()

# Distuinguish between validation and training sets
train$id <- seq(nrow(train))
set.seed(10)
train_temp <- train %>% dplyr::sample_frac(0.70)
validation <- dplyr::anti_join(train, train_temp, by = "id") %>% select(-id)
train <- train_temp %>% select(-id)

rm(train_temp)


set.seed(10)
validation_split <- vfold_cv(validation, v = 5)

# Recipe to prepare data for regression
rec_reg <- recipe(price ~ ., data = validation) %>%
    step_impute_mean(surface_total, lum_val) %>%
    step_mutate_at(
        all_of(
            c(
                "bedrooms",
                "bathrooms",
                "sala_com_X1",
                "upgrade_in_X1",
                "upgrade_out_X1",
                "garage_X1",
                "light_X1"
            )
        ),
        fn = get_mode
    ) %>%
    step_normalize(has_role("num_pred"))
# %>%
# step_poly(
#     surface_total,
#     lum_val,
#     starts_with("less"), starts_with("closest"),
#     degree = 3
# )
