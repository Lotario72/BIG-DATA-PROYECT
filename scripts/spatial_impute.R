source("../scripts/get_mode.R")
source("./data_prep.R")

library("tidyverse")
library("dplyr")

houses_bog <- dplyr::filter(bog_med, city == "Bogotá D.C")
houses_med <- dplyr::filter(bog_med, city == "Medellín")

nei_bog <- readRDS("../stores/nei_bog.Rds")
nei_med <- readRDS("../stores/nei_med.Rds")
nei_cal <- readRDS("../stores/nei_cal.Rds")

nei_bog_150 <- readRDS("../stores/nei_bog_150.Rds")
nei_med_150 <- readRDS("../stores/nei_med_150.Rds")
nei_cal_150 <- readRDS("../stores/nei_cal_150.Rds")

# Impute values using custom mode function that retrieves most repeated values
# in a vecinity
mode_imputer <- function(df, neighbors) {
    # Variables that have values to be imputed
    vars_to_impute <- c(
        "bathrooms",
        "sala_com",
        "upgrade_in",
        "upgrade_out",
        "garage",
        "light"
    )

    for (variable in vars_to_impute) {
        # Create empty column to fill with imputed values
        imputed_var <- paste0("imputed_", variable)
        df[, imputed_var] <- numeric(nrow(df))

        for (value in seq_len(nrow(df))) { # For each property in the newly created column
            # Get indices for its neighbors
            values_neighbors <- df[neighbors[[value]], variable][[1]]
            # Apply custom mode function on the currently iterated variable and set of neighbors
            imputed <- get_mode(values_neighbors)
            # Impute the obtained mode to the currently iterated property
            df[value, imputed_var] <- imputed
        }
    }
    df
}


# Repeat same algorithm as in mode_imputer, but using mean instead of mode
mean_imputer <- function(df, neighbors) {
    vars_to_impute <- c("surface_total")
    for (variable in vars_to_impute) {
        imputed_var <- paste0("imputed_", variable)
        df[, imputed_var] <- numeric(nrow(df))
        for (value in seq_len(nrow(df))) {
            values_neighbors <- df[neighbors[[value]], variable][[1]]
            imputed <- mean(values_neighbors, na.rm = TRUE)
            if (is.nan(imputed)) {
                imputed <- NA
            }
            df[value, imputed_var] <- imputed
        }
    }
    df
}


# Use the columns created by the two previous functions to fill missing values
# in their original variables

tidy_base <- function(df) {
    df <- df %>%
        mutate(
            bathrooms = if_else(
                bathrooms == 0 | is.na(bathrooms), imputed_bathrooms, bathrooms
            ),
            sala_com = if_else(
                sala_com == 0 | is.na(sala_com), imputed_sala_com, sala_com
            ),
            upgrade_in = if_else(
                upgrade_in == 0 | is.na(upgrade_in), imputed_upgrade_in, upgrade_in
            ),
            upgrade_out = if_else(
                upgrade_out == 0 | is.na(upgrade_out), imputed_upgrade_out, upgrade_out
            ),
            garage = if_else(
                garage == 0 | is.na(garage), imputed_garage, garage
            ),
            light = if_else(
                light == 0 | is.na(light), imputed_light, light
            ),
            surface_total = if_else(
                surface_total == 0 | is.na(surface_total), imputed_surface_total, surface_total
            )
        ) %>%
        select(-starts_with("imputed"))

    df
}

# Use search perimeter of 50 meters to impute - Bogota
houses_bog <- mode_imputer(houses_bog, nei_bog)
houses_bog <- mean_imputer(houses_bog, nei_bog)
houses_bog <- tidy_base(houses_bog)

# Use search perimeter of 150 meters to impute remaining missing values - Bogota
houses_bog <- mode_imputer(houses_bog, nei_bog_150)
houses_bog <- mean_imputer(houses_bog, nei_bog_150)
houses_bog <- tidy_base(houses_bog)

saveRDS(houses_bog, "../stores/imputed_bog.Rds")

# Use search perimeter of 50 meters to impute - Medellin
houses_med <- mode_imputer(houses_med, nei_med)
houses_med <- mean_imputer(houses_med, nei_med)
houses_med <- tidy_base(houses_med)

# Use search perimeter of 150 meters to impute remaining missing values - Medellin
houses_med <- mode_imputer(houses_med, nei_med_150)
houses_med <- mean_imputer(houses_med, nei_med_150)
houses_med <- tidy_base(houses_med)

saveRDS(houses_med, "../stores/imputed_med.Rds")

# Use search perimeter of 50 meters to impute - Cali
houses_cal <- mode_imputer(houses_cal, nei_cal)
houses_cal <- mean_imputer(houses_cal, nei_cal)
houses_cal <- tidy_base(houses_cal)

# Use search perimeter of 150 meters to impute remaining missing values - Cali
houses_cal <- mode_imputer(houses_cal, nei_cal_150)
houses_cal <- mean_imputer(houses_cal, nei_cal_150)
houses_cal <- tidy_base(houses_cal)

saveRDS(houses_cal, "../stores/imputed_cal.Rds")
