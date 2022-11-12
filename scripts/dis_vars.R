# Get two values with respect of a geograpghic feature:
# 1) Number of points of that kind within 500 meters from a specific house
# 2) Distance to the closest point of that kind from a specific house
vars_from_dist <- function(city, value) {
    # Load set of polygons created by get_dist.R for a specific feature
    path <- paste0("../stores/dist_", city, "_", value, ".Rds")
    data <- readRDS(path)
    # Create an empty dataframe to store vales of two variables
    less_500m_name <- paste0("less_500m_", value)
    closest_name <- paste0("closest_", value)
    grouped_vars <- data.frame(numeric(nrow(data)), numeric(nrow(data)))
    colnames(grouped_vars) <- c(less_500m_name, closest_name)

    for (i in seq_len(nrow(data))) { # For each house
        # Get all distances from the house to the polygons that belong to a category
        y <- data[i, ] %>% as.numeric()
        # Count how many of those polygons are within 500 meters from the house
        less_500m <- y[y < 500] %>% length()
        # Determine the closest polygon
        closest <- min(y)
        # Associate retrieved values to the house being iterated on
        grouped_vars[i, less_500m_name] <- less_500m
        grouped_vars[i, closest_name] <- closest
    }
    grouped_vars
}

houses_bog <- readRDS("../stores/imputed_bog.Rds")
houses_med <- readRDS("../stores/imputed_med.Rds")
houses_cal <- readRDS("../stores/imputed_cal.Rds")
keyvals <- read.csv("../stores/st_maps_key_val.csv")

# Iterate through each key-value pair from the matrix of variables of interest
# and find the values that were difined by the function vars_from_dist
# for a given city
append_dist_vars <- function(df, city) {
    for (i in keyvals[, "value"]) {
        vars <- vars_from_dist(city, i)
        df <- cbind(df, vars)
    }
    df
}

houses_bog <- append_dist_vars(houses_bog, "bog")
saveRDS(houses_bog, "../stores/dist_vars_imputed_bog.Rds")
houses_med <- append_dist_vars(houses_med, "med")
saveRDS(houses_med, "../stores/dist_vars_imputed_med.Rds")
houses_cal <- append_dist_vars(houses_cal, "cal")
saveRDS(houses_cal, "../stores/dist_vars_imputed_cal.Rds")
