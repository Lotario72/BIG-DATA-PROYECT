library("tidyverse")
library("sf")
library("osmdata")
library("spdep")
library("dplyr")

bog_med <- readRDS("../stores/train.Rds")
cal <- readRDS("../stores/test.Rds")

houses_preproc <- function(houses) {
    houses <- houses %>% dplyr::select(-c(rooms, title, operation_type))
    # Create binary variable for property type to replace property_type variable
    houses <- houses %>% dplyr::mutate(
        house = ifelse(property_type == "Casa", 1, 0)
    )
    houses <- houses %>% dplyr::select(-property_type)
    # Impute missing data in surface_total using surface_covered
    houses <- houses %>% dplyr::mutate(
        surface_total = dplyr::if_else(
            is.na(surface_total), surface_covered, surface_total
        )
    )
    houses <- houses %>% dplyr::select(-surface_covered)

    # Normalize descriptions to lower case
    houses$description <- tolower(houses$description)
    # Replace decimal comma with decimal point in descriptions
    houses$description <- stringr::str_replace_all(
        houses$description,
        "(\\d),(\\d)",
        "\\1.\\2"
    )

    # Use descriptions to retrieve property areas
    areas_retrieved <- stringr::str_match(
        houses$description,
        " (\\d*\\.?\\d+)\\s?m(t|etro|2|\\s|:punct:)"
    )[, 2]

    # Detect cases where points have been used to mark thousands
    point_thousands <- stringr::str_detect(areas_retrieved, "^\\d\\.\\d{3}")
    point_thousands[is.na(point_thousands)] <- FALSE
    # Remove points marking thousands
    areas_retrieved[point_thousands] <- stringr::str_replace_all(
        areas_retrieved[point_thousands],
        "\\.",
        ""
    )
    # Convert values to numerical
    areas_retrieved <- as.numeric(areas_retrieved)
    # Remove values less than 15 (potential errors in parsing)
    areas_retrieved[areas_retrieved < 15] <- NA
    # Use only 1 decimal figure
    houses$areas_retrieved <- round(areas_retrieved, 1)
    houses <- houses %>% dplyr::mutate(
        surface_total = dplyr::if_else(
            is.na(surface_total), areas_retrieved, surface_total
        )
    )
    houses <- houses %>% dplyr::select(-areas_retrieved)

    houses <- houses %>% dplyr::mutate(
        sala_com = dplyr::if_else(
            stringr::str_detect(
                description, "sala|comedor"
            ), 1, 0
        ),
        upgrade_in = dplyr::if_else(
            stringr::str_detect(
                description,
                "chimenea|terraza|social|balc.?n|balcã.n|balc&\\w{6};n"
            ), 1, 0
        ),
        upgrade_out = dplyr::if_else(
            stringr::str_detect(
                description,
                "gimnasio|gym|infantil|ni.?os|jard.?n|niã.os|jardã.n|ni&\\w{6};os|jard&\\w{6};n"
            ), 1, 0
        ),
        garage = dplyr::if_else(
            stringr::str_detect(description, "garaje|garage|parqueadero"), 1, 0
        ),
        light = dplyr::if_else(
            stringr::str_detect(
                description,
                "iluminado|iluminaci.?n|iluminaciã.n|iluminaci&\\w{6};n|luz natural"
            ),
            1, 0
        )
    )

    houses <- houses %>% dplyr::select(-c(description))
    houses
}

# Transform training data
bog_med <- houses_preproc(bog_med)
# Transfom test data
houses_cal <- houses_preproc(cal)
