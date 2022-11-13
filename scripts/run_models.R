source("../scripts/lasso_reg.R")
source("../scripts/ridge_reg.R")
source("../scripts/elastic_reg.R")

source("../scripts/rf_reg.R")
autoplot(result)
ggsave("../stores/rf_reg.png")

source("../scripts/xgb_reg.R")
autoplot(result)
ggsave("../stores/xgb_reg.png")
