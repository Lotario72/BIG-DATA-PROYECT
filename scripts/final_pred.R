source("../scripts/stack_mod.R")
source("../scripts/final_workflows.R")


wf_clas <- workflows("rf", "clas")
fit_clas <- fit(wf_clas, data_train_clas)
pred_clas <- predict(fit_clas, data_test)

wf_reg <- workflows("rf", "reg")
fit_reg <- fit(wf_reg, data_train_reg)
pred_reg <- predict(fit_reg, data_test)
pred_reg <- exp(pred_reg)
pred_reg_zeroes <- ifelse(pred_reg < data_test$Lp, 1, 0)

submission <- read.csv("/stores/submission_template.csv")

submission$classification_model <- pred_clas$.pred_class
submission$regression_model <- pred_reg_zeroes


write.table(
    submission,
    file = "../stores/submission_template.csv",
    sep = ",",
    row.names = FALSE
)
