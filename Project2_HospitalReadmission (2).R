
# loading the packages (ggplot2 for charts, dplyr for tables,
# tidymodels for the models)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(rpart.plot)

# setting a seed so I get the same results every time I run it
set.seed(123)

# making a folder to save tables and charts
if (!dir.exists("outputs")) dir.create("outputs")


# STEP 1: Read the data and check it


# missing values in this data are "?" so I am reading them as NA
df <- read.csv("diabetic_data.csv", na.strings = "?")

# checking the size matches the data dictionary
# (should be 101,766 rows, 50 columns and 71,518 patients)
dim(df)
length(unique(df$patient_nbr))
table(df$readmitted)

# the mapping file tells what each ID code means. I only print it to look
# at the codes (the groups in STEP 5 come from it), so the code still runs
# if the file is not in the folder
if (file.exists("IDS_mapping.csv")) {
  id_map <- readLines("IDS_mapping.csv")
  print(id_map)
}

# STEP 2: Missing values (Section 3.6)
# counting how many "?" are in each column
miss_count <- colSums(is.na(df))
miss_tab <- data.frame(variable = names(miss_count),
                            missing = miss_count,
                            percent = round(100 * miss_count / nrow(df), 2))
miss_tab <- miss_tab[miss_tab$missing > 0, ]

# some ID columns use codes like NULL or Not Mapped instead of "?"
# so I am counting those separately
coded_na <- data.frame(
  variable = c("admission_type_id", "admission_source_id", "discharge_disposition_id"),
  missing = c(sum(df$admission_type_id %in% c(5, 6, 8)),
              sum(df$admission_source_id %in% c(9, 15, 17, 20, 21)),
              sum(df$discharge_disposition_id %in% c(18, 25, 26))))
coded_na$percent <- round(100 * coded_na$missing / nrow(df), 2)

miss_tab <- rbind(miss_tab, coded_na)
miss_tab <- miss_tab[order(-miss_tab$percent), ]
print(miss_tab)
write.csv(miss_tab, "outputs/missing_values.csv", row.names = FALSE)

miss_plot <- ggplot(miss_tab, aes(x = reorder(variable, percent), y = percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Fields with missing or unknown values",
       x = "", y = "Percent of encounters") +
  theme_minimal()
ggsave("outputs/fig_missing_values.png", miss_plot, width = 8, height = 4.5)

# STEP 3: Remove records that do not fit the follow-up decision

# patients who died or went to hospice can't come back or get follow-up,
# so I am taking them out. I also remove stays that didn't really end,
# the newborn code and the 3 rows where gender is Unknown/Invalid
nrow(df)                                                     # 101,766

df <- df[!df$discharge_disposition_id %in% c(11, 19, 20, 21), ]
nrow(df)                                                     # after removing deaths

df <- df[!df$discharge_disposition_id %in% c(13, 14), ]
nrow(df)                                                     # after removing hospice

df <- df[!df$discharge_disposition_id %in% c(9, 12), ]
nrow(df)                                                     # after removing stays that did not end

df <- df[!df$discharge_disposition_id %in% c(10), ]
nrow(df)                                                     # after removing neonatal code

df <- df[df$gender != "Unknown/Invalid", ]
nrow(df)                                                     # final: 99,310


# STEP 4: Create the target variable (Sections 2.4 and 3.4)
# my target is readmitted within 30 days (Yes/No). ">30" and "NO" both
# go to No because the follow-up program only cares about the first 30 days.
# I put Yes as the first level so R treats Yes as the event
df$readmit_30 <- ifelse(df$readmitted == "<30", "Yes", "No")
df$readmit_30 <- factor(df$readmit_30, levels = c("Yes", "No"))

table(df$readmit_30)
round(100 * prop.table(table(df$readmit_30)), 1)       # about 11.4% Yes

target_plot <- ggplot(df, aes(x = readmit_30, fill = readmit_30)) +
  geom_bar(width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "darkorange", "No" = "steelblue")) +
  labs(title = "Readmitted within 30 days", x = "", y = "Number of encounters") +
  theme_minimal()
ggsave("outputs/fig_target.png", target_plot, width = 6, height = 4)

# STEP 5: Group the codes into categories (Section 3.9)
# admission type - emergency, urgent, elective, and the rest as one group
df$admission_type <- "Unknown/Other"
df$admission_type[df$admission_type_id == 1] <- "Emergency"
df$admission_type[df$admission_type_id == 2] <- "Urgent"
df$admission_type[df$admission_type_id == 3] <- "Elective"

# discharge destination into 7 groups
df$discharge_group <- "Unknown"
df$discharge_group[df$discharge_disposition_id == 1] <- "Home"
df$discharge_group[df$discharge_disposition_id %in% c(6, 8)] <- "Home with home health"
df$discharge_group[df$discharge_disposition_id %in% c(3, 4, 24)] <- "Skilled nursing / ICF"
df$discharge_group[df$discharge_disposition_id == 22] <- "Rehab facility"
df$discharge_group[df$discharge_disposition_id %in% c(2, 5, 15, 16, 17, 23, 27, 28, 29, 30)] <- "Other facility or referral"
df$discharge_group[df$discharge_disposition_id == 7] <- "Left against medical advice"

# admission source into 4 groups
df$admission_source <- "Unknown/Other"
df$admission_source[df$admission_source_id == 7] <- "Emergency room"
df$admission_source[df$admission_source_id %in% c(1, 2, 3)] <- "Physician/clinic referral"
df$admission_source[df$admission_source_id %in% c(4, 5, 6, 10, 18, 19, 22, 25, 26)] <- "Transfer from facility"

# specialty - keeping the top 8, rest is "Other" and missing is "Unknown"
top_8 <- names(sort(table(df$medical_specialty), decreasing = TRUE))[1:8]
df$specialty <- "Other"
df$specialty[df$medical_specialty %in% top_8] <- df$medical_specialty[df$medical_specialty %in% top_8]
df$specialty[is.na(df$medical_specialty)] <- "Unknown"

# race - missing becomes "Not recorded" (I only use race for the fairness check)
df$race_group <- ifelse(is.na(df$race), "Not recorded", df$race)

# there are more than 700 diagnosis codes, so I am grouping them
# into 9 groups based on the ICD-9 code ranges
group_diagnosis <- function(code) {
  number <- suppressWarnings(as.numeric(code))
  group <- rep("Other", length(code))
  group[!is.na(number) & number >= 390 & number <= 459] <- "Circulatory"
  group[!is.na(number) & floor(number) == 785] <- "Circulatory"
  group[!is.na(number) & number >= 460 & number <= 519] <- "Respiratory"
  group[!is.na(number) & floor(number) == 786] <- "Respiratory"
  group[!is.na(number) & number >= 520 & number <= 579] <- "Digestive"
  group[!is.na(number) & floor(number) == 787] <- "Digestive"
  group[!is.na(number) & floor(number) == 250] <- "Diabetes"
  group[!is.na(number) & number >= 800 & number <= 999] <- "Injury"
  group[!is.na(number) & number >= 710 & number <= 739] <- "Musculoskeletal"
  group[!is.na(number) & number >= 580 & number <= 629] <- "Genitourinary"
  group[!is.na(number) & floor(number) == 788] <- "Genitourinary"
  group[!is.na(number) & number >= 140 & number <= 239] <- "Neoplasms"
  group[is.na(code)] <- "Missing"
  group
}
df$diag1_group <- group_diagnosis(df$diag_1)
df$diag2_group <- group_diagnosis(df$diag_2)
df$diag3_group <- group_diagnosis(df$diag_3)

table(df$diag1_group)

# STEP 6: Summary statistics and zero values (Sections 3.7 and 3.8)

num_cols <- c("time_in_hospital", "num_lab_procedures", "num_procedures",
                  "num_medications", "number_outpatient", "number_emergency",
                  "number_inpatient", "number_diagnoses")

summary(df[, num_cols])

sum_tab <- data.frame(
  variable = num_cols,
  mean = round(sapply(df[, num_cols], mean), 2),
  median = sapply(df[, num_cols], median),
  sd = round(sapply(df[, num_cols], sd), 2),
  min = sapply(df[, num_cols], min),
  max = sapply(df[, num_cols], max),
  zeros = sapply(df[, num_cols], function(x) sum(x == 0)))
sum_tab$zero_percent <- round(100 * sum_tab$zeros / nrow(df), 1)
print(sum_tab)
write.csv(sum_tab, "outputs/summary_statistics.csv", row.names = FALSE)

# the zeros here are real (like no visits last year), they are not missing.
# comparing the readmission rate for zero vs more than zero
prop.table(table(df$number_inpatient > 0, df$readmit_30), 1)
prop.table(table(df$number_emergency > 0, df$readmit_30), 1)
prop.table(table(df$number_outpatient > 0, df$readmit_30), 1)
prop.table(table(df$num_procedures > 0, df$readmit_30), 1)

# STEP 7: Predictor analysis (Part 4)
overall <- mean(df$readmit_30 == "Yes")

# readmission rate by number of hospital stays last year (5+ together)
df$prior_stays <- ifelse(df$number_inpatient >= 5, "5+",
                               as.character(df$number_inpatient))
inp_rates <- df %>%
  group_by(prior_stays) %>%
  summarise(patients = n(), rate = 100 * mean(readmit_30 == "Yes"))
print(inp_rates)

inp_plot <- ggplot(inp_rates, aes(x = prior_stays, y = rate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100 * overall, linetype = "dashed") +
  labs(title = "Readmission rate by inpatient stays in the prior year",
       x = "Prior inpatient stays", y = "Readmitted within 30 days (%)") +
  theme_minimal()
ggsave("outputs/fig_prior_inpatient.png", inp_plot, width = 7, height = 4)

# readmission rate by where the patient went after discharge
dis_rates <- df %>%
  group_by(discharge_group) %>%
  summarise(patients = n(), rate = 100 * mean(readmit_30 == "Yes"))
print(dis_rates)

dis_plot <- ggplot(dis_rates, aes(x = reorder(discharge_group, rate), y = rate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100 * overall, linetype = "dashed") +
  coord_flip() +
  labs(title = "Readmission rate by discharge destination",
       x = "", y = "Readmitted within 30 days (%)") +
  theme_minimal()
ggsave("outputs/fig_discharge.png", dis_plot, width = 7, height = 4)

# readmission rate by age group
age_rates <- df %>%
  group_by(age) %>%
  summarise(patients = n(), rate = 100 * mean(readmit_30 == "Yes"))
print(age_rates)

age_plot <- ggplot(age_rates, aes(x = age, y = rate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100 * overall, linetype = "dashed") +
  labs(title = "Readmission rate by age band", x = "Age", y = "Readmitted within 30 days (%)") +
  theme_minimal()
ggsave("outputs/fig_age.png", age_plot, width = 8, height = 4)

# race and gender - just checking them here, I am not using them as predictors
prop.table(table(df$race_group, df$readmit_30), 1)
prop.table(table(df$gender, df$readmit_30), 1)
chisq.test(table(df$gender, df$readmit_30))
chisq.test(table(df$race_group, df$readmit_30))

# AUC of each variable by itself, to see which ones are useful.
# numeric variables are used as they are, for categories I use
# the readmission rate of that category
auc_tab <- data.frame(variable = character(), auc = numeric())

for (v in num_cols) {
  auc_val <- roc_auc_vec(df$readmit_30, df[[v]])
  auc_tab <- rbind(auc_tab, data.frame(variable = v, auc = auc_val))
}

cat_cols <- c("race_group", "gender", "age", "admission_type", "discharge_group",
                   "admission_source", "specialty", "diag1_group", "diag2_group",
                   "diag3_group", "max_glu_serum", "A1Cresult", "change",
                   "diabetesMed", "insulin", "metformin")

for (v in cat_cols) {
  cat_rate <- ave(as.numeric(df$readmit_30 == "Yes"), df[[v]])
  auc_val <- roc_auc_vec(df$readmit_30, cat_rate)
  auc_tab <- rbind(auc_tab, data.frame(variable = v, auc = auc_val))
}

auc_tab$auc <- round(auc_tab$auc, 3)
auc_tab <- auc_tab[order(-auc_tab$auc), ]
print(auc_tab)
write.csv(auc_tab, "outputs/standalone_auc.csv", row.names = FALSE)

auc_plot <- ggplot(auc_tab, aes(x = reorder(variable, auc), y = auc)) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "How well each variable ranks early readmissions on its own",
       x = "", y = "AUC (0.5 = no better than guessing)") +
  theme_minimal()
ggsave("outputs/fig_standalone_auc.png", auc_plot, width = 7, height = 6)

# correlation between the numeric variables
cor_mat <- round(cor(df[, num_cols]), 2)
print(cor_mat)
write.csv(cor_mat, "outputs/correlations.csv")

# VIF for the numeric variables, under 5 means no multicollinearity problem
vif <- round(diag(solve(cor(df[, num_cols]))), 2)
print(vif)

# how often each diabetes medicine was used
med_cols <- c("metformin", "repaglinide", "nateglinide", "chlorpropamide",
                   "glimepiride", "acetohexamide", "glipizide", "glyburide",
                   "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose",
                   "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton",
                   "insulin", "glyburide.metformin", "glipizide.metformin",
                   "glimepiride.pioglitazone", "metformin.rosiglitazone",
                   "metformin.pioglitazone")
med_use <- sapply(df[, med_cols], function(x) round(100 * mean(x != "No"), 2))
print(sort(med_use, decreasing = TRUE))
write.csv(data.frame(medicine = names(med_use), percent_used = med_use),
          "outputs/medication_usage.csv", row.names = FALSE)

# STEP 8: Dimension reduction (Part 5)
# trying PCA on the numeric variables. the first component only explains
# around 25% so PCA won't help much, that's why I didn't use it
pca <- prcomp(df[, num_cols], scale. = TRUE)
summary(pca)

# keeping the 8 medicines used in at least 1% of stays. the 13 rare ones
# get combined into one variable below, examide and citoglipton are never used
keep_meds <- c("insulin", "metformin", "glipizide", "glyburide",
                    "pioglitazone", "rosiglitazone", "glimepiride", "repaglinide")
rare_meds <- c("nateglinide", "chlorpropamide", "acetohexamide", "tolbutamide",
                    "acarbose", "miglitol", "troglitazone", "tolazamide",
                    "glyburide.metformin", "glipizide.metformin", "glimepiride.pioglitazone",
                    "metformin.rosiglitazone", "metformin.pioglitazone")

# STEP 9: Data engineering and transformation (Part 6)
# risk keeps going up with more stays, so log instead of a cap
df$log_inpatient <- log(1 + df$number_inpatient)

# emergency visits - rate stops going up after 4, so capping at 4
df$emergency_capped <- pmin(df$number_emergency, 4)

# outpatient visits - only matters if they had any or not
df$any_outpatient <- ifelse(df$number_outpatient > 0, 1, 0)

# very few patients have more than 9 diagnoses so capping at 9
df$diagnoses_capped <- pmin(df$number_diagnoses, 9)

# merging the two youngest age groups because they are too small
df$age_group <- df$age
df$age_group[df$age %in% c("[0-10)", "[10-20)")] <- "[0-20)"

# new variable - 1 if the patient got any of the 13 rare medicines
df$other_diabetes_med <- ifelse(rowSums(df[, rare_meds] != "No") > 0, 1, 0)

# new variable - 1 if they went to a facility (used for the interaction)
df$facility_discharge <- ifelse(df$discharge_group %in%
                                        c("Skilled nursing / ICF", "Rehab facility",
                                          "Other facility or referral"), 1, 0)

# final data for modeling - target, patient id (for the split),
# race and gender (only for fairness), facility flag and my 28 predictors
mdata <- df %>%
  select(readmit_30, patient_nbr, race_group, gender, facility_discharge,
         time_in_hospital, num_lab_procedures, num_procedures, num_medications,
         log_inpatient, emergency_capped, any_outpatient, diagnoses_capped,
         other_diabetes_med, age_group, admission_type, discharge_group,
         admission_source, specialty, diag1_group, diag2_group, diag3_group,
         max_glu_serum, A1Cresult, change,
         insulin, metformin, glipizide, glyburide, pioglitazone,
         rosiglitazone, glimepiride, repaglinide)

# changing the text columns to factors
fac_cols <- c("age_group", "admission_type", "discharge_group", "admission_source",
                 "specialty", "diag1_group", "diag2_group", "diag3_group",
                 "max_glu_serum", "A1Cresult", "change", keep_meds)
for (v in fac_cols) {
  mdata[[v]] <- as.factor(mdata[[v]])
}

str(mdata)

# STEP 10: Split the data by patient (Part 7)
# splitting into train (60%), validation (20%) and test (20%).
# some patients have more than one stay so I split by patient,
# that way the same person is never in two sets
set.seed(123)
pts <- sample(unique(mdata$patient_nbr))
n <- length(pts)

train_ids <- pts[1:round(0.6 * n)]
val_ids <- pts[(round(0.6 * n) + 1):round(0.8 * n)]
test_ids <- pts[(round(0.8 * n) + 1):n]

train <- mdata[mdata$patient_nbr %in% train_ids, ]
valid <- mdata[mdata$patient_nbr %in% val_ids, ]
test <- mdata[mdata$patient_nbr %in% test_ids, ]

# checking the size and readmission rate of each set
split_tab <- data.frame(
  set = c("Training", "Validation", "Test"),
  records = c(nrow(train), nrow(valid), nrow(test)),
  patients = c(length(train_ids), length(val_ids), length(test_ids)),
  readmitted = c(sum(train$readmit_30 == "Yes"),
                 sum(valid$readmit_30 == "Yes"),
                 sum(test$readmit_30 == "Yes")))
split_tab$share <- round(100 * split_tab$records / nrow(mdata), 1)
split_tab$rate <- round(100 * split_tab$readmitted / split_tab$records, 2)
print(split_tab)
write.csv(split_tab, "outputs/partition_table.csv", row.names = FALSE)

# should be 0, no patient in two sets
length(intersect(train_ids, val_ids)) + length(intersect(train_ids, test_ids)) +
  length(intersect(val_ids, test_ids))

# checking the three sets have similar patients (Appendix A)
bal_tab <- data.frame(
  set = c("Training", "Validation", "Test"),
  avg_prior_stays = round(c(mean(exp(train$log_inpatient) - 1),
                            mean(exp(valid$log_inpatient) - 1),
                            mean(exp(test$log_inpatient) - 1)), 2),
  avg_days = round(c(mean(train$time_in_hospital), mean(valid$time_in_hospital),
                     mean(test$time_in_hospital)), 2),
  avg_medications = round(c(mean(train$num_medications), mean(valid$num_medications),
                            mean(test$num_medications)), 2),
  facility_percent = round(100 * c(mean(train$facility_discharge),
                                   mean(valid$facility_discharge),
                                   mean(test$facility_discharge)), 1),
  insulin_percent = round(100 * c(mean(train$insulin != "No"),
                                  mean(valid$insulin != "No"),
                                  mean(test$insulin != "No")), 1))
print(bal_tab)
write.csv(bal_tab, "outputs/partition_balance.csv", row.names = FALSE)

# 5 folds for cross validation inside train, also split by patient
set.seed(123)
folds <- group_vfold_cv(train, group = patient_nbr, v = 5)

# checking the interaction pattern on train only (Figure 12)
train %>%
  group_by(facility_discharge, two_plus_stays = round(exp(log_inpatient) - 1) >= 2) %>%
  summarise(patients = n(), rate = round(100 * mean(readmit_30 == "Yes"), 1))

# STEP 11: Recipes - the preparation steps for each model 
# patient id, race and gender stay in the data but are not used as predictors.
# the recipe steps get redone in each fold so there is no leakage

# recipe for logistic and LASSO - dummies, the interaction term and scaling.
# after making the interaction I drop the facility flag by itself, because
# the discharge group dummies already carry that same information
lr_rec <- recipe(readmit_30 ~ ., data = train) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ log_inpatient:facility_discharge) %>%
  step_rm(facility_discharge) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# recipe for the tree and random forest - they can use the categories as they are
tree_rec <- recipe(readmit_30 ~ ., data = train) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_rm(facility_discharge) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# recipe for XGBoost - needs dummies but no scaling
xgb_rec <- recipe(readmit_30 ~ ., data = train) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_rm(facility_discharge) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# STEP 12: Set up the five models (Part 8)
# model 1 - logistic regression (my baseline, nothing to tune)
lr_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# model 2 - LASSO, tuning the penalty
lasso_mod <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# model 3 - decision tree, depth 5 with at least 200 patients per split,
# tuning only the pruning value (cost_complexity)
tree_mod <- decision_tree(cost_complexity = tune(), tree_depth = 5, min_n = 200) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# model 4 - random forest with 100 trees and fixed settings
# (random forest is slow to tune, so I picked common settings instead:
# 5 variables tried at each split and at least 100 patients in a leaf.
# num.threads = 4 lets it use 4 CPU cores so it runs faster)
rf_mod <- rand_forest(mtry = 5, min_n = 100, trees = 100) %>%
  set_engine("ranger", importance = "impurity", num.threads = 4) %>%
  set_mode("classification")

# model 5 - XGBoost, up to 300 small trees with early stopping
# (stops if it doesn't improve for 20 rounds). fixed settings again
# to keep the run time short: depth 4, learning rate 0.1
xgb_mod <- boost_tree(trees = 300, tree_depth = 4, learn_rate = 0.1,
                        min_n = 50, stop_iter = 20) %>%
  set_engine("xgboost", validation = 0.2, nthread = 4) %>%
  set_mode("classification")

# putting recipe and model together for each one
lr_wf <- workflow() %>% add_recipe(lr_rec) %>% add_model(lr_mod)
lasso_wf <- workflow() %>% add_recipe(lr_rec) %>% add_model(lasso_mod)
tree_wf <- workflow() %>% add_recipe(tree_rec) %>% add_model(tree_mod)
rf_wf <- workflow() %>% add_recipe(tree_rec) %>% add_model(rf_mod)
xgb_wf <- workflow() %>% add_recipe(xgb_rec) %>% add_model(xgb_mod)

# values I am trying for the two models I tune (Appendix B)
lasso_grid <- data.frame(penalty = 10^seq(-5, -1, length.out = 20))

tree_grid <- data.frame(cost_complexity = c(0.0001, 0.001, 0.01))

# picking the settings with the best AUC, but if a simpler one is almost
# as good (within one standard error) I go with the simpler one.
# random forest and XGBoost use fixed settings (STEP 12), so they skip
# this step and get compared on the validation set in STEP 17 instead

# logistic regression - nothing to tune, just getting the CV AUC
lr_cv <- fit_resamples(lr_wf, resamples = folds,
                             metrics = metric_set(roc_auc))
collect_metrics(lr_cv)

# LASSO (simpler = bigger penalty)
set.seed(123)
lasso_cv <- tune_grid(lasso_wf, resamples = folds, grid = lasso_grid,
                      metrics = metric_set(roc_auc))
show_best(lasso_cv, metric = "roc_auc", n = 5)
lasso_best <- select_by_one_std_err(lasso_cv, desc(penalty), metric = "roc_auc")
lasso_best

# decision tree (simpler = bigger pruning value)
set.seed(123)
tree_cv <- tune_grid(tree_wf, resamples = folds, grid = tree_grid,
                     metrics = metric_set(roc_auc))
show_best(tree_cv, metric = "roc_auc", n = 3)
tree_best <- select_by_one_std_err(tree_cv, desc(cost_complexity), metric = "roc_auc")
tree_best

# saving the CV AUC for these three
cv_table <- data.frame(
  model = c("Logistic regression", "LASSO", "Decision tree"),
  cv_auc = round(c(collect_metrics(lr_cv)$mean,
                   show_best(lasso_cv, metric = "roc_auc", n = 1)$mean,
                   show_best(tree_cv, metric = "roc_auc", n = 1)$mean), 4))
print(cv_table)
write.csv(cv_table, "outputs/cv_auc_by_model.csv", row.names = FALSE)
set.seed(123)
lr_fit <- fit(lr_wf, data = train)
lasso_fit <- fit(finalize_workflow(lasso_wf, lasso_best), data = train)
tree_fit <- fit(finalize_workflow(tree_wf, tree_best), data = train)
rf_fit <- fit(rf_wf, data = train)
xgb_fit <- fit(xgb_wf, data = train)

# STEP 15: Class-weighted versions of the five models (Section 9.5)
# only about 11% are readmitted, so I am also trying class weights where
# each readmitted patient counts more. then I compare with and without
# weights on the validation set
yes_wt <- sum(train$readmit_30 == "No") / sum(train$readmit_30 == "Yes")
yes_wt

train_w <- train
train_w$case_wts <- hardhat::importance_weights(
  ifelse(train_w$readmit_30 == "Yes", yes_wt, 1))

# making the recipes again with the weighted data
lr_rec_w <- recipe(readmit_30 ~ ., data = train_w) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ log_inpatient:facility_discharge) %>%
  step_rm(facility_discharge) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

tree_rec_w <- recipe(readmit_30 ~ ., data = train_w) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_rm(facility_discharge) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors())

xgb_rec_w <- recipe(readmit_30 ~ ., data = train_w) %>%
  update_role(patient_nbr, race_group, gender, new_role = "id") %>%
  step_rm(facility_discharge) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# training the weighted versions with the same best settings
set.seed(123)
lr_fit_w <- workflow() %>% add_case_weights(case_wts) %>%
  add_recipe(lr_rec_w) %>% add_model(lr_mod) %>%
  fit(data = train_w)

lasso_fit_w <- workflow() %>% add_case_weights(case_wts) %>%
  add_recipe(lr_rec_w) %>% add_model(lasso_mod) %>%
  finalize_workflow(lasso_best) %>%
  fit(data = train_w)

tree_fit_w <- workflow() %>% add_case_weights(case_wts) %>%
  add_recipe(tree_rec_w) %>% add_model(tree_mod) %>%
  finalize_workflow(tree_best) %>%
  fit(data = train_w)

rf_fit_w <- workflow() %>% add_case_weights(case_wts) %>%
  add_recipe(tree_rec_w) %>% add_model(rf_mod) %>%
  fit(data = train_w)

xgb_fit_w <- workflow() %>% add_case_weights(case_wts) %>%
  add_recipe(xgb_rec_w) %>% add_model(xgb_mod) %>%
  fit(data = train_w)

# the weighted models need a weight column to predict, so I add one with
# all 1s to valid and test (it doesn't change the predictions)
valid$case_wts <- hardhat::importance_weights(rep(1, nrow(valid)))
test$case_wts <- hardhat::importance_weights(rep(1, nrow(test)))

# STEP 16: A function to calculate all my evaluation measures
# function to get all my measures. risk = predicted chance of coming back,
# anyone at or above the cutoff gets flagged for follow-up
get_results <- function(model_name, truth, risk, cutoff) {
  flagged <- risk >= cutoff
  actual <- truth == "Yes"
  tp <- sum(flagged & actual)      # flagged and came back
  fp <- sum(flagged & !actual)     # flagged but did not come back
  fn <- sum(!flagged & actual)     # missed patients who came back
  tn <- sum(!flagged & !actual)    # not flagged and did not come back
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  data.frame(model = model_name,
             auc = round(roc_auc_vec(truth, risk), 4),
             pr_auc = round(pr_auc_vec(truth, risk), 4),
             flagged_percent = round(100 * mean(flagged), 1),
             recall = round(recall, 4),
             precision = round(precision, 4),
             specificity = round(tn / (tn + fp), 4),
             f1 = round(2 * precision * recall / (precision + recall), 4),
             accuracy = round((tp + tn) / length(truth), 4),
             lift = round(precision / mean(actual), 2),
             true_pos = tp, false_pos = fp, false_neg = fn, true_neg = tn)
}

# STEP 17: Compare the models on the validation set 
# predictions on validation for all 10 versions (5 normal + 5 weighted)
val_pred <- data.frame(
  lr = predict(lr_fit, valid, type = "prob")$.pred_Yes,
  lasso = predict(lasso_fit, valid, type = "prob")$.pred_Yes,
  tree = predict(tree_fit, valid, type = "prob")$.pred_Yes,
  rf = predict(rf_fit, valid, type = "prob")$.pred_Yes,
  xgb = predict(xgb_fit, valid, type = "prob")$.pred_Yes,
  lr_w = predict(lr_fit_w, valid, type = "prob")$.pred_Yes,
  lasso_w = predict(lasso_fit_w, valid, type = "prob")$.pred_Yes,
  tree_w = predict(tree_fit_w, valid, type = "prob")$.pred_Yes,
  rf_w = predict(rf_fit_w, valid, type = "prob")$.pred_Yes,
  xgb_w = predict(xgb_fit_w, valid, type = "prob")$.pred_Yes)

# cutoff = score that flags the top 20% of patients (proposed capacity)
cuts <- sapply(val_pred, function(r) unname(quantile(r, 0.80)))
cuts

val_res <- rbind(
  get_results("lr", valid$readmit_30, val_pred$lr, cuts["lr"]),
  get_results("lasso", valid$readmit_30, val_pred$lasso, cuts["lasso"]),
  get_results("tree", valid$readmit_30, val_pred$tree, cuts["tree"]),
  get_results("rf", valid$readmit_30, val_pred$rf, cuts["rf"]),
  get_results("xgb", valid$readmit_30, val_pred$xgb, cuts["xgb"]),
  get_results("lr_w", valid$readmit_30, val_pred$lr_w, cuts["lr_w"]),
  get_results("lasso_w", valid$readmit_30, val_pred$lasso_w, cuts["lasso_w"]),
  get_results("tree_w", valid$readmit_30, val_pred$tree_w, cuts["tree_w"]),
  get_results("rf_w", valid$readmit_30, val_pred$rf_w, cuts["rf_w"]),
  get_results("xgb_w", valid$readmit_30, val_pred$xgb_w, cuts["xgb_w"]))
print(val_res[, c("model", "auc", "recall", "precision", "lift")])
write.csv(val_res, "outputs/validation_results.csv", row.names = FALSE)

# I only use the weighted version if it clearly helps
# (AUC up by more than 0.005 or recall up by more than 0.01)
use_wt <- function(name) {
  plain <- val_res[val_res$model == name, ]
  weighted <- val_res[val_res$model == paste0(name, "_w"), ]
  (weighted$auc - plain$auc > 0.005) | (weighted$recall - plain$recall > 0.01)
}

final_ver <- data.frame(
  model = c("lr", "lasso", "tree", "rf", "xgb"),
  use_weights = c(use_wt("lr"), use_wt("lasso"), use_wt("tree"),
                  use_wt("rf"), use_wt("xgb")))
final_ver$version <- ifelse(final_ver$use_weights,
                          paste0(final_ver$model, "_w"), final_ver$model)
print(final_ver)
write.csv(final_ver, "outputs/class_weight_decision.csv", row.names = FALSE)

# final version of each model
final_mods <- list(
  lr = if (use_wt("lr")) lr_fit_w else lr_fit,
  lasso = if (use_wt("lasso")) lasso_fit_w else lasso_fit,
  tree = if (use_wt("tree")) tree_fit_w else tree_fit,
  rf = if (use_wt("rf")) rf_fit_w else rf_fit,
  xgb = if (use_wt("xgb")) xgb_fit_w else xgb_fit)
final_cuts <- cuts[final_ver$version]
names(final_cuts) <- final_ver$model

# ranking the models on validation by recall first, then AUC
val_final <- val_res[val_res$model %in% final_ver$version, ]
val_final <- val_final[order(-val_final$recall, -val_final$auc), ]
print(val_final)
write.csv(val_final, "outputs/validation_final_models.csv", row.names = FALSE)

best <- sub("_w", "", val_final$model[1])
best

# STEP 18: Final results on the test set (Parts 10 and 11)
# using the test set only once here. cutoffs are from validation
test_pred <- data.frame(
  lr = predict(final_mods$lr, test, type = "prob")$.pred_Yes,
  lasso = predict(final_mods$lasso, test, type = "prob")$.pred_Yes,
  tree = predict(final_mods$tree, test, type = "prob")$.pred_Yes,
  rf = predict(final_mods$rf, test, type = "prob")$.pred_Yes,
  xgb = predict(final_mods$xgb, test, type = "prob")$.pred_Yes)

test_res <- rbind(
  get_results("Logistic regression", test$readmit_30, test_pred$lr, final_cuts["lr"]),
  get_results("LASSO", test$readmit_30, test_pred$lasso, final_cuts["lasso"]),
  get_results("Decision tree", test$readmit_30, test_pred$tree, final_cuts["tree"]),
  get_results("Random forest", test$readmit_30, test_pred$rf, final_cuts["rf"]),
  get_results("XGBoost", test$readmit_30, test_pred$xgb, final_cuts["xgb"]))
print(test_res)
write.csv(test_res, "outputs/test_results.csv", row.names = FALSE)

# out of every 1,000 discharges - how many returning patients we reach,
# how many we miss, and how many follow-ups go to people who wouldn't come back
per_1000 <- data.frame(
  model = test_res$model,
  reached = round(1000 * test_res$true_pos / nrow(test)),
  missed = round(1000 * test_res$false_neg / nrow(test)),
  extra_followups = round(1000 * test_res$false_pos / nrow(test)))
print(per_1000)
write.csv(per_1000, "outputs/test_per_1000_discharges.csv", row.names = FALSE)

# what if the program is smaller (top 5% and top 10%)
small_prog <- data.frame()
for (m in c("lr", "lasso", "tree", "rf", "xgb")) {
  for (share in c(0.05, 0.10)) {
    val_scores <- predict(final_mods[[m]], valid, type = "prob")$.pred_Yes
    cut <- unname(quantile(val_scores, 1 - share))
    result <- get_results(m, test$readmit_30, test_pred[[m]], cut)
    result$program_size <- paste0(100 * share, "%")
    small_prog <- rbind(small_prog, result)
  }
}
print(small_prog[, c("model", "program_size", "recall", "precision", "lift")])
write.csv(small_prog, "outputs/test_smaller_programs.csv", row.names = FALSE)

# ROC curves for all five models on test
roc_lr <- roc_curve(data.frame(truth = test$readmit_30, risk = test_pred$lr), truth, risk)
roc_lasso <- roc_curve(data.frame(truth = test$readmit_30, risk = test_pred$lasso), truth, risk)
roc_tree <- roc_curve(data.frame(truth = test$readmit_30, risk = test_pred$tree), truth, risk)
roc_rf <- roc_curve(data.frame(truth = test$readmit_30, risk = test_pred$rf), truth, risk)
roc_xgb <- roc_curve(data.frame(truth = test$readmit_30, risk = test_pred$xgb), truth, risk)

roc_lr$model <- "Logistic regression"
roc_lasso$model <- "LASSO"
roc_tree$model <- "Decision tree"
roc_rf$model <- "Random forest"
roc_xgb$model <- "XGBoost"
roc_all <- rbind(roc_lr, roc_lasso, roc_tree, roc_rf, roc_xgb)

roc_plot <- ggplot(roc_all, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC curves of the five models on the test set",
       x = "False positive rate", y = "True positive rate (recall)", color = "") +
  theme_minimal()
ggsave("outputs/fig_test_roc.png", roc_plot, width = 7, height = 5)

# recall in the top 20% for each model
recall_plot <- ggplot(test_res, aes(x = reorder(model, recall), y = 100 * recall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Share of early readmissions caught in the top 20% (test set)",
       x = "", y = "Recall (%)") +
  theme_minimal()
ggsave("outputs/fig_test_recall.png", recall_plot, width = 7, height = 4)


# --------------------------------------------------------------
# STEP 19: Calibration of the best model
# --------------------------------------------------------------

# splitting test patients into 10 groups by predicted risk and
# comparing predicted vs what actually happened
calib <- data.frame(truth = test$readmit_30, risk = test_pred[[best]])
calib$risk_group <- ntile(calib$risk, 10)
calib_tab <- calib %>%
  group_by(risk_group) %>%
  summarise(predicted = mean(risk), actual = mean(truth == "Yes"))
print(calib_tab)
write.csv(calib_tab, "outputs/calibration_best_model.csv", row.names = FALSE)

calib_plot <- ggplot(calib_tab, aes(x = predicted, y = actual)) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue") +
  geom_abline(linetype = "dashed") +
  labs(title = "Calibration of the best model on the test set",
       x = "Average predicted risk", y = "Actual readmission rate") +
  theme_minimal()
ggsave("outputs/fig_calibration.png", calib_plot, width = 6, height = 5)

# STEP 20: Fairness check of the best model
# checking if the best model works the same for different age, gender
# and race groups (how many flagged, how many readmissions caught)
fair <- test
fair$flagged <- test_pred[[best]] >= final_cuts[best]
fair$actual <- fair$readmit_30 == "Yes"

fair_age <- fair %>% group_by(group = as.character(age_group)) %>%
  summarise(patients = n(), flagged_percent = round(100 * mean(flagged), 1),
            recall_percent = round(100 * sum(flagged & actual) / sum(actual), 1))
fair_age$attribute <- "Age"

fair_gender <- fair %>% group_by(group = gender) %>%
  summarise(patients = n(), flagged_percent = round(100 * mean(flagged), 1),
            recall_percent = round(100 * sum(flagged & actual) / sum(actual), 1))
fair_gender$attribute <- "Gender"

fair_race <- fair %>% group_by(group = race_group) %>%
  summarise(patients = n(), flagged_percent = round(100 * mean(flagged), 1),
            recall_percent = round(100 * sum(flagged & actual) / sum(actual), 1))
fair_race$attribute <- "Race"

fair_tab <- rbind(fair_age, fair_gender, fair_race)
print(fair_tab)
write.csv(fair_tab, "outputs/fairness_best_model.csv", row.names = FALSE)

# STEP 21: Which predictors matter most in each model
# logistic regression odds ratios (more than 1 = higher risk)
odds <- tidy(extract_fit_engine(final_mods$lr), exponentiate = TRUE)
odds <- odds[odds$term != "(Intercept)", ]
odds <- odds[order(odds$p.value), ]
head(odds, 15)
write.csv(odds, "outputs/logistic_odds_ratios.csv", row.names = FALSE)

# which variables LASSO kept
lasso_coef <- tidy(extract_fit_parsnip(final_mods$lasso))
lasso_coef <- lasso_coef[lasso_coef$term != "(Intercept)", ]
sum(lasso_coef$estimate != 0)          # number of columns LASSO kept
write.csv(lasso_coef, "outputs/lasso_coefficients.csv", row.names = FALSE)

# plotting the decision tree
png("outputs/fig_decision_tree.png", width = 1400, height = 900)
rpart.plot(extract_fit_engine(final_mods$tree), roundint = FALSE)
dev.off()

# random forest variable importance
rf_imp <- extract_fit_engine(final_mods$rf)$variable.importance
rf_imp <- data.frame(variable = names(rf_imp),
                                importance = rf_imp)
rf_imp <- rf_imp[order(-rf_imp$importance), ]
head(rf_imp, 15)
write.csv(rf_imp, "outputs/random_forest_importance.csv", row.names = FALSE)

rf_plot <- ggplot(head(rf_imp, 15),
                      aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random forest: 15 most important predictors", x = "", y = "Importance") +
  theme_minimal()
ggsave("outputs/fig_forest_importance.png", rf_plot, width = 7, height = 5)

# XGBoost variable importance
xgb_imp <- xgboost::xgb.importance(model = extract_fit_engine(final_mods$xgb))
head(xgb_imp, 15)
write.csv(xgb_imp, "outputs/xgboost_importance.csv", row.names = FALSE)

xgb_plot <- ggplot(head(xgb_imp, 15), aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "XGBoost: 15 most important predictors", x = "", y = "Gain") +
  theme_minimal()
ggsave("outputs/fig_xgboost_importance.png", xgb_plot, width = 7, height = 5)

print("Finished. All tables and charts are in the outputs folder.")
