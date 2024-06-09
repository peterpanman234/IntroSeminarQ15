install.packages("fastDummies")
install.packages("dplyr")
install.packages("marginaleffects")

library(fastDummies)
library(dplyr)
library(marginaleffects)

library(readr)
wide_datasetQ15 <- read_csv("wide_datasetQ15.csv")

set.seed(123)

### PART 1: DESCRIPTIVE STATISTICS #################################################################################################################################

num_customers <- length(unique(wide_datasetQ15$panelist))
num_stores <- length(unique(wide_datasetQ15$`store id`))
num_weeks <- length(unique(wide_datasetQ15$week))
check_NoIncome <- length(unique(wide_datasetQ15$NoIncome))  # 1 unique value, meaning there are no households in our data for which income class is unknown

##### PART 2: BINARY PURCHASE DECISION MODELS ######################################################################################################################

### Data preparation

# Check for correlations panelist, Income and FMYsize
wide_datasetQ15 %>% distinct(panelist, Income)  # 250 unique panelist, Income pairs, meaning Income is constant per household 
wide_datasetQ15 %>% distinct(panelist, FMYSize)  # 250 unique panelist, FMYSize pairs, meaning FMYSize is constant per household
wide_datasetQ15 %>% distinct(panelist, `store id`)  # 538 unique panelist, store id pairs, meaning some households visit multiple stores

# Create dummy variables for 1. unique days 2. unique weeks 3. unique stores
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15, select_columns = "day", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "week", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "store id", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "FMYSize", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "Income", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "panelist", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Add dummy for purchase decision, 1 = purchase, 0 = no purchase
wide_datasetQ15_onehot$purchase <- 1 - wide_datasetQ15_onehot$brand_0

# Remove rows for which weekslast = NA, also removes NAs in brandlag
wide_datasetQ15_onehot <- wide_datasetQ15_onehot[!is.na(wide_datasetQ15_onehot$weekslast), ]

# Remove unused variables (lag_0 removed because of multicollinearity)
unused_cols <- c("...1","id","brandlag","brandbought","sumdollars","sumvol","sumunits","panelist","brand_0","brand_1","brand_2","brand_3","brand_4","brand_5","brand_6","lag_0","NoIncome")
wide_datasetQ15_onehot <- wide_datasetQ15_onehot %>% select(-all_of(unused_cols))

### PROBIT MODEL

# Generate probit model and display summary
binarypurchase_probit_mdl <- glm(purchase ~ . ,
                          family = binomial(link = "probit"),
                          data = wide_datasetQ15_onehot)
summary(binarypurchase_probit_mdl)

# Calculate marginal effects
marginal_effects_FMYSize <- coef(binarypurchase_probit_mdl)["FMYSize"] * dnorm(predict(binarypurchase_probit_mdl, type = 'link'))

# Calculate average marginal effect
avg_marginal_effect_FMYSize <- mean(marginal_effects_FMYSize)

### LOGIT MODEL

# Generate logit model and display summary
binarypurchase_logit_model <- glm(purchase ~ . ,
                                  family = binomial(link = "logit"),
                                  data = wide_datasetQ15_onehot)
summary(binarypurchase_logit_model)




