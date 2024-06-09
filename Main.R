install.packages("fastDummies")
install.packages("dplyr")

library(fastDummies)
library(dplyr)

library(readr)
wide_datasetQ15 <- read_csv("wide_datasetQ15.csv")

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

# Create dummy variables for 1. unique stores and 2. unique customers
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15, select_columns = "day", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "week", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "store id", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "FMYSize", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "Income", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "panelist", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Add dummy for purchase decision, 1 = purchase, 0 = no purchase
wide_datasetQ15_onehot$purchase <- 1 - wide_datasetQ15_onehot$brand_0

# Remove unused variables
unused_cols <- c("...1","id","brandbought","sumdollars","brand_0","brand_1","brand_2","brand_3","brand_4","brand_5","brand_6","NoIncome","panelist")
wide_datasetQ15_onehot <- wide_datasetQ15_onehot %>% select(-all_of(unused_cols))


### PROBIT MODEL

# Generate probit model and display summary
binarypurchase_probit_mdl <- glm(purchase ~ . ,
                          family = binomial(link = "probit"),
                          data = wide_datasetQ15_onehot)
summary(binarypurchase_probit_mdl)

### LOGIT MODEL

# Generate logit model and display summary
binarypurchase_logit_model <- glm(purchase ~ . ,
                                  family = binomial(link = "logit"),
                                  data = wide_datasetQ15_onehot)
summary(binarypurchase_logit_model)




