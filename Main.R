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
noincome_sum <- sum(wide_datasetQ15$NoIncome) # sum equal to 0, thus income known of every observation

##### PART 2: BINARY PURCHASE DECISION MODELS ######################################################################################################################

### Data preparation

# Check for correlations panelist, Income and FMYsize
wide_datasetQ15 %>% distinct(panelist, Income)  # 250 unique panelist, Income pairs, meaning Income is constant per household 
wide_datasetQ15 %>% distinct(panelist, FMYSize)  # 250 unique panelist, FMYSize pairs, meaning FMYSize is constant per household
wide_datasetQ15 %>% distinct(panelist, `store id`)  # 538 unique panelist, store id pairs, meaning some households visit multiple stores
range_fmy <- range(wide_datasetQ15$FMYSize)# gives from 1 to 6
wide_datasetQ15 %>% distinct(Income, FMYSize)  # 44 unique panelist, with 11 income types and 6 family sizes, gives enough variation
lmincome = lm(Income~FMYSize, data = wide_datasetQ15) #check for multicolinearity
summary(lmincome) #Review the results, Low R squared thus likely no multicolinearity

# Create dummy variables for 1. unique days 2. unique weeks 3. unique stores
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15, select_columns = "day", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "week", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "store id", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "FMYSize", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "Income", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# wide_datasetQ15_onehot <- dummy_cols(wide_datasetQ15_onehot, select_columns = "panelist", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Add dummy for purchase decision, 1 = purchase, 0 = no purchase
wide_datasetQ15_onehot$purchase <- 1 - wide_datasetQ15_onehot$brand_0

#Create new dataset for prices
wide_datasetQ15_prices <- wide_datasetQ15_onehot

# Identify all price columns
price_columns <- names(wide_datasetQ15_onehot)[grepl("price", names(wide_datasetQ15_onehot))]

# Function to create lagged variables with lag = 1 within each individual
create_lagged_variables <- function(df, columns) {
  df <- df %>%
    group_by(panelist) %>%
    arrange(panelist) %>%
    mutate(across(all_of(columns), 
                  ~lag(., 1), 
                  .names = "{col}_lag1")) %>%
    ungroup()
  return(df)
}

# Create lagged variables for each price column with lag = 1 within each individual
wide_datasetQ15_onehot <- create_lagged_variables(wide_datasetQ15_onehot, price_columns)

wide_datasetQ15_onehot$price_1_difference <- wide_datasetQ15_onehot$price_1 - wide_datasetQ15_onehot$price_1_lag1
wide_datasetQ15_onehot$price_2_difference <- wide_datasetQ15_onehot$price_2 - wide_datasetQ15_onehot$price_2_lag1
wide_datasetQ15_onehot$price_3_difference <- wide_datasetQ15_onehot$price_3 - wide_datasetQ15_onehot$price_3_lag1
wide_datasetQ15_onehot$price_4_difference <- wide_datasetQ15_onehot$price_4 - wide_datasetQ15_onehot$price_4_lag1
wide_datasetQ15_onehot$price_5_difference <- wide_datasetQ15_onehot$price_5 - wide_datasetQ15_onehot$price_5_lag1
wide_datasetQ15_onehot$price_6_difference <- wide_datasetQ15_onehot$price_6 - wide_datasetQ15_onehot$price_1_lag1

# Remove rows for which weekslast = NA, also removes NAs in brandlag
wide_datasetQ15_onehot <- wide_datasetQ15_onehot[!is.na(wide_datasetQ15_onehot$weekslast), ]
wide_datasetQ15_prices <- wide_datasetQ15_prices[!is.na(wide_datasetQ15_prices$weekslast), ]

# Remove unused variables (lag_0 removed because of multicollinearity)
unused_cols <- c("...1","id","brandlag","brandbought","sumdollars","sumvol","sumunits","panelist","brand_0","brand_1","brand_2","brand_3","brand_4","brand_5","brand_6","lag_0","NoIncome","price_1","price_1","price_2","price_3","price_4","price_5","price_6", "price_1_lag1","price_2_lag1","price_3_lag1","price_4_lag1","price_5_lag1","price_6_lag1")
wide_datasetQ15_onehot <- wide_datasetQ15_onehot %>% select(-all_of(unused_cols))

unused_cols_price <- c("...1","id","brandlag","brandbought","sumdollars","sumvol","sumunits","panelist","brand_0","brand_1","brand_2","brand_3","brand_4","brand_5","brand_6","lag_0","NoIncome")
wide_datasetQ15_prices <- wide_datasetQ15_prices %>% select(-all_of(unused_cols_price))
### PROBIT MODEL

# Generate probit model and display summary
binarypurchase_probit_mdl <- glm(purchase ~ . ,
                                 family = binomial(link = "probit"),
                                 data = wide_datasetQ15_onehot)
summary(binarypurchase_probit_mdl)

# Generate probit model with normal prices and display summary
binarypurchase_probit_mdl_prices <- glm(purchase ~ . ,
                                 family = binomial(link = "probit"),
                                 data = wide_datasetQ15_prices)
summary(binarypurchase_probit_mdl_prices)

#Add average price 
wide_datasetQ15_onehot$average_price <- (wide_datasetQ15_onehot$price_1+wide_datasetQ15_onehot$price_2+wide_datasetQ15_onehot$price_3+wide_datasetQ15_onehot$price_4+wide_datasetQ15_onehot$price_5+wide_datasetQ15_onehot$price_6) / 6

# Remove prices
unused_cols_price <- c("price_1","price_2", "price_3","price_4","price_5","price_6")
wide_datasetQ15_onehot <- wide_datasetQ15_onehot %>% select(-all_of(unused_cols_price))

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

# Generate logit model prices and display summary
binarypurchase_logit_model_prices <- glm(purchase ~ . ,
                                  family = binomial(link = "logit"),
                                  data = wide_datasetQ15_prices)
summary(binarypurchase_logit_model_prices)




