
library(ggplot2)
library(randomForest)
library(dplyr)
library(xgboost)

marathon_data <- read.csv("project1.csv")

print(head(data))
summary(marathon_data)

print(marathon_data)

# AIM 1: Scatter plot illustrating marathon performance (%CR) across different age groups and sexes
marathon_data$Sex <- factor(marathon_data$Sex..0.F..1.M., labels = c("Female", "Male"))

ggplot(marathon_data, aes(x=Age..yr., y=X.CR, color=Sex)) +
  geom_point(alpha=0.6) +
  labs(title = "Marathon Performance (%CR) by Age and Sex",
       x = "Age (years)",
       y = "Performance (%CR)",
       color = "Sex") +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"))


## AIM 1: Scatter plot displaying marathon performance (%CR) by sex, with points colored by age
marathon_data$Sex <- factor(marathon_data$Sex..0.F..1.M., labels = c("Female", "Male"))

ggplot(marathon_data, aes(x=Sex, y=X.CR, color=Age..yr.)) +
  geom_point(alpha=0.6) +
  labs(title = "Marathon Performance (%CR) by Age and Sex",
       x = "Sex",
       y = "Performance (%CR)",
       color = "age(Year)") +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"))


### AIM 1: Feature importance plot from a Random Forest model
marathon_data <- na.omit(marathon_data)
sum(is.na(marathon_data$X.CR))  # This should return 0

# Fit the Random Forest model
set.seed(123) 
rf_model <- randomForest(X.CR ~ ., data=marathon_data, ntree=500, importance=TRUE)

importance_data <- importance(rf_model, type=1)  
feature_importance <- data.frame(Feature = rownames(importance_data), Importance = importance_data[, "%IncMSE"])

ggplot(feature_importance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_col(fill="steelblue") +
  labs(title="Feature Importance in Marathon Performance Prediction",
       x="Features",
       y="Importance (Increase in MSE)") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))



# AIM 2: Line graph illustrating the decline in marathon performance as WBGT increases
library(ggplot2)
library(dplyr)
library(lme4)  

marathon_data$Sex <- factor(marathon_data$Sex..0.F..1.M., labels = c("Female", "Male"))
model <- lm(X.CR ~ Age..yr. * Sex + Td..C + Tw..C + Wind + WBGT + SR.W.m2, data=marathon_data)
summary(model)

interaction_plot <- marathon_data %>%
  group_by(Age..yr., Sex) %>%
  summarise(Avg_CR = mean(X.CR, na.rm = TRUE), Avg_WBGT = mean(WBGT, na.rm = TRUE)) %>%
  ggplot(aes(x=Avg_WBGT, y=Avg_CR, color=Sex)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Impact of WBGT on Marathon Performance by Age and Sex",
       x="Average WBGT",
       y="Average Performance (%CR)",
       color="Sex") +
  theme_minimal()

print(interaction_plot)


## AIM2: Feature importance
marathon_data$Sex <- as.numeric(factor(marathon_data$Sex..0.F..1.M.)) 

data_matrix <- xgb.DMatrix(data = as.matrix(marathon_data[, c("Age..yr.", "Sex", "Td..C", "Tw..C", "Wind", "WBGT", "SR.W.m2")]),
                           label = marathon_data$X.CR)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  gamma = 0,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.5,
  colsample_bytree = 0.5
)

set.seed(123)
xgb_model <- xgb.train(params = params, data = data_matrix, nrounds = 100, verbose = 0)

importance_matrix <- xgb.importance(feature_names = colnames(data_matrix), model = xgb_model)
print(importance_matrix) 
xgb.plot.importance(importance_matrix)


## AIM3: Identify the weather parameters (WBGT, Flag conditions, temperature, etc) that have the largest impact on marathon performance
library(ggplot2)
library(dplyr)
library(corrplot)  

correlation_matrix <- cor(marathon_data[, c('X.CR', 'WBGT', 'Td..C', 'Tw..C', 'Wind', 'SR.W.m2')], use = "complete.obs")
corrplot(correlation_matrix, method = "circle")

model <- lm(X.CR ~ WBGT + Td..C + Tw..C + Wind + SR.W.m2, data=marathon_data)
summary(model)

ggplot(marathon_data, aes(x=WBGT, y=X.CR)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Effect of WBGT on Marathon Performance",
       x="WBGT (Wet Bulb Globe Temperature)",
       y="Performance (%CR)") +
  theme_minimal()

