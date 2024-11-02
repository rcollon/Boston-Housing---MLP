
#install.packages("NeuralNetTools")


# Predict the value of prices of the house. Medv is the target variable
library (tidymodels)
library (tidyverse)
library (NeuralNetTools)
data("BostonHousing", package = "mlbench")

tidymodels_prefer()

# Examine dataset
glimpse(BostonHousing)

# Summary Statistics
summary (BostonHousing)
table(BostonHousing$chas)
 
# Train test split
sample <- initial_split (BostonHousing, prop = .8)
trainingBH <- training (sample)
testBH <- testing(sample)

# Engines
show_engines("mlp")

# Model 
modelMLP <- mlp(engine="nnet", mode="regression")

modelMLP

# Recipe
recipeMLP <- recipe(medv ~. , data=trainingBH) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_mutate(chas = as.integer(chas))

# Workflow 
wfMLP <- workflow() %>% 
  add_model(modelMLP) %>% 
  add_recipe(recipeMLP)

# Fit 
fitMLP <- fit(wfMLP, data = trainingBH)

fitMLP

nnModel <- extract_fit_parsnip(fitMLP)$fit

plotnet(nnModel)

# Predict and assess on test data
predictionMLP <- predict (fitMLP, testBH) %>% bind_cols(testBH)
predictionMLP

#Metrics
predictionMLP %>% metrics(truth = medv, estimate = .pred)




