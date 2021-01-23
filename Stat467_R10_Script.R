## Stat 467-Recitation 10

##Example 1##

library(AmesHousing)
data(make_ames())
make_ames()


library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging



# Create training (80%) and test (20%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility
set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .8)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)


fit<-rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova"
) #for regression problem we set the method anova.


Lets visualize the tree.


rpart.plot(fit)


##Parameter Tuning##
  
fit$cptable


hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

# total number of combinations
nrow(hyper_grid)


models <- list() #create a list object

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = Sale_Price ~ .,
    data    = ames_train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}


# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


optimal_tree <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova",
  control = list(minsplit = 10, maxdepth = 12, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = ames_test)
RMSE(pred = pred, obs = ames_test$Sale_Price)


##Bagging##
 
# make bootstrapping reproducible
set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = Sale_Price ~ .,
  data    = ames_train,
  coob    = TRUE
)

bagged_m1


# assess 10-50 bagged trees
ntree <- 10:50

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  model <- bagging(
    formula = Sale_Price ~ .,
    data    = ames_train,
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err
}

plot(ntree, rmse, type = 'l', lwd = 2)
abline(v = 25, col = "red", lty = "dashed")




# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 




# CV bagged model
bagged_cv <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv


# plot most important variables
plot(varImp(bagged_cv), 20)  


