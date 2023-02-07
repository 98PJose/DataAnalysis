### Regression Tree from scratch ###

# function to calculate mean squared error
mean_squared_error <- function(y, y_pred){
  mean((y - y_pred)^2)
}

# function to split a dataset on a feature and a threshold
split_dataset <- function(data, feature, threshold){
  left_index <- data[, feature] <= threshold
  right_index <- data[, feature] > threshold
  list(left=data[left_index, ], right=data[right_index, ])
}

# function to select the best split feature and threshold
best_split <- function(data, target){
  mse <- Inf
  split_feature <- -1
  split_threshold <- -1
  n_features <- ncol(data) - 1 # exclude target column
  
  for(feature in 1:n_features){
    for(threshold in unique(data[, feature])){
      splitted <- split_dataset(data, feature, threshold)
      left <- splitted$left
      right <- splitted$right
      if(nrow(left) == 0 || nrow(right) == 0) next
      
      y_left <- left[, ncol(left)]
      y_right <- right[, ncol(right)]
      mse_left <- mean_squared_error(y_left, mean(y_left))
      mse_right <- mean_squared_error(y_right, mean(y_right))
      mse_split <- mse_left + mse_right
      
      if(mse_split < mse){
        mse <- mse_split
        split_feature <- feature
        split_threshold <- threshold
      }
    }
  }
  
  list(mse=mse, feature=split_feature, threshold=split_threshold)
}

# function to build a regression tree
build_tree <- function(data, target, max_depth=Inf, depth=0){
  if(depth >= max_depth || nrow(data) <= 1) return(mean(data[, ncol(data)]))
  
  split <- best_split(data, target)
  if(split$feature == -1) return(mean(data[, ncol(data)]))
  
  left_index <- data[, split$feature] <= split$threshold
  right_index <- data[, split$feature] > split$threshold
  left_data <- data[left_index, ]
  right_data <- data[right_index, ]
  
  tree <- list(feature=split$feature, threshold=split$threshold, 
               left=build_tree(left_data, target, max_depth, depth+1), 
               right=build_tree(right_data, target, max_depth, depth+1))
  return(tree)
}

# function to make predictions using a regression tree
predict_tree <- function(tree, data){
  if(!is.list(tree)) return(rep(tree, nrow(data)))
  predictions <- numeric(nrow(data))
  for(i in 1:nrow(data)){
    if(data[i, tree$feature] <= tree$threshold){
      predictions[i] <- predict_tree(tree$left, data[i, ])
    }else{
      predictions[i] <- predict_tree(tree$right, data[i, ])
    }
  }
  return(predictions)
}

# example

# generate mock data
set.seed(123)
n <- 1000
x1 <- rnorm(n, mean=10, sd=2)
x2 <- rnorm(n, mean=5, sd=3)
y <- x1 + x2 + rnorm(n, mean=0, sd=1)
data <- data.frame(x1, x2, y)

# split data into training and testing set
train_index <- sample(1:n, size=floor(0.7 * n), replace=FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# build the tree
tree <- build_tree(train_data, "y", max_depth=3)

# make predictions using the tree
train_predictions <- predict_tree(tree, train_data)
test_predictions <- predict_tree(tree, test_data)

# calculate mean squared error for training and testing sets
train_mse <- mean((train_data$y - train_predictions)^2)
test_mse <- mean((test_data$y - test_predictions)^2)

# print results
cat("Train MSE: ", train_mse, "\n")
cat("Test MSE: ", test_mse, "\n")

