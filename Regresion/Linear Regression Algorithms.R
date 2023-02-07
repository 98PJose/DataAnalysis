#### Linear Regression Algorithms ####

#### Numeric Estimation ####

linear_regression_numeric <- function(data,alpha,iterations) {
  # Split data into dependent and independent variables
  x <- as.matrix(data[, -1])
  y <- as.matrix(data[, 1])
  
  # Initialize coefficients
  beta <- matrix(rep(0, ncol(x)), ncol=1)
  
  # Define the hypothesis function
  hypothesis <- function(x, beta) {
    x %*% beta
  }
  
  # Define the cost function
  cost_function <- function(x, y, beta) {
    m <- length(y)
    J <- sum((hypothesis(x, beta) - y)^2)/(2 * m)
    J
  }
  
  # Define the gradient descent algorithm
  gradient_descent <- function(x, y, beta, alpha, iterations) {
    m <- length(y)
    for (i in 1:iterations) {
      beta <- beta - (alpha/m) * t(x) %*% (hypothesis(x, beta) - y)
    }
    beta
  }
  
  # Train the model
  beta <- gradient_descent(x, y, beta, alpha, iterations)
  
  # Make predictions
  predictions <- hypothesis(x, beta)
  
  return(list(coefficients=beta, predictions=predictions))
}

# example

# Generate mock-up data
set.seed(123)
n <- 100
x1 <- runif(n, 0, 10)
x2 <- runif(n, 0, 10)
y <- 2 + 3 * x1 + 4 * x2 + rnorm(n, 0, 2)

# Combine the data into a data frame
data <- data.frame(y, x1, x2)

remove(x1,x2,y,n) # already in data

# fit the model
alpha <- 0.01
iterations <- 1000

linear_regression_numeric(data,alpha,iterations)

#### Analytic Estimation ####

linear_regression_analytic <- function(data) {
  
  # Split data into dependent and independent variables
  x <- as.matrix(data[, -1])
  y <- as.matrix(data[, 1])
  
  # Fit betas
  
  beta <- solve((t(x)%*%x))%*%(t(x)%*%y)
  
  prediction <- x%*%beta
  
  # Output
  
  return(list('Coefficients'=beta,'Predictions'=prediction))
  
}

# example

# generate mock-up data
set.seed(123)
n <- 100
x1 <- runif(n, 0, 10)
x2 <- runif(n, 0, 10)
y <- 2 + 3 * x1 + 4 * x2 + rnorm(n, 0, 2)

# combine the data into a data frame
data <- data.frame(y, x1, x2)

remove(x1,x2,y,n) # already in data

# fit the model
linear_regression_analytic(data)

# plot




