inside_torus <- function(x, y, z, R, r){
  if((sqrt(x^2 + y^2) - R)^2 + z^2 <= r^2)
    return(T)
  return(F)
}

volume_estimation <- function(n){
  # definitions
  R <- 10
  r <- 3
  
  x_min <- -R - r
  x_max <-  R + r
  
  y_min <- -R - r
  y_max <-  R + r
  
  z_min <- -r
  z_max <-  r
  
  exact_value <- 2*R*pi*pi*r*r
  k <- 0
  
  for(i in 1:n)
  {
    a <- runif(1, x_min, x_max)
    b <- runif(1, y_min, y_max)
    c <- runif(1, z_min, z_max)
    if(inside_torus(a, b, c, R, r) == T)
      k <- k + 1
  }
  
  volume_box <- (x_max - x_min) * (y_max - y_min) * (z_max - z_min)
  estimated_value <- volume_box * k / n
  
  absolute_error <- abs(exact_value - estimated_value)
  relative_error <- absolute_error / abs(exact_value)
  cat("for n = ", n, "the relative error =", relative_error, "\n")
}

exB1 <- function()
{
  volume_estimation(10000)
  volume_estimation(20000)
  volume_estimation(50000)
}

inside_triangle <- function(x, y){
  return(y >= 0 && y <= 2*x && y <= 6 - 3*x)
}

area_estimation <- function(n){
  # definitions; i found out the values in the "B2_extra" photo
  x_min <- 0
  x_max <- 2 # not 6 / 5
  
  y_min <- 0
  y_max <- 12 / 5
  
  k <- 0
  
  for(i in 1:n)
  {
    a <- runif(1, x_min, x_max)
    b <- runif(1, y_min, y_max)
    if(inside_triangle(a, b))
      k <- k + 1
  }
  
  exact_value <- 0
  
  volume_box <- x_max * y_max
  estimated_value <- volume_box * k / n
  
  cat("the area of the rectangle =", x_max * y_max, "\n")
  cat("the area (estimated) of T =", estimated_value, "\n\n")
}

exB2 <- function()
{
  n <- 20000
  for(i in 1:5)
  {
    cat("for n =", n, "\n")
    area_estimation(n)
    n <- n + 10000
  }
}

integration <- function(a, b, subproblem, exact_value){
  if(subproblem == 'a')
    f <- function(x) (2*x - 1) / (x*x - x - 6)
  
  if(subproblem == 'b')
    f <- function(x) (x + 4) / (x - 3)^(1 / 3) 
  
  if(subproblem == 'c')
    f <- function(x) (x * exp(-1 * x * x))
  
  n <- 100000
  k <- 0
  
  for(i in 1:n)
  {
    u <- runif(1, a, b)
    k <- k + f(u)
  }
  
  estimated_value <- (b - a) * k / n
  cat("exact value:", exact_value, "\n")
  cat("estimated value:", estimated_value, "\n\n")
}

improved_integration <- function(subproblem, exact_value){
  if(subproblem == 'a')
    f <- function(x) (2*x - 1) / (x*x - x - 6)

  if(subproblem == 'b')
    f <- function(x) (x + 4) / (x - 3)^(1 / 3) 
  
  if(subproblem == 'c')
    f <- function(x) (x * exp(-1 * x * x))
  
  n <- 100000
  k <- 0
  lambda <- 3
  
  for(i in 1:n)
  {
    u = rexp(1, lambda)
    density = lambda * exp(-u * lambda)
    k = k + (f(u) / density )
  }
  
  estimated_value <- k / n 
  
  cat("exact value:", exact_value, "\n")
  cat("estimated value:", estimated_value, "\n\n")
}

exB3 <- function()
{
  #improved_integration('a', log(3) - log(2))
  #improved_integration('b', 61.2)
  integration(-1, 1, 'a', log(3) - log(2))
  integration(3, 11, 'b', 61.2)
  improved_integration('c', 1/2)
}


exB4a <- function()
{
  nr_simulations <- 10000
  n <- 1000
  p <- 1 / 4
  q <- 1 / 100

  results <- c() # empty vector
  
  for(i in 1:nr_simulations)
  {
    users <- 10000
    years <- 0
    
    while(users < 15000)
    {
      users <- users + rbinom(1, n, p) - rbinom(1, users, q)
      years <- years + 1
    }
    results <- append(results, years)
  }
  
  cat("after", mean(results), "years the number of users will be 1500\n")
}

exB4b <- function(nr_simulations = 10000)
{
  # considering that the amount of user can only increase
  n <- 1000
  p <- 1 / 4
  q <- 1 / 100
  
  target <- 15000
  time <- 40 + 10 / 12
  results <- 0
  
  for(i in 1:nr_simulations)
  {
    users <- 10000
    years <- 0
    flag <- T # determines if the target number of users 
    # is obtained after the specified amount of time

    while(years < 40 && flag == T) # forty years
    {
      users <- users + rbinom(1, n, p) - rbinom(1, users, q)
      years <- years + 1
      if(users > target)
        flag <- F
    }
    
    if(flag == T)
    {
      users <- users + (rbinom(1, n, p) - rbinom(1, users, q)) * 10 / 12
      if(users <= target)
        results <- results + 1
    }
  }
  
  cat(results / nr_simulations, "is the required probability\n")
}

exB4c <- function()
{
  error_margin <- 0.01
  confidence_level <- 0.99
  alpha <- 1 - confidence_level
  z <- qnorm(alpha / 2)
  p <- 0.3
  n_min <- p*(1 - p) * (z / error_margin)^2 + 1
  cat("instead of 10000 simulations, we are using", n_min, "and ")
  exB4b(n_min)
}

# calls:

# exB1()
# exB2()
# exB3()
# exB4a()
# exB4b()
# exB4c()