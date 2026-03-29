#'*last three exercises*

exII2 = function()
{
  f <- function(u) exp(-2*u^2)
  
  lambda = 3
  n = 50000
  sum = 0
  
  exact = sqrt(pi)/8
  for(i in 1:n)
  {
    u = rexp(1, lambda)
    density_u = lambda * exp(-u*lambda)
    sum = sum + (f(u) / density_u )
  }
  
  aprox = sum / n 
  
  abs_error = abs(aprox - exact)
  rel_error = abs_error / abs(exact)
  
  cat("estimated value:", format(aprox, digits=5), "\n")
  cat("exact value:", format(exact, digits=5), "\n")
  cat("absolute error:", format(abs_error, digits=4), "\n")
  cat("relative error:", format(rel_error, digits=5), "\n")
}

exIII2 = function()
{
  p1 = 1/4
  p2 = 3/4
  
  lambda1 = 4
  lambda2 = 12
  
  N = 10000
  sum_x = 0
  for (i in 1:N) 
  {
    x = p1*rexp(1, lambda1) + p2*rexp(1, lambda2)
    sum_x = sum_x + x
  }
  expectation = sum_x/N
  
  cat("estimated expectation:", expectation, "\n")
}

exIV1 = function()
{
  n = 100000
  x = rgeom(n, 0.3)
  y = rgeom(n, 0.5)
  
  p = mean(x > y^2)
  
  n = 100
  error = 1
  while (error > 0.005) {
    x = c(x, rgeom(n, 0.3))
    y = c(y, rgeom(n, 0.5))
    p_new = mean(x > y^2)
    error = qnorm(0.975)*sqrt(p_new*(1-p_new)/length(x))
    n = n + 100 
  }
  
  cat("estimated probability:", p, "\n")
  cat("number of runs:", length(x), "\n")
}

# ------------------------------------------------------------------------------
# i tried my best doesn't work properly
check_infection_all <- function(x, size = 40, target){
  nr <- 0
  for(i in 1:size)
  {
    if(x[i] == 1)
      nr <- nr + 1
  }
  return(nr >= target)
}
add_infected <- function(pc){
  # construct vector of indexes of uninfected computers
  index_uninfected <- c()
  for(i in 1:40)
  {
    if(pc[i] == 0)
      index_uninfected <- c(index_uninfected, i)
  }
  
  nr_infected <- 40 - length(index_uninfected)
  index_temporary_vector <- c()
  for(infected_pc in 1:nr_infected) # each infected can infect
    for(i in 1:length(index_uninfected)) # each uninfected 
    {
      probability <- runif(1, 0, 10)
      if(probability <= 2)
      {
        # choose randomly who gets infected
        temp <- sample(length(index_uninfected), 1)
        # add it to a temporary vector (if it doesn't already exists)
        x <- union(index_temporary_vector, index_uninfected[temp])
        index_temporary_vector <- x
      }
    }
  
  for(i in 1:length(index_temporary_vector))
    pc[index_temporary_vector[i]] <- 1
  return(pc)
}
remove_infected <- function(pc, size = 40, k){
  #construct vector of infected indexes
  index_infected <- c()
  for(i in 1:size)
    if(pc[i] == 1)
      index_infected <- union(index_infected, i)
  
  ck <- k
  for(i in 1:size)
    if(pc[i] == 1 && ck != 0)
    {
      # choose randomly which one will be cured
      temp <- sample(length(index_infected), 1)
      actual_index <- index_infected[temp]
      
      #remove infection
      pc[actual_index] <- 0
      index_infected[ !index_infected == actual_index]
      ck <- ck - 1
    }
  return(pc)
}
  
help_IV2 <- function(k, target = 40, max_time = 100000)
{
  nr_pc <- 40
  pc <- rep(0, 40)
  
  index_first_infected <- sample(40, 1) # choose randomly
  index_infected_vector <- c(index_first_infected)
  
  pc[index_first_infected] <- 1
  nr_infected <- 1
  
  time <- 0
  while(time < max_time && check_infection_all(pc, target = target) == F)
  {
    pc <- add_infected(pc)
    if(nr_infected < k)
      return(F) # remove all of them 
    pc <- remove_infected(pc, nr_pc, k)
    time <- time + 1
  }
  
  return(check_infection_all(pc, target = target))
}

exIV2ab <- function(k, target = 40, runs = 10000)
{
  nr <- 0
  for(i in 1:runs)
  {
    if(help_IV2(k, target) == T)
      nr <- nr + 1
  }
  
  cat("for k =", k, "the desired probability, in", runs, "runs")
  cat(" is", nr / runs, "\n")
}

exIV2c <- function(k)
{
  margin_error <- 0.01
  alpha <- 1 - 0.95
  z <- qnorm(alpha / 2)
  p <- 0 # change the value later
  n <- p*(1 - p)*(z / margin_error)^2
  exIVab(k, target = 15, runs = n)
}

exIV2 <- function()
{
  # a
  cat("a\n")
  for(i in 1:5)
  {
    k <- 2*i
    exIV2ab(k)
  }
  cat("\nb\n")
  
  #b
  for(i in 1:5)
  {
    k <- 2*i
    exIV2ab(k, target = 15)
  }
  cat("\nc\n")
  
  #c
  for(i in 1:5)
  {
    k <- 2*i
    exIV2c(k)
  }
}

# IT IS RECOMMENDED TO RUN "exIV2"