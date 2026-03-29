#'*first three exercises*

# exI1
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return(4* N_C /N);
}

sphere_volume = function(n){
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if(x*x + y*y + z*z <= 1)
      N_C = N_C + 1;
  }
  
  return(8*N_C/N);
}

exI1 = function()
{
  abs_err = abs(exI1(10000) - 4*pi/3);
  rel_err = abs_err/(4*pi/3);
  print(abs_err);
  print(rel_err);
}

# explanations
MC_integration = function(N){
  sum = 0;
  for(i in 1:N)
  {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  return(10*sum/N);
}

MC_improvd = function(n){
  sum = 0;
  for(i in 1:n)
  {
    u = rexp(1, 1);
    sum = sum + exp(-u*u) / exp(-u);
  }
  return (sum / n);
}

exI2 = function() 
{
  f <- function(x) -2*x^2 + 5*x - 2
  
  a = 0
  b = 2
  exact_area = integrate(f, a, b)$value
  
  n = 10000
  x = runif(n, a, b)
  y = runif(n, 0, max(f(x)))
  
  inside = y <= f(x)
  prop_inside = mean(inside)
  
  area = (b - a) * max(f(x)) * prop_inside
  rel_error = abs(exact_area - area) / exact_area
  
  result = list(area = area, exact_area = exact_area, rel_error = rel_error)
    
  cat("estimated area:", format(result$area, digits = 8), "\n")
  cat("exact area:", format(result$exact_area, digits = 8), "\n")
  cat("relative error:", format(result$rel_error, digits = 2), "%\n")
}

exII1b = function()
{
  f <- function(x) exp(x)
  
  a = 1
  b = 4
  exact = 51.87987
  
  n = 10000
  sum = 0
  for(i in 1:n)
  {
    u = runif(1, a, b)
    sum = sum + f(u)
  }
  
  aprox = (b-a) / n * sum
  
  abs_error = abs(exact - aprox)
  rel_error = abs_error / exact
  
  cat("estimated value:", format(aprox, digits=7), "\n")
  cat("exact value:", format(exact, digits=7), "\n")
  cat("absolute error:", format(abs_error, digits=5), "\n")
  cat("relative error:", format(rel_error, digits=5), "\n")
}

exII1d = function()
{
  f <- function(x) 1 / (4*x^2 - 1)
  
  exact = log(3/4)
  sum = 0
  n = 10000
  
  for(i in 1:n)
  {
    u = rexp(1, 1)
    sum = sum + (f(u) / exp(-u))
  }
  
  aprox = sum / n
  abs_error = abs(aprox - exact)
  rel_error = abs_error / abs(exact)
  
  cat("estimated value:", format(aprox, digits=5), "\n")
  cat("exact value:", format(exact, digits=5), "\n")
  cat("absolute error:", format(abs_error, digits=5), "\n")
  cat("relative error:", format(rel_error, digits=5), "\n")
}