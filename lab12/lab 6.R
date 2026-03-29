# known variance 
zconfidence_interval <- function(alpha, n, sample_mean, sigma){
  critical_z <- qnorm(1 - alpha/2, 0, 1)
  a <- sample_mean - critical_z * sigma / sqrt(n)
  b <- sample_mean + critical_z * sigma / sqrt(n)
  interval <- c(a, b)
  return(interval)
}
zconfidence_interval_file <- function(alpha, sigma, filename){
  sample <- scan(file_name)
  sample_mean <- mean(sample)
  n <- length(sample)
    
  critical_z <- qnorm(1 - alpha/2, 0, 1)
  a <- sample_mean - critical_z * sigma / sqrt(n)
  b <- sample_mean + critical_z * sigma / sqrt(n)
  interval <- c(a, b)
  return(interval)
}

exII2 <- function()
{
  cat(zconfidence_interval(0.1, 25, 67.53, 10))
}

exII3 <- function()
{
  cat(zconfidence_interval(0.05, 50, 5, 0.5))
}

# larger confidence == larger interval

exII6 <- function()
{
  cat(zconfidence_interval_file(0.05, 5, "history.txt"))
}

# unknown sigma
tconfidence_interval <- function(alpha, n, sample_mean, s){
  # sigma = s
  # critical_z = critical_t
  critical_t <- qt(1 - alpha/2, n - 1)
  a <- sample_mean - critical_t * s / sqrt(n)
  b <- sample_mean + critical_t * s / sqrt(n)
  interval <- c(a, b)
  return(interval)
}
tconfidence_interval_file <- function(alpha, file_name){
  
  sample <- scan(file_name)
  sample_mean <- mean(sample)
  s <- sd(sample)
  n <- length(sample)
  
  critical_t <- qt(1 - alpha/2, n - 1)
  a <- sample_mean - critical_t * s / sqrt(n)
  b <- sample_mean + critical_t * s / sqrt(n)
  interval <- c(a, b)
  return(interval)
}

exIII2 <- function()
{
  cat(tconfidence_interval(0.01, 196, 44.65, sqrt(2.25)))
}

exIII3 <- function()
{
  cat("for a\n")
  cat(tconfidence_interval(0.01, 49, 12, 1.75), "\n")
  cat(tconfidence_interval(0.05, 49, 12, 1.75), "\n")
  
  cat("for b:", tconfidence_interval(0.05, 49, 13.5, 1.25))
}

exIII5 <- function()
{
  sample <- c(12, 11, 12, 10, 11, 12, 13, 12, 11, 11, 13, 14, 10)
  sample_mean <- mean(sample)
  n <- length(sample)
  s <- sd(sample)
  
  # print
  cat(tconfidence_interval(0.1, n, sample_mean, s))
  cat(tconfidence_interval(0.05, n, sample_mean, s))
  cat(tconfidence_interval(0.01, n, sample_mean, s))
}

exIII4 <- function()
{
  cat(tconfidence_interval_file(0.05, "history.txt"))
  cat(tconfidence_interval_file(0.01, "history.txt"))
}

# statistical hypotheses testing

test_proportion <- function(alpha, n, succeses, p0, type){
  p_prime = succeses / n
  z_score = (p_prime - p0) / sqrt(p0*(1 - p0)/n)
  
  if(type == 'r')
    critical_z <- qnorm(1 - alpha, 0, 1)
  if(type == 'l')
    critical_z <- qnorm(alpha)
  if(type == 's')
    critical_z <- qnorm(1 - alpha/2)
  
  return(list(score = z_score, crit = critical_z))
}

exIV2 <- function()
{
  alpha <- 0.05
  successes <- 20
  p0 <- 0.10
  n <- 150
  
  result <- test_proportion(alpha, n, successes, p0, 'l')
  
  cat("z_score :", result$score, "\n")
  cat("z_criti :", result$crit, "\n")
  
  if (result$score > result$crit) 
    cat("The percentage of defective components is significantly greater than 10% (we retain the alternative hypothesis).\n")
  else 
    cat("We cannot say that the percentage of defective components is significantly greater than 10% (we do not reject the null hypothesis).\n")
}