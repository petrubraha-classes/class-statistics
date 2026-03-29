exD1 <- function()
{
  data <- read.csv("probabilitati.csv", header = TRUE, sep = "")$probabilitati
  n <- length(data)
  x <- mean(data)
  variance <- 92.16
  
  alpha1 <- 0.05
  alpha2 <- 0.01
  
  critical_z1 <- qnorm(1 - alpha1 / 2)
  critical_z2 <- qnorm(1 - alpha2 / 2)
  
  margin_err1 <- sqrt(variance / n) * critical_z1
  margin_err2 <- sqrt(variance / n) * critical_z2
  
  
  cat("first interval : [", x - margin_err1)
  cat(",", x + margin_err1, "] with 95% confidence interval\n")

  cat("secnd interval : [", x - margin_err2)
  cat(",", x + margin_err2, "] with 99% confidence interval\n")
}

exD2 <- function()
{
  data <- read.csv("statistica.csv", header = TRUE, sep = "")$statistica
  n <- length(data)
  x <- mean(data)
  
  alpha1 <- 0.05
  alpha2 <- 0.01
  
  critical_t1 <- qt(1 - alpha1 / 2, n - 1)
  critical_t2 <- qt(1 - alpha2 / 2, n - 1)
  
  margin_err1 <- sd(data) / sqrt(n) * critical_t1
  margin_err2 <- sd(data) / sqrt(n) * critical_t2
  
  
  cat("first interval : [", x - margin_err1)
  cat(",", x + margin_err1, "] with 95% confidence interval\n")
  
  cat("secnd interval : [", x - margin_err2)
  cat(",", x + margin_err2, "] with 99% confidence interval\n")
}

exD3 <- function()
{
  # H0 the change was not significant
  # Ha the change was => right tailed
  
  n <- 100
  trouble_makers <- 14
  
  p_prim <- (n - trouble_makers) / n
  p_zero <- 0.85
  
  alpha1 <- 0.01
  alpha2 <- 0.05
  
  z_score <- (p_prim - p_zero) / sqrt(p_zero * (1 - p_zero) / n)
  critical_z1 = qnorm(1 - alpha1)
  critical_z2 = qnorm(1 - alpha2)
  
  result1 <- "can not reject H0"
  if(z_score > critical_z1)
    result1 <- "H0 is rejected"
  result2 <- "can not reject H0"
  if(z_score > critical_z2)
    result2 <- "H0 is rejected"
  
  cat("for 1% level of significance", result1, "\n")
  cat("for 5% level of significance", result2, "\n")
}

# calls:

# exD1()
# exD2()
# exD3()