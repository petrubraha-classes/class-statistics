# exI1c
density_I = function(mu, sigma) {
  x = seq(mu - 4 * sigma, mu + 4 * sigma, length = 1000)
  y = dnorm(x, mean = mu, sd = sigma)
  plot(x, y, main = 'normal distribution')
}

exI1c = function()
{
  density_I(0, 1)
}

# exII2
student_mean = function(n, df) {
  samples = rt(n, df = df)
  return(mean(samples))
}

density_II = function(df, n_values) {
  results = matrix(nrow = length(n_values), ncol = length(df))
  rownames(results) = n_values
  colnames(results) = df
  for (i in 1:length(n_values)) {
    for (j in 1:length(df)) {
      sample_mean = student_mean(n_values[i], df[j])
      results[i, j] = sample_mean
    }
  }
  return(results)
}

exII2 = function()
{
  df_values = c(2, 3, 4, 5)
  n_values = c(1000, 10000, 100000, 1000000)
  results = density_II(df_values, n_values)
  
  print("Sample means:")
  print(results)
  print("Expected mean (0):")
  print(matrix(0, nrow = length(n_values), ncol = length(df_values)))
}

#III2)
gamma_sample_mean = function(n, alpha, lam) {
  samples = rgamma(n, shape = alpha, rate = lam)
  return(mean(samples))
}

verify_gamma_CLT = function(alpha, lam, n, num_trials) {
  sample_means = numeric(num_trials)
  for (i in 1:num_trials) {
    sample_mean = gamma_sample_mean(n, alpha, lam)
    sample_means[i] = sample_mean
  }
  return(sample_means)
}

clt_approximation = function(alpha, lam, n, z) {
  mean_sample = alpha / lam
  var_sample = alpha / (lam^2)
  sd_sample = sqrt(var_sample / n)
  return(pnorm(mean_sample + z * sd_sample))
}

exIII2 = function()
{
  alpha = 2
  lam = 1
  n = 50
  N_values = c(5000, 10000, 20000)
  z_values = c(-1.5, 0, 1.5)
  
  for (N in N_values) {
    cat("For N =", N, "\n")
    for (z in z_values) {
      sample_means = verify_gamma_CLT(alpha, lam, n, N)
      clt_prob = clt_approximation(alpha, lam, n, z)
      cat("Approximation for z =", z, ":", clt_prob, "\n")
    }
  }
}

#IV2)
prob_X_greater_than_k = function(k, n, p) {
  if (k >= n)
    return(0)
  return(1 - pbinom(k, size = n, prob = p))
}

exIV2 = function()
{
  k = 3
  n = 10
  p = 0.5
  prob = prob_X_greater_than_k(k, n, p)
  cat("P(X >", k, ") =", prob, "\n") 
}