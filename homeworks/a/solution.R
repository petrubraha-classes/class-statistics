exA1a = function(lambda, p, n, m, k)
{
  x = seq(k, m, length(m-k))
  prob_x_p = dpois(x, lambda)
  prob_x_g = dgeom(x, p)
  prob_x_b = dbinom(x, size = n, prob = p)
  
  print(prob_x_b) #bin
  print(prob_x_g) #geo
  print(prob_x_p) #pis
}

exA1b = function(lambda, p, n, m, k)
{
  x = seq(k, m, length(m-k))
  prob_x_p = dpois(x, lambda)
  prob_x_g = dgeom(x, p)
  prob_x_b = dbinom(x, size = n, prob = p)
  
  plot(x, prob_x_b, main='binomial distribution', xlab='x', ylab='probability')
  plot(x, prob_x_g, main='geometric distribution', xlab='x', ylab='probability')
  plot(x, prob_x_p, main='poisson distribution', xlab='x', ylab='probability')
}

exA1c = function(lambda)
{
  final = 1-10^(-6);
  sum = 0
  k0 = 0
  while(sum <= final)
  {
    sum = sum + (lambda^k0) * exp(-lambda) / factorial(k0)
    k0 = k0 + 1
  }
  print(k0)
}

exA2a = function()
{
  data = read.csv("note_PS.csv", header = T, sep = ',')
  p_mark = data[['P']]
  s_mark = data[['S']]
  
  absolute_p = table(p_mark)
  relative_p = absolute_p / length(p_mark)
  
  absolute_s = table(s_mark)
  relative_s = absolute_s / length(s_mark)
  
  # as random variables?
  cat("media notelor la probabilitati este:", mean(p_mark), "\n") 
  cat("media notelor la statistica este:", mean(s_mark), "\n")
}

exA2b = function(file_name, sample_name)
{
  values = read.csv(file_name)
  sample = values[[sample_name]]
  
  q1 = as.vector(quantile(sample))[2]
  q3 = as.vector(quantile(sample))[4]
  iqr = q3 - q1
  for(i in 1:length(sample))
  {
    temp = sample[i]
    if(temp < q1 - 1.5*iqr | temp > q3 + 1.5*iqr )
      sample[!sample == temp]
  }
  
  interval = seq(0, 10, by=1)
  freq = hist(sample, breaks = interval, plot = FALSE)$counts
  bin_midpoints = (interval[-length(interval)] + interval[-1]) / 2
  
  plot(bin_midpoints, freq, type = "b",
       main = "frequency distribution",
       xlab = "interval value", ylab = "frequency", xaxt="n")
  axis(1, at=interval)
}

# calls: 

# exA1a(3, 0.5, 10, 9, 1)
# exA1b(3, 0.5, 10, 9, 1)
# exA1c(3)
# exA2a()
# exA2b("note_PS.csv", "P")