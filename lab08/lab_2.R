#lab 2
exI1 = function()
{
  date = scan("sample1.txt")
  stem(date)
}

exI2 = function()
{
  table = read.csv("unemploy2012.csv", header = T, sep = ';')
  rate = table[['rate']]
  interval = c(0, 4, 6, 8, 10, 12, 14, 30)
  hist(rate, breaks = interval, right = T, freq = T)
}

exI3 = function()
{
  table = read.csv("life_expect.csv", header = T, sep = ',')
  
  male = table[['male']]
  min_man = min(male)
  max_man = max(male)
  interval = seq(min_man, max_man, length = 8)
  hist(male, breaks = interval, right = T, freq = F)
  
  female = table[['female']]
  min_fem = min(female)
  max_fem = max(female)
  second_interval = seq(min_fem, max_fem, length = 8)
  hist(female, breaks = second_interval, right = T, freq = F)
}

exII1 = function()
{
  date = scan("sample1.txt")
  mean(sample)
  median(sample)
}

exII2 = function()
{
  table = read.csv("life_expect.csv", header = T)
  male = table[['male']]
  mean(male)
  median(male)
  
  female = table[['female']]
  mean(female)
  median(female)
}

outliers_mean = function(x){
  m = mean(x)
  s = sd(x)
  
  outliers = vector()
  k = 0
  for (i in 1:length(x))
  {
    if (x[i] < m-2*s | x[i] > m+2*s)
    {
      k = k + 1
      outliers[k] = x[i]
    }
  }
  return(outliers)
}

outliers_iqr = function(x){
  q_1 = as.vector(quantile(x))[2]
  q_3 = as.vector(quantile(x))[4]
  iqr = q_3 - q_1
  
  outliers = vector()
  k = 0
  for (i in 1:length(x))
  {
    if (x[i] < q_1 - 1.5 * iqr | x[i] > q_3 + 1.5 * iqr)
    {
      k = k + 1
      outliers[k] = x[i]
    }
  }
  return(outliers)
}

exIII1 = function(x)
{
  return(outliers_mean(x))
}

#exIII1(c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17))

exIII2 = function(x)
{
  return(outliers_iqr(x))
}

exIII3 = function()
{
  sample = scan("sample2.txt")
  summary(sample)
  
  q = outliers_mean(sample)
  w = outliers_iqr(sample)
  
  flag = T
  if(length(q) != length(w))
    flag = F
  else
  {
    for(i in 1:length(q))
      if(q[i] != w[i])
        flag = F
  }
  
  if(flag == F)
    print("not equal")
  else
    print("equal")
}