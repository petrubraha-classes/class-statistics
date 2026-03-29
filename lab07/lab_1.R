vector_sqrt = function(x) {
  for(i in 1:length(x)) {
    if(x[i] > 0)
      x[i] = sqrt(x[i])
    else
      x[i] = sqrt(-x[i])
  }
}

variance = function(x, p){
  expectation = sum(x*p)
  variance = sum(p*(x-expectation)^2)
  return (variance)
}

ex8 = function()
{
  table = read.table("test.txt", header=T)
  x = table[['AA']]
  y = table[['BB']]
  plot(x, y, type='l', main='exercice 8 barplot')
  b = x*y
  c = abs(x-y)/(sum(x-y)^2)
}

ex9 = function(n, lam)
{
  x=0:(n-1)
  y=dpois(x, lam)
  barplot(y, space=0, main='exercice 9 barplot', xlab="x axis", ylab="y axis")
}

ex10 = function(n, p)
{
  x=0:(n-1)
  y=dgeom(x, p)
  barplot(y, space=0, main='exercice 10 barplot', xlab="x axis", ylab="y axis")
}

#ex9 (15, 0.3)
#ex10(15, 3)