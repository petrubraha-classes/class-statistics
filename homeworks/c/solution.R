random_permutation <- function(n) {
  U = runif(n)
  permutation = 1:n
  
  # sort
  for (i in 2:n) 
  {
    key = U[i]
    key_index = permutation[i]
    j = i - 1
    
    while (j >= 1 && U[j] > key) {
      U[j + 1] = U[j]
      permutation[j + 1] = permutation[j]
      j = j - 1
    }
    
    U[j + 1] = key
    permutation[j + 1] = key_index
  }
}

exC1a <- function(n)
{
  random_permutation(n)
}

# b-----------------------------------------------------------------------------

bits_generation <- function(n, k) {
  bit_strings <- list()
  for (i in 1:n) 
  {
    string <- sample(c(0,1), k, replace <- T)
    bit_strings[[i]] <- string
  }
  
  return(bit_strings)
}

cmp <- function(Wi, Wj) {
  Lij <- min(length(Wi), length(Wj))
  for (h in 1:Lij) {
    if (Wi[[h]] < Wj[[h]])
      return(T) 
    else if (Wi[[h]] > Wj[[h]])
      return(F) # Wi nu este strict mai mic decat Wj
  }
  
  # Wi == Wj => add
  while(T) 
  {
    if (length(Wi) < length(Wj))
      Wi <- c(Wi, sample(c(0, 1), 1))
    else if (length(Wj) < length(Wi))
      Wj <- c(Wj, sample(c(0, 1), 1))
    else {
      Wi <- c(Wi, sample(c(0, 1), 1))
      Wj <- c(Wj, sample(c(0, 1), 1))
    }
    
    if (Wi[[length(Wi)]] < Wj[[length(Wj)]])
      return(T)
    else if (Wi[[length(Wi)]] > Wj[[length(Wj)]])
      return(F)
  }
}

exC1b <- function()
{
  n <- 6
  k <- 5
  words <- bits_generation(n, k)
  words
  
  Wi <- words[[4]] 
  Wj <- words[[6]]
  
  rezultat <- cmp(Wi, Wj)
  cat("Wi < Wj:", rezultat, "\n")
}

# c-----------------------------------------------------------------------------

random_qsort <- function(U){
  if (length(U) <= 1)
    return(U)
  
  pivot_index <- sample(1:length(U), 1)
  pivot <- U[[pivot_index]]
  
  st <- list()
  dr <- list()
  
  for (i in 1:length(U))
    if (i != pivot_index) {
      if (cmp(U[[i]], pivot) == T)
        st <- c(st, list(U[[i]]))
      else
        dr <- c(dr, list(U[[i]]))
    }
  
  sorted_st <- random_qsort(st)
  sorted_dr <- random_qsort(dr)
  
  return(c(sorted_st, list(pivot), sorted_dr))
}

exC1c <- function()
{
  n <- 9 
  k <- 7
  words <- bits_generation(n, k)
  random_qsort(words)
}

# d-----------------------------------------------------------------------------

equal_words <- function(Wi, Wj) {
  Lij <- min(length(Wi), length(Wj))
  
  for (i in 1:Lij)
    if (Wi[[i]] != Wj[[i]])
      return(F)
  return(T)
}

random_prqs <- function(n, k) {
  words <- bits_generation(n, k)
  sorted_words <- random_qsort(words)
  
  index_vector <- numeric(n)
  for (i in 1:length(sorted_words))
    for (j in 1:length(words))
      if ( equal_words ( sorted_words[[i]], words[[j]] ) ) {
        index_vector[i] <- j
        break
      }
  
  return(index_vector)
}

exC1d <- function()
{
  print(random_prqs(6, 5))
}

# C2a---------------------------------------------------------------------------

exC2a <- function(n)
{
  graph <- matrix(0, nrow = n * 2 + 1, ncol = n * 2 + 1)
  
  for (i in 1:(n * 2 + 1))
    for (j in 1:(n * 2 + 1))
    {
      graph[i, j] <- 1
      graph[i, i] <- 0
    }
  
  a <- sample(1:(n * 2 + 1), n + 1)
  b <- setdiff(1:(n * 2 + 1), a)
  
  cut_edges <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) 
    for (j in 1:n)
      if (graph[a[i], b[j]] == 1)
        cut_edges[i, j] <- 1
  
  return(sum(cut_edges)) # cardinality
}

# b-----------------------------------------------------------------------------

# somethings wrong
exC2b <- function()
{
  # we will use the amplification process for the algorithm
  # variables choosen arbitrarly
  runs <- 10000
  n <- 4
  
  max_cut_cardinality <- 0
  for (i in 1:runs) 
  {
    result <- max_cut_random_algorithm(n)
    if(result > max_cut_cardinality)
      max_cut_cardinality <- result
  }
  
  cat("after", runs, "runs, the max cardinality found is", max_cut_cardinality)
}

# calls:

# exC1a(15)
# exC1b()
# exC1c()
# exC1d()
# exC2a(15)
# exC2b()