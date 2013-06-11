# What's the fastest way to determine if a vector is constant?

is.constant1 <- function(x) all(x == x[1])

# One less comparison, but has to duplicate whole vector
is.constant2 <- function(x) all(x[-1] == x[1])

# Use unique
is.constant3 <- function(x) length(unique(x)) == 1



x_hard <- c(rep(1, 1000), 2)
x_easy <- c(2, rep(1, 1000))

library(microbenchmark) 
options(digits = 3)

print(microbenchmark(
  is.constant1(x_hard),  
  is.constant2(x_hard),
  is.constant3(x_hard),

  is.constant1(x_easy),  
  is.constant2(x_easy),
  is.constant3(x_easy)
  
))