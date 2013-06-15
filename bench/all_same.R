# What's the fastest way to determine if a vector is all the same value?

# Note: doesn't handle NA and zero-length vectors
all_same1 <- function(x) all(x == x[1])

# One less comparison, but has to duplicate whole vector
# Note: doesn't handle NA and zero-length vectors
all_same2 <- function(x) all(x[-1] == x[1])

# Use unique. Handles NA and zero-length vectors
all_same3 <- function(x) length(unique(x)) == 1

# Like all_same1, but also handles NA and zero-length vectors
all_same4 <- function(x) {
  nas <- is.na(x)
  if (length(x) == 0 || all(nas))
    TRUE
  else if (any(nas))
    FALSE
  else
    all(x == x[1])
}


x_hard <- c(rep(1, 1000), 2)
x_easy <- c(2, rep(1, 1000))

library(microbenchmark)
options(digits = 3)

print(microbenchmark(
  all_same1(x_hard),
  all_same2(x_hard),
  all_same3(x_hard),
  all_same4(x_hard),

  all_same1(x_easy),
  all_same2(x_easy),
  all_same3(x_easy),
  all_same4(x_easy)

))
