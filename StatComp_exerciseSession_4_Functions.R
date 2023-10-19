library(microbenchmark)
library(methods)

## 1)
Harmonic <- function(n){
  sum( rep(1,n) / seq(1:n) )
}

## 2)
rHarmonic <- function(n){
  if (n > 0){
    stopifnot(n >= 0)
    1/n + rHarmonic(n - 1)
  # } else if (n < 0){
  #   stopifnot(n <= 0)
  #   1/n + rHarmonic(n + 1)
  }
  else {
    0
  }
}

## 3)
microbenchmark(Harmonic(1000))
microbenchmark(rHarmonic(1000))

## 4)
`%fill%` <- function(a, b){
  a[which(is.na(a))] <- b
}

## 5)
`fill<-` <- function(x, value){
  x[which(is.na(x))] <- value
  x
}

## 6)
set.seed(123)
vec3 <- sample(c(5, -9, NA), size = 1e6, replace = TRUE)
microbenchmark(vec3 %fill% 0)

set.seed(123)
vec3 <- sample(c(5, -9, NA), size = 1e6, replace = TRUE)
microbenchmark(fill(vec3) <- 0)

## 7)
vec1 <- c(5, 55, NA, -9, -99, NA, NA, -5, -9, 999)
full1 <- function(a){
  Filter(Negate(is.na), a)
}
full2 <- function(a){
  a[which(!is.na(a))]
}

full1(vec1)
full2(vec1)
microbenchmark(full1(vec1))
microbenchmark(full2(vec1))

## 8)
vec1 <- c(5, 55, NA, -9, -99, NA, NA, -5, -9, 999)

twofun <- function(x, fun = sum){
  f <- fun
  temp <- as.numeric(length(x)-1)
  for (i in 1:(length(x)-1)){
    components <- c(x[i], x[i + 1])
    temp[i] <- f(components)
  }
  # return temp
  temp
}

twofun(vec1, fun = var)

## 9)
twofun <- function(x, fun = sum, ...){
  f <- fun
  temp <- as.numeric(length(x)-1)
  for (i in 1:(length(x)-1)){
    components <- c(x[i], x[i + 1])
    temp[i] <- f(components, ...)
  }
  # return temp
  temp
}

## 10)
twofunfun <- function(fun){
  fun1 <- fun
  twofun <- function(x, fun = fun1, ...){
    f <- fun
    temp <- as.numeric(length(x)-1)
    for (i in 1:(length(x)-1)){
      components <- c(x[i], x[i + 1])
      temp[i] <- f(components, ...)
    }
    # return temp
    temp
  }
  # return twofun
  invisible(twofun)
}

twomean <- twofunfun(fun = mean)
twomean(vec1)
# [1]  30  NA  NA -54  NA  NA  NA  -7 495

exists("fun",environment(twomean))
get("fun",environment(twomean))


## 11)
vec4 <- c("a", "rose", "is", "a", "rose", "is", "a", "rose")
class(vec4) <- "text"

library(methods)
# create method "freqs" for class "text"
freqs.text <- function(x){
  table(x)
}
# create generic for method "freqs" so no .text is needed
freqs <- function(x, ...){
  UseMethod("freqs")
}
freqs(vec4)

## 12)
# S4
setClass("txt4", contains="character")
setGeneric("count", function(x) standardGeneric("count"))
setMethod("count", "txt4", function(x) table(x))
vec5 <- new("txt4", c("a", "rose", "is", "a", "rose", "is", "a", "rose"))
count(vec5)

# RC
setRefClass("txtR", fields = c(x = "character"),
            methods = list(count = function() table(x)))
vec6 <- new("txtR", x = c("a", "rose", "is", "a", "rose", "is", "a", "rose"))
vec6$count()

## 13)
set.seed(123)
vec7 <- sample(letters, 1e6, replace = TRUE)

vec8 <- new("txt4", vec7)
vec9 <- new("txtR", x = vec7)
class(vec7) <- "text"

system.time(table(vec7, dnn = NULL))
system.time(freqs(vec7))
system.time(count(vec8))
system.time(vec9$count())

microbenchmark(table(vec7, dnn = NULL))
microbenchmark(freqs(vec7))
microbenchmark(count(vec8))
microbenchmark(vec9$count())

## 14)
linmod <- lm(dist ~ speed, data = cars)
linmod

parameters <- function(mod) {
  coefs <- coefficients(mod)
  coefs #add return of coefs object
}
predictions <- function(x, mod) {
  coefs <- parameters(mod) #store returned object in "coefs" so predictions() can access it
  coefs[1] + coefs[2] * x
}
predictions(x = 5:7, mod = linmod)