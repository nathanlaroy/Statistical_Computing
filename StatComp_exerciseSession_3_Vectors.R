## Exercise session VECTORS

# 1)
(mat1 <- matrix(letters, nrow= 13, byrow = T))

# 2)
(mat2 <- matrix(letters, nrow = 2))
(t(mat2) == mat1)

# 3)
(mat1[, 2, drop = F])
(mat1[, -1, drop = F])
(t(mat1)[rep(c(F,T),13), drop = F])

# 4)
library(microbenchmark)
microbenchmark(mat1[, 2, drop = F])
microbenchmark(mat1[, -1, drop = F])
microbenchmark(t(mat1)[rep(c(F,T),13), drop = F])

# 5)
(mat1[c(1,3,5,8,11)])
(mat1[rbind(c(1,1), c(3,1), c(5,1), c(8,1), c(11,1))])
microbenchmark(mat1[c(1,3,5,8,11)])
microbenchmark(mat1[rbind(c(1,1), c(3,1), c(5,1), c(8,1), c(11,1))])

# 6)
vec1 <- 10:1
vec2 <- 1:10
vec1^vec2

# 7)
vec1 <- 10:1
vec2 <- c(3,2)
vec1^vec2

# 8)
mat3 <- matrix(seq(30, 270, by = 30), nrow = 3, ncol = 3)
mat3

divisor <- as.vector(c(2, 3, 5))

mat3/divisor
t(t(mat3)/divisor)

# 9)
vec2 <- 1:6
vec2

outer(vec2, vec2, "+")

# 10)
all_comb <- expand.grid(vec2,vec2)
mapply(sum, all_comb[,1], all_comb[,2])

microbenchmark(as.vector(outer(vec2, vec2, "+")))
microbenchmark(mapply(sum, all_comb[,1], all_comb[,2]))

# 11)
set.seed(123)
vec3 <- sample(vec1, size = 100, replace = TRUE)
head(vec3, n = 10)
tail(vec3, n = 10)

microbenchmark(cumsum(vec3))
microbenchmark(Reduce("+", vec3, accumulate = T))

# 12)
# a for-loop solution
a <- c()
for (i in 1:length(vec3)-1){
  b <- vec3[i] + vec3[i + 1]
  a <- append(a,b)
}
# a vapply solution
