### Exercise session 2

# 1
(vec1 <- rep(3, times=5))
(vec2 <- rep(c(3,1,2), each = 2))
(vec3 <- rep(c(3,1,2), length.out = 5))
(vec4 <- rep(c(3,1,2), times = c(3,1,2))) #not 'each'

# 2
(vec5 <- seq(from = 1, to = 9, by = 2))
(vec6 <- seq(along.with=c(3,2,1), from = 1, to = 10))
(vec7 <- seq(from = 1, to = 10, length.out = 7))
(vec8 <- seq(from = 10, to = 2, by = -2))

# 3
system.time(
  rep(3, times=1e6)
) # returns 0, 0, 0
system.time(
  replicate(1e6, 3)
) # returns 0, 0, 1.11 time elapsed

# 4
object.size(vec9a <- 1:10) # obj size = 96 bytes
setClass("int_seq_class", slots = c(x = "integer"))
object.size(vec9b <- new("int_seq_class",x = 1:10)) # object size = 728 bytes

object.size(vec10a <- seq(1, 10, by = 1)) # 176 bytes
setClass("num_seq_class", slots = c(x = "numeric"))
object.size(vec10b <- new("num_seq_class", x = seq(1, 10, by = 1))) # 808 bytes

object.size(vec11a <- matrix(1:10, nrow = 5)) # 264 bytes
setClass("mat_seq_class", slots = c(x = "matrix"))
object.size(vec11b <- new("mat_seq_class", x = matrix(1:10, nrow = 5))) # 896 bytes

object.size(vec12a <- matrix(seq(1, 10, by = 1), nrow = 5)) # 344 bytes
object.size(vec12b <- new("mat_seq_class", x = matrix(seq(1, 10, by = 1), nrow = 5))) # 976 bytes

object.size(obj05 <- 1:10) # 96 bytes
class(obj05) <- "humpty"
object.size(obj05) # 320 bytes

object.size(obj06 <- seq(1, 10, by = 1)) # 176 bytes
class(obj06) <- "dumpty"
object.size(obj06) # 400 bytes

## order==>  1:10  seq(1, 10, by = 1)  matrix(1:10, nrow = 5)  obj05 <- 1:10  matrix(seq(1, 10, by = 1), nrow = 5)  obj06 <- seq(1, 10, by = 1)

# 5
object.size(rep(c(TRUE, FALSE), 5)) # 96 bytes
object.size(seq(1, 10, by = 1)) # 176 bytes
object.size(vector(mode = "logical", length = 10)) # 96 bytes
object.size(vector(mode = "numeric", length = 10)) # 176 bytes
object.size(vector(mode = "list", length = 10)) # 176 bytes
object.size(vector(mode = "character", length = 10)) # 232 bytes

# Q: what surprises you?
# A: vector of type "list" has same size as 'double', even though more "demanding" type

# 6
object.size(NA) # 56 bytes
object.size(NULL) # 0 bytes
object.size(obj07 <- c(label = NULL)) # 0 bytes
object.size(obj08 <- c(label = NA)) # 280 bytes

# 7
(pi == 2*log(1i)/1i) # returns TRUE

# 8
setClass("alphabetS4", slots = c(symbols = "character", size = "numeric", type = "character"))
alphabet_vec1 <- new("alphabetS4", symbols = letters, size = 26, type = "roman")


setRefClass("alphabetRC", fields = c(symbols = "character", size = "numeric", type = "character"))
alphabet_vec2 <- new("alphabetRC", symbols = letters, size = 26, type = "roman")

# 9
list1 <- list(a = alphabet_vec1, b = alphabet_vec2)
env1 <- new.env()
assign("alphabet_vec2", value = alphabet_vec2, envir = env1)

object.size(alphabet_vec1) # 2736 bytes
object.size(alphabet_vec2) # 688 bytes
object.size(list1) # 3776 bytes
object.size(env1) # 56 bytes

# rank==> env1  alphabet_vec1  alphabet_vec2  list1

# 10
vecX <- c(symbols = "a", size = "1", type = "roman")
expr1 <- expression(vecY <- c(symbols = "a", size = "1", type = "roman"))

vecX == (vecY <- eval(expr1)) # returns {symbols: TRUE, size: TRUE, type: TRUE}
object.size(expr1) # 1120 bytes

# 11
error_message <- simpleError("Watch out for this!")
attributes(error_message)
# expectation: object.size(error_message) > object.size("Watch our for this!"), 
# because former is list, which is more demanding
object.size(error_message) > object.size("Watch our for this!") # returns TRUE

# 12
is.recursive(error_message) # returns TRUE
