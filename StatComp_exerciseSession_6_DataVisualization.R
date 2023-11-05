library(lattice)
library(ggplot2)
par(mfrow = c(2,2), mfcol = c(2,2))

# 1
mtcars$cyl <- factor(mtcars$cyl)

coplot(mtcars$mpg ~ mtcars$disp | mtcars$cyl, 
       show.given = F, 
       xlab = c("disp","Given : factor(cyl)"), 
       ylab = c("mpg"))

# 2
coplot(mtcars$mpg ~ mtcars$disp | mtcars$cyl, 
       show.given = F, 
       xlab = c("disp","Given : factor(cyl)"), 
       ylab = c("mpg"),
       panel = function(x,y,...){
         points(x = x, y = y)
         lines(lowess(x=x,y=y), col = "red")
       })

# 3

# 4
plt1 <- xyplot(mtcars$mpg ~ mtcars$disp,
                 xlab = "disp",
                 ylab = "mpg")
object.size(plt1) # 371200

# 5
plt2 <- xyplot(mtcars$mpg ~ mtcars$disp | mtcars$cyl, 
               show.given = F, 
               xlab = c("disp"),
               ylab = c("mpg"),
               )
object.size(plt2) # 375792

# 6
plt3 <- xyplot(mtcars$mpg ~ mtcars$disp | mtcars$cyl, 
               show.given = F, 
               xlab = c("disp"),
               ylab = c("mpg"),
               panel = function(x,y,...){
                 panel.xyplot(x,y, ...)
                 panel.loess(x,y, ...)
               }
)
object.size(plt3) # 388584

# 7
plt4 <- ggplot(mtcars, 
               mapping = aes(x = disp, y = mpg))
object.size(plt4) # 12984

# 8
plt4 + geom_point()

# 9
plt4 + geom_point() + facet_wrap(facets = ~cyl,
                                 nrow = 2, as.table = F)

# 10
plt4 + geom_point() + 
  facet_wrap(facets = ~cyl,
             nrow = 2, as.table = F) + 
  geom_smooth(method = "loess", se = F)
