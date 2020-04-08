library(plotrix)
library(zeallot)

set.seed(1)
data_generation <- function () {
      n=100
      xx1=rnorm(n)
      xx2=(xx1+rnorm(n))/sqrt(2)
      yy=xx1+xx2/2+rnorm(n)/5

      x1 <<- xx1-mean(xx1)
      x2 <<- xx2-mean(xx2)
      y <<- yy-mean(yy)
}
data_generation()
dotproduct <- function(vector1, vector2) {
  return(sum(vector1 * vector2))
}

vector_length <- function(vector) {
  return(sqrt(dotproduct(vector, vector)))
}

angle <- function(vector1, vector2) {
  return(acos(dotproduct(vector1, vector2) / (vector_length(vector1) * vector_length(vector2))))
}

cat("The arcos value(angle) between x1 and x2 is : ", angle(x1, x2))

# Creates empty plot and variables
plot(1,type="n",axes=F,xlim=c(-1,10),ylim=c(-1,10),xlab="",ylab="")

# pseudo_X1
pseudo_x1 = c(vector_length(x1), 0)
# arrows(0,0,pseudo_x1[1],pseudo_x1[2])
draw.radial.line(0, pseudo_x1[1], center = c(0,0))
text(pseudo_x1[1] + 0.2, pseudo_x1[2] + 0.3, expression(x[1]),cex=1.5)

# pseudo_X2
pseudo_x2 = c(4.15, 8.6)
draw.radial.line(0,vector_length(x2),center=c(0,0), angle = angle(x1, x2))
# arrows(0,0, pseudo_x2[1], pseudo_x2[2] )
text(pseudo_x2[1] + 1, pseudo_x2[2] + 1.5, expression(x[2]),cex=1.5)

# drawing arc
draw.arc(0,0,1, angle2 = angle(x1, x2))
text(2.7,1, angle(x1,x2),cex=1.5)


# 알고리듬
data_generation()

# bind as matrix for operations
X <- cbind(x1, x2)

# maintain dimensions
c(n, p) %<-% dim(X)

# initial residual and beta
residual <- y
beta <- rep(0, p+1)
threshold <- 0.000001

# Count iterations
n_iterations <- 0

# Iterate until convergence
while (abs(cor(residual, x1)) > threshold | abs(cor(residual, x2)) > threshold) {
  if (abs(cor(residual, x1)) > abs(cor(residual, x2))){
    model <- lm(residual~x1)
    beta <- beta + c(model$coefficients[1], model$coefficients[2], 0)
    y_fit <- beta[1] + beta[2] * x1 + beta[3] * x2
    residual <- y - y_fit
  } else {
    model <- lm(residual~x2)
    beta <- beta + c(model$coefficients[1], 0, model$coefficients[2])
    y_fit <- beta[1] + beta[2] * x1 + beta[3] * x2
    residual <- y - y_fit
  }
  n_iterations <- n_iterations + 1
}
cat('Total iteration: ', n_iterations, '\n')
cat('Intercept: ', beta[1], ', beta_1: ', beta[2], ', beta_2: ', beta[3])
