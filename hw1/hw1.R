library(plotrix)
set.seed(1)

n=100
xx1=rnorm(n)
xx2=(xx1+rnorm(n))/sqrt(2)
yy=xx1+xx2/2+rnorm(n)/5

x1=xx1-mean(xx1)
x2=xx2-mean(xx2)
y=yy-mean(yy)

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
text(pseudo_x2[1] + 1, sudo_x2[2] + 1.5, expression(x[2]),cex=1.5)

# drawing arc
draw.arc(0,0,1, angle2 = angle(x1, x2))
text(2.7,1, angle(x1,x2),cex=1.5)



# euijoon
b0 = 0
b1 = 0
b2 = 0
fit = b0+b1*x1+b2*x2
res = y - fit
cor(res,x1)
cor(res,x2)

fitvector = c()

while (abs(cor(res,x1))>1e-2 | abs(cor(res,x2))>1e-2){

fit = b0+b1*x1+b2*x2
res = y - fit
fitvector = cbind(fitvector,fit)

if (abs(cor(res,x1))>abs(cor(res,x2))){
  v = x1
  b1 = b1 + as.numeric(lm(res~v)$coef[2])
}else{
  v = x2
  b2 = b2 + as.numeric(lm(res~v)$coef[2])
}

cat("b1 is", b1, 'and', "b2 is", b2, '\n')
}

cor(res,x1)
cor(res,x2)

fitvector

for (j in 1:ncol(fitvector)){
  print(angle(x1, fitvector[,j]))
}


# bohyeon
ybar = mean(y)
beta_hat = c(ybar, 0, 0)
residual = y - ybar

a = 1
while (! ( (-1e-10 < cor(residual, x1) & cor(residual, x1) < 1e-10) & (-1e-10 < cor(residual, x2) & cor(residual, x2) < 1e-10))){
  if (abs(cor(residual, x1)) > abs(cor(residual, x2))){
    model = lm(residual~x1)
    coef = as.vector(model$coefficients)
    beta_hat = beta_hat + c(coef[1], coef[2], 0)
    fitted_values = beta_hat[1] + beta_hat[2] * x1 + beta_hat[3] * x2
    residual = y - fitted_values
  }
  else if (abs(cor(residual, x1)) < abs(cor(residual, x2))){
    model = lm(residual~x2)
    coef = as.vector(model$coefficients)
    beta_hat = beta_hat + c(coef[1], 0, coef[2])
    fitted_values = beta_hat[1] + beta_hat[2] * x1 + beta_hat[3] * x2
    residual = y - fitted_values
  }
  a = a + 1
}
cat('총 iteration 횟수는', a)

cat('iteration을 통해 수렴한 계수는 다음과 같다.', beta_hat)

lse_result = lm(y~x1+x2)
coef(lse_result)

# donook
library(zeallot)

X = cbind(x1, x2)
c(n, p) %<-% dim(X)
r <- y
beta