#Econs Winter Project Code
library("MASS")
set.seed(1)


#PArt 1a
mu = c(1,0)
sd = matrix(c(1,0,0,1), nrow=2, ncol=2)
m = mvrnorm(10,mu,sd)
m

Mu = c(0,1)
Sd = matrix(c(1,0,0,1), nrow=2, ncol=2)
M = mvrnorm(10,Mu,Sd)
M




#Part 1b
A = matrix(c(0.1,0,0,0.1), nrow=2, ncol=2)
p = matrix(0, nrow=100, ncol=2)

for (i in 1:10)
{
    a = c(m[i,1],m[i,2])
    l = mvrnorm(10,a,A)
    
    for (j in 1:10)
    {
      p[(i-1)*10+j,1] = l[j,1]
      p[(i-1)*10+j,2] = l[j,2]
    }
}  




#Part 1c
B = matrix(c(0.1,0,0,0.1), nrow=2, ncol=2)
q = matrix(0, nrow=100, ncol=2)
q

for (i in 1:10)
{
  a = c(M[i,1],M[i,2])
  m = mvrnorm(10,a,A)
  
  for (j in 1:10)
  {
    q[(i-1)*10+j,1] = m[j,1]
    q[(i-1)*10+j,2] = m[j,2]
  }
}  
q


#PLOTTING THE GENERATED DATA
plot(p, xlab = "Independent Variable", ylab="Dependent Variable", main="Scatterplot of Simulated Data")       #Plotting the data of part 1b
plot(q, xlab = "Independent Variable", ylab="Dependent Variable", main="Scatterplot of Simulated Data")       #Plotting the data of part 1c






#Part 2
#Linear Regression For Part 1b
model.b = lm(p[,2]~p[,1])
summary(model.b)
coef(model.b)
plot(p, xlab = "Independent Variable", ylab="Dependent Variable", main="Scatterplot of Simulated Data")
abline(model.b)


#Linear Regression For Part 1c
model.c = lm(q[,2]~q[,1])
summary(model.c)
coef(model.c)
plot(q, xlab = "Independent Variable", ylab="Dependent Variable", main="Scatterplot of Simulated Data")
abline(model.c)




#Linear Regression For both parts combined
X = numeric(200)
Y = numeric(200)

#Copying the data to a new variable to combine them
for (k in 1:100)
{
  X[k] = p[k,1]
  Y[k] = p[k,2]
}

for (k in 101:200)
{
  X[k] = q[k-100,1]
  Y[k] = q[k-100,2]
}

#Model Fitting
Model.combined = lm(Y~X)
coef(Model.combined)
summary(Model.combined)

plot(Y~X, xlab = "Independent Variable", ylab="Dependent Variable", main="Scatterplot of Simulated Data")
abline(Model.combined)
