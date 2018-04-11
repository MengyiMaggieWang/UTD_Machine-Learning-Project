hour <- read.csv("/Users/mengyiwang/Downloads/Bike-Sharing-Dataset/hour.csv")
View(hour)
#Split dataset into "training"(70%) and "validation" (30%)
ind <- sample(2,nrow(hour),replace=TRUE,prob = c(0.7,0.3))
TRdata <- hour[ind==1,]
TEdata <- hour[ind==1,]
N_TRdata<- TRdata[,c(6,12,13,14,17)]
#pick hr，atemp，hum，windspeed，cnt
#implement the gradient descent algorithm 
Gradientdescent <- function(x1,x2,x3,x4,y,alpha, conv_threshold, max_iterations) {
  beta0 <- 1
  beta1 <- 1
  beta2 <- 1
  beta3 <- 1
  beta4 <- 1
  m <- nrow(N_TRdata)
  yhat <- beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4
  MSE <- sum((y - yhat) ^ 2) / m
  converged = FALSE
  iterations = 0
  while(converged == FALSE) {
    ## Implement the gradient descent algorithm
    MSE <- sum((y - yhat) ^ 2) / m
       beta0_new <<- beta0 - alpha * ((1 / m) * (sum(yhat - y)))
       beta1_new <<- beta1 - alpha * ((1 / m) * (sum((yhat - y) * x1)))
       beta2_new <<- beta2 - alpha * ((1 / m) * (sum((yhat - y) * x2)))
       beta3_new <<- beta3 - alpha * ((1 / m) * (sum((yhat - y) * x3)))
       beta4_new <<- beta4 - alpha * ((1 / m) * (sum((yhat - y) * x4)))
    beta0 <- beta0_new
    beta1 <- beta1_new
    beta2 <- beta2_new
    beta3 <- beta3_new
    beta4 <- beta4_new
    yhat <- beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4
    MSE_new <- sum((y - yhat) ^ 2) / m
    if(MSE - MSE_new <= conv_threshold) {
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3,beta4))
    }
    iterations = iterations + 1
    if(iterations > max_iterations) { 
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3,beta4))
    }
  }
}
# Assign data of training dataset to the gradientdescent model
Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.001, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
#linear regression model
#with test data
yhat <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif <- data.frame(N_TEdata$cnt-yhat)
MSEt <-sum(dif^2)/nrow(dif)
#with train data
yhat <- beta0_new+beta1_new*N_TRdata$hr+beta2_new*N_TRdata$atemp+beta3_new*N_TRdata$hum+beta4_new*N_TRdata$windspeed
dif <- data.frame(N_TRdata$cnt-yhat)
MSEt <-sum(dif^2)/nrow(dif)

#change alpha value
Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.005, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat1 <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif1<- data.frame(N_TEdata$cnt-yhat1)
MSE1<-sum(dif1^2)/nrow(dif1)

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.01, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat2 <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif2<- data.frame(N_TEdata$cnt-yhat2)
MSE2<-sum(dif2^2)/nrow(dif2)

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.015, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat3 <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif3<- data.frame(N_TEdata$cnt-yhat3)
MSE3<-sum(dif3^2)/nrow(dif3) 

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.02, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat4 <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif4<- data.frame(N_TEdata$cnt-yhat4)
MSE4<-sum(dif4^2)/nrow(dif4)

#plot of relationship between Alpha and cost function(MSE)
alpha<-c('0.005','0.01','0.015','0.02')
MSE_alph <-c(MSE1,MSE2,MSE3,MSE4)
plot(alpha,MSE_alph,type = "o")

#change Convergence Threshold value
Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.01, 0.001, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat_a <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif_a<- data.frame(N_TEdata$cnt-yhat_a)
MSE_a<-sum(dif_a^2)/nrow(dif_a) 

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.01, 0.01, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat_b <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif_b<- data.frame(N_TEdata$cnt-yhat_b)
MSE_b<-sum(dif_b^2)/nrow(dif_b) 

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.01, 0.05, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat_c <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif_c<- data.frame(N_TEdata$cnt-yhat_c)
MSE_c<-sum(dif_c^2)/nrow(dif_c) 

Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$windspeed,N_TRdata$cnt,0.01, 0.1, 10000)
N_TEdata<- TEdata[,c(6,12,13,14,17)]
yhat_d <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum+beta4_new*N_TEdata$windspeed
dif_d<- data.frame(N_TEdata$cnt-yhat_d)
MSE_d<-sum(dif_d^2)/nrow(dif_d)

#plot of relationship betwee Convergence Threshold and cost function(MSE)
Conv_Threshold<-c('0.001','0.01','0.05','0.1')
MSE_thre<-c(MSE_a,MSE_b,MSE_c,MSE_d)
plot(Conv_Threshold,MSE_thre,type = "o")

# select three features hr,atemp,hum
N_TRdata<- TRdata[,c(6,12,13,17)]
Gradientdescent <- function(x1,x2,x3,y,alpha, conv_threshold, max_iterations) {
  beta0 <- 1
  beta1 <- 1
  beta2 <- 1
  beta3 <- 1
  m <- nrow(N_TRdata)
  yhat <- beta0+beta1*x1+beta2*x2+beta3*x3
  MSE <- sum((y - yhat) ^ 2) / m
  converged = FALSE
  iterations = 0
  while(converged == FALSE) {
    ## Implement the gradient descent algorithm
    MSE <- sum((y - yhat) ^ 2) / m
    beta0_new <<- beta0 - alpha * ((1 / m) * (sum(yhat - y)))
    beta1_new <<- beta1 - alpha * ((1 / m) * (sum((yhat - y) * x1)))
    beta2_new <<- beta2 - alpha * ((1 / m) * (sum((yhat - y) * x2)))
    beta3_new <<- beta3 - alpha * ((1 / m) * (sum((yhat - y) * x3)))
    beta0 <- beta0_new
    beta1 <- beta1_new
    beta2 <- beta2_new
    beta3 <- beta3_new
    yhat <- beta0+beta1*x1+beta2*x2+beta3*x3
    MSE_new <- sum((y - yhat) ^ 2) / m
    if(MSE - MSE_new <= conv_threshold) {
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3))
    }
    iterations = iterations + 1
    if(iterations > max_iterations) { 
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3))
    }
  }
}
# Assign data of training dataset to the gradientdescent model
Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$hum,N_TRdata$cnt,0.001, 100, 10000)
N_TEdata<- TEdata[,c(6,12,13,17)]
#test error 
yhat <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum
dif <- data.frame(N_TEdata$cnt-yhat)
MSEx <-sum(dif^2)/nrow(dif)
#train error
yhat <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$hum
dif <- data.frame(N_TRdata$cnt-yhat)
MSEy <-sum(dif^2)/nrow(dif)

#pick up hr, hum,windspeed to predict output
N_TRdata<- TRdata[,c(6,12,14,17)]
#implement the gradient descent algorithm 
Gradientdescent <- function(x1,x2,x3,y,alpha, conv_threshold, max_iterations) {
  beta0 <- 1
  beta1 <- 1
  beta2 <- 1
  beta3 <- 1
  m <- nrow(N_TRdata)
  yhat <- beta0+beta1*x1+beta2*x2+beta3*x3
  MSE <- sum((y - yhat) ^ 2) / m
  converged = FALSE
  iterations = 0
  while(converged == FALSE) {
    ## Implement the gradient descent algorithm
    MSE <- sum((y - yhat) ^ 2) / m
    beta0_new <<- beta0 - alpha * ((1 / m) * (sum(yhat - y)))
    beta1_new <<- beta1 - alpha * ((1 / m) * (sum((yhat - y) * x1)))
    beta2_new <<- beta2 - alpha * ((1 / m) * (sum((yhat - y) * x2)))
    beta3_new <<- beta3 - alpha * ((1 / m) * (sum((yhat - y) * x3)))
    beta0 <- beta0_new
    beta1 <- beta1_new
    beta2 <- beta2_new
    beta3 <- beta3_new
    yhat <- beta0+beta1*x1+beta2*x2+beta3*x3
    MSE_new <- sum((y - yhat) ^ 2) / m
    if(MSE - MSE_new <= conv_threshold) {
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3))
    }
    iterations = iterations + 1
    if(iterations > max_iterations) { 
      converged = TRUE
      return(paste("Optimal intercept:", beta0, "Optimal slope:", beta1,beta2,beta3))
    }
  }
}
# Assign data of training dataset to the gradientdescent model
Gradientdescent(N_TRdata$hr,N_TRdata$atemp,N_TRdata$windspeed,N_TRdata$cnt,0.001, 100, 10000)
N_TEdata<- TEdata[,c(6,12,14,17)]
#linear regression model
#with test data
yhat <- beta0_new+beta1_new*N_TEdata$hr+beta2_new*N_TEdata$atemp+beta3_new*N_TEdata$windspeed
dif <- data.frame(N_TEdata$cnt-yhat)
MSEx <-sum(dif^2)/nrow(dif)
#with train data
yhat <- beta0_new+beta1_new*N_TRdata$hr+beta2_new*N_TRdata$atemp++beta3_new*N_TRdata$windspeed
dif <- data.frame(N_TRdata$cnt-yhat)
MSEy <-sum(dif^2)/nrow(dif)

