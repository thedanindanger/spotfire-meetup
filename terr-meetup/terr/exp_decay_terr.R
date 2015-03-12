#exponential decay fit example

#create dummy data
#set.seed(123)
#ts <- c(1:100)
#exp_dec <- 1000*(.9^ts)

#exp_dec_err <- sapply(exp_dec, function(x) rnorm(1,x,.34*x))

ts <- DateOrder
values <- Value
data <- data.frame(ts,values)

#prediction fuction 1 y = (a*(b^ts))

ts <- data[,1]
exp_dec_err <- data[,2]

fr <- function(x) {   
  a <- x[1]
  b <- x[2]
  err <- exp_dec_err - (a*(b^ts))
  sum(err^2) / length(err)
}

pars <- optim(c(1000,.5),fr)

y1_hat = pars$par[1]*(pars$par[2]^ts)

#beta decay function (a*(beta*(b^t)))

fr <- function(x) {   
  a <- x[1]
  b <- x[2]
  beta <- x[3]
  err <- exp_dec_err - (a*(beta*(b^ts)))
  sum(err^2) / length(err)
}

pars <- optim(c(1000,.5, 1),fr)

y2_hat = pars$par[1]*(pars$par[3]*(pars$par[2]^ts))

dataPred <- data
dataPred$y1_hat <- y1_hat
dataPred$y2_hat <- y2_hat
dataPred

###Linear Regression

ts <- input1
values <- input2

fr <- function(x) {   
  a <- x[1]
  b <- x[2]
  err <- values - (-1*(a*ts)+b)
  sum(err^2) / length(err)
}

pars <- optim(c(100,10000),fr)

output <- (-1*(pars$par[1]*ts)+pars$par[2])


###Expontenial Decay Option
 
ts <- input1
values <- input2

fr <- function(x) {   
  a <- x[1]
  b <- x[2]
  err <- values - (a*(b^ts))
  sum(err^2) / length(err)
}

pars <- optim(c(1000,.5),fr)

output = pars$par[1]*(pars$par[2]^ts)

