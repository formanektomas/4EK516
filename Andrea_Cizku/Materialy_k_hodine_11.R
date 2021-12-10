################################################ Lecture 3 ################################################ 

########## AR(1) ##########

rm(list = ls())

# conditional distribution
mu.c = 0.59 + 0.42 * (-0.37)
var.c = 0.752
1 - pnorm(0,mu.c,sqrt(var.c))

# unconditional distribution
mu.unc = 0.59 / (1 - 0.42)
var.unc = 0.752 / (1 - 0.42^2)
1 - pnorm(0,mu.unc,sqrt(var.unc))

1 - pnorm(0,mu.unc,var.unc)


########## NLAR(1) -- one-step-ahead ##########

rm(list = ls())

# Set number of draws
N = 10000 

# Set value of x_T
x_T = 0.2

# Generate N values of epsilon_T+1
eps_T1 = 0.1 * rnorm(N)

# Calculate N values of x_T+h
x_T1 = tanh(0.9 * x_T + eps_T1) 

# Calculate P(x_T+1>0.4)
P04 = (1/N) * sum(x_T1 > 0.4)
P04 = mean(x_T1 > 0.4)

# Print result
P04 


########## NLAR(1) -- h-step-ahead ##########

# Set steps ahead
h = 3
# Set number of draws
N = 10000
# Set value of x_T
x_T = 0.2

# Generate N values of epsilon_T+1
eps_T1 = 0.1 * rnorm(h * N)
dim(eps_T1) = c(h, N)
eps_T1 = as.data.frame(t(eps_T1))

# Calculate N values of x_T+h
x = data.frame(rep(x_T,N))
for (t in 1:h) {
  x[,t+1] = tanh(0.9*x[,t] + eps_T1[,t]) 
}

# Calculate P(x_T+1>0.4)
P04 = (1/N) * sum(x[,h+1]>0.4)
P04 = mean(x[,h+1]>0.4)

# Print result
P04


########## GARCH -- VaR ##########

rm(list = ls())

# Load Data
x = read.csv('C:/Users/Satan/Dropbox/doktorske/materialy_uceni/4EK516_Pokrocila_ekonometrie_2/seminars/seminar_3/stock_returns.csv',
             header = F)
x = as.vector(x$V1)
plot(x, type = 'l')

# Parameter Values
T = length(x)

# create vectors
sigma2 = rep(NA,T)
VaR10 = rep(NA,T+1)
VaR05 = rep(NA,T+1)
VaR01 = rep(NA,T+1)
VaR001 = rep(NA,T+1)

# Filter Volatilites
sigma2[1] = var(x)

for (t in 1:T) {
  sigma2[t+1] = 0.11 + 0.08 * x[t]^2 + 0.91 * sigma2[t]
}

# Calculate VaR and ES
for (t in 1:T+1) {

  VaR10[t] = qnorm(0.1,0,sqrt(sigma2[t]))
  VaR05[t] = qnorm(0.05,0,sqrt(sigma2[t]))
  VaR01[t] = qnorm(0.01,0,sqrt(sigma2[t]))
  VaR001[t] = qnorm(0.001,0,sqrt(sigma2[t]))

}

# Plot Data
plot(x,type = 'l', ylim = c(-14,20))
lines(sigma2, col = 'purple')
lines(VaR10, col = 'red')
lines(VaR05, col = 'yellow')
lines(VaR01, col = 'blue')
lines(VaR001, col = 'green')


########## NLAR -- numerical forecast ##########

rm(list = ls())

# Set steps ahead
h = 10
# Set number of draws
N = 10000
# Set value of x_T
x_T = 0.2


# Generate N values of epsilon_T+1
eps_T1 = 0.1 * rnorm(h * N)
dim(eps_T1) = c(h, N)
eps_T1 = as.data.frame(t(eps_T1))

# Calculate N values of x_T+h
x = data.frame(rep(x_T,N))
for (t in 1:h) {
  x[,t+1] = tanh(0.9*x[,t] + eps_T1[,t]) 
}

# Calculate point forecasts (conditional mean) 
# and confidence bounds (5 and 95 percentiles) recursively
x_hat = x_T
upper_bound= x_T
lower_bound= x_T

for (t in 1:h) {
  x_hat[t+1] = mean(x[,t+1])
  upper_bound[t+1] = quantile(x[,t+1],0.95)
  lower_bound[t+1] = quantile(x[,t+1],0.05)
}

# Plot Data

plot(0:10,x_hat,type = 'l', ylim = c(-0.3,0.4))
lines(0:10,upper_bound, col = 'red')
lines(0:10,lower_bound, col = 'red')


########## NLAR -- impulse response function ##########

rm(list = ls())

# Set number of draws
N = 10000

# Parameter Values
s = 3     #Set shock time s
x0 = 0.2  #Set origin value
e = -1    #Set shock size e
h = 7     #Set steps ahead for IRF

# Generate Innovations
eps = 0.1 * rnorm((s+h) * N) #Generate innovation values
dim(eps) = c(s+h, N)
eps = as.data.frame(t(eps))

eps[,1:(s-1)] = 0   #Set shocks to zero for t<s
eps[,s] = e         #Set shocks to e for t=s

# Generate IRF

# Set time-series to x for t<s
x = rep(x0,(s-1)*N)
dim(x) = c(s-1, N)
x = as.data.frame(t(x)) 
# Set time-series to x+e for t=s
x[,s] = rep(x0+e,N)    

for (t in (s+1):(s+h)) {
  x[,t] = tanh(0.9*x[,t-1] + eps[,t]) 
}

# Calculate point forecasts (conditional mean) 
# and confidence bounds (5 and 95 percentiles) recursively 
x_tilde = rep(NA,s+h)
upper_bound= rep(NA,s+h)
lower_bound= rep(NA,s+h)

for (t in 1:(s+h)) {
  x_tilde[t] = mean(x[,t])
  upper_bound[t] = quantile(x[,t],0.95)
  lower_bound[t] = quantile(x[,t],0.05)
}

# Plot Data
plot(-2:7,upper_bound,type = 'l',  col = 'red', ylim = c(-1.2,0.4))
lines(-2:7,lower_bound, col = 'red')
lines(-2:7,x_tilde, col = 'black')

