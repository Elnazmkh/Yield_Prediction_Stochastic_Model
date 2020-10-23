
df_5year_st <-read.csv("/users/elnazmahdi/Desktop/df_5year_st.csv")
attach(df_5year_st)

sd(DGS5)
mean(DGS5)
acf(df_5year_st, lag.max = 1251, plot = TRUE)
#--------
par(mfrow=c(1,1))
descdist(DGS5 , boot = 1000)
#----------
fn =fitdist(DGS5, "norm")
fl =fitdist(DGS5, "logis")
#----------------
par(mfrow=c(2,2))
plot.legend = c("normal", "logistic")
denscomp(list(fn, fl) , legendtext = plot.legend)
qqcomp(list(fn, fl) , legendtext = plot.legend)
cdfcomp(list(fn, fl) , legendtext = plot.legend)
ppcomp(list(fn, fl) , legendtext = plot.legend)
#----------
summary(fn)
summary(fl)

detach(DGS5)

#---------25 Increment
df_5year_25_st<-read.csv("/users/elnazmahdi/Desktop/df_5year_25_st.csv")
attach(df_5year_25_st)

sd(DGS5)
mean(DGS5)
acf(df_5year_25_st, lag.max = 50, plot = TRUE)
#--------
par(mfrow=c(1,1))
descdist(DGS5 , boot = 1000)
#----------
fn =fitdist(DGS5, "norm")
fl =fitdist(DGS5, "logis")
#----------------
par(mfrow=c(2,2))
plot.legend = c("normal", "logistic")
denscomp(list(fn, fl) , legendtext = plot.legend)
qqcomp(list(fn, fl) , legendtext = plot.legend)
cdfcomp(list(fn, fl) , legendtext = plot.legend)
ppcomp(list(fn, fl) , legendtext = plot.legend)
#----------
summary(fn)
summary(fl)

detach(DGS5)




#------------------------------------------ABM ----------------------------------

ABM <- function (nsim, t, mu, sigma, r0, dt)
{
  abm <- matrix(ncol = nsim, nrow = t)
  for (simu in 1:nsim){
    abm[1, simu] <- r0
    for (day in 2:t) {
      epsilon <- rnorm(1)
      abm[day, simu] <- abm[(day-1), simu] + (mu * dt) + (sigma * epsilon * sqrt(dt))
    }
  }
  return(abm)
}

#------------------------------------------GBM  ---------------------------
GBM <- function (nsim, t, mu, sigma, r0, dt)
{
  gbm <- matrix(ncol = nsim, nrow = t)
  for (simu in 1:nsim) {
    gbm[1, simu] <- r0
    for (day in 2:t) {
      epsilon <- rnorm(1,mean=0,sd=sqrt(dt))
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  return(gbm)
}

#-------------------------------------Real Data from before --------------------------------
rate_5year <-read.csv("/users/elnazmahdi/Desktop/US_Treasury_Datasets/DGS5.csv")
row_to_keep = rate_5year[rate_5year$DGS5 != '.',]
historical_data = as.numeric(row_to_keep$DGS5)
#-------------------------------------to be predicted data --------------------------------
to_be_predicted_data <-read.csv("/users/elnazmahdi/Desktop/RealDGS5.csv")
to_be_predicted_data <-to_be_predicted_data$DGS5
na.omit(to_be_predicted_data)

#--------------------------------Arithmatic Brownian Motion--------------------------------
numsim = 1000
t = 30
t0 = 60
r0 = tail(historical_data, t)[1]
mu = mean(head(tail(DGS5,t+t0),t0))
sigma= sd(head(tail(DGS5,t+t0),t0))
dt = 1
s <- seq(1, t)

abm <- ABM(numsim, t, mu, sigma , r0 , dt)
rate <- rowMeans(abm)
rate_sd <-rowSds(abm)/sqrt(numsim)
upper_conf <- rate + 1.96 * rate_sd
lower_conf <- rate - 1.96 * rate_sd
MSE(rate,tail(historical_data,t))

#----- plotting all trajectories and estimated mean and real yields ----------
matplot(abm, type='l', xlab='time (days)', ylab='yield(%)', col=1:numsim , xaxt = "n",cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = tail(historical_data,t),type = 'l',col= "orange", lwd = "4", add = TRUE)
matplot(y = rate,type = 'l',col= "blue", lwd = "3", add = TRUE)
legend("bottomleft", legend=c("Predicted Rates (Mean)", "Real Rates"),
       col=c("blue", "orange"), lty=1,lwd = "2")

#---------- plotting just estimation and real yields

matplot(tail(historical_data,t), type='l', xlab='time (days)', ylab='yield(%)', col="orange" , xaxt = "n",ylim=c(0,0.4),cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = rate,type = 'l',col= "blue", lwd = "3", add = TRUE)
matplot(y = lower_conf,type = 'l',col= "red",lty=3, lwd = "3", add = TRUE)
matplot(y = upper_conf,type = 'l',col= "red", lty=3, lwd = "3", add = TRUE)

legend("topright", legend=c("Predicted Rates (Mean)", "Real Rates", "confidence interval 95%"),
       col=c("blue", "orange" , "red"), lty =c(1,1,3) ,lwd = "2")


MSE(rate,tail(historical_data,t))

#---------------ABM :  PDF for a yield at a specific time like t = 100 -------

hist(abm[t,], xlab = "Estimated Yield(%)", breaks = seq(min(abm[t,])-0.02, max(abm[t,])+0.02, 0.02) , freq = FALSE , cex.lab=1.5 , main = "Distribution of Estimated Yield for t = 30")
lines(density(abm[t,]), col="blue", lwd=2)
abline(v = mean(abm[t,]), col="green", lwd=3, lty=2)
abline(v = (mean(abm[t,]) - 1.96*sd(abm[t,])), col="red", lwd=3, lty=2)
abline(v = (mean(abm[t,]) + 1.96*sd(abm[t,])), col="red", lwd=3, lty=2)
legend("topleft", legend=c("Empirical Data Density", "Mean", "confidence interval 95%"),
       col=c("blue", "green" , "red"), lty =c(1,1,3) ,lwd = 1)



par(mfrow=c(1,1))
descdist(abm[t,] , boot = 1000)









#-----------------------------Geometric Brownian Motion ----------------------------------
df_5year_logreturns<-read.csv("/users/elnazmahdi/Desktop/df_5year_logreturns.csv")
  numsim = 1000
  t = 30
  t0 = 100
  r0 = tail(historical_data, t)[1]
  sigma= sd(head(tail(df_5year_logreturns$logReturn,t+t0),t0))
  mu = mean(head(tail(df_5year_logreturns$logReturn,t+t0),t0)) + (sigma*sigma)/2
  dt = 1
  s <- seq(1, t)
  
  gbm <- GBM(numsim, t, mu, sigma , r0 , dt)
  rateg <- rowMeans(gbm)
  rateg_sd <-rowSds(gbm)/sqrt(numsim)
  upper_conf_g <- rateg + 1.96 * rateg_sd
  lower_conf_g <- rateg - 1.96 * rateg_sd
  MSE(rateg,tail(historical_data,t))

#----------------------plot modeling
matplot(gbm, type='l', xlab='time (days)', ylab='yield(%)', col=1:numsim , xaxt = "n" , cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y=tail(historical_data, t),type = 'l',col= "orange", lwd = "4", add = TRUE)
matplot(y = rateg,type = 'l',col= "blue", lwd = "3", add = TRUE)

legend("topleft", legend=c("Predicted Rates (Mean)", "Real Rates"),
       col=c("blue", "orange"), lty=1,lwd = "2")

#---------- plotting just estimation and real yields

matplot(tail(historical_data,t), type='l', xlab='time (days)', ylab='yield(%)', col="orange" , xaxt = "n",ylim=c(0.1,0.4),, cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = rateg,type = 'l',col= "blue", lwd = "3", add = TRUE)
matplot(y = lower_conf_g,type = 'l',col= "red",lty=3, lwd = "3", add = TRUE)
matplot(y = upper_conf_g,type = 'l',col= "red", lty=3, lwd = "3", add = TRUE)

legend("topright", legend=c("Predicted Rates (Mean)", "Real Rates", "confidence interval 95%"),
       col=c("blue", "orange" , "red"), lty =c(1,1,3) ,lwd = "2")

MSE(rateg,tail(historical_data,t))


#---------------GBM :  PDF for a yield at a specific time like t = 100 -------

hist(gbm[t,], xlab = "Estimated Yield(%)", breaks = seq(min(gbm[t,])-0.02, max(gbm[t,])+0.02, 0.02) , freq = FALSE , cex.lab=1.5 , main = "Distribution of Estimated Yield for t = 30")
lines(density(gbm[t,]), col="blue", lwd=2)
abline(v = mean(gbm[t,]), col="green", lwd=3, lty=2)
abline(v = (mean(gbm[t,]) - 1.96*sd(gbm[t,])), col="red", lwd=3, lty=2)
abline(v = (mean(gbm[t,]) + 1.96*sd(gbm[t,])), col="red", lwd=3, lty=2)
legend("topright", legend=c("Empirical Data Density", "Mean", "confidence interval 95%"),
       col=c("blue", "green" , "red"), lty =c(1,1,3) ,lwd = 1)



par(mfrow=c(1,1))
descdist(gbm[t,] , boot = 1000)







#--------------- Log returns Density -----------------
x1 = df_5year_logreturns$logReturn

par(mfrow=c(1,1))
plot(density(x1), main = "Distribution of Yield Changes")
curve(dnorm(x,mean=mean(x1),sd= sd(x1)), col = "red", lwd =2, add=TRUE)
curve(dlogis(x,mean(x1),sd(x1)), col = "green" ,add=TRUE)
curve(dcauchy(x,mean(x1),sd(x1)), col = "blue" ,add=TRUE)
legend("topleft", legend=c("Empirical Data", "Normal", "Logistic", "Cauchy"),
       col=c("black", "red","green", "blue"), lty=1, cex=0.8 , lwd = 3)

ks.test(x1,rnorm(t1, mean(x1),sd(x1)))
ks.test(x1,rlogis(t1, mean(x1),sd(x1)))
ks.test(x1,rcauchy(t1, mean(x1),sd(x1)))



