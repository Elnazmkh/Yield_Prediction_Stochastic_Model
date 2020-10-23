


#------------------------------1-Day Increment Data--------------------------------
#------------------------------Reading Data---------------------------------------------
df_1year_st <-read.csv("./Stationary-1-day/df_1year_st.csv")
attach(df_1year_st)

#-----------------------------Check for Autocorrelation--------------------------------
acf(df_1year_st, lag.max = 1251, plot = TRUE , cex.lab=1.5 , main = "Correlogram of Yield Changes")


#------------------------------Distribution Fitting----------------------------------
#---------------------------------Cullen and Frey Graph
par(mfrow=c(1,1))
descdist(DGS1 , boot = 1000)

#----------------------Fitting into Normal, Logistic and Cauchy Distribution--------------------------------
fn =fitdist(DGS1, "norm")
fl =fitdist(DGS1, "logis")
fc =fitdist(DGS1, "cauchy")
par(mfrow=c(2,2))
plot.legend = c("normal", "logistic","cauchy")
denscomp(list(fn, fl,fc) , legendtext = plot.legend)
qqcomp(list(fn, fl,fc) , legendtext = plot.legend)
cdfcomp(list(fn, fl,fc) , legendtext = plot.legend)
ppcomp(list(fn, fl,fc) , legendtext = plot.legend)
#----------
summary(fn)
summary(fl)
summary(fc)

#------------------------Maximum Goodness of Fit --------------------------------
#----------------------------Camer-von Mises -----------------------------------
gofn =fitdist(DGS1, "norm" , method = "mge" , gof="CvM")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="CvM")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="CvM")
summary(gofn)
summary(gofl)
summary(gofc)
#-------------------------------Kolmogrov-Smirnov---------------------------------------
gofn =fitdist(DGS1, "norm" , method = "mge" , gof="KS")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="KS")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="KS")
summary(gofn)
summary(gofl)
summary(gofc)

#-------------------------------Anderson-Darling---------------------------------------
gofn =fitdist(DGS1, "norm" , method = "mge" , gof="AD")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="AD")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="AD")
summary(gofn)
summary(gofl)
summary(gofc)

#-----------------------------------Plotting Emperical data and Final decision by k test -----------------------------
t1= length(DGS1)
par(mfrow=c(1,1))

plot(density(tail(DGS1,t1)), main = "Distribution of Yield Changes" ,cex.lab=1.5)
curve(dnorm(x,mean=mean(tail(DGS1,t1)),sd= sd(tail(DGS1,t1))), col = "red", lwd =3, add=TRUE)
curve(dlogis(x,mean(tail(DGS1,t1)),sd(tail(DGS1,t1))), col = "green" ,add=TRUE,lwd = 2)
curve(dcauchy(x,mean(tail(DGS1,t1)),sd(tail(DGS1,t1))), col = "blue" ,add=TRUE,lwd = 2)

legend("topleft", legend=c("Empirical Data", "Normal", "Logistic", "Cauchy"),
       col=c("black", "red","green", "blue"), lty=1, cex=0.8 , lwd = 3)

ks.test(tail(DGS1,t1),rnorm(t1, mean(tail(DGS1,t1)),sd(tail(DGS1,t1))))
ks.test(tail(DGS1,t1),rlogis(t1, mean(tail(DGS1,t1)),sd(tail(DGS1,t1))))
ks.test(tail(DGS1,t1),rcauchy(t1, mean(tail(DGS1,t1)),sd(tail(DGS1,t1))))

ks.test(tail(DGS1,t1),"pnorm", mean = mean(tail(DGS1,t1)),sd = sd(tail(DGS1,t1)))
ks.test(rnorm(1000), "pnorm")

# --------------------------------------------------------------------------------------
detach(df_1year_st)



#------------------------------25-Day Increment Data--------------------------------
#------------------------------Reading Data---------------------------------------------
df_1year_25_st<-read.csv("./Stationary-25-day/df_1year_25_st.csv")
attach(df_1year_25_st)

#-----------------------------Check for Autocorrelation--------------------------------
acf(df_1year_25_st, lag.max = 1251, plot = TRUE , cex.lab=1.5 , main = "Correlogram of Yield Changes")

#------------------------------Distribution Fitting----------------------------------
#---------------------------------Cullen and Frey Graph
par(mfrow=c(1,1))
descdist(DGS1 , boot = 1000)

#----------------------Fitting into Normal, Logistic and Cauchy Distribution--------------------------------
fn =fitdist(DGS1, "norm")
fl =fitdist(DGS1, "logis")
fc =fitdist(DGS1, "cauchy")

par(mfrow=c(2,2))
plot.legend = c("normal", "logistic","cauchy")
denscomp(list(fn, fl,fc) , legendtext = plot.legend)
qqcomp(list(fn, fl,fc) , legendtext = plot.legend)
cdfcomp(list(fn, fl,fc) , legendtext = plot.legend)
ppcomp(list(fn, fl,fc) , legendtext = plot.legend)
#----------
summary(fn)
summary(fl)
summary(fc)

 ###----- --------------------------------goodness of fit--------------------------------
#----------------------------Camer-von Mises -----------------------------------

gofn =fitdist(DGS1, "norm" , method = "mge" , gof="CvM")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="CvM")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="CvM")
summary(gofn)
summary(gofl)
summary(gofc)

#----------------------------Kolmogrov-Smirnov -----------------------------------
gofn =fitdist(DGS1, "norm" , method = "mge" , gof="KS")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="KS")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="KS")
summary(gofn)
summary(gofl)
summary(gofc)
#---------------------------Anderson-Darling -----------------------------------

gofn =fitdist(DGS1, "norm" , method = "mge" , gof="AD")
gofl =fitdist(DGS1, "logis", method = "mge" , gof="AD")
gofc =fitdist(DGS1, "cauchy", method = "mge" , gof="AD")
summary(gofn)
summary(gofl)
summary(gofc)


detach(df_1year_25_st)


#------------------------------------Stochastic Modeling ---------------------------
#--------------------------Arithmatic Brownian Motion ------------------------------

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

#----------------------------Geometric Brownian Motion  ---------------------------
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
rate_1year <-read.csv("./US_Treasury_Datasets/DGS1.csv")
row_to_keep = rate_1year[rate_1year$DGS1 != '.',]
historical_data = as.numeric(row_to_keep$DGS1)

#--------------------------------ABM Modeling--------------------------------
df_1year_st <-read.csv("./Stationary-1-day/df_1year_st.csv")
attach(df_1year_st)

numsim = 1000
t = 30
t0 = 30
r0 = tail(historical_data, t)[1]
mu = mean(head(tail(DGS1,t+t0),t0))
sigma= sd(head(tail(DGS1,t+t0),t0))
dt = 1
s <- seq(1, t)

abm <- ABM(numsim, t, mu, sigma , r0 , dt)

rate <- rowMeans(abm)
rate_sd <-rowSds(abm)/sqrt(numsim)

upper_conf <- rate + 1.96 * rate_sd
lower_conf <- rate - 1.96 * rate_sd

MSE(rate,tail(historical_data,t))

#----- plotting all trajectories and estimated mean and real yields -----------------------------
matplot(abm, type='l', xlab='time (days)', ylab='yield(%)', col=1:numsim , xaxt = "n",cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = tail(historical_data,t),type = 'l',col= "orange", lwd = "4", add = TRUE)
matplot(y = rate,type = 'l',col= "blue", lwd = "3", add = TRUE)
legend("bottomleft", legend=c("Predicted Rates (Mean)", "Real Rates"),
       col=c("blue", "orange"), lty=1,lwd = "2")

#-------------------- plotting just the estimation and the real yields --------------------------------

matplot(tail(historical_data,t), type='l', xlab='time (days)', ylab='yield(%)', col="orange" , xaxt = "n",ylim=c(0.05,0.2),cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = rate,type = 'l',col= "blue", lwd = "3", add = TRUE)
matplot(y = lower_conf,type = 'l',col= "red",lty=3, lwd = "3", add = TRUE)
matplot(y = upper_conf,type = 'l',col= "red", lty=3, lwd = "3", add = TRUE)

legend("topright", legend=c("Predicted Rates (Mean)", "Real Rates", "confidence interval 95%"),
       col=c("blue", "orange" , "red"), lty =c(1,1,3) ,lwd = "2")



#---------------ABM :  PDF for a yield at a specific time like t = 100 -------

hist(abm[t,], xlab = "Estimated Yield(%)", breaks = seq(min(abm[t,])-0.02, max(abm[t,])+0.02, 0.02) , freq = FALSE , cex.lab=1.5 , main = "Distribution of Estimated Yield for t = 30")
lines(density(abm[t,]), col="blue", lwd=2)
abline(v = mean(abm[t,]), col="green", lwd=3, lty=2)
abline(v = (mean(abm[t,]) - 1.96*sd(abm[t,])), col="red", lwd=3, lty=2)
abline(v = (mean(abm[t,]) + 1.96*sd(abm[t,])), col="red", lwd=3, lty=2)
legend("topleft", legend=c("Empirical Data Density", "Mean", "confidence interval 95%"),
       col=c("blue", "green" , "red"), lty =c(1,1,3) ,lwd = 1)


#---------------------------------- distribution fitting of model samples  --------------------

par(mfrow=c(1,1))
descdist(abm[t,] , boot = 1000)





#-----------------------------GBM Modeling ----------------------------------
df_1year_logreturns<-read.csv("./log_returns/df_1year_logreturns.csv")
df_1year_st <-read.csv("./Stationary-1-day/df_1year_st.csv")
attach(df_1year_st)

numsim = 1000
t = 30
t0 = 30
r0 = tail(historical_data, t)[1]
sigma= sd(head(tail(df_1year_logreturns$logReturn,t+t0),t0))
mu = mean(head(tail(df_1year_logreturns$logReturn,t+t0),t0)) + (sigma*sigma)/2
dt = 1
s <- seq(1, t)

gbm <- GBM(numsim, t, mu, sigma , r0 , dt)

rateg <- rowMeans(gbm)
rateg_sd <-rowSds(gbm)/sqrt(numsim)

upper_conf_g <- rateg + 1.96 * rateg_sd
lower_conf_g <- rateg - 1.96 * rateg_sd
MSE(rateg,tail(historical_data,t))

#---------------- plotting all trajectories and estimated mean and real yields -----------------------------
matplot(gbm, type='l', xlab='time (days)', ylab='yield(%)', col=1:numsim , xaxt = "n" , cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y=tail(historical_data, t),type = 'l',col= "orange", lwd = "4", add = TRUE)
matplot(y = rateg,type = 'l',col= "blue", lwd = "3", add = TRUE)

legend("topleft", legend=c("Predicted Rates (Mean)", "Real Rates"),
       col=c("blue", "orange"), lty=1,lwd = "2")

#---------- plotting just estimation and real yields----------------

matplot(tail(historical_data,t), type='l', xlab='time (days)', ylab='yield(%)', col="orange", xaxt = "n", ylim=c(0.1,0.17), cex.lab=1.5)
axis(side = 1, at = s,labels = TRUE)
matplot(y = rateg,type = 'l',col= "blue", lwd = "3", add = TRUE)
matplot(y = lower_conf_g,type = 'l',col= "red",lty=3, lwd = "3", add = TRUE)
matplot(y = upper_conf_g,type = 'l',col= "red", lty=3, lwd = "3", add = TRUE)

legend("topright", legend=c("Predicted Rates (Mean)", "Real Rates", "confidence interval 95%"),
       col=c("blue", "orange" , "red"), lty =c(1,1,3) ,lwd = "2")


#---------------GBM :  PDF for a yield at a specific time like t = 30 -------

h <- hist(gbm[t,], xlab = "Estimated Yield(%)", breaks = seq(min(gbm[t,])-0.02, max(gbm[t,])+0.02, 0.02) , freq = FALSE , cex.lab=1.5 , main = "Distribution of Estimated Yield for t")
lines(density(gbm[t,]), col="blue", lwd=2)
abline(v = mean(gbm[t,]), col="green", lwd=3, lty=2)
abline(v = 0.1183743, col="red", lwd=3, lty=2)
abline(v = 0.1223502 , col="red", lwd=3, lty=2)
legend("topright", legend=c("Empirical Data Density", "Mean", "confidence interval 95%"),
       col=c("blue", "green" , "red"), lty =c(1,1,3) ,lwd = 1)



par(mfrow=c(1,1))
descdist(gbm[t,] , boot = 1000)







#--------------- Log returns Density check -----------------

t1=30
x1 = tail(df_1year_logreturns$logReturn,t1)

par(mfrow=c(1,1))
plot(density(x1), main = "Distribution of Log of Yield Changes(last 30 days)",ylim=c(0,5))
curve(dnorm(x,mean=mean(x1),sd= sd(x1)), col = "red", lwd =2, add=TRUE)
legend("topleft", legend=c("Empirical Data", "Normal"),
       col=c("black", "red"), lty=1, cex=0.8 , lwd = 3)

ks.test(x1, "pnorm" , mean = mean(x1),sd = sd(x1))





t2= 30
x2 = tail(DGS1,t2)
ks.test(x2, "pnorm", mean=mean(x2), sd=sd(x2))

par(mfrow=c(1,1))
plot(density(x2), main = "Distribution of Yield Changes(last 30 days)" ,cex.lab=1.5 , ylim=c(0,40))
curve(dnorm(x,mean=mean(x2),sd= sd(x2)), col = "red", lwd =3, add=TRUE)
legend("topleft", legend=c("Empirical Data", "Normal"),
       col=c("black", "red"), lty=1, cex=0.8 , lwd = 3)


ks.test(x2, "pnorm" , mean = mean(x2),sd = sd(x2))

whitenoise.test(x2)
shapiro.test(x2)

whitenoise.test(DGS1)
shapiro.test(DGS1)
