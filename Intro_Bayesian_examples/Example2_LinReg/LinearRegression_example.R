#Basic example: distance related linearly to speed

#Installing RSTAN: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(ggplot2)
library(rstan)

#Let's use the CARS data set already available in R:

#Plot the data with the linear relation we're aiming for:
ggplot(cars, aes(x=speed, y=dist))+geom_point()+geom_smooth(method="lm")

#Frequentist approach:
lm_res<-lm(dist ~ speed, data=cars)
summary(lm_res) #Significant linear relation. Both coefficients beta0 and beta1 are significant

#Store the coefficients for later use:
alpha_frequentist<-coefficients(lm_res)[1]
beta_frequentist<-coefficients(lm_res)[2]

#Bayesian approach in Stan:
stan_data<-list(N = nrow(cars),
                dist = cars$dist,
                speed = cars$speed)

fit_model <- stan(file = "Example2_LinReg/lm_distSpeed.stan", 
                  data = stan_data,
                  iter = 4000, chains = 4)
fit_df<-as.data.frame(fit_model)
#Have a look at the mean point estimates for each parameter and see that they resemble a lot those from the frequentist approach
#Also look at the model convergence (n_eff and Rhat)
print(fit_model)

#Have a look at the samples done by the HMC algorithm: we get a number of occurrences of each value for alpha which is proportional to its probability
hist(fit_df[["alpha"]], xlab = "speed (intercept values)", main = "Sampling over alpha - histogram")

#Rescaled and with the density on top:
ggplot(data.frame(x=fit_df$alpha), aes(x=x))+
    geom_histogram(aes(y=..density..),binwidth=1, colour="black", fill="white")+
    geom_density(alpha=0.2, fill="#FF6666")+
    xlab("speed (intercept values)")+ggtitle("Sampling over alpha - density function")+ylab("")+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), title = element_text(size = 16))

#Store the coefficients:
alpha_bayesian<-mean(fit_df$alpha)
beta_bayesian<-mean(fit_df$beta)

#Plot the regression line found under both paradigms:
#Frequentist:
plot2<-ggplot(cars, aes(x=speed, y=dist))+geom_point()+
            geom_abline(slope = beta_frequentist, intercept = alpha_frequentist, color = "royalblue3", size=2)+
            xlab("Speed")+ylab("Distance before STOP")+
            ggtitle("Linear relation between distance and speed")+
            theme(axis.text = element_text(size=14), axis.title = element_text(size=16), title = element_text(size=14))
show(plot2)

#Bayesian:
plot2<-plot2+geom_abline(slope = beta_bayesian, intercept = alpha_bayesian, color="chartreuse3", lty=2, size=2)
show(plot2)
