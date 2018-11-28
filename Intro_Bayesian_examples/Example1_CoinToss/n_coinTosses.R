#EXAMPLE: COIN TOSS 

if(!require(ggplot2)){
    install.packages("ggplot2", dependencies = TRUE)
    library(ggplot2)
}

#Setting all the possible values for theta:
theta_values<-seq(from=0, to=1, length.out = 200)

#Compute the likelihood function:
likelihood<-dbinom(6, 10, prob=theta_values)

#Set the parameters of the coin prior distribution:
mu<-0.7
sd_est<-0.2
alpha<-((1-mu)/sd_est^2 - 1/mu)*mu^2
beta<-alpha*(1/mu -1)

prior<-dbeta(theta_values, alpha, beta)/length(theta_values)

#Plot the prior distribution:
plot1<-ggplot(data.frame(x=theta_values, y=prior), aes(x=x, y=y))+geom_path(color="brown3", size=1.1)+
    xlab(expression("Values of "*theta))+ylab(expression("Probability of each "*theta))+
    coord_cartesian(xlim=c(0, 1), ylim=c(0, 0.02))+
    ggtitle(expression("Distribution of "*theta*" as we learn"))+
    theme(axis.text = element_text(size=14), axis.title = element_text(size=16), title = element_text(size=14))
show(plot1)

#Use Bayes formula:
posterior<-likelihood*prior
#Make the posterior density add up to 1:
posterior<-posterior/sum(posterior)

#Add it to the plot:
plot1<-plot1+geom_path(x=theta_values, y=posterior, color="gray30", size=1.1)
show(plot1)

#Toss the coin again:
prior<-posterior

#we get one Tails:
likelihood<-dbinom(0,1,prob=theta_values)

#Use Bayes formula again:
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

#Add it to the plot:
plot1<-plot1+geom_path(x=theta_values, y=posterior, color="gray30", size=1.1)
show(plot1)

#Toss the coin 5 more times and all were heads:
prior<-posterior

#We get 0 Heads out of 5 coin tosses:
likelihood<-dbinom(0,5,prob = theta_values)

posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

#Add it to the plot:
plot1<-plot1+geom_path(x=theta_values, y=posterior, color="gray30", size=1.1)
show(plot1)


#All the iterative process above is equivalent to gathering all the data together, the initial beta prior, and computing the final posterior distribution:
likelihood<-dbinom(6, 16, prob=theta_values)
prior<-dbeta(theta_values, alpha, beta)/length(theta_values)

posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

#Add it to the plot:
plot1<-plot1+geom_path(x=theta_values, y=posterior, color="yellow3", lty=2, size=1.1)
show(plot1)


#We can get some estimates from this distribution:
#Expected value:
expected_value<-sum(posterior*theta_values)

#And compare it to the observed frequency:
observed_freq<-6/16

#Quantiles:
samples<-sample(theta_values, prob = posterior, replace = TRUE, size = 1e6)
quantile(samples, probs = c(0,0.025, 0.5, 0.975)) #Mode, and the other percentiles...

#Find the Highest (Posterior) Density Interval
if(!require(HDInterval)){
    install.packages("HDInterval", dependencies = TRUE)
}
library(HDInterval)
hdi(samples, credMass = 0.95)
