data{
    int<lower=1> N;
    vector[N] dist;
    vector[N] speed;
}

parameters{
    //Coefficients in the linear model
    real alpha;
    real beta;
    
    //Standard deviation found in the model for the likelihood function
    real<lower=0> sigma;
}

model{
    //Give priors for the parameters (weakly informative)
    alpha ~ normal(0, 1000);
    beta ~ normal(0, 1000);
    
    //Likelihood function
    for(k in 1:N){
        dist[k] ~ normal(alpha + beta * speed[k], sigma);
    }
}
