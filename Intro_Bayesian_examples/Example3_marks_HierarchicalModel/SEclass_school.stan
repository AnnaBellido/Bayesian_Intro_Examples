data{
    int<lower=1> n_schools;
    int<lower=1> n_SEclasses;
    int<lower=1> N;
    int<lower=1> SEclass_map[N];
    int<lower=1> school_id[N];
    real<lower=0> marks[N];
}

parameters{
    real<lower=0> marks_all;
    real<lower=0> disper_all;
    
    real marks_SEclass[n_SEclasses];
    // real<lower=0> disper_SEclass[n_SEclasses];
    
    real offset_school[n_schools];
    real<lower=0> disper_school[n_schools];
}

model{
    //Set priors for the global population of marks:
    marks_all ~ normal(7,3);
    disper_all ~ cauchy(0,1);
    
    //Set the region marks to sample from the general population:
    for(i in 1:n_SEclasses){
        marks_SEclass[i] ~ normal(marks_all, disper_all);
    }
    
    //Set priors for the school mean marks:
    for(i in 1:n_schools){
        offset_school[i] ~ normal(0,1);
        disper_school[i] ~ cauchy(0,1);
    }
    
    //Likelihood function for marks (our data):
    for(k in 1:N){
        marks[k] ~ normal(marks_SEclass[SEclass_map[k]]+offset_school[school_id[k]], disper_school[school_id[k]]);
    }
    
}
