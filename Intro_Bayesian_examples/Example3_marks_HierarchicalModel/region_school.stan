data{
    int<lower=1> n_schools;
    int<lower=1> n_regions;
    int<lower=1> N;
    int<lower=1> regions_map[n_schools];
    int<lower=1> school_id[N];
    real<lower=0> marks[N];
}

parameters{
    real<lower=0> marks_all;
    real<lower=0> disper_all;
    
    real marks_region[n_regions];
    real<lower=0> disper_regions[n_regions];
    
    real marks_school[n_schools];
    real<lower=0> disper_school[n_schools];
}

model{
    //Set priors for the global population of marks:
    marks_all ~ normal(7,3);
    disper_all ~ cauchy(0,1);
    
    //Set the region marks to sample from the general population:
    for(i in 1:n_regions){
        marks_region[i] ~ normal(marks_all, disper_all);
    }
    
    //Set priors for the school mean marks:
    for(i in 1:n_schools){
        marks_school[i] ~ normal(marks_region[regions_map[i]], disper_regions[regions_map[i]]);
        disper_school[i] ~ cauchy(0,1);
    }
    
    //Likelihood function for marks (our data):
    for(k in 1:N){
        marks[k] ~ normal(marks_school[school_id[k]], disper_school[school_id[k]]);
    }
    
}
