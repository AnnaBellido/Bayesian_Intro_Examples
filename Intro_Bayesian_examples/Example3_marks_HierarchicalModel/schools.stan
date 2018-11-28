data{
    int<lower=1> n_schools;
    int<lower=1> N;
    int<lower=0> school_id[N];
    real<lower=0> marks[N];
}

parameters{
    real<lower=0, upper=10> marks_all;
    real<lower=0> disper_all;
    
    real marks_school[n_schools];
    real<lower=0> disper_school[n_schools];
}

model{
    //Set priors for the global population of marks:
    marks_all ~ normal(7,3);
    disper_all ~ cauchy(0,1);
    
    //Set priors for the offset of each school:
    for(i in 1:n_schools){
        marks_school[i] ~ normal(marks_all,disper_all);
        disper_school[i] ~ cauchy(0,1);
    }
      
    //Set the likelihood function:
    for(k in 1:N){
        marks[k] ~ normal(marks_school[school_id[k]], disper_school[school_id[k]]);
    }
}

