// The 'data' block list all input variables that are given to Stan from R. You need to specify the size of the arrays
data {
  int ntrials;  // number of trials per participant; "int" means that the s are integers
  int nsub;     // number of subjects
  vector [ntrials] rewardA;     // if rewarded when chose shape A
  vector [ntrials] punishA;     // if lost when chose shape A
  vector [ntrials] rewardB;
  vector [ntrials] punishB;
  int choices[ntrials,nsub];     // if chose shape A
  int includeTrial[ntrials];     // whether the data from this trial should be fitted (0 for trials to exclude)
  int pat_con[nsub];    // whether each subject is patient or control, for group fitting
}


// The 'parameters' block defines the parameter that we want to fit
parameters {
  // Stan syntax explanation:
  // real : parameters are real numbers
  // <lower=0,upper=1> : parameter is in the range of 0 to 1
  // alpha : name of the parameter
  // [nsub,2] : size of the parameter (number of rows, number of columns)
  // Group level parameters
  real<lower=0> alpha_a; // group level learning rate mean - pat con ; pos neg
  real<lower=0> alpha_b; // group level learning rate sd by pat and con
  real<lower=0> sensitivity_a; // group level mean for temperature by pat and con
  real<lower=0> sensitivity_b; // group level sd for temperature by pat and con
  real<lower=0> lapse_a;
  real<lower=0> lapse_b;

  // Single subject parameters
  real<lower=0,upper=1> alpha[nsub]; // learning rate - separate learning rates for positive and negative
  real<lower=0> sensitivity[nsub];   // temperature (i.e. how consistent choices are); one per participant
  real<lower=0,upper=1> lapse[nsub];
}

// This block runs the actual model
model {
  
  matrix [2,ntrials] outcome;
  
  outcome=[(rewardA-punishA)',(rewardB-punishB)'];

  // Priors
  alpha_a ~ normal(1,10);
  alpha_b ~ normal(1,10);
  sensitivity_a ~ normal(1,10);
  sensitivity_b ~ normal(1,10);
  lapse_a ~ normal(1,10);
  lapse_b ~ normal(1,10);

  // Priors for the individual subjects are the group (pat or con)
  alpha ~ beta(alpha_a,alpha_b);
  sensitivity ~ gamma(sensitivity_a,sensitivity_b);
  lapse ~ beta(lapse_a,lapse_b);


  for (p in 1:nsub){ // run the model for each subject
    vector [2] Q;
    
    Q = rep_vector(0,2); // first trial, best guess is that values are at 0
    
    for (t in 1:ntrials){
      choices[t,p] ~ categorical((1-lapse[p])*softmax(Q)+lapse[p]/2);

      // learning model
      Q[choices[t,p]] = Q[choices[t,p]] + alpha[p] * (sensitivity[p]*outcome[choices[t,p],t] - Q[choices[t,p]]); // reward denoted by go_outcome[t
    }
  }
}

generated quantities {
  
  real loglik[nsub];
  
  {
  
    matrix [2,ntrials] outcome;
  
    outcome=[(rewardA-punishA)',(rewardB-punishB)'];
  
  
    for (p in 1:nsub){ // run the model for each subject
      vector [2] Q;
      
      loglik[p]=0;
      
      Q = rep_vector(0,2); // first trial, best guess is that values are at 0
      
      for (t in 1:ntrials){
        loglik[p] += categorical_lpmf(choices[t,p]|(1-lapse[p])*softmax(Q)+lapse[p]/2);
  
        // learning model
        Q[choices[t,p]] = Q[choices[t,p]] + alpha[p] * (sensitivity[p]*outcome[choices[t,p],t] - Q[choices[t,p]]); // reward denoted by go_outcome[t
      }
    }
  }
}
