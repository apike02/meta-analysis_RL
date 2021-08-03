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
  real alpha_a; 
  real beta_a; 


  real<lower=0> alpha_b; 
  real<lower=0> beta_b;

  // Single subject parameters
  real alpha_raw[nsub]; 
  real beta_raw[nsub];   
}

transformed parameters {
  real<lower=0,upper=1> alpha[nsub];
  real<lower=0>beta[nsub];
  
  for (p in 1:nsub){
    alpha[p] = Phi_approx(alpha_a + alpha_b*alpha_raw[p]);
    beta[p] = exp(beta_a + beta_b*beta_raw[p]);
  }
}

// This block runs the actual model
model {
  
  matrix [2,ntrials] outcome;
  
  outcome=[(rewardA-punishA)',(rewardB-punishB)'];

  // Priors
  alpha_a ~ normal(0,3);
  alpha_b ~ cauchy(0,5);
  beta_a ~ normal(0,3);
  beta_b ~ cauchy(0,5);

  // Priors for the individual subjects are the group (pat or con)
  alpha_raw ~ std_normal();
  beta_raw ~ std_normal();


  for (p in 1:nsub){ // run the model for each subject
    vector [2] Q;
    
    Q = rep_vector(0,2); // first trial, best guess is that values are at 0
    
    for (t in 1:ntrials){
      choices[t,p] ~ categorical_logit(Q*beta[p]);

      // learning model
      Q[choices[t,p]] = Q[choices[t,p]] + alpha[p] * (outcome[choices[t,p],t] - Q[choices[t,p]]); // reward denoted by go_outcome[t
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
        loglik[p] += categorical_logit_lpmf(choices[t,p]|Q*beta[p]);
  
        // learning model
        Q[choices[t,p]] = Q[choices[t,p]] + alpha[p] * (outcome[choices[t,p],t] - Q[choices[t,p]]); // reward denoted by go_outcome[t
      }
    }
  }
}