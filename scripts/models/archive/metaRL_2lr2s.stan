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
  real<lower=0> alpha_win_a;  
  real<lower=0> alpha_win_b;
  real<lower=0> alpha_loss_a;
  real<lower=0> alpha_loss_b;
  real<lower=0> sensitivity_win_a; 
  real<lower=0> sensitivity_win_b;
  real<lower=0> sensitivity_loss_a;
  real<lower=0> sensitivity_loss_b; 

  // Single subject parameters
  real<lower=0,upper=1> alpha_win[nsub]; // learning rate - separate learning rates for positive and negative
  real<lower=0,upper=1> alpha_loss[nsub];
  real<lower=0> sensitivity_win[nsub];   // temperature (i.e. how consistent choices are); one per participant
  real<lower=0> sensitivity_loss[nsub];
}

// This block runs the actual model
model {
  
  matrix [2,ntrials] reward;
  matrix [2,ntrials] punish;
  
  reward=[rewardA',rewardB'];
  punish=[punishA',punishB'];
  
  
  // Priors
  alpha_win_a ~ normal(1,10);
  alpha_win_b ~ normal(1,10);
  alpha_loss_a ~ normal(1,10);
  alpha_loss_b ~ normal(1,10);
  sensitivity_win_a ~ normal(1,10);
  sensitivity_win_b ~ normal(1,10);
  sensitivity_loss_a ~ normal(1,10);
  sensitivity_loss_b ~ normal(1,10);

  // Priors for the individual subjects are the group (pat or con)
  alpha_win ~ beta(alpha_win_a,alpha_win_b);
  alpha_loss ~ beta(alpha_loss_a,alpha_loss_b);
  sensitivity_win ~ gamma(sensitivity_win_a,sensitivity_win_b);
  sensitivity_loss ~ gamma(sensitivity_loss_a,sensitivity_loss_b);


  for (p in 1:nsub){ // run the model for each subject
    vector [2] Q_reward;
    vector [2] Q_punish;
    
    Q_reward = rep_vector(0,2); // first trial, best guess is that values are at 0
    Q_punish = rep_vector(0,2);
    
    for (t in 1:ntrials){
      choices[t,p] ~ categorical_logit(Q_reward-Q_punish);

      // learning model
      Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity_win[p]*reward[choices[t,p],t] - Q_reward[choices[t,p]]); 
      Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity_loss[p]*punish[choices[t,p],t] - Q_punish[choices[t,p]]); 
    }
  }
}

generated quantities {
  
  real loglik[nsub];
  
  {
  
    matrix [2,ntrials] reward;
    matrix [2,ntrials] punish;
  
    reward=[rewardA',rewardB'];
    punish=[punishA',punishB'];
  
  
  
    for (p in 1:nsub){ // run the model for each subject
      vector [2] Q_reward;
      vector [2] Q_punish;
    
      Q_reward = rep_vector(0,2); // first trial, best guess is that values are at 0
      Q_punish = rep_vector(0,2);
      
      loglik[p]=0;
      
      
      for (t in 1:ntrials){
        loglik[p] += categorical_logit_lpmf(choices[t,p]|(Q_reward-Q_punish));
  
        // learning model
        Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity_win[p]*reward[choices[t,p],t] - Q_reward[choices[t,p]]); 
        Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity_loss[p]*punish[choices[t,p],t] - Q_punish[choices[t,p]]); 
      }
    }
  }
}