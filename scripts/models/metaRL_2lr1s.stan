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
  real alpha_win_a;
  real alpha_loss_a;
  real<lower=0> sensitivity_a; 

  real<lower=0> alpha_win_b;  
  real<lower=0> alpha_loss_b;
  real<lower=0> sensitivity_b; 
  
  // Single subject parameters
  real alpha_win_raw[nsub]; // learning rate - separate learning rates for positive and negative
  real alpha_loss_raw[nsub];   // temperature (i.e. how consistent choices are); one per participant
  real<lower=0> sensitivity[nsub];
}

transformed parameters {
  real<lower=0,upper=1> alpha_win[nsub];
  real<lower=0,upper=1> alpha_loss[nsub];

  for (p in 1:nsub){
    alpha_win[p] = Phi_approx(alpha_win_a + alpha_win_b*alpha_win_raw[p]);
    alpha_loss[p] = Phi_approx(alpha_loss_a + alpha_loss_b*alpha_loss_raw[p]);
  }
}

// This block runs the actual model
model {
  matrix [2,ntrials] reward;
  matrix [2,ntrials] punish;
  
  reward=[rewardA',rewardB'];
  punish=[punishA',punishB'];
  
  
  // Priors
  alpha_win_a ~ normal(0,3);
  alpha_win_b ~ cauchy(0,5);
  alpha_loss_a ~ normal(0,3);
  alpha_loss_b ~ cauchy(0,5);
  sensitivity_a ~ normal(1,5);
  sensitivity_b ~ normal(1,5);

  // Priors for the individual subjects are the group (pat or con)
  alpha_win_raw ~ std_normal();
  alpha_loss_raw ~ std_normal();
  sensitivity ~ gamma(sensitivity_a,sensitivity_b);


  for (p in 1:nsub){ // run the model for each subject
    vector [2] Q_reward;
    vector [2] Q_punish;
    
    Q_reward = rep_vector(0,2); // first trial, best guess is that values are at 0
    Q_punish = rep_vector(0,2);
    
    for (t in 1:ntrials){
      choices[t,p] ~ categorical_logit(Q_reward-Q_punish);

      // learning model
      Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity[p]*reward[choices[t,p],t] - Q_reward[choices[t,p]]); 
      Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity[p]*punish[choices[t,p],t] - Q_punish[choices[t,p]]); 
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
        Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity[p]*reward[choices[t,p],t] - Q_reward[choices[t,p]]); 
        Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity[p]*punish[choices[t,p],t] - Q_punish[choices[t,p]]); 
      }
    }
  }
}
