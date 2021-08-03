// The 'data' block list all input variables that are given to Stan from R. You need to specify the size of the arrays
data {
  int ntrials;  // number of trials per participant; "int" means that the s are integers
  int nsub;     // number of subjects
  int rewardA[ntrials];     // if rewarded when chose shape A
  int punishA[ntrials];     // if lost when chose shape A
  int choices[ntrials,nsub];     // if chose shape A
}


// The 'parameters' block defines the parameter that we want to fit
parameters {
  
  // parameter 1
  
  real<lower=0> alpha_win_a; 
  real<lower=0> alpha_loss_a;
  real<lower=0> sensitivity_win_a;
  real<lower=0> sensitivity_loss_a;
  real<lower=0> lapse_a; 
  
  // parameter 2 
  real<lower=0> alpha_win_b;
  real<lower=0> alpha_loss_b;
  real<lower=0> sensitivity_win_b;
  real<lower=0> sensitivity_loss_b;
  real<lower=0> lapse_b;
  
  // raw parameter
  vector [nsub] alpha_win_raw; 
  vector [nsub] alpha_loss_raw;
  vector [nsub] sensitivity_win_raw;
  vector [nsub] sensitivity_loss_raw;
  vector [nsub] lapse_raw;  
}

transformed parameters {
  vector<lower=0,upper=1>[nsub] alpha_win;
  vector<lower=0,upper=1>[nsub] alpha_loss;
  vector<lower=0>[nsub] sensitivity_win;
  vector<lower=0>[nsub] sensitivity_loss;
  vector<lower=0,upper=1>[nsub] lapse;
  
  for (p in 1:nsub){
    alpha_win[p] = Phi_approx(alpha_win_a + alpha_win_b*alpha_win_raw[p]);
    alpha_loss[p] = Phi_approx(alpha_loss_a + alpha_loss_b*alpha_loss_raw[p]);
    sensitivity_win[p] = Phi_approx(sensitivity_win_a + sensitivity_win_b*sensitivity_win_raw[p])*20;
    sensitivity_loss[p] = Phi_approx(sensitivity_loss_a + sensitivity_loss_b*sensitivity_loss_raw[p])*20;
    lapse[p] = Phi_approx(lapse_a + lapse_b*lapse_raw[p]);
  }
  
}

// This block runs the actual model
model {
  vector [2] Q_reward [nsub];  
  vector [2] Q_punish [nsub];
  vector [2] Q;
  
  // Prior parameter 1
  alpha_win_a ~ normal(0,1);
  alpha_loss_a ~ normal(0,1);
  lapse_a ~ normal(0,1);
  sensitivity_win_a ~ normal(0,1);
  sensitivity_loss_a ~ normal(0,1);

  // Prior parameter 2
  alpha_win_b ~ cauchy(0,5);
  alpha_loss_b ~ cauchy(0,5);
  sensitivity_win_b ~ cauchy(0,5);
  sensitivity_loss_b ~ cauchy(0,5);
  lapse_b ~ cauchy(0,5);


  // Priors for the raw difference for each sub
  alpha_win_raw ~ normal(0,1);
  alpha_loss_raw ~ normal(0,1);
  sensitivity_win_raw ~ normal(0,1);
  sensitivity_loss_raw ~ normal(0,1);
  lapse_raw ~ normal(0,1);

    // temporary variables that we will compute for each person and each trial
  Q_reward = rep_array(rep_vector(0,2),nsub); // first trial, best guess is that values are at 0
  Q_punish = rep_array(rep_vector(0,2),nsub); // first trial, best guess is that values are at 0

  for (t in 1:ntrials){
    //action model
    for (p in 1:nsub){
        Q = softmax(Q_reward[p] - Q_punish[p]) * (1-lapse[p]) + lapse[p]/2;
    }
    choices[t,] ~ categorical(Q);

    for (p in 1:nsub){
      //learning model
      Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity_win[p]*rewardA[t]- Q_reward[choices[t,p]]);
      Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity_loss[p]*punishA[t] - Q_punish[choices[t,p]]);
    }
  }
}
