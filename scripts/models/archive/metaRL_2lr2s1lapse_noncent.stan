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
    sensitivity_win[p] = Phi_approx(sensitivity_win_a + sensitivity_win_b*sensitivity_win_raw[p]);
    sensitivity_loss[p] = Phi_approx(sensitivity_loss_a + sensitivity_loss_b*sensitivity_loss_raw[p]);
    lapse[p] = Phi_approx(lapse_a + lapse_b*lapse_raw[p]);
  }
  
}

// This block runs the actual model
model {
  vector [2] Q_reward;  
  vector [2] Q_punish;
  vector [2] Q;
  
  // Prior parameter 1
  alpha_win_a ~ std_normal();
  alpha_loss_a ~ std_normal();
  lapse_a ~ std_normal();
  sensitivity_win_a ~ std_normal();
  sensitivity_loss_a ~ std_normal();

  // Prior parameter 2
  alpha_win_b ~ cauchy(0,5);
  alpha_loss_b ~ cauchy(0,5);
  sensitivity_win_b ~ cauchy(0,5);
  sensitivity_loss_b ~ cauchy(0,5);
  lapse_b ~ cauchy(0,5);


  // Priors for the raw difference for each sub
  alpha_win_raw ~ std_normal();
  alpha_loss_raw ~ std_normal();
  sensitivity_win_raw ~ std_normal();
  sensitivity_loss_raw ~ std_normal();
  lapse_raw ~ std_normal();


  for (p in 1:nsub){ // run the model for each subject
    // temporary variables that we will compute for each person and each trial
    Q_reward = rep_vector(0,2); // first trial, best guess is that values are at 0
    Q_punish = rep_vector(0,2);

    for (t in 1:ntrials){
      //action model
      Q = softmax(Q_reward - Q_punish) * (1-lapse[p]) + lapse[p]/2;
      choices[t,p] ~ categorical(Q);

      //learning model
      Q_reward[choices[t,p]] = Q_reward[choices[t,p]] + alpha_win[p] * (sensitivity_win[p]*rewardA[t]- Q_reward[choices[t,p]]);
      Q_punish[choices[t,p]] = Q_punish[choices[t,p]] + alpha_loss[p] * (sensitivity_loss[p]*punishA[t] - Q_punish[choices[t,p]]);
    }
  }
}
