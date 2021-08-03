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
  real<lower=0> alpha_win_a; // group level learning rate mean - pat con ; pos neg
  real<lower=0> alpha_win_b;
  real<lower=0> alpha_loss_a;
  real<lower=0> alpha_loss_b; // group level learning rate sd by pat and con
  real<lower=0> lapse_a; // group level mean for lapse by pat and con
  real<lower=0> lapse_b; // group level sd for lapse by pat and con
  real<lower=0> sensitivity_a;
  real<lower=0> sensitivity_b;
  // Single subject parameters
  real<lower=0,upper=1> alpha_win[nsub]; // learning rate - separate learning rates for positive and negative
  real<lower=0,upper=1> alpha_loss[nsub];
  real<lower=0,upper=1> lapse[nsub];   // lapse (i.e. how consistent choices are); one per participant
  real<lower=0> sensitivity[nsub];
}

// This block runs the actual model
model {
  // Priors
  alpha_win_a ~ normal(1.1,0.5);
  alpha_win_b ~ normal(1.1,0.5);
  alpha_loss_a ~ normal(1.1,0.5);
  alpha_loss_b ~ normal(1.1,0.5);
  lapse_a ~ normal(1.1,0.5);
  lapse_b ~ normal(1.1,0.5);
  sensitivity_a ~ normal(1,10);
  sensitivity_b ~ normal(1,10);


  // Priors for the individual subjects are the group (pat or con)
  alpha_win ~ beta(alpha_win_a,alpha_win_b);
  alpha_loss ~ beta(alpha_loss_a,alpha_loss_b);
  lapse ~ beta(lapse_a,lapse_b);
  sensitivity ~ gamma(sensitivity_a,sensitivity_b);

  for (p in 1:nsub){ // run the model for each subject
    // temporary variables that we will compute for each person and each trial
    vector [4] Q;  //Q values: reward A, punish A, reward B, punish B

    Q = rep_vector(0,4);

    for (t in 1:ntrials){
      vector [2] Q_summarise;
      vector [2] Q_temp;
      //action model
      Q_summarise[1] = Q[1]-Q[2];
      Q_summarise[2] = Q[3]-Q[4];
      
      Q_temp[1] = (1-lapse[p])*exp(Q_summarise[1])/(exp(Q_summarise[1])+exp(Q_summarise[2])) + lapse[p]/2;
      Q_temp[2] = (1-lapse[p])*exp(Q_summarise[2])/(exp(Q_summarise[1])+exp(Q_summarise[2])) + lapse[p]/2;
      choices[t,p] ~ categorical(Q_temp);

      //learning model
      if (choices[t,p]==1){
        Q[1] = Q[1] + alpha_win[p] * (sensitivity[p]*rewardA[t]- Q[1]); // Q learning for rewards for A
        Q[2] = Q[2] + alpha_loss[p] * (sensitivity[p]*punishA[t] - Q[2]);
        //don't update unchosen
      } else {
        Q[3] = Q[3] + alpha_win[p] * (sensitivity[p]*rewardB[t]- Q[3]); // Q learning for rewards for A
        Q[4] = Q[4] + alpha_loss[p] * (sensitivity[p]*punishB[t] - Q[4]);
      }
    }
  }
}
