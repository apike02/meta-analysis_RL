// The 'data' block list all input variables that are given to Stan from R. You need to specify the size of the arrays
data {
  int ntrials;  // number of trials per participant; "int" means that the values are integers
  int nsub;     // number of subjects
  int stim [ntrials];     // the stimulus they were presented with per trial
  vector [ntrials] go_outcome;     // the outcome they received if they made a 'go' response
  vector [ntrials] nogo_outcome; // the outcome they received if they made a 'nogo' response
  int choices[ntrials,nsub];     // the response they made (go or nogo)
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
  real<lower=0> lapse_a;
  real<lower=0> lapse_b;
  real go_bias_a;
  real<lower=0> go_bias_b;
  real app_bias_a;
  real<lower=0> app_bias_b;
  real av_bias_a;
  real<lower=0> av_bias_b;

  // Single subject parameters
  real<lower=0,upper=1> alpha_win[nsub]; 
  real<lower=0,upper=1> alpha_loss[nsub];
  real<lower=0> sensitivity_win[nsub];
  real<lower=0> sensitivity_loss[nsub];
  real<lower=0,upper=1> lapse[nsub];
  real go_bias[nsub];
  real app_bias[nsub];
  real av_bias[nsub];
}

// This block runs the actual model
model {

  // Priors
  alpha_win_a ~ normal(1,10);
  alpha_win_b ~ normal(1,10);
  alpha_loss_a ~ normal(1,10);
  alpha_loss_b ~ normal(1,10);
  sensitivity_win_a ~ normal(1,10);
  sensitivity_win_b ~ normal(1,10);
  sensitivity_loss_a ~ normal(1,10);
  sensitivity_loss_b ~ normal(1,10);
  lapse_a ~ normal(1,10);
  lapse_b ~ normal(1,10);
  go_bias_a ~ normal(1,10);
  go_bias_b ~ normal(1,10);
  app_bias_a ~ normal(1,10);
  app_bias_b ~ normal(1,10);
  av_bias_a ~ normal(1,10);
  av_bias_b ~ normal(1,10);
  


  // Priors for the individual subjects are the group (pat or con)
  alpha_win ~ beta(alpha_win_a,alpha_win_b);
  alpha_loss ~ beta(alpha_loss_a,alpha_loss_b);
  sensitivity_win ~ gamma(sensitivity_win_a,sensitivity_win_b);
  sensitivity_loss ~ gamma(sensitivity_loss_a,sensitivity_loss_b);
  lapse ~ beta(lapse_a,lapse_b);
  go_bias ~ normal(go_bias_a,go_bias_b);
  app_bias ~ normal(app_bias_a,app_bias_b);
  av_bias ~ normal(av_bias_a,av_bias_b);


  for (p in 1:nsub){ // run the model for each subject
    vector [4] Q_go;
    vector [4] Q_nogo;
    vector [2] Q_temp;
    vector [2] Q;
    vector [4] value;
    
    Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
    Q_nogo = rep_vector(0,4);
    value = rep_vector(0,4);
    
    for (t in 1:ntrials){
      real alpha;
      real sensitivity;
      real pav_bias;
      if (stim[t]==1||stim[t]==3){
        alpha = alpha_win[p];
        sensitivity = sensitivity_win[p];
        pav_bias = app_bias[p];
      } else {
        alpha = alpha_loss[p];
        sensitivity=sensitivity_loss[p];
        pav_bias = av_bias[p];
      }
    
      Q_temp[1] = Q_go[stim[t]] + go_bias[p] + pav_bias*value[stim[t]]; 
      Q_temp[2] = Q_nogo[stim[t]];
      
      Q = (1-lapse[p])*softmax(Q_temp) + lapse[p]/2;
      
      choices[t,p] ~ categorical(Q);

      // learning model
      if (choices[t,p]==1){
        Q_go[stim[t]] = Q_go[stim[t]] + alpha * (sensitivity*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
        value[stim[t]]=value[stim[t]] + alpha* (sensitivity*go_outcome[t] - value[stim[t]]);
        } else { //for nogo choices
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha * (sensitivity*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
        value[stim[t]]=value[stim[t]] + alpha* (sensitivity*nogo_outcome[t] - value[stim[t]]);
      }
    }
  }
}

generated quantities{
  
  real loglik[nsub];
    {
    
    for (p in 1:nsub){ // run the model for each subject
      vector [4] Q_go;
      vector [4] Q_nogo;
      vector [2] Q_temp;
      vector [2] Q;
      vector [4] value;
      
      loglik[p]=0;
      
      Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
      Q_nogo = rep_vector(0,4);
      value = rep_vector(0,4);
      
      for (t in 1:ntrials){
        real alpha;
        real sensitivity;
        real pav_bias;
        if (stim[t]==1||stim[t]==3){
          alpha = alpha_win[p];
          sensitivity = sensitivity_win[p];
          pav_bias = app_bias[p];
        } else {
          alpha = alpha_loss[p];
          sensitivity=sensitivity_loss[p];
          pav_bias = av_bias[p];
        }
        Q_temp[1] = Q_go[stim[t]] + go_bias[p] + pav_bias*value[stim[t]]; 
        Q_temp[2] = Q_nogo[stim[t]];
        Q = (1-lapse[p])*softmax(Q_temp) + lapse[p]/2;

        loglik[p] += categorical_lpmf(choices[t,p]|Q);
  
        // learning model
        if (choices[t,p]==1){
          Q_go[stim[t]] = Q_go[stim[t]] + alpha * (sensitivity*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
          value[stim[t]]=value[stim[t]] + alpha* (sensitivity*go_outcome[t] - value[stim[t]]);
          } else { //for nogo choices
          Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha * (sensitivity*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
          value[stim[t]]=value[stim[t]] + alpha* (sensitivity*nogo_outcome[t] - value[stim[t]]);
        }
      }
    }
  }
}
