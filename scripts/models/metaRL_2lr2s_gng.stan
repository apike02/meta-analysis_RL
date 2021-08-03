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
  real alpha_win_a;
  real alpha_loss_a;
  real<lower=0> sensitivity_win_a;
  real<lower=0> sensitivity_loss_a;

  real<lower=0> alpha_win_b;  
  real<lower=0> alpha_loss_b;
  real<lower=0> sensitivity_win_b;
  real<lower=0> sensitivity_loss_b;
  
  // Single subject parameters
  real alpha_win_raw[nsub]; 
  real alpha_loss_raw[nsub];   
  real<lower=0> sensitivity_win[nsub];
  real<lower=0> sensitivity_loss[nsub];
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

  // Priors
  alpha_win_a ~ normal(0,3);
  alpha_win_b ~ cauchy(0,5);
  alpha_loss_a ~ normal(0,3);
  alpha_loss_b ~ cauchy(0,5);
  sensitivity_win_a ~ normal(1,5);
  sensitivity_win_b ~ normal(1,5);
  sensitivity_loss_a ~ normal(1,5);
  sensitivity_loss_b ~ normal(1,5);

  // Priors for the individual subjects are the group (pat or con)
  alpha_win_raw ~ std_normal();
  alpha_loss_raw ~ std_normal();
  sensitivity_win ~ gamma(sensitivity_win_a,sensitivity_win_b);
  sensitivity_loss ~ gamma(sensitivity_loss_a,sensitivity_loss_b);

  for (p in 1:nsub){ // run the model for each subject
    vector [4] Q_go;
    vector [4] Q_nogo;
    vector [2] Q;
    
    Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
    Q_nogo = rep_vector(0,4);
    
    for (t in 1:ntrials){
      real alpha;
      real sensitivity;
      if (stim[t]==1||stim[t]==3){
        alpha = alpha_win[p];
        sensitivity = sensitivity_win[p];
      } else {
        alpha=alpha_loss[p];
        sensitivity = sensitivity_loss[p];
      }
      
      Q[1] = Q_go[stim[t]];
      Q[2] = Q_nogo[stim[t]];
      choices[t,p] ~ categorical_logit(Q);

      // learning model
      if (choices[t,p]==1){
        Q_go[stim[t]] = Q_go[stim[t]] + alpha * (sensitivity*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
        } else { //for nogo choices
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha * (sensitivity*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
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
      vector [2] Q;
      
      loglik[p]=0;
      
      Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
      Q_nogo = rep_vector(0,4);
      
      for (t in 1:ntrials){
        real alpha;
        real sensitivity;
        if (stim[t]==1||stim[t]==3){
          alpha = alpha_win[p];
          sensitivity = sensitivity_win[p];
        } else {
          alpha=alpha_loss[p];
          sensitivity = sensitivity_loss[p];
        }
      
        Q[1] = Q_go[stim[t]];
        Q[2] = Q_nogo[stim[t]];
        loglik[p] += categorical_logit_lpmf(choices[t,p]|Q);
  
        // learning model
        if (choices[t,p]==1){
          Q_go[stim[t]] = Q_go[stim[t]] + alpha * (sensitivity*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
          } else { //for nogo choices
          Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha * (sensitivity*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
        }
      }
    }
  }
}