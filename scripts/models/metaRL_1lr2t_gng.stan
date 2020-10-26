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
  real<lower=0> alpha_a; 
  real<lower=0> alpha_b; 
  real<lower=0> beta_win_a;
  real<lower=0> beta_win_b; 
  real<lower=0> beta_loss_a;
  real<lower=0> beta_loss_b;

  // Single subject parameters
  real<lower=0,upper=1> alpha[nsub]; 
  real<lower=0> beta_win[nsub];
  real<lower=0> beta_loss[nsub];
}

// This block runs the actual model
model {

  // Priors
  alpha_a ~ normal(1,10);
  alpha_b ~ normal(1,10);
  beta_win_a ~ normal(1,10);
  beta_win_b ~ normal(1,10);
  beta_loss_a ~ normal(1,10);
  beta_loss_b ~ normal(1,10);


  // Priors for the individual subjects are the group (pat or con)
  alpha ~ beta(alpha_a,alpha_b);
  beta_win ~ gamma(beta_win_a,beta_win_b);
  beta_loss ~ gamma(beta_loss_a,beta_loss_b);


  for (p in 1:nsub){ // run the model for each subject
    vector [4] Q_go;
    vector [4] Q_nogo;
    vector [2] Q;
    
    Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
    Q_nogo = rep_vector(0,4);
    
    for (t in 1:ntrials){
      real beta;
      if (stim[t]==1||stim[t]==3){
        beta = beta_win[p];
      } else {
        beta = beta_loss[p];
      }
      
      Q[1] = Q_go[stim[t]]*beta;
      Q[2] = Q_nogo[stim[t]]*beta;
      choices[t,p] ~ categorical_logit(Q);

      // learning model
      if (choices[t,p]==1){
        Q_go[stim[t]] = Q_go[stim[t]] + alpha[p] * (go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
        } else { //for nogo choices
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha[p] * (nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
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
        real beta;
        if (stim[t]==1||stim[t]==3){
          beta = beta_win[p];
        } else {
          beta = beta_loss[p];
        }
      
        Q[1] = Q_go[stim[t]]*beta;
        Q[2] = Q_nogo[stim[t]]*beta;
        loglik[p] += categorical_logit_lpmf(choices[t,p]|Q);
  
        // learning model
        if (choices[t,p]==1){
          Q_go[stim[t]] = Q_go[stim[t]] + alpha[p] * (go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
          } else { //for nogo choices
          Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha[p] * (nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
        }
      }
    }
  }
}