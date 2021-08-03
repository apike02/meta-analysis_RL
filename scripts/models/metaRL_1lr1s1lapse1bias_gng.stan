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
  real alpha_a; 
  real<lower=0> sensitivity_a; 
  real lapse_a;
  real go_bias_a;

  real<lower=0> alpha_b; 
  real<lower=0> sensitivity_b;
  real<lower=0> lapse_b;
  real<lower=0> go_bias_b;

  // Single subject parameters
  real alpha_raw[nsub]; 
  real<lower=0> sensitivity[nsub];   
  real lapse_raw[nsub];
  real go_bias_raw[nsub];
}

transformed parameters {
  real<lower=0,upper=1> alpha[nsub];
  real<lower=0>lapse[nsub];
  real go_bias[nsub];
  
  for (p in 1:nsub){
    alpha[p] = Phi_approx(alpha_a + alpha_b*alpha_raw[p]);
    lapse[p] = Phi_approx(lapse_a + lapse_b*lapse_raw[p]);
    go_bias[p] = go_bias_a + go_bias_b*go_bias_raw[p];
  }
}

// This block runs the actual model
model {
  
  // Priors
  alpha_a ~ normal(0,3);
  alpha_b ~ cauchy(0,5);
  sensitivity_a ~ normal(1,5);
  sensitivity_b ~ normal(1,5);
  lapse_a ~ normal(0,3);
  lapse_b ~ cauchy(0,5);
  go_bias_a ~ normal(0,3);
  go_bias_b ~ cauchy(0,5);

  // Priors for the individual subjects are the group (pat or con)
  alpha_raw ~ std_normal();
  sensitivity ~ gamma(sensitivity_a,sensitivity_b);
  lapse_raw ~ std_normal();
  go_bias_raw ~ std_normal();


  for (p in 1:nsub){ // run the model for each subject
    vector [4] Q_go;
    vector [4] Q_nogo;
    vector [2] Q_temp;
    vector [2] Q;
    
    Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
    Q_nogo = rep_vector(0,4);
    
    for (t in 1:ntrials){
      Q_temp[1] = Q_go[stim[t]] + go_bias[p]; 
      Q_temp[2] = Q_nogo[stim[t]];
      
      Q = (1-lapse[p])*softmax(Q_temp) + lapse[p]/2;
      
      choices[t,p] ~ categorical(Q);

      // learning model
      if (choices[t,p]==1){
        Q_go[stim[t]] = Q_go[stim[t]] + alpha[p] * (sensitivity[p]*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
        } else { //for nogo choices
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha[p] * (sensitivity[p]*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
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
      
      loglik[p]=0;
      
      Q_go = rep_vector(0,4); // first trial, best guess is that values are at 0
      Q_nogo = rep_vector(0,4);
      
      for (t in 1:ntrials){
        Q_temp[1] = Q_go[stim[t]] + go_bias[p]; 
        Q_temp[2] = Q_nogo[stim[t]];
        Q = (1-lapse[p])*softmax(Q_temp) + lapse[p]/2;

        loglik[p] += categorical_lpmf(choices[t,p]|Q);
  
        // learning model
        if (choices[t,p]==1){
          Q_go[stim[t]] = Q_go[stim[t]] + alpha[p] * (sensitivity[p]*go_outcome[t] - Q_go[stim[t]]); // reward denoted by go_outcome[t
          } else { //for nogo choices
          Q_nogo[stim[t]] = Q_nogo[stim[t]] + alpha[p] * (sensitivity[p]*nogo_outcome[t] - Q_nogo[stim[t]]); // reward denoted by go_outcome[t]
        }
      }
    }
  }
}
