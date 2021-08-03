function logprob=beta_lpmf(param,prior1,prior2,tol)
prob=betacdf(param+0.5*tol,prior1,prior2)-betacdf(param-0.5*tol,prior1,prior2);
logprob=log(prob+eps);
end

