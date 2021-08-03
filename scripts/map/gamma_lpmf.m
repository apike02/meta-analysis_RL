function logprob=gamma_lpmf(param,prior1,prior2,tol)
prob=gamcdf(param+0.5*tol,prior1,prior2)-gamcdf(param-0.5*tol,prior1,prior2);
logprob=log(prob+eps);
end

