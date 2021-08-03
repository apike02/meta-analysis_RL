function logprob=norm_lpmf(param,prior1,prior2,tol)
prob=normcdf(param+0.5*tol,prior1,prior2)-normcdf(param-0.5*tol,prior1,prior2);
logprob=log(prob+eps);
end

