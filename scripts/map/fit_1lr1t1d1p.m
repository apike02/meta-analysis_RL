function likelihood = fit_1lr1t1d1p(params,fullid,simdata,tol,prior)

%col names: study pat./con id trial reward pun choices fullid
%Q is value function

participant= simdata(simdata(:,8)==fullid,:);

rewardA= participant(:,5);
punishA=participant(:,6);

choices=participant(:,7);
group=unique(participant(:,2));
%fixed parameters
trials = size(simdata,1)./max(simdata(:,8));
%free params
alpha=params(1);
beta=params(2);
decay=params(3);
perseverance=params(4);
qa=0;
qb=0;

outcomeA=rewardA-punishA;
outcomeB=(1-rewardA)-(1-punishA);

prob=zeros(2,1); %trials by number of options
actual_prob=zeros(trials,1);
choice_trace=zeros(2,1);

for t=1:trials
    QA=qa-max([qa qb]);
    QB=qb-max([qa qb]);
    prob(1)=exp(QA.*beta-perseverance*choice_trace(1))./(exp(QA.*beta-perseverance*choice_trace(1))+exp(QB.*beta-perseverance*choice_trace(2)));
    prob(2)=exp(QB.*beta-perseverance*choice_trace(2))./(exp(QB.*beta-perseverance*choice_trace(2))+exp(QA.*beta-perseverance*choice_trace(1)));

    if choices(t)==1
        actual_prob(t)=prob(1);
    else
        actual_prob(t)=prob(2);
    end

    if choices(t)==1
    qa=qa+alpha*(outcomeA(t) - qa); %don't update qb
    choice_trace(1)=choice_trace(1)+decay*(1-choice_trace(1));
    choice_trace(2)=choice_trace(2)+decay*(0-choice_trace(2));
    else 
    qb=qb+alpha*(outcomeB(t) - qb);
    choice_trace(1)=choice_trace(1)+decay*(0-choice_trace(1));
    choice_trace(2)=choice_trace(2)+decay*(1-choice_trace(2));
    end
end



actual_prob_log = log(actual_prob);
if nargin<4
    likelihood=-(sum(actual_prob_log));
else 
    if group==1
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,1}(1),prior{1,1}(2),tol)+...
        gamma_lpmf(beta,prior{2,1}(1),prior{2,1}(2),tol)+...
        beta_lpmf(decay,prior{3,1}(1),prior{3,1}(2),tol)+...
        norm_lpmf(perseverance,prior{4,1}(1),prior{4,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1/2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,2}(1),prior{1,2}(2),tol)+... 
        gamma_lpmf(beta,prior{2,2}(1),prior{2,2}(2),tol)+...
        beta_lpmf(decay,prior{3,2}(1),prior{3,2}(2),tol)+...
        norm_lpmf(perseverance,prior{4,2}(1),prior{4,2}(2),tol);
    end
        likelihood=-(sum(actual_prob_log)+log_prior);
end