function likelihood = fit_1lr2t(params,fullid,simdata,tol,prior)

%col names: study pat/con id trial reward pun choices fullid
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
beta_rew=params(2);
beta_pun=params(3);
qa_rew=0;
qb_rew=0;
qa_pun=0;
qb_pun=0;

rewardB = 1-rewardA;
punishB = 1-punishA;

prob=zeros(2,1); %trials by number of options
actual_prob=zeros(trials,1);

for t=1:trials
    qa=qa_rew.*beta_rew-qa_pun.*beta_pun;
    qb=qb_rew.*beta_rew-qb_pun.*beta_pun;
    QA=qa-max([qa qb]);
    QB=qb-max([qa qb]);
    prob(1)=exp(QA)./(exp(QA)+exp(QB));
    prob(2)=exp(QB)./(exp(QB)+exp(QA));

    if choices(t)==1
        actual_prob(t)=prob(1);
    else
        actual_prob(t)=prob(2);
    end



    if choices(t)==1
    qa_rew=qa_rew+alpha*(rewardA(t) - qa_rew);
    qa_pun=qa_pun+alpha*(punishA(t) - qa_pun);
    else 
    qb_rew=qb_rew+alpha*(rewardB(t) - qb_rew);
    qb_pun=qb_pun+alpha*(punishB(t) - qb_pun);
    end
end



actual_prob_log = log(actual_prob);
if nargin<4
    likelihood=-(sum(actual_prob_log));
else 
    if group==1
        %1 is param 1, 2 is group 2 (con), 1/2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,1}(1),prior{1,1}(2),tol)+...
        gamma_lpmf(beta_rew,prior{2,1}(1),prior{2,1}(2),tol)+...
        gamma_lpmf(beta_pun,prior{3,1}(1),prior{3,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1/2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,2}(1),prior{1,2}(2),tol)+...
        gamma_lpmf(beta_rew,prior{2,2}(1),prior{2,2}(2),tol)+...
        gamma_lpmf(beta_pun,prior{3,2}(1),prior{3,2}(2),tol);
    end
    likelihood=-(sum(actual_prob_log)+log_prior);
end