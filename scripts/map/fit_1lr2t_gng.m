function likelihood = fit_1lr2t_gng(params,fullid,simdata,tol,prior)

%col names: study pat./con id trial reward pun choices fullid
%Q is value function

participant= simdata(simdata(:,9)==fullid,:);

stim= participant(:,5);
go_outcome=participant(:,6);
nogo_outcome=participant(:,7);

choices=participant(:,8);
group=unique(participant(:,2));
%fixed parameters
trials = size(simdata,1)./max(simdata(:,9));
%free params
alpha=params(1);
beta_rew=params(2);
beta_pun=params(3);
qa_rew_go=0;
qa_rew_nogo=0;
qa_pun_go=0;
qa_pun_nogo=0;
qb_rew_go=0;
qb_rew_nogo=0;
qb_pun_go=0;
qb_pun_nogo=0;
qc_rew_go=0;
qc_rew_nogo=0;
qc_pun_go=0;
qc_pun_nogo=0;
qd_rew_go=0;
qd_rew_nogo=0;
qd_pun_go=0;
qd_pun_nogo=0;


prob=zeros(2,1); %trials by number of options
actual_prob=zeros(trials,1);

for t=1:trials
    if stim(t)==1
        q_go=qa_rew_go.*beta_rew + qa_pun_go.*beta_pun;
        q_ng=qa_rew_nogo.*beta_rew + qa_pun_nogo.*beta_pun;
        Q_go=q_go-max([q_go q_ng]);
        Q_nogo=q_ng-max([q_go q_ng]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qa_rew_go=qa_rew_go+alpha*(go_outcome(t) - qa_rew_go); %don't update nogo
            qa_pun_go=qa_pun_go+alpha*(0 - qa_pun_go); % no pun as A is always rewarding
           else 
            qa_rew_nogo=qa_rew_nogo+alpha*(nogo_outcome(t) - qa_rew_nogo);
            qa_pun_nogo=qa_pun_nogo+alpha*(0 - qa_pun_nogo); 
           end
    elseif stim(t)==2
        q_go=qb_rew_go.*beta_rew + qb_pun_go.*beta_pun;
        q_ng=qb_rew_nogo.*beta_rew + qb_pun_nogo.*beta_pun;
        Q_go=q_go-max([q_go q_ng]);
        Q_nogo=q_ng-max([q_go q_ng]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qb_rew_go=qb_rew_go+alpha*(0 - qb_rew_go); %don't update nogo %B always pun
            qb_pun_go=qb_pun_go+alpha*(go_outcome(t) - qb_pun_go); 
           else 
            qb_rew_nogo=qb_rew_nogo+alpha*(0 - qb_rew_nogo);
            qb_pun_nogo=qb_pun_nogo+alpha*(nogo_outcome(t) - qb_pun_nogo); 
           end
     elseif stim(t)==3
        q_go=qc_rew_go.*beta_rew + qc_pun_go.*beta_pun;
        q_ng=qc_rew_nogo.*beta_rew + qc_pun_nogo.*beta_pun;
        Q_go=q_go-max([q_go q_ng]);
        Q_nogo=q_ng-max([q_go q_ng]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qc_rew_go=qc_rew_go+alpha*(go_outcome(t) - qc_rew_go); %don't update nogo
            qc_pun_go=qc_pun_go+alpha*(0 - qc_pun_go); % no pun as A is always rewarding
           else 
            qc_rew_nogo=qc_rew_nogo+alpha*(nogo_outcome(t) - qc_rew_nogo);
            qc_pun_nogo=qc_pun_nogo+alpha*(0 - qc_pun_nogo); 
           end
     elseif stim(t)==4
        q_go=qd_rew_go.*beta_rew + qd_pun_go.*beta_pun;
        q_ng=qd_rew_nogo.*beta_rew + qd_pun_nogo.*beta_pun;
        Q_go=q_go-max([q_go q_ng]);
        Q_nogo=q_ng-max([q_go q_ng]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qd_rew_go=qd_rew_go+alpha*(0 - qd_rew_go); %don't update nogo
            qd_pun_go=qd_pun_go+alpha*(go_outcome(t) - qd_pun_go); % no pun as A is always rewarding
           else 
            qd_rew_nogo=qd_rew_nogo+alpha*(0 - qd_rew_nogo);
            qd_pun_nogo=qd_pun_nogo+alpha*(nogo_outcome(t) - qd_pun_nogo); 
           end
    end 

    if choices(t)==1
        actual_prob(t)=prob(1);
    else
        actual_prob(t)=prob(2);
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