function likelihood = fit_2lr1t1d1p_gng(params,fullid,simdata,tol,prior)

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
alpha_rew=params(1);
alpha_pun=params(2);
beta=params(3);
decay=params(4);
perseverance=params(5);


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
choice_trace_go=zeros(4,1);
choice_trace_nogo=zeros(4,1);

for t=1:trials
    if stim(t)==1
        qa_go = (qa_rew_go + qa_pun_go).*beta;
        qa_nogo = (qa_rew_nogo + qa_pun_nogo).*beta;
        Q_go = qa_go - max([qa_go qa_nogo])-perseverance*choice_trace_go(1);
        Q_nogo = qa_nogo - max([qa_go qa_nogo])-perseverance*choice_trace_nogo(1);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qa_rew_go=qa_rew_go+alpha_rew*(go_outcome(t) - qa_rew_go); %don't update nogo
            qa_pun_go=qa_pun_go+alpha_pun*(0 - qa_pun_go); % no pun as A is always rewarding
            choice_trace_go(1)=choice_trace_go(1)+decay.*(1-choice_trace_go(1));
            choice_trace_nogo(1)=choice_trace_nogo(1)+decay.*(0-choice_trace_nogo(1));
           else 
            qa_rew_nogo=qa_rew_nogo+alpha_rew*(nogo_outcome(t) - qa_rew_nogo);
            qa_pun_nogo=qa_pun_nogo+alpha_pun*(0 - qa_pun_nogo); 
            choice_trace_go(1)=choice_trace_go(1)+decay.*(0-choice_trace_go(1));
            choice_trace_nogo(1)=choice_trace_nogo(1)+decay.*(1-choice_trace_nogo(1));
           end
    elseif stim(t)==2
        qb_go = (qb_rew_go + qb_pun_go).*beta;
        qb_nogo = (qb_rew_nogo + qb_pun_nogo).*beta;
        Q_go = qb_go - max([qb_go qb_nogo])-perseverance*choice_trace_go(2);
        Q_nogo = qb_nogo - max([qb_go qb_nogo])-perseverance*choice_trace_nogo(2);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qb_rew_go=qb_rew_go+alpha_rew*(0 - qb_rew_go); %don't update nogo %B always pun
            qb_pun_go=qb_pun_go+alpha_pun*(go_outcome(t) - qb_pun_go); 
            choice_trace_go(2)=choice_trace_go(2)+decay.*(1-choice_trace_go(2));
            choice_trace_nogo(2)=choice_trace_nogo(2)+decay.*(0-choice_trace_nogo(2));
           else 
            qb_rew_nogo=qb_rew_nogo+alpha_rew*(0 - qb_rew_nogo);
            qb_pun_nogo=qb_pun_nogo+alpha_pun*(nogo_outcome(t) - qb_pun_nogo); 
            choice_trace_go(2)=choice_trace_go(2)+decay.*(0-choice_trace_go(2));
            choice_trace_nogo(2)=choice_trace_nogo(2)+decay.*(1-choice_trace_nogo(2));
           end
     elseif stim(t)==3
        qc_go = (qc_rew_go + qc_pun_go).*beta;
        qc_nogo = (qc_rew_nogo + qc_pun_nogo).*beta;
        Q_go = qc_go - max([qc_go qc_nogo])-perseverance*choice_trace_go(3);
        Q_nogo = qc_nogo - max([qc_go qc_nogo])-perseverance*choice_trace_nogo(3);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qc_rew_go=qc_rew_go+alpha_rew*(go_outcome(t) - qc_rew_go); %don't update nogo
            qc_pun_go=qc_pun_go+alpha_pun*(0 - qc_pun_go); % no pun as A is always rewarding
            choice_trace_go(3)=choice_trace_go(3)+decay.*(1-choice_trace_go(3));
            choice_trace_nogo(3)=choice_trace_nogo(3)+decay.*(0-choice_trace_nogo(3));
           else 
            qc_rew_nogo=qc_rew_nogo+alpha_rew*(nogo_outcome(t) - qc_rew_nogo);
            qc_pun_nogo=qc_pun_nogo+alpha_pun*(0 - qc_pun_nogo); 
            choice_trace_go(3)=choice_trace_go(3)+decay.*(0-choice_trace_go(3));
            choice_trace_nogo(3)=choice_trace_nogo(3)+decay.*(1-choice_trace_nogo(3));
           end
     elseif stim(t)==4
        qd_go = (qd_rew_go + qd_pun_go).*beta;
        qd_nogo = (qd_rew_nogo + qd_pun_nogo).*beta;
        Q_go = qd_go - max([qd_go qd_nogo])-perseverance*choice_trace_go(4);
        Q_nogo = qd_nogo - max([qd_go qd_nogo])-perseverance*choice_trace_nogo(4);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qd_rew_go=qd_rew_go+alpha_rew*(0 - qd_rew_go); %don't update nogo
            qd_pun_go=qd_pun_go+alpha_pun*(go_outcome(t) - qd_pun_go); % no pun as A is always rewarding
            choice_trace_go(4)=choice_trace_go(4)+decay.*(1-choice_trace_go(4));
            choice_trace_nogo(4)=choice_trace_nogo(4)+decay.*(0-choice_trace_nogo(4));
           else 
            qd_rew_nogo=qd_rew_nogo+alpha_rew*(0 - qd_rew_nogo);
            qd_pun_nogo=qd_pun_nogo+alpha_pun*(nogo_outcome(t) - qd_pun_nogo); 
            choice_trace_go(4)=choice_trace_go(4)+decay.*(0-choice_trace_go(4));
            choice_trace_nogo(4)=choice_trace_nogo(4)+decay.*(1-choice_trace_nogo(4));
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
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha_rew,prior{1,1}(1),prior{1,1}(2),tol)+...
        beta_lpmf(alpha_pun,prior{2,1}(1),prior{2,1}(2),tol)+...
        gamma_lpmf(beta,prior{3,1}(1),prior{3,1}(2),tol)+...
        beta_lpmf(decay,prior{4,1}(1),prior{4,1}(2),tol)+...
        norm_lpmf(perseverance,prior{5,1}(1),prior{5,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha_rew,prior{1,2}(1),prior{1,2}(2),tol)+...
        beta_lpmf(alpha_pun,prior{2,2}(1),prior{2,2}(2),tol)+...
        gamma_lpmf(beta,prior{3,2}(1),prior{3,2}(2),tol)+...
        beta_lpmf(decay,prior{4,1}(1),prior{4,1}(2),tol)+...
        norm_lpmf(perseverance,prior{5,2}(1),prior{5,2}(2),tol);
    end
    likelihood=-(sum(actual_prob_log)+log_prior);
end