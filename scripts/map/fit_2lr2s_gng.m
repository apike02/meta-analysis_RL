function likelihood = fit_2lr2s_gng(params,fullid,simdata,tol,prior)

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
rewsens=params(3);
punsens=params(4);


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
        qa_go = (qa_rew_go + qa_pun_go);
        qa_nogo = (qa_rew_nogo + qa_pun_nogo);
        Q_go=qa_go-max([qa_go qa_nogo]);
        Q_nogo=qa_nogo-max([qa_go qa_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qa_rew_go=qa_rew_go+alpha_rew*(rewsens*go_outcome(t) - qa_rew_go); %don't update nogo
            qa_pun_go=qa_pun_go+alpha_pun*(punsens*0 - qa_pun_go); % no pun as A is always rewarding
           else 
            qa_rew_nogo=qa_rew_nogo+alpha_rew*(rewsens*nogo_outcome(t) - qa_rew_nogo);
            qa_pun_nogo=qa_pun_nogo+alpha_pun*(punsens*0 - qa_pun_nogo); 
           end
    elseif stim(t)==2
        qb_go = (qb_rew_go + qb_pun_go);
        qb_nogo = (qb_rew_nogo + qb_pun_nogo);
        Q_go=qb_go-max([qb_go qb_nogo]);
        Q_nogo=qb_nogo-max([qb_go qb_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qb_rew_go=qb_rew_go+alpha_rew*(rewsens*0 - qb_rew_go); %don't update nogo %B always pun
            qb_pun_go=qb_pun_go+alpha_pun*(punsens*go_outcome(t) - qb_pun_go); 
           else 
            qb_rew_nogo=qb_rew_nogo+alpha_rew*(rewsens*0 - qb_rew_nogo);
            qb_pun_nogo=qb_pun_nogo+alpha_pun*(punsens*nogo_outcome(t) - qb_pun_nogo); 
           end
     elseif stim(t)==3
        qc_go = (qc_rew_go + qc_pun_go);
        qc_nogo = (qc_rew_nogo + qc_pun_nogo);
        Q_go=qc_go-max([qc_go qc_nogo]);
        Q_nogo=qc_nogo-max([qc_go qc_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qc_rew_go=qc_rew_go+alpha_rew*(rewsens*go_outcome(t) - qc_rew_go); %don't update nogo
            qc_pun_go=qc_pun_go+alpha_pun*(punsens*0 - qc_pun_go); % no pun as A is always rewarding
           else 
            qc_rew_nogo=qc_rew_nogo+alpha_rew*(rewsens*nogo_outcome(t) - qc_rew_nogo);
            qc_pun_nogo=qc_pun_nogo+alpha_pun*(punsens*0 - qc_pun_nogo); 
           end
     elseif stim(t)==4
        qd_go = (qd_rew_go + qd_pun_go);
        qd_nogo = (qd_rew_nogo + qd_pun_nogo);
        Q_go=qd_go-max([qd_go qd_nogo]);
        Q_nogo=qd_nogo-max([qd_go qd_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qd_rew_go=qd_rew_go+alpha_rew*(rewsens*0 - qd_rew_go); %don't update nogo
            qd_pun_go=qd_pun_go+alpha_pun*(punsens*go_outcome(t) - qd_pun_go); % no pun as A is always rewarding
           else 
            qd_rew_nogo=qd_rew_nogo+alpha_rew*(rewsens*0 - qd_rew_nogo);
            qd_pun_nogo=qd_pun_nogo+alpha_pun*(punsens*nogo_outcome(t) - qd_pun_nogo); 
           end
    end 

    if choices(t)==1
        actual_prob(t)=prob(1);
    else
        actual_prob(t)=prob(2);
    end

end



actual_prob = actual_prob + eps;
actual_prob_log = log(actual_prob);
if nargin<4
    likelihood=-(sum(actual_prob_log));
else 
    if group==1
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha_rew,prior{1,1}(1),prior{1,1}(2),tol)+...
        beta_lpmf(alpha_pun,prior{2,1}(1),prior{2,1}(2),tol)+...
        gamma_lpmf(rewsens,prior{3,1}(1),prior{3,1}(2),tol)+...
        gamma_lpmf(punsens,prior{4,1}(1),prior{4,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha_rew,prior{1,2}(1),prior{1,2}(2),tol)+...
        beta_lpmf(alpha_pun,prior{2,2}(1),prior{2,2}(2),tol)+...
        gamma_lpmf(rewsens,prior{3,2}(1),prior{3,2}(2),tol)+...
        gamma_lpmf(punsens,prior{4,2}(1),prior{4,2}(2),tol);
    end
    likelihood=-(sum(actual_prob_log)+log_prior);
end