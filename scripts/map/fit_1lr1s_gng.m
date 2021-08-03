function likelihood = fit_1lr1s_gng(params,fullid,simdata,tol,prior)

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
sensitivity=params(2);


qa_go=0;
qa_nogo=0;
qb_go=0;
qb_nogo=0;
qc_go=0;
qc_nogo=0;
qd_go=0;
qd_nogo=0;

prob=zeros(2,1); %trials by number of options
actual_prob=zeros(trials,1);

for t=1:trials
    if stim(t)==1
        Q_go=qa_go-max([qa_go qa_nogo]);
        Q_nogo=qa_nogo-max([qa_go qa_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qa_go=qa_go+alpha*(sensitivity*go_outcome(t) - qa_go); %don't update nogo
           else 
            qa_nogo=qa_nogo+alpha*(sensitivity*nogo_outcome(t) - qa_nogo);
           end
    elseif stim(t)==2
        Q_go=qb_go-max([qb_go qb_nogo]);
        Q_nogo=qb_nogo-max([qb_go qb_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qb_go=qb_go+alpha*(sensitivity*go_outcome(t) - qb_go); 
           else 
            qb_nogo=qb_nogo+alpha*(sensitivity*nogo_outcome(t) - qb_nogo);
           end
     elseif stim(t)==3
        Q_go=qc_go-max([qc_go qc_nogo]);
        Q_nogo=qc_nogo-max([qc_go qc_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qc_go=qc_go+alpha*(sensitivity*go_outcome(t) - qc_go); 
           else 
            qc_nogo=qc_nogo+alpha*(sensitivity*nogo_outcome(t) - qc_nogo);
           end
     elseif stim(t)==4
        Q_go=qd_go-max([qd_go qd_nogo]);
        Q_nogo=qd_nogo-max([qd_go qd_nogo]);
        prob(1)=exp(Q_go)./(exp(Q_go)+exp(Q_nogo));
        prob(2)=exp(Q_nogo)./(exp(Q_nogo)+exp(Q_go));
           if choices(t)==1
            qd_go=qd_go+alpha*(sensitivity*go_outcome(t) - qd_go); 
           else 
            qd_nogo=qd_nogo+alpha*(sensitivity*nogo_outcome(t) - qd_nogo);
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
        log_prior=beta_lpmf(alpha,prior{1,1}(1),prior{1,1}(2),tol)+...
        gamma_lpmf(sensitivity,prior{2,1}(1),prior{2,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1/2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,2}(1),prior{1,2}(2),tol)+... 
        gamma_lpmf(sensitivity,prior{2,2}(1),prior{2,2}(2),tol);
    end
        likelihood=-(sum(actual_prob_log)+log_prior);
end