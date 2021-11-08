function likelihood = fit_1lr1t1d1p_gng(params,fullid,simdata,tol,prior)

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
beta=params(2);
decay=params(3);
perseverance=params(4);
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
choice_trace_go=zeros(4,1);
choice_trace_nogo=zeros(4,1);

for t=1:trials
    if stim(t)==1
        Q_go=qa_go-max([qa_go qa_nogo]);
        Q_nogo=qa_nogo-max([qa_go qa_nogo]);
        prob(1)=exp(Q_go.*beta-perseverance*choice_trace_go(1))./(exp(Q_go.*beta-perseverance*choice_trace_go(1))+exp(Q_nogo.*beta-perseverance*choice_trace_nogo(1)));
        prob(2)=exp(Q_nogo.*beta-perseverance*choice_trace_nogo(1))./(exp(Q_nogo.*beta-perseverance*choice_trace_nogo(1))+exp(Q_go.*beta-perseverance*choice_trace_nogo(1)));
           if choices(t)==1
            qa_go=qa_go+alpha*(go_outcome(t) - qa_go); %don't update nogo
            choice_trace_go(1)=choice_trace_go(1)+decay.*(1-choice_trace_go(1));
            choice_trace_nogo(1)=choice_trace_nogo(1)+decay.*(0-choice_trace_nogo(1));
           else 
            qa_nogo=qa_nogo+alpha*(nogo_outcome(t) - qa_nogo);
            choice_trace_go(1)=choice_trace_go(1)+decay.*(0-choice_trace_go(1));
            choice_trace_nogo(1)=choice_trace_nogo(1)+decay.*(1-choice_trace_nogo(1));
           end
    elseif stim(t)==2
        Q_go=qb_go-max([qb_go qb_nogo]);
        Q_nogo=qb_nogo-max([qb_go qb_nogo]);
        prob(1)=exp(Q_go.*beta-perseverance*choice_trace_go(2))./(exp(Q_go.*beta-perseverance*choice_trace_go(2))+exp(Q_nogo.*beta-perseverance*choice_trace_nogo(2)));
        prob(2)=exp(Q_nogo.*beta-perseverance*choice_trace_nogo(2))./(exp(Q_nogo.*beta-perseverance*choice_trace_nogo(2))+exp(Q_go.*beta-perseverance*choice_trace_go(2)));
           if choices(t)==1
            qb_go=qb_go+alpha*(go_outcome(t) - qb_go); %don't update nogo
            choice_trace_go(2)=choice_trace_go(2)+decay.*(1-choice_trace_go(2));
            choice_trace_nogo(2)=choice_trace_nogo(2)+decay.*(0-choice_trace_nogo(2));
           else 
            qb_nogo=qb_nogo+alpha*(nogo_outcome(t) - qb_nogo);
            choice_trace_go(2)=choice_trace_go(2)+decay.*(0-choice_trace_go(2));
            choice_trace_nogo(2)=choice_trace_nogo(2)+decay.*(1-choice_trace_nogo(2));
           end
     elseif stim(t)==3
        Q_go=qc_go-max([qc_go qc_nogo]);
        Q_nogo=qc_nogo-max([qc_go qc_nogo]);
        prob(1)=exp(Q_go.*beta-perseverance*choice_trace_go(3))./(exp(Q_go.*beta-perseverance*choice_trace_go(3))+exp(Q_nogo.*beta-perseverance*choice_trace_nogo(3)));
        prob(2)=exp(Q_nogo.*beta-perseverance*choice_trace_nogo(3))./(exp(Q_nogo.*beta-perseverance*choice_trace_nogo(3))+exp(Q_go.*beta-perseverance*choice_trace_go(3)));
           if choices(t)==1
            qc_go=qc_go+alpha*(go_outcome(t) - qc_go); %don't update nogo
            choice_trace_go(3)=choice_trace_go(3)+decay.*(1-choice_trace_go(3));
            choice_trace_nogo(3)=choice_trace_nogo(3)+decay.*(0-choice_trace_nogo(3));
           else 
            qc_nogo=qc_nogo+alpha*(nogo_outcome(t) - qc_nogo);
            choice_trace_go(3)=choice_trace_go(3)+decay.*(0-choice_trace_go(3));
            choice_trace_nogo(3)=choice_trace_nogo(3)+decay.*(1-choice_trace_nogo(3));
           end
     elseif stim(t)==4
        Q_go=qd_go-max([qd_go qd_nogo]);
        Q_nogo=qd_nogo-max([qd_go qd_nogo]);
        prob(1)=exp(Q_go.*beta-perseverance*choice_trace_go(4))./(exp(Q_go.*beta-perseverance*choice_trace_go(4))+exp(Q_nogo.*beta-perseverance*choice_trace_nogo(4)));
        prob(2)=exp(Q_nogo.*beta-perseverance*choice_trace_nogo(4))./(exp(Q_nogo.*beta-perseverance*choice_trace_nogo(4))+exp(Q_go.*beta-perseverance*choice_trace_go(4)));
           if choices(t)==1
            qd_go=qd_go+alpha*(go_outcome(t) - qd_go); %don't update nogo
            choice_trace_go(4)=choice_trace_go(4)+decay.*(1-choice_trace_go(4));
            choice_trace_nogo(4)=choice_trace_nogo(4)+decay.*(0-choice_trace_nogo(4));
           else 
            qd_nogo=qd_nogo+alpha*(nogo_outcome(t) - qd_nogo);
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