function likelihood = fit_1lr1s1lapse1bias_gng(params,fullid,simdata,tol,prior)

%1 action bias

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
lapse=params(3);
act_bias=params(4);

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
        go_weight = qa_go + act_bias;
        nogo_weight = qa_nogo;
        g_w=go_weight-max([go_weight nogo_weight]);
        ng_w=nogo_weight-max([go_weight nogo_weight]);
        prob(1)=(1-lapse).*exp(g_w)./(exp(g_w)+exp(ng_w))+lapse/2;
        prob(2)=(1-lapse).*exp(ng_w)./(exp(ng_w)+exp(g_w))+lapse/2;
           if choices(t)==1
            qa_go=qa_go+alpha*(sensitivity*go_outcome(t) - qa_go); %don't update nogo
           else 
            qa_nogo=qa_nogo+alpha*(sensitivity*nogo_outcome(t) - qa_nogo);
           end
    elseif stim(t)==2
        go_weight = qb_go + act_bias;
        nogo_weight = qb_nogo;
        g_w=go_weight-max([go_weight nogo_weight]);
        ng_w=nogo_weight-max([go_weight nogo_weight]);
        prob(1)=(1-lapse).*exp(g_w)./(exp(g_w)+exp(ng_w))+lapse/2;
        prob(2)=(1-lapse).*exp(ng_w)./(exp(ng_w)+exp(g_w))+lapse/2;
           if choices(t)==1
            qb_go=qb_go+alpha*(sensitivity*go_outcome(t) - qb_go); 
           else 
            qb_nogo=qb_nogo+alpha*(sensitivity*nogo_outcome(t) - qb_nogo);
           end
     elseif stim(t)==3
        go_weight = qc_go + act_bias;
        nogo_weight = qc_nogo;
        g_w=go_weight-max([go_weight nogo_weight]);
        ng_w=nogo_weight-max([go_weight nogo_weight]);
        prob(1)=(1-lapse).*exp(g_w)./(exp(g_w)+exp(ng_w))+lapse/2;
        prob(2)=(1-lapse).*exp(ng_w)./(exp(ng_w)+exp(g_w))+lapse/2;
           if choices(t)==1
            qc_go=qc_go+alpha*(sensitivity*go_outcome(t) - qc_go);
           else 
            qc_nogo=qc_nogo+alpha*(sensitivity*nogo_outcome(t) - qc_nogo);
           end
     elseif stim(t)==4
        go_weight = qd_go + act_bias;
        nogo_weight = qd_nogo;
        g_w=go_weight-max([go_weight nogo_weight]);
        ng_w=nogo_weight-max([go_weight nogo_weight]);
        prob(1)=(1-lapse).*exp(g_w)./(exp(g_w)+exp(ng_w))+lapse/2;
        prob(2)=(1-lapse).*exp(ng_w)./(exp(ng_w)+exp(g_w))+lapse/2;
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
        gamma_lpmf(sensitivity,prior{2,1}(1),prior{2,1}(2),tol)+...
        beta_lpmf(lapse,prior{3,1}(1),prior{3,1}(2),tol)+...
        norm_lpmf(act_bias,prior{4,1}(1),prior{4,1}(2),tol);
    else 
        %1 is param 1, 2 is group 2 (con), 1./2 is first or second prior param
        log_prior=beta_lpmf(alpha,prior{1,2}(1),prior{1,2}(2),tol)+...
        gamma_lpmf(sensitivity,prior{2,2}(1),prior{2,2}(2),tol)+...
        beta_lpmf(lapse,prior{3,2}(1),prior{3,2}(2),tol)+...
        norm_lpmf(act_bias,prior{4,2}(1),prior{4,2}(2),tol);
    end
    likelihood=-(sum(actual_prob_log)+log_prior);
end