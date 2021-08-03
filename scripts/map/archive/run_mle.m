function results_mle=run_mle

%% load in necessary inputs
tic;
tstart=tic;
simdata=csvread('simulated_data.csv',1,1); 

identifiers=unique(simdata(:,8));

model_list={'1lr1b','2lr1b','1lr2b','2lr2b','1lr1s','2lr1s','1lr2s',...
    '2lr2s','1lr1s1lapse','2lr1s1lapse','1lr2s1lapse','2lr2s1lapse'};

func_list=strcat('fit_',model_list);
func_list=cellfun(@str2func,func_list,'UniformOutput',false);

%% default values for bounds and starting values

lr_start=rand(1); %random number between 0 and 1
b_start=rand(1)*100; %random number between 0 and 1
s_start=rand(1)*100; %random number between 0 and 100
lapse_start=rand(1); %random number between 0 and 100

lr_ub=1;
b_ub=Inf;
s_ub=Inf;
lapse_ub=1;

lr_lb=0;
b_lb=0;
s_lb=0;
lapse_lb=0;

results_mle=struct;
outputs_mle=struct;

%% run loop
for model=1:1
    model_name=string(model_list(model));
    disp(model_name);
    n_lr=find_prev_number(model_name,'lr');
    n_b=find_prev_number(model_name,'b');
    n_s=find_prev_number(model_name,'s');
    n_lapse=find_prev_number(model_name,'lapse');
    
    x0=[repmat(lr_start,1,n_lr) repmat(b_start,1,n_b)...
        repmat(s_start,1,n_s) repmat(lapse_start,1,n_lapse)];
    ub=[repmat(lr_ub,1,n_lr) repmat(b_ub,1,n_b)...
        repmat(s_ub,1,n_s) repmat(lapse_ub,1,n_lapse)]; %#ok<*REPMAT>
    lb=[repmat(lr_lb,1,n_lr) repmat(b_lb,1,n_b)...
        repmat(s_lb,1,n_s) repmat(lapse_lb,1,n_lapse)];
    
    temp_results=zeros(length(identifiers),sum([n_lr,n_b,n_s,n_lapse])+2);
    temp_func=func_list{model};
    outputs=cell(length(identifiers),1);
    
     for i=1:1
     disp(identifiers(i))
     opts = optimoptions(@fmincon,'Display','off');
     [x,likelihood,exitflag,output]=fmincon(@(params)temp_func(params,identifiers(i),simdata),x0,[],[],[],[],lb,ub,[],opts); %had options originally?

     
%      rng default % For reproducibility
%      opts = optimoptions(@fmincon,'Display','off','UseParallel',true);
%      problem = createOptimProblem('fmincon',...
%          'objective',@(params)temp_func(params,identifiers(i)),...
%          'x0',x0,...
%          'lb',lb,...
%          'ub',ub,...
%          'options',opts);
%      ms = MultiStart('UseParallel',1);
%      [x,likelihood,exitflag,output] = run(ms,problem,10);
%      
     temp_results(i,:)=[x likelihood exitflag];
     outputs{i}=output;

     end

    results_mle.(strcat('mle_',model_name))=[temp_results identifiers];
    outputs_mle.(strcat('mle_',model_name))= outputs;

    
end

save('results_mle.mat', 'results_mle')
save('mle_outputs.mat', 'outputs_mle')
toc(tstart);
end

