function results_mle=run_mle(task,model_list,genrec,singleprior,transformed)

if nargin<1
    task=input('Which task do you wish to run?','s');
end

nstartpoints=10;

%% load in necessary inputs

workingdir='~Scratch/metaRL/';

if genrec==0
    simdata=csvread(strcat('simulated_data_',string(task),'.csv'),1,1); 
else 
    simdata=csvread(strcat(workingdir,'/map/generate_recover/',string(model_list{1}),'/data_',string(task),'.csv'),1,1); 
end

%tests save is working
save([workingdir,'/map/test_',task,'.mat'], 'workingdir')

%set tolerance of how close you define before convergence
tolerance=1e-6;


%%
if task=='t5'
    func_list=strcat('fit_',model_list,'_gng');
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,9));
elseif transformed==1
    func_list=strcat('fit_transformed_',model_list);
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,8));
else
    func_list=strcat('fit_',model_list);
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,8));
end 
%% default values for bounds and starting values

lr_start=rand(1); %random number between 0 and 1
b_start=rand(1); %random number between 0 and 1
s_start=rand(1); %random number between 0 and 1
lapse_start=rand(1); %random number between 0 and 1
bias_start=rand(1); %random number between 0 and 1
decay_start=rand(1);
perseverance_start=rand(1);

if transformed==1
    lr_ub=Inf;
    lr_lb=-Inf;
    b_lb=-Inf;
else
    lr_ub=1;
    lr_lb=0;
    b_lb=0;
end

b_ub=Inf;
s_ub=Inf;
lapse_ub=1;
bias_ub=Inf;
decay_ub=1;
perseverance_ub=Inf;

s_lb=0;
lapse_lb=0;
bias_lb=-Inf;
decay_lb=0;
perseverance_lb=-Inf;

results_mle=struct;
outputs_mle=struct;

%% run loop
for model=1:length(model_list)
    model_name=string(model_list(model));
    disp(model_name);
    n_lr=find_prev_number(model_name,'lr');
    n_b=find_prev_number(model_name,'t');
    n_s=find_prev_number(model_name,'s');
    n_lapse=find_prev_number(model_name,'lapse');
    n_bias=find_prev_number(model_name,'bias');
    n_
    
    x0=[repmat(lr_start,1,n_lr) repmat(b_start,1,n_b)...
        repmat(s_start,1,n_s) repmat(lapse_start,1,n_lapse)...
        repmat(bias_start,1,n_bias)];
    ub=[repmat(lr_ub,1,n_lr) repmat(b_ub,1,n_b)...
        repmat(s_ub,1,n_s) repmat(lapse_ub,1,n_lapse)...
        repmat(bias_ub,1,n_bias)]; 
    lb=[repmat(lr_lb,1,n_lr) repmat(b_lb,1,n_b)...
        repmat(s_lb,1,n_s) repmat(lapse_lb,1,n_lapse)...
        repmat(bias_lb,1,n_bias)];n_decay=find_prev_number(model_name,'d');
    n_perseverance=find_prev_number(model_name,'p')-n_lapse;
    
    x0=[repmat(lr_start,1,n_lr) repmat(b_start,1,n_b)...
        repmat(s_start,1,n_s) repmat(lapse_start,1,n_lapse)...
        repmat(bias_start,1,n_bias) repmat(decay_start,1,n_decay)...
        repmat(perseverance_start,1,n_perseverance)];
    ub=[repmat(lr_ub,1,n_lr) repmat(b_ub,1,n_b)...
        repmat(s_ub,1,n_s) repmat(lapse_ub,1,n_lapse)...
        repmat(bias_ub,1,n_bias) repmat(decay_ub,1,n_decay)...
        repmat(perseverance_ub,1,n_perseverance)]; 
    lb=[repmat(lr_lb,1,n_lr) repmat(b_lb,1,n_b)...
        repmat(s_lb,1,n_s) repmat(lapse_lb,1,n_lapse)...
        repmat(bias_lb,1,n_bias) repmat(decay_lb,1,n_decay)...
        repmat(perseverance_lb,1,n_perseverance)];
        
    temp_results=zeros(length(identifiers),sum([n_lr,n_b,n_s,n_lapse,n_bias,n_decay,n_perseverance])+2);
    temp_func=func_list{model};
    outputs=cell(length(identifiers),1);
    
     for i=1:length(identifiers)
         if (rem(i,50)==0)
             disp(identifiers(i)) %shows progress every 50 participants
         end
     %disp(identifiers(i))
     %[x,likelihood,exitflag,output]=fmincon(@(params)temp_func(params,identifiers(i),simdata),x0,[],[],[],[],lb,ub,[]); %had options originally?

     
     rng default % For reproducibility
     opts = optimoptions(@fmincon,'Display','off','UseParallel',true);
     problem = createOptimProblem('fmincon',...
         'objective',@(params)temp_func(params,identifiers(i),simdata),...
         'x0',x0,...
         'lb',lb,...
         'ub',ub,...
         'options',opts);
     ms = MultiStart('UseParallel',1,...
         'Display','off',...
         'StartPointsToRun','bounds',...
         'XTolerance',tolerance);
         
         %nb out1 is the best local minima -- it is the one that maximizes f(x) out of the set of local minima that the program found
         %out2 is the value of the objective function at out: out2 = f(out1)
         %out3 is an exit flag that tells us why our program converged/failed to converge
         %out4 lists information about number of iterations ect.
            
     [x,likelihood,exitflag,output] = run(ms,problem,nstartpoints);
     
     temp_results(i,:)=[x likelihood exitflag];
     outputs{i}=output;

     end
    
    

    results_mle.(strcat('mle_',model_name))=[temp_results identifiers];
    outputs_mle.(strcat('mle_',model_name))=outputs;
    
end

if singleprior==1
    save([workingdir,'/map/singleprior/results_mle_',task,'.mat'], 'results_mle')
    save([workingdir,'/map/singleprior/outputs_mle_',task,'.mat'], 'outputs_mle')
    %save([workingdir,'/mle/singleprior/results_mle_',task,'.mat'], 'results_mle')
    %save([workingdir,'/mle/singleprior/outputs_mle_',task,'.mat'], 'outputs_mle')
elseif genrec == 0
    save([workingdir,'/map/results_mle_',task,'.mat'], 'results_mle')
    save([workingdir,'/map/outputs_mle_',task,'.mat'], 'outputs_mle')
    %save([workingdir,'/mle/results_mle_',task,'.mat'], 'results_mle')
    %save([workingdir,'/mle/outputs_mle_',task,'.mat'], 'outputs_mle')
else 
    save([workingdir,'/map/generate_recover/',model_list{1},'/results_mle_',task,'.mat'], 'results_mle')
    save([workingdir,'/map/generate_recover/',model_list{1},'/outputs_mle_',task,'.mat'], 'outputs_mle')
    %save([workingdir,'/mle/generate_recover/',model_list{1},'/results_mle_',task,'.mat'], 'results_mle')
    %save([workingdir,'/mle/generate_recover/',model_list{1},'/outputs_mle_',task,'.mat'], 'outputs_mle')
end

