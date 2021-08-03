function results_map=run_map(task,model_list,genrec,singleprior,transformed)

if nargin<1
    task=input('Which task do you wish to run?','s');
end

nstartpoints=10;

%% load in necessary inputs

workingdir='~Scratch';

if singleprior==1
    load(strcat(workingdir,'/map/singleprior/priors_',string(task),'.mat'));
    simdata=csvread(strcat('/simulated_data_',task,'.csv'),1,1); 
elseif genrec==0
    load(strcat(workingdir,'/map/priors_',string(task),'.mat'));
    simdata=csvread(strcat('/simulated_data_',task,'.csv'),1,1); 
else 
    load(strcat(workingdir,'/map/generate_recover/',string(model_list{1}),'/priors_',string(task),'.mat'));
    simdata=csvread(strcat(workingdir,'/map/generate_recover/',string(model_list{1}),'/data_',task,'.csv'),1,1); 
end


%set tolerance of how close you define before convergence
tolerance=1e-6;

%%
if task=='t4'
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

s_lb=0;
lapse_lb=0;
bias_lb=-Inf;

results_map=struct;

%% run loop
for model=1:length(model_list)
    model_name=string(model_list(model));
    disp(model_name);
    n_lr=find_prev_number(model_name,'lr');
    n_b=find_prev_number(model_name,'t');
    n_s=find_prev_number(model_name,'s');
    n_lapse=find_prev_number(model_name,'lapse');
    n_bias=find_prev_number(model_name,'bias');
    
    x0=[repmat(lr_start,1,n_lr) repmat(b_start,1,n_b)...
        repmat(s_start,1,n_s) repmat(lapse_start,1,n_lapse)...
        repmat(bias_start,1,n_bias)];
    ub=[repmat(lr_ub,1,n_lr) repmat(b_ub,1,n_b)...
        repmat(s_ub,1,n_s) repmat(lapse_ub,1,n_lapse)...
        repmat(bias_ub,1,n_bias)]; 
    lb=[repmat(lr_lb,1,n_lr) repmat(b_lb,1,n_b)...
        repmat(s_lb,1,n_s) repmat(lapse_lb,1,n_lapse)...
        repmat(bias_lb,1,n_bias)];
    
    temp_results=zeros(length(identifiers),sum([n_lr,n_b,n_s,n_lapse,n_bias])+2);
    temp_priors=priors.(strcat('fit_',model_name));
    %temp_priors_mat=cell2mat(temp_priors'); %transpose so column = param
    temp_func=func_list{model};
    
     for i=1:length(identifiers)
         if (rem(i,50)==0)
             disp(identifiers(i)) %shows progress every 50 participants
         end
     %disp(identifiers(i))
     %[x,likelihood,exitflag,output]=fmincon(@(params)temp_func(params,identifiers(i),simdata,tol,temp_priors_mat),x0,[],[],[],[],lb,ub,[]); %had options originally?

     
     rng default % For reproducibility
     opts = optimoptions(@fmincon,'Display','off','UseParallel',true);
     problem = createOptimProblem('fmincon',...
         'objective',@(params)temp_func(params,identifiers(i),simdata,tolerance,temp_priors),...
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

    results_map.(strcat('map_',model_name))=[temp_results identifiers];
    outputs_map.(strcat('map_',model_name))=outputs;
    
end
if singleprior==1
    save([workingdir,'/map/singleprior/results_map_',task,'.mat'], 'results_map')
    save([workingdir,'/map/singleprior/outputs_map_',task,'.mat'], 'outputs_map')
elseif genrec==0
    save([workingdir,'/map/results_map_',task,'.mat'], 'results_map')
    save([workingdir,'/map/outputs_map_',task,'.mat'], 'outputs_map')
else 
    save([workingdir,'/map/generate_recover/',model_list{1},'/results_map_',task,'.mat'], 'results_map')
    save([workingdir,'/map/generate_recover/',model_list{1},'/outputs_map_',task,'.mat'], 'outputs_map')
end
end

