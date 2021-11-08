function priors=run_prior_estimation(task,model_list,genrec,singleprior,transformed)
%%
if nargin<1
    task=input('Which task do you wish to run?','s');
end
%%
workingdir='~Scratch/metaRL/';

if singleprior==1
    load(strcat(workingdir,'/map/singleprior/results_mle_',task,'.mat'));
    simdata=csvread(strcat('simulated_data_',string(task),'.csv'),1,1);
    
elseif genrec==0
    load(strcat(workingdir,'/map/results_mle_',task,'.mat'));
    simdata=csvread(strcat('simulated_data_',string(task),'.csv'),1,1);
    
else 
    load(strcat(workingdir,'/map/generate_recover/',model_list{1},'/results_mle_',task,'.mat'));
    simdata=csvread(strcat(workingdir,'/map/generate_recover/',string(model_list{1}),'/data_',string(task),'.csv'),1,1); 
end

idx=find(simdata(:,4)==1); %this indexes the row value for each first trial
groups=simdata(idx,2);

%%
for model=1:length(model_list)
    model_name=string(model_list(model));
    n_lr=find_prev_number(model_name,'lr');
    n_b=find_prev_number(model_name,'t');
    n_s=find_prev_number(model_name,'s');
    n_lapse=find_prev_number(model_name,'lapse');
    n_bias=find_prev_number(model_name,'bias');
    n_decay=find_prev_number(model_name,'d');
    n_perseverance=find_prev_number(model_name,'p')-n_lapse;
    
    if transformed==1
        dist=[repmat('n',1,n_lr),repmat('n',1,n_b),repmat('n',1,n_s),...
        repmat('n',1,n_lapse),repmat('n',1,n_bias)];
    else 
    dist=[repmat('b',1,n_lr),repmat('g',1,n_b),repmat('g',1,n_s),...
        repmat('b',1,n_lapse),repmat('n',1,n_bias),repmat('b',1,n_decay),...
        repmat('n',1,n_perseverance)];
    end
    
    temp=results_mle.(strcat('mle_',model_name));
    temp=[temp groups];
    temp_pat=temp(temp(:,end)==1,:);
    temp_con=temp(temp(:,end)==0,:);
    temp_priors=get_prior_params(temp_pat,temp_con,dist,singleprior);
    %plot_mle(temp_pat,temp_con,model_name,temp_priors,dist);
    priors.(strcat('fit_',model_name))=temp_priors;
end
%%
if singleprior==1
    save(strcat(workingdir,'/map/singleprior/priors_',string(task),'.mat'), 'priors')
elseif genrec==0
    save(strcat(workingdir,'/map/priors_',string(task),'.mat'), 'priors')
else
    save(strcat(workingdir,'/map/generate_recover/',model_list{1},'/priors_',string(task),'.mat'), 'priors')
end
end