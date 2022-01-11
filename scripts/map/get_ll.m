function likelihood=get_ll(task,model_list,genrec,singleprior,transformed)

workingdir='~/Scratch/metaRL/trials_2000/map/';
model_name=model_list;

if nargin<1
    task=input('Which task do you wish to run?','s');
end

%% load in necessary inputs
if singleprior==1
    load(strcat(workingdir,'/singleprior/results_map_',task,model_name,'.mat'));
    simdata=csvread(strcat(workingdir,'../simulated_data_',task,'.csv'),1,1); 
elseif genrec==0
    load(strcat(workingdir,'/results_map_',task,model_name,'.mat'));
    simdata=csvread(strcat(workingdir,'../simulated_data_',task,'.csv'),1,1); 
else 
    load(strcat(workingdir,'/generate_recover/',model_list{1},'/results_map_',task,'.mat'));
    simdata=csvread(strcat(workingdir,'../generate_recover/',string(model_list{1}),'/data_',task,'.csv'),1,1); 
end

%set tolerance of how close you define before convergence
tolerance=1e-6;

%%
if task=='t5'
    func_list=strcat('fit_',model_list,'_gng');
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,8));
elseif transformed==1
    func_list=strcat('fit_transformed_',model_list);
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,7));
else 
    func_list=strcat('fit_',model_list);
    func_list=cellfun(@str2func,func_list,'UniformOutput',false);
    identifiers=unique(simdata(:,7));
end 
%% run these params

for model=1:length(model_list)
    model_name=string(model_list(model));
    disp(model_name);
    map_name=strcat('results_map.map_',model_name);
    
   
    temp_results=zeros(length(identifiers),1);
    temp_func=func_list{model};
    results=eval(map_name);
    
        for i=1:length(identifiers)
            disp(identifiers(i))
            params=results(i,1:size(results,2)-3);
            temp_results(i)=temp_func(params,identifiers(i),simdata);
        end
        
    results_map.(strcat('map_',model_name))=[results temp_results]; %stores with existing MAP analysis
  
end
if singleprior==1
    save([workingdir,'/singleprior/results_map_',task,model_name,'.mat'], 'results_map')
elseif genrec==0
    save([workingdir,'/results_map_',task,model_name,'.mat'], 'results_map')
else 
    save([workingdir,'generate_recover/',model_list{1},'/results_map_',task,'.mat'], 'results_map')
end
end

