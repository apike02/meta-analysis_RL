function outputdata=untransform(task,model_list,genrec,singleprior)
%% load in necessary inputs
if singleprior==1
    load(strcat('N:/Alex/metaRL/map/singleprior/results_mle_',task,'.mat'));
    load(strcat('N:/Alex/metaRL/map/singleprior/results_map_',task,'.mat'));
elseif genrec==0
    load(strcat('N:/Alex/metaRL/map/results_mle_',task,'.mat'));
    load(strcat('N:/Alex/metaRL/map/results_map_',task,'.mat'));
else
    load(strcat('N:/Alex/metaRL/map/generate_recover/',model_list{1},'/results_mle_',task,'.mat'));
    load(strcat('N:/Alex/metaRL/map/generate_recover/',model_list{1},'/results_map_',task,'.mat'));
end

for model=1:length(model_list)
    model_name=string(model_list(model));
    disp(model_name);
    map_name=strcat('results_map.map_',model_name);
    mle_name=strcat('results_mle.mle_',model_name);
    map_results=eval(map_name);
    mle_results=eval(mle_name);

    n_lr=find_prev_number(model_name,'lr');
    n_b=find_prev_number(model_name,'t');
    n_s=find_prev_number(model_name,'s');
    n_lapse=find_prev_number(model_name,'lapse');
    n_bias=find_prev_number(model_name,'bias');
    
    map_results(:,1:n_lr)=inverse_logit(map_results(:,1:n_lr));
    map_results(:,n_lr+1:n_lr+n_b)=exp(map_results(:,n_lr+1:n_lr+n_b));
    results_map.(strcat('map_',model_name))=map_results;
    
    mle_results(:,1:n_lr)=inverse_logit(mle_results(:,1:n_lr));
    mle_results(:,n_lr+1:n_lr+n_b)=exp(mle_results(:,n_lr+1:n_lr+n_b));
    results_mle.(strcat('mle_',model_name))=mle_results;

end
if singleprior==1
    save(['N:/Alex/metaRL/map/singleprior/results_mle_',task,'.mat'], 'results_mle')
    save(['N:/Alex/metaRL/map/singleprior/results_map_',task,'.mat'], 'results_map')
elseif genrec==0   
    save(['N:/Alex/metaRL/map/results_mle_',task,'.mat'], 'results_mle')
    save(['N:/Alex/metaRL/map/results_map_',task,'.mat'], 'results_map')
else
    save(['N:/Alex/metaRL/map/generate_recover/',model_list{1},'/results_mle_',task,'.mat'], 'results_mle')
    save(['N:/Alex/metaRL/map/generate_recover/',model_list{1},'/results_map_',task,'.mat'], 'results_map')
end
    
end


