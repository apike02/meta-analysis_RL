tasklist={'t1'};
models={'1lr1t','2lr1t','1lr2t','2lr2t','1lr1s','2lr1s','1lr2s',...
    '2lr2s','1lr1s1lapse','2lr1s1lapse','1lr2s1lapse','2lr2s1lapse',...
    '1lr1s1lapse1bias','1lr1s1lapse2bias','1lr1s1lapse3bias'...
    '2lr2s1lapse1bias','2lr2s1lapse2bias','2lr2s1lapse3bias'};
singleprior=0;
genrec=0;
transformed=0;

%test saving


for job = 1:length(tasklist)
    task=tasklist{job};
    if task=='t4'
        model_list=models;
    else 
        model_list=models(1:12);
    end
    disp('running MLE')
    run_mle(task,model_list,genrec,singleprior,transformed); %first 0 means generate and recover is off
    % second indicator is 1 if running for a single prior
    poolobj = gcp('nocreate');
    delete(poolobj);
    disp('running prior estimation')
    run_prior_estimation(task,model_list,genrec,singleprior,transformed); 
    disp('running MAP')
    run_map(task,model_list,genrec,singleprior,transformed);
    disp('getting log likelihood')
    get_ll(task,model_list,genrec,singleprior,transformed);
end