tasklist={'t1','t2','t3','t4','t5'};
models={'1lr1t'};
singleprior=0;
genrec=1;
transformed=0;

%test saving


for job = 1:length(tasklist)
    task=tasklist{job};
    if task=='t5'
        model_list=models;
    else 
        model_list=models;
    end
    disp('running MLE')
    parpool;
    run_mle(task,model_list,genrec,singleprior,transformed); %first 0 means generate and recover is off
    %second indicator is 1 if running for a single prior
    poolobj = gcp('nocreate');
    delete(poolobj);
    disp('running prior estimation')
    run_prior_estimation(task,model_list,genrec,singleprior,transformed); 
    disp('running MAP')
    parpool;
    run_map(task,model_list,genrec,singleprior,transformed);
    disp('getting log likelihood')
    get_ll(task,model_list,genrec,singleprior,transformed);
    poolobj = gcp('nocreate');
    delete(poolobj);
end