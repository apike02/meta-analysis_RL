tasklist={'t4'};
model_list={'1lr2t'};
singleprior=0;
genrec=1;
transformed=0;

for job = 1:length(tasklist)
    task=tasklist{job};
    model=model_list(job);
    disp('running MLE')
    run_mle(task,model,genrec,singleprior,transformed) %just does one model per task
    poolobj = gcp('nocreate');
    delete(poolobj);
    disp('running prior estimation')
    run_prior_estimation(task,model,genrec,singleprior,transformed);
    disp('running MAP')
    run_map(task,model,genrec,singleprior,transformed)
    disp('getting log likelihood')
    get_ll(task,model,genrec,singleprior,transformed);
    if transformed==1
        untransform(task,model,genrec,singleprior);
    end
end