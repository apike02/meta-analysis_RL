simdata=csvread('simulated_data.csv',1,1); 

identifiers=unique(simdata(:,8));

clear('simdata');
%% 1lr1b
x0=[0.4,0.5];
ub=[1,Inf];
lb=[0,0];

results_1lr1b=zeros(length(identifiers),4);

 parfor i=1:length(identifiers)
 disp(identifiers(i))
 [x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr1b(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
 results_1lr1b(i,:)=[x likelihood exitflag];
 
 end

results_1lr1b(:,5)=identifiers;

save('N:\Alex\metaRL\map\results_1lr1b.mat', 'results_1lr1b')


%% 2lr1b
x0=[0.4,0.4,0.5];
ub=[1 1 Inf];
lb=[0 0 0];

results_2lr1b=zeros(length(identifiers),5);

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr1b(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr1b(i,:)=[x likelihood exitflag];
end

results_2lr1b(:,6)=identifiers;

save('N:\Alex\metaRL\map\results_2lr1b.mat', 'results_2lr1b')

%% 1lr2b
x0=[0.4,0.5,0.5];
ub=[1 Inf Inf];
lb=[0 0 0];

results_1lr2b=zeros(length(identifiers),5);

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr2b(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_1lr2b(i,:)=[x likelihood exitflag];
end

results_1lr2b(:,6)=identifiers;

save('N:\Alex\metaRL\map\results_1lr2b.mat', 'results_1lr2b')

%% 2lr2b
x0=[0.4,0.4,0.5,0.5];
ub=[1 1 Inf Inf];
lb=[0 0 0 0];

results_2lr2b=zeros(length(identifiers),6); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr2b(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr2b(i,:)=[x likelihood exitflag];
end

results_2lr2b(:,7)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_2lr2b.mat', 'results_2lr2b')

%% 1lr1s
x0=[0.4,1];
ub=[1 Inf];
lb=[0 0];

results_1lr1s=zeros(length(identifiers),4); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr1s(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_1lr1s(i,:)=[x likelihood exitflag];
end

results_1lr1s(:,5)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_1lr1s.mat', 'results_1lr1s')

%% 1lr2s
x0=[0.4,1 1];
ub=[1 Inf Inf];
lb=[0 0 0];

results_1lr2s=zeros(length(identifiers),5); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr2s(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_1lr2s(i,:)=[x likelihood exitflag];
end

results_1lr2s(:,6)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_1lr2s.mat', 'results_1lr2s')

%% 2lr1s
x0=[0.4,0.4,1];
ub=[1 1 Inf];
lb=[0 0 0];

results_2lr1s=zeros(length(identifiers),5); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr1s(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr1s(i,:)=[x likelihood exitflag];
end

results_2lr1s(:,6)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_2lr1s.mat', 'results_2lr1s')

%% 2lr2s
x0=[0.4,0.4,1 1];
ub=[1 1 Inf Inf];
lb=[0 0 0 0];

results_2lr2s=zeros(length(identifiers),6); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr2s(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr2s(i,:)=[x likelihood exitflag];
end

results_2lr2s(:,6)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_2lr2s.mat', 'results_2lr2s')

%% 1lr1s1lapse
x0=[0.4,1,0.5];
ub=[1 Inf 1];
lb=[0 0 0];

results_1lr1s1lapse=zeros(length(identifiers),5); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr1s1lapse(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_1lr1s1lapse(i,:)=[x likelihood exitflag];
end

results_1lr1s1lapse(:,6)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_1lr1s1lapse.mat', 'results_1lr1s1lapse')

%% 2lr1s1lapse
x0=[0.4,0.4,1,0.5];
ub=[1 1 Inf 1];
lb=[0 0 0 0];

results_2lr1s1lapse=zeros(length(identifiers),6); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr1s1lapse(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr1s1lapse(i,:)=[x likelihood exitflag];
end

results_2lr1s1lapse(:,7)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_2lr1s1lapse.mat', 'results_2lr1s1lapse')

%% 1lr2s1lapse
x0=[0.4,1,1,0.5];
ub=[1 Inf Inf 1];
lb=[0 0 0 0];

results_1lr2s1lapse=zeros(length(identifiers),6); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_1lr2s1lapse(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_1lr2s1lapse(i,:)=[x likelihood exitflag];
end

results_1lr2s1lapse(:,7)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_1lr2s1lapse.mat', 'results_1lr2s1lapse')

%% 2lr2s1lapse
x0=[0.4,0.4,1,1,0.5];
ub=[1 1 Inf Inf 1];
lb=[0 0 0 0 0];

results_2lr2s1lapse=zeros(length(identifiers),7); %nparam+2 for likelihood and flag

parfor i=1:length(identifiers)
disp(identifiers(i))
[x,likelihood,exitflag,output]=fmincon(@(params)fit_2lr2s1lapse(params,identifiers(i)),x0,[],[],[],[],lb,ub,[]); %had options originally?
results_2lr2s1lapse(i,:)=[x likelihood exitflag];
end

results_2lr2s1lapse(:,8)=identifiers; %nparam+3

save('N:\Alex\metaRL\map\results_2lr2s1lapse.mat', 'results_2lr2s1lapse')
