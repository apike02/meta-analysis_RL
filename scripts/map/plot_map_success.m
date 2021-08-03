load('outputs')

model_list={'1lr1b','2lr1b','1lr2b','2lr2b','1lr1s','2lr1s','1lr2s',...
    '2lr2s','1lr1s1lapse','2lr1s1lapse','1lr2s1lapse','2lr2s1lapse'};

for model=1:length(model_list)    
    disp(model_list{model})
    a=cell2mat(getfield(outputs_map,strcat('map_',model_list{model})));
    success=[a.localSolverSuccess];
    plot(success,'o');
    disp(sum(find(success~=20)));
end 
