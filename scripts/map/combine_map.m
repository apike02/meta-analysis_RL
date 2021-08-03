results_map_1=load('../../map/results_map_t5_sp.mat')
results_map_2=load('../../map/results_map_t5_13on_sp.mat')
results_1=results_map_1.results_map
results_2=results_map_2.results_map
results_map=cell2struct([struct2cell(results_1);struct2cell(results_2)],[fieldnames(results_1);fieldnames(results_2)])
save('../../map/results_map_t5_sp.mat','results_map')