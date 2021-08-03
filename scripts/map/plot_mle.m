function []=plot_mle(results_pat,results_con,model,priors,dist)
%mkdir(fullfile('N:/Alex/metaRL/map/mle_figures/'));
savedir='N:/Alex/metaRL/map/mle_figures';
%for all parameters (ncol - 3 (those 3 are likelihood, exitflag, num, group))
for param=1:size(results_pat,2)-4
    hist(rmoutliers(results_pat(:,param)));
    if nargin>4
        hold on
        if dist(param)=='b'
            x = 0:0.01:1;
            y = size(results_pat,1)/2*betapdf(x,priors{param,1}(1),priors{param,1}(2));
        elseif dist(param)=='n'
            x = -10:0.1:10;
            y = size(results_pat,1)/2*normpdf(x,priors{param,1}(1),priors{param,1}(2));
        else
            x = 0:0.01:max(rmoutliers(results_pat(:,param)));
            y = size(results_pat,1)/2*gampdf(x,priors{param,1}(1),priors{param,1}(2));
        end
        plot(x,y,'r')
        hold off
    end
    saveas(gcf,fullfile(savedir,strcat(model,'_parameter_',string(param),'_pat.png')));
    hist(rmoutliers(results_con(:,param)));
    if nargin>4
        hold on
        if dist(param)=='b'
            x = 0:0.01:1;
            y = size(results_con,1)/2*betapdf(x,priors{param,2}(1),priors{param,2}(2));
        elseif dist(param)=='n'
            x = -10:0.1:10;
            y = size(results_con,1)/2*normpdf(x,priors{param,1}(1),priors{param,1}(2));
        else
            x = 0:0.01:max(rmoutliers(results_con(:,param)));
            y = size(results_con,1)/2*gampdf(x,priors{param,2}(1),priors{param,2}(2));
        end
        plot(x,y,'r')
        hold off
    end
    saveas(gcf,fullfile(savedir,strcat(model,'_parameter_',string(param),'_con.png')));
end
end