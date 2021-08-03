function priors=get_prior_params(results_pat,results_con,dist,singleprior)
%for all parameters (ncol - 4 (those 3 are likelihood, exitflag, num, group))
for param=1:size(results_pat,2)-4
    if dist(param)=='b'
        if singleprior==1
            temp=[results_pat;results_con]; %combines pat and con
            priors{param,1}=betafit(rmoutliers(temp(:,param)));
            priors{param,2}=priors{param,1};
        else 
            priors{param,1}=betafit(results_pat(:,param));
            priors{param,2}=betafit(results_con(:,param));
            priors{param,1}(priors{param,1}<1) = 1;
            priors{param,2}(priors{param,2}<1) = 1;
        end
    elseif dist(param)=='n'
        if singleprior==1
            temp=[results_pat;results_con]; %combines pat and con
            [a,b]=normfit(rmoutliers(temp(:,param)));
            priors{param,1}=[a,b];
            priors{param,2}=[a,b];
        else
            [a,b]=normfit(rmoutliers(results_pat(:,param)));
            priors{param,1}=[a,b];
            [a,b]=normfit(rmoutliers(results_con(:,param)));
            priors{param,2}=[a,b];
        end
    else
        if singleprior==1
            temp=[results_pat;results_con]; %combines pat and con
            priors{param,1}=gamfit(rmoutliers(temp(:,param)));
            priors{param,2}=priors{param,1};
        else
            priors{param,1}=gamfit(rmoutliers(results_pat(:,param)));
            priors{param,2}=gamfit(rmoutliers(results_con(:,param)));
        end
    end
end
end