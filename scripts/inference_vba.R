inference_vba<- function (model,task,data_details,workingdir,analysis_type,priors){ 
  
  source(paste0(workingdir,'/scripts/effect_sizes_text.R'))
  source(paste0(workingdir,'/scripts/plottext_cohend.R'))
  
  if (missing(priors)){
    priors<-'separate'
  } 
  
  suffix<-paste0(analysis_type,priors)

  
  library('emmeans')
  library('afex')
  library('ggplot2')
  library('cowplot')
  library('effsize')
  
  source(paste0(workingdir,'/scripts/get_previous_character.R'))
  source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
  
  scriptdir=paste0(workingdir,'/scripts/')
  imgdir=paste0(workingdir,'/figures/')
  
  condata<-load(paste0(workingdir,'stan_outputs/fit_',model,'_HC_',task,'.RData'))
  patdata<-load(paste0(workingdir,'stan_outputs/fit_',model,'_PA_',task,'.RData'))
  mname_con<-paste0('fit_',model,'_HC')
  mname_pat<-paste0('fit_',model,'_PA')
  
  n_lr<-get_previous_character(model,pattern='lr') #home-brewed function that gets previous character to a pattern
  n_b<-get_previous_character(model,pattern='t')
  n_s<-get_previous_character(model,pattern='s')
  n_lapse<-get_previous_character(model,pattern='lapse')
  n_bias<-get_previous_character(model,pattern='bias')
  n_param = sum(c(n_lr,n_b,n_s,n_lapse,n_bias))
  
  if (n_lr==2) param_names<-c('alpha_win','alpha_loss')
  if (n_lr==1) param_names<-c('alpha')
  if (n_b==2) param_names<-c(param_names,'beta_win','beta_loss')
  if (n_b==1) param_names<-c(param_names,'beta')
  if (n_s==2) param_names<-c(param_names,'sensitivity_win','sensitivity_loss')
  if (n_s==1) param_names<-c(param_names,'sensitivity')
  if (n_lapse==1) param_names<-c(param_names,'lapse')
  if (n_bias==1) param_names<-c(param_names,'go_bias')
  if (n_bias==2) param_names<-c(param_names,'go_bias','pav_bias')
  if (n_bias==3) param_names<-c(param_names,'go_bias','app_bias','av_bias')
  
  param_df<-data.frame(rbind(data_details[data_details$pat_con==1,],data_details[data_details$pat_con==0,]))
  
  for (parameter in 1:n_param){
    a<-c(get_posterior_mean(eval(parse(text=mname_pat)),param_names[parameter]),
         get_posterior_mean(eval(parse(text=mname_con)),param_names[parameter]))
    param_df<-cbind(param_df,(c(a)))
  }
  
  colnames(param_df)<-c('study','pat_con','id','trial','reward','pun','choices','fullid',param_names)
  
  print(sum(param_df$pat_con))
  
  param_df$pat_con<-as.factor(param_df$pat_con)
  param_df$study<-as.factor(param_df$study)
  
  effect_sizes_text(workingdir,analysis_type,priors,'vba',param_names,task,param_df)
  sink()
  
  plots<-list()
  for (parameter in 1:n_param){
    model<-permuco::aovperm(eval(parse(text=param_names[parameter])) ~ pat_con * study, data = param_df)
    print(model)
    pat_data<-select(param_df[param_df$pat_con=='1',],contains(param_names[parameter]))
    con_data<-select(param_df[param_df$pat_con=='0',],contains(param_names[parameter]))
    summary<-param_df%>%
      group_by(pat_con)%>%
      summarise(median=round(median(eval(parse(text=param_names[parameter]))),2),
                iqr=round(IQR(eval(parse(text=param_names[parameter]))),2))
    print(summary)
    d<-cohen.d(pat_data[,1],con_data[,1])
    print(d)
    d<-round(d$estimate,2)
    plottext<-plottext_cohend(model,d)
    ggplot(param_df,aes(x=pat_con,y=eval(parse(text=param_names[parameter])),group=pat_con,fill=pat_con))+
      stat_summary(fun = mean, geom = "bar") + 
      stat_summary(fun.data = mean_se, geom = "errorbar")+
      labs(x='Group',y=param_names[parameter])+
      theme_classic()+
      scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
      ggsave(paste0(imgdir,param_names[parameter],task,'_bar',suffix,'.png'))
    if (param_names[parameter]=='beta_win'||param_names[parameter]=='beta_loss'||
        param_names[parameter]=='sensitivity_win'||param_names[parameter]=='sensitivity_loss'||
        param_names[parameter]=='beta'||param_names[parameter]=='sensitivity'){
      plots[[parameter]]<-local({
        parameter<-parameter
        plot<-ggplot(param_df,aes(x=pat_con,y=log(eval(parse(text=param_names[parameter]))),group=pat_con,fill=pat_con))+
        geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
        geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.1,
                   show.legend = FALSE)+
        geom_boxplot(aes(fill=NA),width = .1, outlier.shape = NA, show.legend=FALSE) +
        #stat_summary(fun = mean, geom="point", shape=20, size=2, color="black",
        #             show.legend = FALSE) +
        #stat_summary(fun.data = mean_se, geom = "errorbar", width=0.1,
        #             show.legend = FALSE)+
        labs(x='Group',y=paste0('Log ',param_names[parameter]),fill='Group',
             title=paste0('Task ', substr(task,2,3)))+
        theme_classic()+
        theme(text=element_text(size=16))+
        scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
        #scale_fill_manual(values=c('#FC766AFF','#5F4B8BFF'))+
        #scale_fill_manual(values=c('#B87333','#5CC8D7FF'))+
        scale_fill_manual(labels=c('Control','Patient'),values=c('#FC766AFF','#5B84B1FF'))+
        annotate(geom='text',x=1.5,y=6,
                 label=plottext,
                 parse=TRUE)+
        geom_segment(x = 1, xend = 1, 
                     y = 5.5, yend = 5.3,
                     colour = "black") +
        geom_segment(x = 1, xend = 2, 
                     y = 5.5, yend = 5.5,
                     colour = "black") +
        geom_segment(x = 2, xend = 2, 
                     y = 5.5, yend = 5.3,
                     colour = "black")
        ggsave(paste0(imgdir,param_names[parameter],task,suffix,'.png'), scale=0.6)
      print(plot)
      })
    } else {
      plots[[parameter]]<-local({
        parameter<-parameter
        plot<-ggplot(param_df,aes(x=pat_con,y=eval(parse(text=param_names[parameter])),group=pat_con,fill=pat_con))+
        geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
        geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.1,
                   show.legend = FALSE)+
        geom_boxplot(aes(fill=NA),width = .1, outlier.shape = NA, show.legend=FALSE) +
        #stat_summary(fun = mean, geom="point", shape=20, size=2, color="black",
        #             show.legend = FALSE) +
        #stat_summary(fun.data = mean_se, geom = "errorbar", width=0.1,
        #             show.legend = FALSE)+
        labs(x='Group',y=param_names[parameter],fill='Group', 
             title=paste0('Task ', substr(task,2,3)))+
        theme_classic()+
        theme(text=element_text(size=16))+
        scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
        scale_fill_manual(labels=c('Control','Patient'),values=c('#FC766AFF','#5B84B1FF'))+
        annotate(geom='text',x=1.5,y=1.1,
                 label=plottext,
                 parse=TRUE)+
        geom_segment(x = 1, xend = 1, 
                     y = 1.02, yend = 1.05,
                     colour = "black") +
        geom_segment(x = 1, xend = 2, 
                     y = 1.05, yend = 1.05,
                     colour = "black") +
        geom_segment(x = 2, xend = 2, 
                     y = 1.02, yend = 1.05,
                     colour = "black") 
        ggsave(paste0(imgdir,param_names[parameter],task,suffix,'.png'), scale=0.6)
      print(plot)
      })
    }
    # afex_plot(model, x = "pat_con", trace = "study", error = "between",
    #          mapping = c("color"),
    #          data_geom = ggbeeswarm::geom_beeswarm,
    #          data_arg = list(
    #            dodge.width = 0.5,  ## needs to be same as dodge
    #            cex = 0.8))+
    #  jtools::theme_apa()
    # ggsave(paste0('N:/Alex/metaRL/map/figures/',param_names[parameter],'interact',task,'.png'))
  }
  return(plots)
}
