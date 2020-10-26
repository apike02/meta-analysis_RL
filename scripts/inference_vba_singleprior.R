inference_vba_singleprior<- function (model,task,data_details,workingdir,suffix){ 
  
  if (missing(suffix)){
    suffix<-NULL
  }
  
  library('emmeans')
  library('afex')
  library('ggplot2')
  library('cowplot')
  library('effsize')
  
  source(paste0(workingdir,'/scripts/get_previous_character.R'))
  source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
  
  scriptdir=paste0(workingdir,'/scripts/')
  imgdir=paste0(workingdir,'/figures/')
  
  data<-load(paste0(workingdir,'stan_outputs/fit_',model,'_ALL_',task,'.RData'))
  mname<-paste0('fit_',model,'_ALL')
  
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
  
  param_df<-data_details
  
  for (parameter in 1:n_param){
    a<-c(get_posterior_mean(eval(parse(text=mname)),param_names[parameter]))
    param_df<-cbind(param_df,(c(a)))
  }
  
  colnames(param_df)<-c('study','pat_con','id','trial','reward','pun','choices','fullid',param_names)
  
  param_df$pat_con<-as.factor(param_df$pat_con)
  
  for (parameter in 1:n_param){
    model<-aov_ez('fullid',param_names[parameter],param_df,between=c('study','pat_con'),type=3)
    print(summary(model))
    pat_data<-select(param_df[param_df$pat_con=='1',],contains(param_names[parameter]))
    con_data<-select(param_df[param_df$pat_con=='0',],contains(param_names[parameter]))
    print(cohen.d(pat_data[,1],con_data[,1]))
    ggplot(param_df,aes(x=pat_con,y=eval(parse(text=param_names[parameter])),group=pat_con,fill=pat_con))+
      stat_summary(fun.y = mean, geom = "bar") + 
      stat_summary(fun.data = mean_se, geom = "errorbar")+
      labs(x='Group',y=param_names[parameter])+
      theme_classic()+
      scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
      ggsave(paste0(imgdir,param_names[parameter],task,'_bar',suffix,'.png'))
    if (param_names[parameter]=='beta_win'||param_names[parameter]=='beta_loss'||
        param_names[parameter]=='sensitivity_win'||param_names[parameter]=='sensitivity_loss'||
        param_names[parameter]=='beta'||param_names[parameter]=='sensitivity'){
      ggplot(param_df,aes(x=pat_con,y=log(eval(parse(text=param_names[parameter]))),group=pat_con,fill=pat_con))+
        geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
        geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.1)+
        #geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black") +
        stat_summary(fun.data = mean_se, geom = "errorbar", width=0.1)+
        labs(x='Group',y=param_names[parameter])+
        theme_classic()+
        theme(text=element_text(size=16))+
        scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
        ggsave(paste0(imgdir,param_names[parameter],task,suffix,'.png'), scale=0.6)
    } else {
      ggplot(param_df,aes(x=pat_con,y=eval(parse(text=param_names[parameter])),group=pat_con,fill=pat_con))+
        geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
        geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.1)+
        #geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black") +
        stat_summary(fun.data = mean_se, geom = "errorbar", width=0.1)+
        labs(x='Group',y=param_names[parameter])+
        theme_classic()+
        theme(text=element_text(size=16))+
        scale_x_discrete(labels=c("0"="Control","1"="Patient"))+
        ggsave(paste0(imgdir,param_names[parameter],task,suffix,'.png'), scale=0.6)
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
}
