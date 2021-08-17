custom_forestplot<-function(meta_analysis,fix_or_random){

  if (fix_or_random=='fixed'){
    meta_estimate<-meta_analysis$TE.fixed
    meta_lower<-meta_analysis$lower.fixed
    meta_upper<-meta_analysis$upper.fixed
    label1<-'Fixed Effects Meta-Analysis'
  } else if (fix_or_random=='random'){
    meta_estimate<-meta_analysis$TE.random
    meta_lower<-meta_analysis$lower.random
    meta_upper<-meta_analysis$upper.random
    label1<-'Random Effects Meta-Analysis'
  }

  effectsizes<-data.frame(name=meta_analysis$studlab,
                          estimate=meta_analysis$TE,
                          lower=meta_analysis$lower,
                          upper=meta_analysis$upper)
  #effectsizes<-effectsizes[complete.cases(effectsizes),]
  effectsizes$name<-as.factor(effectsizes$name)

  labels<-c('',label1,rev(unique(levels(effectsizes$name)[effectsizes$name]))) #blank gives space for greater labels
  labelcols<-c('transparent',rep('black',length(labels)-1))

  limits<-c(min(c(-0.5,effectsizes$lower,effectsizes$upper),na.rm=TRUE),max(c(0.5,effectsizes$lower,effectsizes$upper),na.rm=TRUE)) #limits of -0.1 and 0.1 or the maximum/minimum values

  ggplot(effectsizes,aes(x=name,y=estimate,fill=estimate))+
    geom_hline(yintercept=0,linetype='dashed',alpha=0.5)+
    geom_hline(yintercept=meta_estimate,linetype='dotted',alpha=0.2)+
    geom_pointrange(position=position_dodge2(0.5),aes(ymin=lower,ymax=upper),show.legend = FALSE)+
    geom_point(size=2,position=position_dodge2(0.5),colour='black',shape=21)+
    scale_fill_gradient2(limits=c(-1,1),low='blue',mid='white',high='red',midpoint=0)+
    theme_classic()+
    #scale_shape_manual(values=c(21,22,23))+
    labs(x='Analysis')+
    geom_errorbar(aes(x=2,y=meta_estimate,ymin=meta_lower,ymax=meta_upper),size=1,colour='black',show.legend=FALSE,width=0)+
    geom_point(aes(x=2,y=meta_estimate),shape=21,size=2,fill='black')+
    geom_rect(aes(xmin=1.8, xmax=2.2, ymin=meta_lower, ymax=meta_upper),alpha=0.3,fill='forestgreen')+
    scale_x_discrete(limit=labels,labels=labels)+
    theme(axis.text.y = element_text(face = c('plain','bold',rep('plain',nrow(effectsizes)))),axis.ticks.y = element_line(colour = labelcols))+
    #annotate("segment", x = 0.7, xend = 0.7, y = -0.05, yend = 0.05, colour = "grey", size = 1, arrow = arrow(ends='both'))+
    annotate("text", x = 1.2, y = 0.5*limits[2], colour = "grey", label='greater in patients',size=3)+
    annotate("text", x = 1.2, y = 0.5*limits[1], colour = "grey", label='greater in controls',size=3)+
    coord_flip()+
    ylim(limits)
}
