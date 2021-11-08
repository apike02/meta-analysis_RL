confusion_matrix <- function(original,estimated,paramnames){
  
  if(missing(paramnames)) {
    paramnames=colnames(original)
  } else {
    colnames(estimated)<-paramnames
  }
  
  library('pheatmap')
  
  #this makes a confusion matrix where the diagonal is correlations between simulated and estimated parameters;
  #off diagonal is the cross correlations between ESTIMATED parameters
  
  breaksList = seq(-1, 1, by = 0.02)
  matrix<-cor(estimated)
  diag(matrix)<-diag(cor(original,estimated))
  
  # pheatmap(
  #   mat               = matrix,
  #   border_color      = NA,
  #   color             = colorRampPalette(c("blue4", "white", "firebrick1"))(100),
  #   breaks            = breaksList,
  #   show_colnames     = TRUE,
  #   show_rownames     = TRUE,
  #   fontsize          = 14,
  #   treeheight_row    = 0, 
  #   treeheight_col    = 0,
  #   cluster_rows      = FALSE,
  #   cluster_cols      = FALSE,
  #   width             = 5, 
  #   height            = 5,
  #   display_numbers   = TRUE,
  # )
  
  plot<-ggplot(data=melt(matrix),aes(x=Var1,y=Var2))+
    geom_tile(aes(fill=value))+
    geom_text(aes(Var2, Var1, label = round(value,2)), color = "black")+
    coord_equal()+
    scale_fill_gradient2(low='navy',mid='white',high='firebrick1',midpoint=0,limits=c(-1,1))+
    labs(x= "", y = "", fill = "Correlation Coefficient")+
    theme(panel.background = element_blank(), axis.ticks = element_blank())+
    scale_x_discrete(position = "top",guide = guide_axis(angle = 45)) +
    scale_y_discrete(limits = rev(levels(melt(matrix)$Var2)))
  
  print(plot)

  
}
