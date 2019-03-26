plot.results = function(report, name.method, problem){
  # Generation of Graphics...
  plot1 = ggplot(report,aes(x=iteration,y=nodes.in.frontier))+geom_point(col="dodgerblue")+
    labs(x="Iteration Number",
         y="Nodes stored in the frontier",
         title="Size of the Frontier",
         caption = "University of Deusto")+theme_minimal()

  plot2 = ggplot(report,aes(x=nodes.added.to.frontier))+geom_histogram(fill="dodgerblue",
                                                                    bins=length(unique(report$nodes.added.to.frontier)),
                                                                    binwidth = 0.1)+
    labs(x="Number nodes added to frontier",
         y="Frequency",
         title="Frontier growth",
         caption = "University of Deusto")+theme_minimal()
  
  plot3 = ggplot(report,aes(x=deep.of.expanded))+geom_histogram(fill="dodgerblue",
                                                                bins=length(unique(report$deep.of.expanded)),
                                                                binwidth = 0.1)+
    labs(x="Deep",
         y="Number of Nodes",
         title="Deep of the nodes in the frontier",
         caption = "University of Deusto")+theme_minimal()
  
  grid.arrange(plot1, plot2, plot3, ncol=1, top=paste0(name.method,": ",problem$name))
}