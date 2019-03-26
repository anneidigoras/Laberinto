analyze.results = function(all){
  results <- data.frame()
  
  for (i in 1:length(all)) {
    name <- all[[i]]$name
    print(name)
    solution.found <- any(!is.na(all[[i]]$state.final))
    
    if (any(solution.found)) {
      # Checking the solution
      solution.length <- length(all[[i]]$state.final$actions)
      solution.cost   <- all[[i]]$state.final$cost
      print("  Solution Found! :)")
      actions <- all[[i]]$state.final$actions
      state.current <- problem$state.initial
      print("Initial State: ")
      to.string(state.current)
      
      for (a in 1:nrow(actions)) {
        action <- actions[a,]
        state.current <- effect(state.current,action)  
        print(paste0("After applying action: "))
        print(action)
        to.string(state.current)
      }
    } else {
      solution.length <- -1
      solution.cost   <- -1
      print("  No Solution Found :(")
    }
    
    number.expanded <- length(all[[i]]$report$iteration)
    maximum.deep <- max(all[[i]]$report$deep.of.expanded)
    maximum.frontier <- max(all[[i]]$report$nodes.in.frontier)
    
    results = rbind(results, data.frame(name=name,
                                        solution.found=solution.found,
                                        solution.length=solution.length,
                                        solution.cost=solution.cost,
                                        number.expanded=number.expanded,
                                        maximum.deep=maximum.deep,
                                        maximum.frontier=maximum.frontier))
  }
  
  return(results)
}