Deep.Limited.Search = function(problem,count.limit=100,deep.limit=10){
  name.method = paste("Deep Limited Search",deep.limit)
  
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions = problem$actions
  
  node = list(state=state.initial,
              actions=c(),
              deep=0,
			  cost=0)
  frontier = list(node)
  firstnode = node
  
  count = 1
  report = data.frame(iteration=numeric(),
                      nodes.in.frontier=numeric(),
                      deep.of.expanded=numeric(),
                      nodes.added.to.frontier=numeric())
  while (!is.final.state(firstnode$state,state.final) & count<=count.limit){
    if (count%%100==0){
      print(paste0("Count: ",count,", Nodes in the frontier: ",length(frontier)))
    }    
    frontierlength = length(frontier)
    if (frontierlength==0){
      break
    }
    
    firstnode = frontier[[length(frontier)]]
    frontier[[length(frontier)]] = NULL

    if (is.final.state(firstnode$state,state.final)){
      print("Final State Found")
      break
    }
    
    for (i in 1:nrow(actions)){
      action = actions[i, ]
      state  = firstnode$state
      if (is.applicable(firstnode$state,action,problem)){
        newnode = list()
        newnode$state = effect(state,action)
        newnode$actions = rbind(firstnode$actions,action)
        newnode$deep = firstnode$deep+1
		    newnode$cost = firstnode$cost+get.cost(actions[i,],state)
        # Condition added to not overpassing the limit
        if (newnode$deep<=deep.limit){
          frontier = append(frontier,list(newnode))
        }
      }
    }
    
    report = rbind(report,
                   data.frame(iteration = count,
                              nodes.in.frontier = frontierlength,
                              deep.of.expanded = firstnode$deep,
                              nodes.added.to.frontier = length(frontier)-frontierlength))
    
    count = count+1
  }
  
  result = list()
  result$report = report
  result$name = name.method
  
  # Show the obtained (or not) final solution
  if (count >= count.limit | length(frontier)==0){
    print("Maximum Number of iterations reached. No solution found")
    result$state.final = NA
  } else{
    print("Solution found!!")
    to.string(firstnode$state)
    print("Actions: ")
    print(firstnode$actions)
    result$state.final = firstnode
  }

  plot.results(report,name.method,problem)
 
  return(result)
}