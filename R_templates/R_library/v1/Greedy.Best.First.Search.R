Greedy.Best.First.Search = function(problem,count.limit=100){
  name.method = "Greedy Best-First Search"
  
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions = problem$actions
  
  # Value field added to store the value of f(n)
  node = list(state=state.initial,
              actions=c(),
              deep=0,
              cost=0,
              value=get.evaluation(state.initial,problem))
  frontier = list(node)
  firstnode = node
  
  # Line added to initialize the expanded list
  expanded = list() 
  
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
    
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    if (is.final.state(firstnode$state,state.final)){
      print("Final State Found")
      break
    }
    
    # Line added to include the node to expand in the expanded list
    expanded = append(expanded,list(firstnode))
    for (i in 1:nrow(actions)){
      action = actions[i, ]
      state  = firstnode$state
      if (is.applicable(firstnode$state,action,problem)){
        newnode = list()
        newnode$state = effect(state,action)
        newnode$actions = rbind(firstnode$actions,action)
        newnode$deep = firstnode$deep+1
        newnode$cost = firstnode$cost+get.cost(actions[i,],state)
        # Value field added to store the value of f(n)
        newnode$value = get.evaluation(newnode$state,problem)
        # Instead of including the node directly in the frontier
        # it is checked that it is not neither in the frontier or expanded lists
        if (!any(sapply(frontier,function (x) identical(x$state,newnode$state)))){
          if (!any(sapply(expanded,function (x) identical(x$state,newnode$state)))){
            frontier = append(frontier,list(newnode))
          }
        }
      }
    }
    # Line modified to order the frontier according with the value of the function
    frontier = frontier[order(sapply(frontier,function (x) x$value))]
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


