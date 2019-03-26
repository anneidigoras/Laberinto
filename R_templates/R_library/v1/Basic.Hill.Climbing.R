Basic.Hill.Climbing = function(problem, count.limit=100) {
  name.method = "Basic Hill Climbing"
  
  state.initial    = problem$state.initial
  actions = problem$actions
  distance.matrix = problem$distance.matrix
  end = FALSE
  
  # Value field added to store the value of f(n)
  node = list(state=state.initial,
              actions=c(),
              deep=0,
              cost=get.cost(state.initial, distance.matrix))
  
  frontier = list(node)
  firstnode = node
  
  expanded = list() 
  
  count = 1
  report = data.frame(iteration=numeric(),
                      nodes.in.frontier=numeric(),
                      deep.of.expanded=numeric(),
                      nodes.added.to.frontier=numeric())
  
  while (count <= count.limit) {
    if (count%%100==0) {
      print(paste0("Count: ",count,", Nodes in the frontier: ",length(frontier)))
      print(c(min(sapply(frontier,function (x) x$cost)), max(sapply(frontier,function (x) x$cost))))
    }
    
    frontierlength = length(frontier)
    
    if (frontierlength==0){
      break
    }
    
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    expanded = append(expanded, list(firstnode))
    
    for (i in 1:nrow(actions)) {
      action = actions[i, ]
      state  = firstnode$state
      
      newnode = list()
      newnode$state = effect(state,action)
      newnode$actions = rbind(firstnode$actions, action)
      newnode$deep = firstnode$deep + 1
      newnode$cost = get.cost(newnode$state, distance.matrix)
      
      # Expanded management modified 1: if not in frontier nor expanded --> include
      if (!any(sapply(frontier,function (x) identical(x$state, newnode$state)))) {
          if (!any(sapply(expanded,function (x) identical(x$state, newnode$state)))) {
            frontier = append(frontier, list(newnode))
          }
      }
    }
    
    report = rbind(report,
                   data.frame(iteration = count,
                   nodes.in.frontier = frontierlength,
                   deep.of.expanded = firstnode$deep,
                   nodes.added.to.frontier = length(frontier)-frontierlength))
    
    frontier = frontier[order(sapply(frontier,function (x) x$cost))]
    frontier = list(frontier[[1]])
    
    if (firstnode$cost <= frontier[[1]]$cost) {
      end = TRUE
      break
    }
    
    count = count+1
  }
  
  result = list()
  result$report = report
  result$name = name.method
  
  if (end == FALSE){
    print("Maximum Number of iterations reached, still room for improvement.")
    to.string(firstnode$state)
    print("Actions: ")
    print(firstnode$actions)
    result$state.final = firstnode
  } else{
    print("No best cost among neighbours. ")
    to.string(firstnode$state)
    print("Actions: ")
    print(firstnode$actions)
    result$state.final = firstnode
  }
  
  plot.results(report, name.method, problem)
  
  return(result)
}


