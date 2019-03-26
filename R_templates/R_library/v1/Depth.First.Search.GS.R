Depth.First.Search.GS = function(problem, it.limit=100) {
  method.name <- "Depth First Search Graph Search"
  solution=FALSE
  
  # Get the required values from the problem
  state.initial   <- problem$state.initial
  state.final     <- problem$state.final
  actions         <- problem$actions
  
  # Make a node with the initial problem state
  node.first      <- list(state=state.initial, actions=c(), deep=0, cost=0)
  # Insert node into the frontier data structure
  frontier        <- list(node.first)
  # Create expanded list
  expanded        <- list()     
  
  it.count <- 1
  report <- data.frame(iteration = numeric(),
                       nodes.in.frontier = numeric(),
                       deep.of.expanded = numeric(),
                       nodes.added.to.frontier = numeric())
  
  # (an extra condition is added to control the maximum number of iterations)
  while (!is.final.state(node.first$state, state.final) & length(frontier) != 0 & it.count <= it.limit) {
    if (it.count %% 100 == 0) {
      print(paste0("Iteration: ", it.count, ", Nodes in frontier: ", length(frontier)))
    }
    
    node.first <- frontier[[1]]
    frontier[[1]] <- NULL
    frontier.length <- length(frontier)
    
    if (is.final.state(node.first$state, state.final)) {
      print("Final State Found!")
      solution=TRUE
      break
    }
    
    # Add node.first in expanded list
    expanded = append(expanded, list(node.first))
    
    for (i in 1:nrow(actions)) {
      action <- actions[i, ]
      state  <- node.first$state
      
      if (is.applicable(node.first$state, action, problem)){
        node.new          <- list()
        node.new$state    <- effect(state, action)
        node.new$actions  <- rbind(node.first$actions, action)
        node.new$deep     <- node.first$deep + 1
        node.new$cost     <- node.first$cost + get.cost(actions[i,], state)
		    
        # Check if the state of node.new is in frotier or expanded
        if (!any(sapply(frontier, function (x) identical(x$state, node.new$state)))) {
          if (!any(sapply(expanded, function (x) identical(x$state, node.new$state)))) {
            frontier <- append(list(node.new), frontier)
          }
        }
      }
    }
  
    report <- rbind(report, 
                   data.frame(iteration = it.count,
                              nodes.in.frontier = length(frontier),
                              deep.of.expanded = node.first$deep,
                              nodes.added.to.frontier = length(frontier) - frontier.length))
    
    it.count <- it.count+1
  }
  
  result        <- list()
  result$report <- report
  result$name   <- method.name
  
  # Show the obtained (or not) final solution
  if (!solution) {
    print("Maximum Number of iterations reached. No solution found")
    result$state.final <- NA
  } else{
    print("Solution found!!")
    to.string(node.first$state)
    print("Actions: ")
    print(node.first$actions)
    result$state.final <- node.first
  }
  
  plot.results(report, method.name, problem)
  
  return(result)
}