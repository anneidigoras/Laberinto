Uniform.Cost.Search = function(problem, it.limit=100){
  method.name <- "Uniform Cost Tree Search"
  
  
  
  # Get the required values from the problem
  state.initial   <- problem$state.initial
  state.final     <- problem$state.final
  actions         <- problem$actions
  
  
  # 1. Make a node with the initial problem state
  node.first      <- list(state=state.initial, actions=c(), deep=0, cost=0)
  # 2. Insert node into the frontier data structure
  frontier        <- list(node.first)
  
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
    
    