# =======================================================================
# Names:Anne Idigoras y Josune Ordoñes
# Group Number: Wakanda
# Assignment:Laberinto de Zapatos
# Date:
# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to 
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================

# =======================================================================
# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)

#rows = 7
#columns = 7
#set.seed(123) #para aleatoriedad

#0=pie izquierdo
#1=pie derecho

#perm = sample(0:(rows*columns-1)) #nose cmo hacer para que la permutacion sea de ceros y unos
#state.initial = matrix(sample(0:1, rows*columns,replace=TRUE),rows,columns)
#state.final   = matrix(sample(0:1,rows*columns,replace=TRUE),rows,columns)


entorno<- data.frame(
   x= c (1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7),
   y= c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7),
  pie = replicate(1,sample(0:1,49,rep=TRUE)),
   muroA = replicate(1,sample(0:1,49,rep=TRUE)),
  muroB = replicate(1,sample(0:1,49,rep=TRUE)),
  muroI = replicate(1,sample(0:1,49,rep=TRUE)),
  muroD =  replicate(1,sample(0:1,49,rep=TRUE))
)
actions.possible = data.frame(action=c("Arriba","Abajo","Izquierda","Derecha"),cost=1)


initialize.problem = function(rows=7, columns=7, perm = sample(0:(rows*columns-1))){
  problem = list()
  
   problem$state.initial = matrix(perm,nrow=rows,byrow = TRUE)
   problem$state.final   = matrix(0:(rows*columns-1),nrow=rows,byrow = TRUE)
   problem$actions.possible = data.frame(action=c("Arriba","Abajo","Izquierda","Derecha"),cost=1)
   problem$rows = rows
   problem$columns = columns
   problem$name = paste0("Problema de los zapatos (",rows,"x",columns,") - [",paste0(perm,collapse="-"),"]")

  return(problem)
}


# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state, action, problem){
  ...
  return(...)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state, action){
  result = ...
  return(...)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalstate){
  ...
  return(...)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  ...
  print(...)
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  ...
  return(...)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  ...
  return(...)
}