# =======================================================================
# Names:Anne Idigoras y Josune Ordo??es
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


initialize.problem = function(lista=c(0,0,1,0,1,0,1,0,0),rows = 3,columns = 3,dfcomb=NULL){
  problem = list()
  problem$mapa=  matrix(lista,rows,columns)
  problem$rows=rows
  problem$columns=columns
  
  problem$muros=dfcomb
  problem$state.initial = c(rows,1) #estado inicial
  problem$state.final   = c(1,columns) #estado final
  problem$actions.possible=data.frame(action=c("Up","Down","Left","Right"),cost=1)
  # problem$name = <Insert code here>
  # problem$<aditional info> = <Insert code here>
  return(problem)
}

# entorno<- data.frame(
#    x= c (1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7),
#    y= c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7),
#   pie = replicate(1,sample(0:1,49,rep=TRUE)),
#    muroA = replicate(1,sample(0:1,49,rep=TRUE)),
#   muroB = replicate(1,sample(0:1,49,rep=TRUE)),
#   muroI = replicate(1,sample(0:1,49,rep=TRUE)),
#   muroD =  replicate(1,sample(0:1,49,rep=TRUE))
# )
# actions.possible = data.frame(action=c("Arriba","Abajo","Izquierda","Derecha"),cost=1)


# initialize.problem = function(rows=7, columns=7, perm = sample(0:(rows*columns-1))){
#   problem = list()
#   
#    problem$state.initial = matrix(perm,nrow=rows,byrow = TRUE)
#    problem$state.final   = matrix(0:(rows*columns-1),nrow=rows,byrow = TRUE)
#    problem$actions.possible = data.frame(action=c("Arriba","Abajo","Izquierda","Derecha"),cost=1)
#    problem$rows = rows
#    problem$columns = columns
#    problem$name = paste0("Problema de los zapatos (",rows,"x",columns,") - [",paste0(perm,collapse="-"),"]")
# 
#   return(problem)
# }


# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state, action, problem){
  #estado fila
  row=state[1]
  #estado columna
  col=state[2]

  action = action$action
  #subset nos permite seleccionar lo que queremos del dataframe; en este caso muro, fila y columna
  d<-subset(problem$muros,x==row & y==col)
  
  #caso de ir arriba
  if (action == "Up"){
    
    if(row!=1){# si fila distinto de 1
    
      if(problem$mapa[row-1,col]!=problem$mapa[row,col])
      {
        
        if(d[,"Up"]==0)
        {
          result=TRUE
        }
        else{
          result=FALSE
        }
      }
      else{
        result=FALSE
      }
    }
    else{
      result = FALSE
    }
  }
  #caso de ir abajo
  
  if (action == "Down"){
    if(row!=problem$rows){
      if(problem$mapa[row+1,col]!=problem$mapa[row,col])
      {
        if(d[,"Down"]==0)
        {
          result=TRUE
        }
        else{
          result=FALSE
        }
      }
      else{
        result=FALSE
      }
    }
    else{
      result = FALSE
    }
  }
  ...
  return(...)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state, action){
  action<-action$action
  
  ubicacion<- which(state==0, indice=TRUE)
  row <- ubicacion[1]
  col <- ubicacion[2]
  result <- state 
  
  if (action == "Up") {
    result[1]<-result[1]-1
  }
  
  if (action == "Down") {
    result[1]<-result[1]+1
  }
  
  if (action == "Left") {
    result[2]<-result[2]-1
  }
  
  if (action == "Right") {
    result[2]<-result[2]+1
  }
 
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalstate){
  return(identical(state,finalstate))
}
# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  print(state)
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  return(action$cost)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  print(sqrt((state[1]-problem$state.final[1])^2+(state[2]-problem$state.final[2])^2))
  return(sqrt((state[1]-problem$state.final[1])^2+(state[2]-problem$state.final[2])^2))
}