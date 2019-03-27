# =======================================================================
# Names: Anne Idigoras y Josune Ordo??ez
# Group Number: Wakanda
# Assignment:Laberinto de Zapatos
# Date:
# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire template folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to 
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================

# Configuring the Environment
rm(list=ls())
cat("\014")
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# LIBRARIES (add any needed library here)
library(rstudioapi)
library(ggplot2)
library(gridExtra)


# ADDITIONAL FUNCTIONS (add any used method here)
source("[TEMPLATE].problem-interface-XX.R")
source("R_library/v1/Analyze.Results.R")
source("R_library/v1/Plot.Results.R")
source("R_library/v1/Breadth.First.Search.TS.R")
source("R_library/v1/Uniform.Cost-TS.R")
source("R_library/v1/A.Star.R")

# =======================================================================
# Check the proper operation of implemented function here!
MyData <- read.csv(file="datos.csv", header=TRUE, sep=";")
a<-MyData[,"Tablero"]

#Solo si es cuadrado
rows = sqrt(length(a))
columns = sqrt(length(a))

A=matrix(a,rows,columns)
print(A)

a<-c(0,0,0,0,0,1,0,1,0)
A=matrix(a,rows,columns)
print(A)
rows=3
columns=3
MyData$Tablero<-NULL
colnames(MyData)[1] <- "x"


# =======================================================================
# Solving of the problem (you have to adapt it)
source("[TEMPLATE].problem-interface-XX.R")
source("R_library/v1/A.Star.R")
problem   = initialize.problem(a,rows,columns,MyData)
limit = 10000
bfs=Breadth.First.Search(problem,limit)
bfs = A.star(problem,limit)

res1 = A.star(problem,"<OTHER_PARAMETERS>")
res2 = Breadth.First.Search(problem,"<OTHER_PARAMETERS>")
all = list(res1, res2)
analyze.results(list(res1,res2),problem)

