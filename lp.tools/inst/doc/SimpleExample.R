## ---- Packages, warning = FALSE, message = FALSE-------------------------
require(lp.tools)
require(Matrix)

## ------------------------------------------------------------------------
num.v <- 10
set.seed(60606)
Initialize(num.v)
SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")
AddConstraint(description = "Upper Bound",
              mat = Diagonal(num.v), 
              rhs = 1, 
              dir = "<=")
AddConstraint(description = "Random Bounds", 
              mat = matrix(runif(3 * num.v), ncol=num.v),
              rhs = 1, 
              dir = "<=")
AddConstraint(description = "Random Bound", 
              mat = runif(num.v),
              rhs = 1, 
              dir = "<=")
AddConstraint(description = "Random Bounds", 
              mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
              rhs = 1, 
              dir = "<=")
GetCostOfConstraint()

Initialize(num.v, lock.variables = rep(.3, 4))

SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")
AddConstraint(description = "Upper Bound", 
              mat = Diagonal(num.v), 
              rhs = 1, 
              dir = "<=")
AddConstraint(description = "Random Bounds", 
              mat = matrix(runif(3 * num.v), ncol=num.v),
              rhs = 1, 
              dir = "<=")

Initialize(num.v, lock.variables = rep(.3, 4))

SetObjective(objective.numer = runif(num.v), objective.denom  = runif(num.v), 
             description = "Fraction", direction = "max")
AddConstraint(description = "Upper Bound", 
              mat = Diagonal(num.v), 
              rhs = 1, 
              dir = "<=")
AddConstraint(description = "Random Bounds", 
              mat = matrix(runif(3 * num.v), ncol=num.v),
              rhs = 1, 
              dir = "<=")
RemoveConstraint(description = "Random Bounds")

