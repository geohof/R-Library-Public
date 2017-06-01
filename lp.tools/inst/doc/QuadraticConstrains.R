## ---- Packages, warning = FALSE, message = FALSE-------------------------
require(lp.tools)
require(Matrix)
require(gurobi)

## ------------------------------------------------------------------------
num.v <- 3
Initialize(num.v, optimizer = "gurobi")
SetObjective(objective = c(1,0,0), description = "x-value", direction = "max")

AddConstraint(description = "xyz - sum",
              mat = c(1, 1, 1), 
              rhs = 1, 
              dir = "=")

AddQuadConstraint(description = "Cone", 
                  mat = spMatrix(3, 3, c(1, 2, 3), c(1, 2, 3), c(1.0, 1.0, -1.0)))
AddQuadConstraint(description = "Rotated cone", 
                  mat = spMatrix(3, 3, c(1, 2), c(1, 3), c(1.0, -1.0)))


