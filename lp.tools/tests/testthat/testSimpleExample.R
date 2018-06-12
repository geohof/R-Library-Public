library(lp.tools)
context("SimpleExamples")


test_that("Simple Example", {
  num.v <- 10
  set.seed(60606)
  oo <- Initialize(num.v)
  SetObjective(opt.obj = oo, objective = rep(1, num.v), description = "Sum", direction = "max")
  AddConstraint(opt.obj = oo, description = "Upper Bound",
                mat = Diagonal(num.v), 
                rhs = 1, 
                dir = "<=")
  AddConstraint(opt.obj = oo, description = "Random Bounds", 
                mat = matrix(runif(3 * num.v), ncol=num.v),
                rhs = 1, 
                dir = "<=")
  AddConstraint(opt.obj = oo, description = "Random Bound", 
                mat = runif(num.v),
                rhs = 1, 
                dir = "<=")
  AddConstraint(opt.obj = oo, description = "Random Bounds", 
                mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
                rhs = 1, 
                dir = "<=")
  expect_equal(GetLastSolution(opt.obj = oo)$objval, 
               2.515656365)
  expect_equal(GetLastSolution(opt.obj = oo)$solution, 
               c(0, 0, 0.08752517, 0, 0, 1, 0, 0, 0.91715995, 0.51097124))
  GetCostOfConstraint(opt.obj = oo)
})

test_that("Lock Variables", {
  num.v <- 10
  set.seed(60606)
  oo <- Initialize(num.v, lock.variables = rep(.3, 4))

  SetObjective(oo, objective = rep(1, num.v), description = "Sum", direction = "max")
  AddConstraint(oo, description = "Upper Bound", 
                mat = Diagonal(num.v), 
                rhs = 1, 
                dir = "<=")
  AddConstraint(oo, description = "Random Bounds", 
                mat = matrix(runif(3 * num.v), ncol=num.v),
                rhs = 1, 
                dir = "<=")

  expect_equal(GetLastSolution(oo)$solution, 
               c(0.3, 0.3, 0.3, 0.3, 0.145551940979471, 1, 0, 0, 0.0903138608130597, 0))
})
  
test_that("Lock Variables (Gurobi)", {
  require(gurobi)
  num.v <- 10
  set.seed(60606)
  oo <- Initialize(num.v, lock.variables = rep(.3, 4), optimizer = "gurobi")

  SetObjective(oo, objective = rep(1, num.v), description = "Sum", direction = "max")
  AddConstraint(oo, description = "Upper Bound", 
                mat = Diagonal(num.v), 
                rhs = 1, 
                dir = "<=")
  AddConstraint(oo, description = "Random Bounds", 
                mat = matrix(runif(3 * num.v), ncol=num.v),
                rhs = 1, 
                dir = "<=")

  expect_equal(GetLastSolution(oo)$solution, 
               c(0.3, 0.3, 0.3, 0.3, 0.145551940979471, 1, 0, 0, 0.0903138608130597, 0))
})
  
test_that("Fractional objective", {
  num.v <- 10
  set.seed(60606)
  oo <- Initialize(num.v, lock.variables = rep(.3, 4))

  SetObjective(oo, objective.numer = runif(num.v), objective.denom  = runif(num.v), 
               description = "Fraction", direction = "max")
  AddConstraint(oo, description = "Upper Bound", 
                mat = Diagonal(num.v), 
                rhs = 1, 
                dir = "<=")
  AddConstraint(oo, description = "Random Bounds", 
                mat = matrix(runif(3 * num.v), ncol=num.v),
                rhs = 1, 
                dir = "<=")
  RemoveConstraint(oo, description = "Random Bounds")
  expect_equal(GetLastSolution(oo)$objval, 3.55015170456942)
})



test_that("Quadratic Objective", {
  num.v <- 4
  oo <- Initialize(num.v, optimizer = "gurobi")
  SetObjective(oo, objective = rep(0, num.v), 
               quad.obj = diag(x = rep(1, num.v)),
               description = "Quad dist to origin", direction = "min")

  AddConstraint(oo, description = "sum(x_i) > 1",
                mat = rep(1, num.v), 
                rhs = 1, 
                dir = ">")

  expect_equal(GetLastSolution(oo)$objval, stop("paste val here"))
})
