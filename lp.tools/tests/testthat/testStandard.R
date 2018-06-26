library(lp.tools)
context("SimpleExamples")

test_that("Simple Example", {
  num.v <- 10
  set.seed(60606)
  lpp <- 
    LPProblem$new(num.v)$
    SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
    AddConstraint(description = "Upper Bound",
                  mat = Diagonal(num.v), 
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                  mat = matrix(runif(3 * num.v), ncol=num.v),
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bound", 
                  mat = runif(num.v),
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                  mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
                  rhs = 1, 
                  dir = "<=")
  expect_equal(lpp$GetSolution()$objval, 
               2.515656365)
  expect_equal(lpp$GetSolution()$solution, 
               c(0, 0, 0.08752517, 0, 0, 1, 0, 0, 0.91715995, 0.51097124))
  coc <- lpp$GetCostOfConstraint()
  expect_equal(coc, structure(list(
    desc = structure(c(2L, 3L, 1L), .Label = c("Random Bound", "Random Bounds", "Upper Bound"), class = "factor"), 
    objective = c(6.45525582328322, 2.51698939156604, 2.51565636532287), 
    cost = c(3.93959945796035, 0.00133302624317144, 0), message = structure(c(1L, 1L, 1L), .Label = "", class = "factor")), 
    .Names = c("desc", "objective", "cost", "message"), row.names = c(2L, 1L, 3L), class = "data.frame"))
  # No Side effects:
  expect_equal(lpp$GetSolution()$objval, 
               2.515656365)
  expect_equal(lpp$GetSolution()$solution, 
               c(0, 0, 0.08752517, 0, 0, 1, 0, 0, 0.91715995, 0.51097124))
  lpp$RemoveConstraint("Upper Bound")
  expect_equal(lpp$GetSolution()$objval, 2.51698939156604)
  lpp$RemoveConstraint("Random Bounds")
  expect_equal(lpp$GetSolution()$objval, 134.753647467034)
})

test_that("Lock Variables", {
  num.v <- 10
  set.seed(60606)
  lpp <- 
    LPProblem$new(num.v, lock.variables = rep(.3, 4))$
    SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
    AddConstraint(description = "Upper Bound", 
                  mat = Diagonal(num.v), 
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                  mat = matrix(runif(3 * num.v), ncol=num.v),
                  rhs = 1, 
                  dir = "<=")
    expect_equal(lpp$GetSolution()$solution, 
               c(0.3, 0.3, 0.3, 0.3, 0.145551940979471, 1, 0, 0, 0.0903138608130597, 0))
})
  
test_that("Lock Variables (Gurobi)", {
  require(gurobi)
  num.v <- 10
  set.seed(60606)
  lpp <- 
    LPProblem$new(num.v, lock.variables = rep(.3, 4), optimizer = "gurobi")$
    SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
    AddConstraint(description = "Upper Bound", 
                  mat = Diagonal(num.v), 
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                mat = matrix(runif(3 * num.v), ncol=num.v),
                rhs = 1, 
                dir = "<=")

    expect_equal(lpp$GetSolution()$solution, 
                 c(0.3, 0.3, 0.3, 0.3, 0.145551940979471, 1, 0, 0, 0.0903138608130597, 0))
})
  
test_that("Fractional objective", {
  num.v <- 10
  set.seed(60606)
  lpp <- 
    LPProblem$new(num.v, lock.variables = rep(.3, 4))$
    SetObjective(objective.numer = runif(num.v), objective.denom  = runif(num.v), 
                 description = "Fraction", direction = "max")$
    AddConstraint(description = "Upper Bound", 
                  mat = Diagonal(num.v), 
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                  mat = matrix(runif(3 * num.v), ncol=num.v),
                  rhs = 1, 
                  dir = "<=")$
    RemoveConstraint(description = "Random Bounds")
  expect_equal(lpp$GetSolution()$objval, 3.55015170456942)
  cc <- lpp$CheckConstraints(solution = lpp$GetSolution()$solution + .01)
  expect_equal(cc$summary$num.violation, 2)
})

test_that("Quadratic Objective", {
  num.v <- 4
  lpp <- 
    LPProblem$new(num.v, optimizer = "gurobi")$
    SetObjective(objective = rep(0, num.v), 
                 quad.obj = diag(x = rep(1, num.v)),
                 description = "Quad dist to origin", direction = "min")$
    AddConstraint(description = "sum(x_i) >= 1",
                  mat = rep(1, num.v), 
                  rhs = 1, 
                  dir = ">=")
  expect_equal(lpp$GetSolution()$solution, rep(.25, 4))
  expect_equal(lpp$GetSolution()$objval, .25)
})

test_that("Quadratic Constraint", {
  num.v <- 4
  lpp <- 
    LPProblem$new(num.v, optimizer = "gurobi")$
    SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
    AddQuadConstraint(description = "Inside Circle", 
                          mat = diag(x = rep(1, num.v)), rhs = 1)
  expect_equal(lpp$GetSolution()$solution, rep(.5, 4))
  expect_equal(lpp$GetSolution()$objval, 2)
})


test_that("Cloning", {
  num.v <- 10
  set.seed(60606)
  lpp <- 
    LPProblem$new(num.v)$
    SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
    AddConstraint(description = "Upper Bound",
                  mat = Diagonal(num.v), 
                  rhs = 1, 
                  dir = "<=")$
    AddConstraint(description = "Random Bounds", 
                  mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
                  rhs = 1, 
                  dir = "<=")
  lpp2 <- lpp$clone()
  lpp$AddConstraint(description = "Random Bounds", 
                    mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
                    rhs = 1, 
                    dir = "<=")
  expect_equal(lpp$GetSolution()$objval, 3.18024089759523)
  lpp2$AddConstraint(description = "Random Bounds", 
                     mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
                     rhs = 1, 
                     dir = "<=")
  expect_equal(lpp$GetSolution()$objval, 3.18024089759523)
  expect_equal(lpp2$GetSolution()$objval, 3.28221554142143)
})
