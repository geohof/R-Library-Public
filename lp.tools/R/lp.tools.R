#' Tools to solve LP problems
#' 
#' Helps formulating constraints and obectives for 
#' Linear Programming (LP) problems.
#' 
"_PACKAGE"




#' Solve a Linear Programming (LP) problem with \code{lp.tools}
#' 
#' @description The \code{LPProblem} class is the core of the \code{lp.tools} package. TODO: more
#' @details Details: TODO.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords Linear Programming, LP
#' @return Object of \code{\link{R6Class}} LPP 
#' @format \code{\link{R6Class}} object.
#' @field num.v Number of variables in the liner problem.
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{LPProblem$new(...)}}{Creates a new \code{LPProblem}. Usually the first step, before providing more information about the LP Problem.}
#'   \item{\code{AddConstraint(...)}}{Add one or more linear constraints.}
#'   \item{\code{TODO: All other methods}}{TODO}
#'   
#' }
#' 
#' @section Actives (aka Properties):
#'   
#' \describe{
#'  \item{\code{num.v}}{Number of variables in the liner problem.}
#'  
#' }
#' 
#' @usage # lpp <- LPProblem$new(num.v = 10)
#'
#' @export
#' @examples
#' 
#' require(lp.tools)
#' require(Matrix)
#' num.v <- 10
#' set.seed(60606)
#' lpp <- 
#'   LPProblem$new(num.v)$
#'   SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")$
#'   AddConstraint(description = "Upper Bound",
#'                 mat = Diagonal(num.v), 
#'                 rhs = 1, 
#'                 dir = "<=")$
#'   AddConstraint(description = "Random Bounds", 
#'                 mat = matrix(runif(3 * num.v), ncol=num.v),
#'                 rhs = 1, 
#'                 dir = "<=")$
#'   AddConstraint(description = "Random Bound", 
#'                 mat = runif(num.v),
#'                 rhs = 1, 
#'                 dir = "<=")$
#'   AddConstraint(description = "Random Bounds", 
#'                 mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
#'                 rhs = 1, 
#'                 dir = "<=")
#' lpp$GetCostOfConstraint()
#' lpp$RemoveConstraint("UpperBounds")

#' LPProblem$new(num.v, lock.variables = rep(.3, 4))$
#' SetObjective(objective = rep(1, num.v), 
#'              description = "Sum", direction = "max")$
#' AddConstraint(description = "Upper Bound", 
#'               mat = Diagonal(num.v), 
#'               rhs = 1, 
#'               dir = "<=")$
#' GetSolution()
#' 
#' set.seed(60606)
#' lpp <- 
#'   LPProblem$new(num.v, lock.variables = rep(.3, 4))$
#'   SetObjective(objective.numer = runif(num.v), 
#'                objective.denom  = runif(num.v), 
#'                description = "Fraction", direction = "max")$
#'   AddConstraint(description = "Upper Bound", 
#'                 mat = Diagonal(num.v), 
#'                 rhs = 1, 
#'                 dir = "<=")
#' lpp$CheckConstraints(solution = lpp$GetSolution()$solution + .01)



#' @import Matrix
#' @import methods
#' @import R6


LPProblem <- R6Class("LPProblem", public = list(
  num.v = 0,
  optimizer = "lpsolve",
  direction = "max",
  lock.variables = numeric(0),
  lpsolve.scale = 0, 
  gurobi.params = list(), 
  gurobi.output = "gurobi.txt",
  const.description = character(0),
  const.rhs = numeric(0),
  const.dir = character(0),
  lock.filter = logical(0),
  unlock.filter = logical(0),
  num.unlocked = 0,
  num.locked = 0,
  locked.vec = numeric(0),
  const.mat = sparseMatrix(
      i = integer(0), j = integer(0), 
      x = numeric(0), dims = c(0, 0)),
  obj.description = "",
  fractional = NULL,
  objective.full = NULL,
  objective = NULL,
  quad.obj.full = NULL,
  quad.obj = NULL,
  quad.const.list = NULL,
  quad.const.desc = NULL,
  constant.numer = NULL,
  objective.numer = NULL,
  constant.denom = NULL,
  objective.denom = NULL,
  trans.objective = NULL,
  last.solution = NULL,
  trans.const.mat = NULL,
  trans.const.rhs = NULL,
  trans.const.dir = NULL,
  GetTimeStamp = function()paste(format(Sys.time(), "%H-%M-%S"), ": ", sep=""),
  
  initialize = function(num.v, optimizer = "lpsolve", direction = "max", 
                         lock.variables,
                         lpsolve.scale = 0, 
                         gurobi.params = list(), 
                         gurobi.output = "gurobi.txt"
                         ){
    self$num.v <- num.v
    self$optimizer <- optimizer
    self$direction <- direction
    if (!missing(lock.variables)){
      self$lock.variables <- lock.variables
    }
    self$lpsolve.scale <- lpsolve.scale 
    self$gurobi.params <- gurobi.params
    self$gurobi.output <- gurobi.output

    if (optimizer=="gurobi"){
      ret <- CheckGurobi()
      if (ret != "Gurobi good to go."){
        stop(ret)
      }
    }
    if (!missing(direction)){
      self$direction <- direction
    }
    if (!missing(lock.variables)){
      if(num.v==sum(!is.na(lock.variables))){
        warning("lp.tools: All variables are locked.")
      }
      if(length(lock.variables) < num.v){
        lock.variables <- c(lock.variables, rep(NA, num.v - length(lock.variables)))
      }
      self$lock.variables <- lock.variables
      self$num.unlocked <- sum(is.na(lock.variables))
    }else{
      self$lock.variables <- rep(NA, num.v)
    }
    self$lock.filter <- !is.na(self$lock.variables)
    self$unlock.filter <- !self$lock.filter
    self$num.unlocked <- sum(self$unlock.filter)
    self$num.locked <- sum(self$lock.filter)
    self$locked.vec <- self$lock.variables[self$lock.filter]
    self$const.mat <- sparseMatrix(
      i = integer(0), j = integer(0), 
      x = numeric(0), dims = c(0, self$num.unlocked))
  },
  
  AddConstraint = function(description = "", mat, rhs, dir, 
                            avoid.duplicates = FALSE, 
                            row.normalization = TRUE, rerun = TRUE, 
                            feedback = TRUE){
    if (class(mat)=="logical"){
      if(is.na(mat)[1]){
        if(feedback)
          cat(self$GetTimeStamp(),
            "No constraint added for ", 
            paste(unique(description), collapse = ", "), "\r\n")
        return(invisible(0))
      }
    }
    if (class(mat)=="dsparseVector"){
      mat <- as.vector(mat)
    }
    if (is.null(nrow(mat))){
      mat <- matrix(data = mat, nrow = 1)
    }
    mat <- drop0(x = mat)
    if (ncol(mat) < self$num.v){
      zero.mat <- sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                               dims = c(nrow(mat), self$num.v - ncol(mat)))
      mat <- cbind(mat, zero.mat)
    }
    num.const <- nrow(mat)
    if(length(description)==1){
      description <- rep(description, num.const)    
    }
    if(length(rhs)==1){
      rhs <- rep(rhs, num.const)
    }
    if(length(dir)==1){
      dir <- rep(dir, num.const)
    }

    which.valid <- !(is.na(rowSums(mat)) | is.na(rhs))
    mat <- mat[which.valid, , drop=F]
    rhs <- rhs[which.valid]
    dir <- dir[which.valid]
    description <- description[which.valid]
      if (sum(which.valid) > 0){
        # Now apply variable locking
#       unlocked.mat <- sparseMatrix(
#         dims = c(self$num.v, self$num.unlocked),
#         i = which(self$unlock.filter),
#         j = 1:self$num.unlocked,
#         x = rep(1, self$num.unlocked))
#       
#       locked.mat <- sparseMatrix(
#         dims = c(self$num.v, self$num.locked),
#         i = which(self$lock.filter),
#         j = 1:self$num.locked,
#         x = rep(1, self$num.locked))
      
                
        rhs.reduction <- as.vector(mat[,self$lock.filter, drop=FALSE] %*% 
             self$locked.vec)
        rhs <- rhs - rhs.reduction
        rhs[abs(rhs) < 1E-6] <- 0 
        mat <- mat[,self$unlock.filter, drop=FALSE]
        f <- rowSums(mat ^ 2) > 0
        if  (sum(((rhs[!f] < 0) & (dir[!f] %in% c("<=", "="))) |
                 ((rhs[!f] > 0) & (dir[!f] %in% c(">=", "=")))) > 0)
          warning("Blocking contradicts constraint.")
        mat <- mat[f, , drop = FALSE]
        rhs <- rhs[f]
        dir <- dir[f]
        description <- description[f]
        # Apply normalization
        # norm.matrix <- Diagonal(x = abs(1 / rhs))
        if (row.normalization){
          abs.mat <- as(abs(mat), Class = "dgCMatrix")
          norm.matrix <- Matrix::Diagonal(x = 1 / rowSums(abs.mat))
          mat <- norm.matrix %*% mat 
          rhs <- as.vector(norm.matrix %*% rhs)
        }
        #      mat <- drop0(x = mat, tol = 1e-4)
        if (avoid.duplicates){
          f <- rep(TRUE, length(rhs))
          for(i in seq(length.out = nrow(mat))){
            m <- apply(self$const.mat, MARGIN = 1, function(x) identical(x, mat[i,]))
            if (sum(m) > 0){
              f[i] <- FALSE
            }
          }
          mat <- mat[f, , drop = FALSE]
          rhs <- rhs[f]
          dir <- dir[f]
          description <- description[f]
        }

        self$const.description <- c(self$const.description, description)
        self$const.mat <- rbind(self$const.mat, mat)
        self$const.rhs <- c(self$const.rhs, rhs)
        self$const.dir <- c(self$const.dir, dir)
        ret.value <- length(rhs)
      }else{
        ret.value <- 0
      }
    if(ret.value > 0){
      self$UpdateFractionalObjective()
      if(feedback)
        cat(self$GetTimeStamp(),
            "Number of inequalities added for ", 
            paste(unique(description), collapse = ", "), ": ", 
            length(rhs), ". ", sep = "")
        if(rerun){
          ret <- self$Solve(dont.stop = TRUE)$GetSolution()        
          if (ret$status > 0){
            if(feedback)
              cat(ret$status.message)
          }else{
            if(feedback)
              cat("Optimal ", self$obj.description, ": ", 
                ret$objval, sep = "")
          }
        }
        if(feedback)
          cat("\r\n")
      }else{
        if(feedback)
          cat(self$GetTimeStamp(), "No constraint added.\r\n", sep="")
      }
    return(invisible(self))
  },

  SetObjective = function(objective, quad.obj, constant.numer = 0, objective.numer,
                          constant.denom = 0, objective.denom, scale.factor = 1, 
                          direction, description = "value"){
    self$obj.description <- description
    if (!missing(direction)){
      self$direction <- direction
    }
    if(!missing(objective)){
      self$fractional <- FALSE
      self$objective.full <- c(objective, rep(0, self$num.v - length(objective)))
      self$objective <- self$objective.full[self$unlock.filter]
      if(!missing(quad.obj)){
        self$quad.obj.full <- quad.obj
        self$quad.obj <- quad.obj[self$unlock.filter, self$unlock.filter]
      }else{
        self$quad.obj = NULL
      }
    }else{
      self$fractional <- TRUE
      self$constant.numer <- (constant.numer  +
        sum(self$locked.vec * objective.numer[self$lock.filter])) / scale.factor
      self$objective.numer <- objective.numer[self$unlock.filter] / scale.factor
      self$constant.denom <- (constant.denom +
        sum(self$locked.vec * objective.denom[self$lock.filter])) / scale.factor
      self$objective.denom <- objective.denom[self$unlock.filter] / scale.factor
  
      self$trans.objective <- c(self$objective.numer, self$constant.numer)
    }
    return(invisible(self))
  },

  UpdateFractionalObjective = function(){
    if (self$fractional == TRUE){
      self$trans.const.mat <- cbind(self$const.mat, -self$const.rhs)
      self$trans.const.mat <- 
        rbind(self$trans.const.mat, 
              c(self$objective.denom, self$constant.denom))
      self$trans.const.rhs <- c(rep(0, nrow(self$const.mat)), 1)
      self$trans.const.dir <- c(self$const.dir, "=")
    }
  },

  Solve = function(dont.stop = FALSE){
    if (is.null(self$fractional)){
      stop("Use SetObjective before you call Solve.")
    }else if (!self$fractional){    
      objective <- self$objective
      const.mat <- self$const.mat
      const.dir <- self$const.dir
      const.rhs <- self$const.rhs
    }else{
      objective <- self$trans.objective
      const.mat <- self$trans.const.mat
      const.dir <- self$trans.const.dir
      const.rhs <- self$trans.const.rhs
    }
    if (self$optimizer == "lpsolve"){
      if(!is.null(self$quad.const.list)){
        warning("Quadratic constraints ignored when optimizer is lpsolve.")
      }
      tmp.sol <- lp(direction = self$direction, 
                    objective.in = objective, 
                    #const.mat = as.matrix(const.mat),
                    const.dir = const.dir,
                    const.rhs = const.rhs,
                    dense.const = GetThreeCols(const.mat),
                    scale = self$lpsolve.scale
      )
      status.message <- ""
      if(tmp.sol$status==3){
        status.message <- "The model is unbounded."
      }else if(tmp.sol$status==2){
        status.message <- "The model is infeasible."
      }else if(tmp.sol$status==5){
        status.message <- "Numerical failure encountered."
      }else if(tmp.sol$status > 0){
        status.message <- paste("lpsolve status = ", tmp.sol$status, sep="")  
      }
      if (nchar(status.message) > 0 & !dont.stop){
        stop(paste(status.message, 
              " See http://lpsolve.sourceforge.net/5.5/solve.htm", sep=""))
      }
    }else if (self$optimizer == "gurobi"){
      model <- list()
      model$A <- const.mat
      model$obj <- objective
      model$sense <- const.dir
      model$rhs <- const.rhs
      model$modelsense <- self$direction
      if(!is.null(self$quad.obj)){
        if(self$fractional == TRUE){
          stop("Currently, fractional objectives can't be combined with quadratic objectives.")
        }
        model$Q <- self$quad.obj
      }
      if(!is.null(self$quad.const.list)){
        if(self$fractional == TRUE){
          stop("Currently, fractional objectives can't be combined with quadratic constaints.")
        }
        model$quadcon <- self$quad.const.list
      }
      if((!is.null(self$quad.const.list)) | (!is.null(self$quad.obj))){
        if(self$num.locked > 0){
          stop("Currently, variable locking can't be combined with quadratic objectives or constaints.")
        }
        model$quadcon <- self$quad.const.list
      }
      sink(self$gurobi.output)
      tmp.sol <- gurobi(model, self$gurobi.params)
      sink()
      status.message <- tmp.sol$status
      #    unlink("Output/gurobi.txt")
      tmp.sol <- list(objval = tmp.sol$objval, solution = tmp.sol$x,
                      status = tmp.sol$status)
      if(tmp.sol$status == "OPTIMAL"){
        tmp.sol$status <- 0
      }else{
        tmp.sol$status <- 1
      }
    }
    if(tmp.sol$status > 0){
      ret <- list(objval = NA, solution = NA,
                  status = tmp.sol$status, status.message = status.message)
    }else{
      sol <- rep(0, self$num.v)
      ret <- list(status = tmp.sol$status, status.message = status.message)
      if(self$fractional){
        ret$t <- tmp.sol$solution[self$num.unlocked + 1L]
        sol[self$unlock.filter] <- tmp.sol$solution[1:self$num.unlocked]
        sol <- sol / ret$t
        sol[self$lock.filter] <- self$locked.vec
        ret$objval <- tmp.sol$objval
      }else{
        sol[self$lock.filter] <- self$locked.vec
        sol[self$unlock.filter] <- tmp.sol$solution
        ret$objval <- tmp.sol$objval 
        if(ret$objval != tmp.sol$objval){
          stop("Objective value from optimizer: ", tmp.sol$objval, 
               "  Calucalted objective value: ", sum(sol * self$objective.full))
        }
      }
      ret$solution <- sol
    }
    # max(const.mat %*% tmp.sol.l$solution - const.rhs)
    # max(const.mat %*% tmp.sol.g$x - const.rhs)
    self$last.solution <- ret
    return(invisible(self))
  },

  RemoveConstraint = function(description, rerun = TRUE, feedback = TRUE){
    f <- self$const.description!=description
    self$const.mat <- self$const.mat[f, , drop=FALSE]
    self$const.rhs <- self$const.rhs[f]
    self$const.dir <- self$const.dir[f]
    self$const.description <- self$const.description[f]
    self$UpdateFractionalObjective()
    if(feedback)
      cat(self$GetTimeStamp(),
          "Number of inequalities removed for ", 
          paste(unique(description), collapse = ", "), ": ", 
          sum(!f), ". ", sep = "")
    if(rerun){
      ret <- self$Solve(dont.stop = TRUE)$GetSolution()
      if (ret$status > 0){
        if(feedback)
          cat(ret$status.message)
      }else{
        if(feedback)
          cat("Optimal ", self$obj.description, ": ", ret$objval, sep = "")
      }
    }
    if(feedback)
      cat("\r\n")
    return(invisible(self))
  },

  GetSolution = function(){
    return(self$last.solution)
  },

  GetCostOfConstraint = function(exclude = character(0)){
    tmp.sol <- self$Solve(dont.stop = TRUE)$GetSolution()
    target.objval <- tmp.sol$objval
    tmp.const.mat <- self$const.mat
    tmp.const.rhs <- self$const.rhs
    tmp.const.dir <- self$const.dir
    tmp.const.description <- self$const.description
  
    unique.description <- unique(tmp.const.description)
    unique.description <- unique.description[!unique.description %in% exclude]
  
    num.const <- length(unique.description)
  
    objval.vec <- numeric(0)
    status.message.vec <- numeric(0)
    for(i in 1:num.const){
      f <- tmp.const.description!=unique.description[i]
      self$const.mat <- tmp.const.mat[f, , drop = FALSE]
      self$const.rhs <- tmp.const.rhs[f]
      self$const.dir <- tmp.const.dir[f]
      self$const.description <- tmp.const.description[f]
      self$UpdateFractionalObjective()
      tmp.sol <- self$Solve(dont.stop = TRUE)$GetSolution()  
      objval.vec[i] <- tmp.sol$objval
      status.message.vec[i] <- tmp.sol$status.message
    }
  
    self$const.mat <- tmp.const.mat
    self$const.rhs <- tmp.const.rhs 
    self$const.dir <- tmp.const.dir 
    self$const.description <- tmp.const.description
    self$UpdateFractionalObjective()
   
    tmp.sign <- ifelse(self$direction=="max", 1, -1)
    ret <- data.frame(desc = unique.description, 
                      objective = objval.vec, 
                      cost = tmp.sign * (objval.vec - target.objval), 
                      message = status.message.vec)
    ret <- ret[order(ret$objective * tmp.sign, decreasing = TRUE),]
    return(ret)
  },

  CheckConstraints = function(solution, exclude = character(0),
                              tolerance = 1E-6){
    unique.description <- unique(self$const.description)
    unique.description <- unique.description[!unique.description %in% exclude]
#   if(length(unique.description) == 0){
#     cat("No constraints to check.")
#     return(data.frame())
#   }
    if (!all(solution[self$lock.filter]==self$locked.vec)){
      cat("Locked variables not reflected in this solution.\r\n")
    }
    lhs <- as.vector(self$const.mat %*% solution[self$unlock.filter])
    rhs <- self$const.rhs
#   tmp.ret <- 
#     ifelse(self$const.dir=="<=", lhs <= rhs + tolerance * abs(rhs),
#            ifelse(self$const.dir=="=", abs(lhs - rhs) < tolerance * abs(rhs),# lhs  == rhs,
#                   ifelse(self$const.dir==">=", lhs  >= rhs - tolerance * abs(rhs), NA)))
    tmp.ret <- 
      ifelse(self$const.dir=="<=", lhs <= rhs + tolerance,
      ifelse(self$const.dir=="=", abs(lhs - rhs) < tolerance,
      ifelse(self$const.dir==">=", lhs  >= rhs - tolerance, NA)))
    if(sum(is.na(tmp.ret)) > 0) stop("Unknown constraint direction.")
    ret.tab <- data.frame(description = character(0),
                          num.inequality = integer(0),
                          num.violation = integer(0),
                          stringsAsFactors = FALSE)
    for(i in seq(length.out = length(unique.description))){
      f <- self$const.description == unique.description[i]
      num.viol <- sum(!tmp.ret[f])
      inc.tab <- data.frame(description = unique.description[i],
                            num.inequality = sum(f),
                            num.violation = num.viol)
#                           tmp.out1 = 1E6 * (lhs[f] - rhs[f])[1],
#                           tmp.out2 = self$const.dir[f][1],
#                           rhs = rhs[f][1],
#                           lhs = lhs[f][1],
#                           tmp.out3 = (lhs <= rhs + tolerance * abs(rhs))[f][1],
#                           tmp.out4 = (lhs <= rhs + tolerance)[f][1])
      ret.tab[i, ] <- inc.tab
    }
    f <- self$const.description %in% unique.description
    tmp.detail <- data.frame(description = self$const.description, lhs, rhs, 
                             dir = self$const.dir, ok = tmp.ret)
    return(list(summary = ret.tab, 
                detail = tmp.detail[f,]))
  },

  AddQuadConstraint = function(description = "", mat, rhs = 0, rerun = TRUE, 
                               feedback = TRUE){
  # TODO: Build in intelligence about parameters
    if(is.null(self$quad.const.list)){
      self$quad.const.list <- list()
      self$quad.const.desc <- character(0)
    }
    l <- length(self$quad.const.list)
    quad.const <- list()
    quad.const$Qc <- mat
    quad.const$rhs <- rhs
    self$quad.const.list[[l + 1L]] <- quad.const
    self$quad.const.desc[l + 1L] <- description

    if(feedback){
      cat(self$GetTimeStamp(),
        "Added quadratic constraint for ",description, ". ", sep = "")
    }
    if(rerun){
      ret <- self$Solve(dont.stop = TRUE)$GetSolution()        
      if (ret$status > 0){
        if(feedback){
          cat(ret$status.message)
        }
      }else{
        if(feedback){
          cat("Optimal ", self$obj.description, ": ", 
              ret$objval, sep = "")
        }
      }
    }
    if(feedback){
      cat("\r\n")
    }
    return(invisible(self))
  }  
))

GetThreeCols <- function(mat){
  mat <- as(mat, Class = "dgCMatrix")
  df <- data.frame(
    i = mat@i + 1, 
    j = rep(1:length(mat@p[-1]), diff(mat@p)), 
    x = mat@x)
  return(as.matrix(df))
}

CheckGurobi <- function(){
  if(!"gurobi" %in% installed.packages()[, 1]){
    return("Gurobi R package not installed.")
  }else{  
    require(gurobi)
    gurobi.available <- TRUE
    model <- list(A = matrix(1), obj = 1, sense = "<=", rhs = 1) 
    r <- tryCatch(
      tmp.sol <- gurobi(model = model, params = list(OutputFlag = 0)),
      error = function(e) gurobi.available <<- FALSE
    )
    if(!gurobi.available){
      return("Gurobi not responging.")
    }else{
      return("Gurobi good to go.")
    }
  }
}
CheckGurobi()
