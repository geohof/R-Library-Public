#' Tools to solve LP problems
#' 
#' Helps formulating constraints and obectives for 
#' Linear Programming (LP) problems.
#' 
#' @name lp.tools
#' @aliases Initialize
#' @aliases RunLP
#' @param grid.id A unique identifier for each grid tile.
#' @param lon A longitude value.
#' @param lat A latitiude value.
#' @return 
#' ToDo
#' @note %% ~~further notes~~
#' @author Georg
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2

#' @export
#' @examples
#' require(lp.tools)
#' require(Matrix)
#' num.v <- 10
#' set.seed(60606)
#' Initialize(num.v)
#' SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")
#' AddConstraint(description = "Upper Bound",
#'               mat = Diagonal(num.v), 
#'               rhs = 1, 
#'               dir = "<=")
#' AddConstraint(description = "Random Bounds", 
#'               mat = matrix(runif(3 * num.v), ncol=num.v),
#'               rhs = 1, 
#'               dir = "<=")
#' AddConstraint(description = "Random Bound", 
#'               mat = runif(num.v),
#'               rhs = 1, 
#'               dir = "<=")
#' AddConstraint(description = "Random Bounds", 
#'               mat = Matrix(matrix(runif(3 * (num.v - 1)), ncol=(num.v - 1))),
#'               rhs = 1, 
#'               dir = "<=")
#' GetCostOfConstraint()
#' 
#' Initialize(num.v, lock.variables = rep(.3, 4))
#' 
#' SetObjective(objective = rep(1, num.v), description = "Sum", direction = "max")
#' AddConstraint(description = "Upper Bound", 
#'               mat = Diagonal(num.v), 
#'               rhs = 1, 
#'               dir = "<=")
#' AddConstraint(description = "Random Bounds", 
#'               mat = matrix(runif(3 * num.v), ncol=num.v),
#'               rhs = 1, 
#'               dir = "<=")
#' 
#' Initialize(num.v, lock.variables = rep(.3, 4))
#' 
#' SetObjective(objective.numer = runif(num.v), objective.denom  = runif(num.v), 
#'              description = "Fraction", direction = "max")
#' AddConstraint(description = "Upper Bound", 
#'               mat = Diagonal(num.v), 
#'               rhs = 1, 
#'               dir = "<=")
#' AddConstraint(description = "Random Bounds", 
#'               mat = matrix(runif(3 * num.v), ncol=num.v),
#'               rhs = 1, 
#'               dir = "<=")
#' RemoveConstraint(description = "Random Bounds")

#' @import Matrix


#' @export 
#' @rdname lp.tools
SetLPSolveScale <- function(lpsolve.scale){
  lp.env$lpsolve.scale <- lpsolve.scale
}

#' @export 
#' @rdname lp.tools
Initialize <- function(num.v, optimizer, direction, lock.variables){
  
  
  lp.env$num.v <- num.v
  lp.env$const.description <- character(0)
  lp.env$const.rhs <- numeric(0)
  lp.env$const.dir <- character(0)
  if (!missing(optimizer)){
    lp.env$optimizer <- optimizer
  }
  if (!missing(direction)){
    lp.env$direction <- direction
  }
  if (!missing(lock.variables)){
    if(num.v==sum(!is.na(lock.variables))){
      warning("lp.tools: All variables are locked.")
    }
    if(length(lock.variables) < num.v){
      lock.variables <- c(lock.variables, rep(NA, num.v - length(lock.variables)))
    }
    lp.env$lock.variables <- lock.variables
    lp.env$num.unlocked <- sum(is.na(lock.variables))
  }else{
    lp.env$lock.variables <- rep(NA, num.v)
  }
  lp.env$lock.filter <- !is.na(lp.env$lock.variables)
  lp.env$unlock.filter <- !lp.env$lock.filter
  lp.env$num.unlocked <- sum(lp.env$unlock.filter)
  lp.env$num.locked <- sum(lp.env$lock.filter)
  lp.env$locked.vec <- lp.env$lock.variables[lp.env$lock.filter]
  lp.env$const.mat <- sparseMatrix(
    i = integer(0), j = integer(0), 
    x = numeric(0), dims = c(0, lp.env$num.unlocked))
}

#' @export 
#' @rdname Backup
#' @title Backup

Backup <- function(){
  for(var.name in ls(lp.env, all.names=TRUE)){
    backup.lp.env[[var.name]] <- lp.env[[var.name]]
  }
}

#' @export 
#' @rdname lp.tools
Restore <- function(){
  for(var.name in ls(backup.lp.env, all.names=TRUE)){
    lp.env[[var.name]] <- backup.lp.env[[var.name]]
  }
}


#' @export 
#' @rdname lp.tools
AddConstraint <- function(description = "", mat, rhs, dir, 
                          avoid.duplicates = FALSE, rerun = TRUE, 
                          feedback = TRUE){
  #if (length(is.na(mat))==1 & is.na(mat)[1]){
  if (class(mat)=="logical"){
    if(is.na(mat)[1]){
      if(feedback)
        cat(lp.env$GetTimeStamp(),
          "No constraint added for ", 
          paste(unique(description), collapse = ", "), "\r\n")
      return(invisible(0))
    }
  }
  #}else{
    if (class(mat)=="dsparseVector"){
      mat <- as.vector(mat)
    }
    if (is.null(nrow(mat))){
      mat <- matrix(data = mat, nrow = 1)
    }
    #mat <- as(mat, Class = "dgCMatrix")
    #mat <- as(Matrix(mat), Class = "dgCMatrix")
    #mat <- Matrix(mat)
    mat <- drop0(x = mat)
    if (ncol(mat) < lp.env$num.v){
      zero.mat <- sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                               dims = c(nrow(mat), lp.env$num.v - ncol(mat)))
      mat <- cBind(mat, zero.mat)
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
#     print("mat")
#     print(mat)
#     print(rowSums(mat))
#     print(class(abs.mat))
#     print(abs.mat)
#     print(rowSums(abs.mat))
    
    which.valid <- !(is.na(rowSums(mat)) | is.na(rhs))
    mat <- mat[which.valid, , drop=F]
    rhs <- rhs[which.valid]
    dir <- dir[which.valid]
    description <- description[which.valid]
    if (sum(which.valid) > 0){
      # Now apply variable locking
      mat[,lp.env$unlock.filter]
#       unlocked.mat <- sparseMatrix(
#         dims = c(lp.env$num.v, lp.env$num.unlocked),
#         i = which(lp.env$unlock.filter),
#         j = 1:lp.env$num.unlocked,
#         x = rep(1, lp.env$num.unlocked))
#       
#       locked.mat <- sparseMatrix(
#         dims = c(lp.env$num.v, lp.env$num.locked),
#         i = which(lp.env$lock.filter),
#         j = 1:lp.env$num.locked,
#         x = rep(1, lp.env$num.locked))
      
                
      rhs.reduction <- as.vector(mat[,lp.env$lock.filter, drop=FALSE] %*% 
           lp.env$locked.vec)
      rhs <- rhs - rhs.reduction
      rhs[abs(rhs) < 1E-6] <- 0 
      mat <- mat[,lp.env$unlock.filter, drop=FALSE]
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
      abs.mat <- as(abs(mat), Class = "dgCMatrix")
      norm.matrix <- Matrix::Diagonal(x = 1 / rowSums(abs.mat))
      mat <- norm.matrix %*% mat 
      rhs <- as.vector(norm.matrix %*% rhs)
      #      mat <- drop0(x = mat, tol = 1e-4)
      if (avoid.duplicates){
        f <- rep(TRUE, length(rhs))
        for(i in seq(length.out = nrow(mat))){
          m <- apply(lp.env$const.mat, MARGIN = 1, function(x) identical(x, mat[i,]))
          if (sum(m) > 0){
            f[i] <- FALSE
          }
        }
        mat <- mat[f, , drop = FALSE]
        rhs <- rhs[f]
        dir <- dir[f]
        description <- description[f]
      }

      lp.env$const.description <- c(lp.env$const.description, description)
      lp.env$const.mat <- rBind(lp.env$const.mat, mat)
      lp.env$const.rhs <- c(lp.env$const.rhs, rhs)
      lp.env$const.dir <- c(lp.env$const.dir, dir)
      ret.value <- length(rhs)
    }else{
      ret.value <- 0
    }
    if(ret.value > 0){
      UpdateFractionalObjective()
      if(feedback)
        cat(lp.env$GetTimeStamp(),
            "Number of inequalities added for ", 
            paste(unique(description), collapse = ", "), ": ", 
            length(rhs), ". ", sep = "")
      if(rerun){
        ret <- RunLP(dont.stop = TRUE)        
        if (ret$status > 0){
          if(feedback)
            cat(ret$status.message)
        }else{
          if(feedback)
            cat("Optimal ", lp.env$obj.description, ": ", 
              ret$objval, sep = "")
        }
      }
      if(feedback)
        cat("\r\n")
    }else{
      if(feedback)
        cat(lp.env$GetTimeStamp(), "No constraint added.\r\n", sep="")
    }
  return(invisible(ret.value))
}  

UpdateFractionalObjective <- function(){
  if (lp.env$fractional == TRUE){
    lp.env$trans.const.mat <- cBind(lp.env$const.mat, -lp.env$const.rhs)
    lp.env$trans.const.mat <- 
      rBind(lp.env$trans.const.mat, 
            c(lp.env$objective.denom, lp.env$constant.denom))
    lp.env$trans.const.rhs <- c(rep(0, nrow(lp.env$const.mat)), 1)
    lp.env$trans.const.dir <- c(lp.env$const.dir, "=")
  }
}

#' @export 
#' @rdname lp.tools
#' @param objective If this parameter is specified, then it defines the 
#' objective. If it is not sepcified, then the objective is fractional 
#' and is defined by the other four parameters. 
SetObjective <- function(objective, constant.numer = 0, objective.numer,
                         constant.denom = 0, objective.denom, scale.factor = 1, 
                         direction, description = "value"){
  lp.env$obj.description <- description
  if (!missing(direction)){
    lp.env$direction <- direction
  }
  if(!missing(objective)){
    lp.env$fractional <- FALSE
    lp.env$objective.full <- c(objective, rep(0, lp.env$num.v - length(objective)))
    lp.env$objective <- lp.env$objective.full[lp.env$unlock.filter]
  }else{
    lp.env$fractional <- TRUE
    lp.env$constant.numer <- (constant.numer  +
      sum(lp.env$locked.vec * objective.numer[lp.env$lock.filter])) / scale.factor
    lp.env$objective.numer <- objective.numer[lp.env$unlock.filter] / scale.factor
    lp.env$constant.denom <- (constant.denom +
      sum(lp.env$locked.vec * objective.denom[lp.env$lock.filter])) / scale.factor
    lp.env$objective.denom <- objective.denom[lp.env$unlock.filter] / scale.factor
  
    lp.env$trans.objective <- c(lp.env$objective.numer, lp.env$constant.numer)

  }
}

#' @export 
#' @rdname lp.tools
RunLP <- function(dont.stop = FALSE){
  if (is.null(lp.env$fractional)){
    stop("Use SetObjective before you call RunLP.")
  }else if (!lp.env$fractional){    
    objective <- lp.env$objective
    const.mat <- lp.env$const.mat
    const.dir <- lp.env$const.dir
    const.rhs <- lp.env$const.rhs
  }else{
    objective <- lp.env$trans.objective
    const.mat <- lp.env$trans.const.mat
    const.dir <- lp.env$trans.const.dir
    const.rhs <- lp.env$trans.const.rhs
  }
  
  if (lp.env$optimizer == "lpsolve"){
    tmp.sol <- lp(direction = lp.env$direction, 
                  objective.in = objective, 
                  #const.mat = as.matrix(const.mat),
                  const.dir = const.dir,
                  const.rhs = const.rhs,
                  dense.const = GetThreeCols(const.mat),
                  ,scale = lp.env$lpsolve.scale
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
    if(tmp.sol$status > 0){
      ret <- list(objval = NA, solution = NA,
                  status = tmp.sol$status, status.message = status.message)
    }else{
      sol <- rep(0, lp.env$num.v)
      ret <- list(status = tmp.sol$status, status.message = status.message)
      if(lp.env$fractional){
        ret$t <- tmp.sol$solution[lp.env$num.unlocked + 1L]
        sol[lp.env$unlock.filter] <- tmp.sol$solution[1:lp.env$num.unlocked]
        sol <- sol / ret$t
        sol[lp.env$lock.filter] <- lp.env$locked.vec
        ret$objval <- tmp.sol$objval
      }else{
        sol[lp.env$lock.filter] <- lp.env$locked.vec
        sol[lp.env$unlock.filter] <- tmp.sol$solution
        ret$objval <- sum(sol * lp.env$objective.full)  
      }
      ret$solution <- sol
    }
    
  }else if (optimizer == "gurobi"){
    model <- list()
    model$A <- const.mat
    model$obj <- objective
    model$sense <- const.dir
    model$rhs <- const.rhs
    model$modelsense <- lp.env$direction
    sink("Output/gurobi.txt")
    tmp.sol <- gurobi(model)
    sink()
    #    unlink("Output/gurobi.txt")
    ret <- list(objval = tmp.sol$objval, solution = tmp.sol$x,
                status = tmp.sol$status)
  }
  # max(const.mat %*% tmp.sol.l$solution - const.rhs)
  # max(const.mat %*% tmp.sol.g$x - const.rhs)
  lp.env$last.solution <- ret
  return(ret)
}

#' @export 
#' @rdname lp.tools
GetValue <- function(object.name){
  return(lp.env[[object.name]])
}

#' @export 
#' @rdname lp.tools
GetThreeCols <- function(mat){
  mat <- as(mat, Class = "dgCMatrix")
  df <- data.frame(
    i = mat@i + 1, 
    j = rep(1:length(mat@p[-1]), diff(mat@p)), 
    x = mat@x)
  return(as.matrix(df))
}

#' @export 
#' @rdname lp.tools
GetCostOfConstraint <- function(exclude = character(0)){
  tmp.sol <- RunLP(dont.stop = TRUE)
  target.objval <- tmp.sol$objval
  tmp.const.mat <- lp.env$const.mat
  tmp.const.rhs <- lp.env$const.rhs
  tmp.const.dir <- lp.env$const.dir
  tmp.const.description <- lp.env$const.description
  
  unique.description <- unique(tmp.const.description)
  unique.description <- unique.description[!unique.description %in% exclude]
  
  num.const <- length(unique.description)
  
  objval.vec <- numeric(0)
  status.message.vec <- numeric(0)
  for(i in 1:num.const){
    f <- tmp.const.description!=unique.description[i]
    lp.env$const.mat <- tmp.const.mat[f, , drop = FALSE]
    lp.env$const.rhs <- tmp.const.rhs[f]
    lp.env$const.dir <- tmp.const.dir[f]
    lp.env$const.description <- tmp.const.description[f]
    UpdateFractionalObjective()
    tmp.sol <- RunLP(dont.stop = TRUE)  
    objval.vec[i] <- tmp.sol$objval
    status.message.vec[i] <- tmp.sol$status.message
  }
  
  lp.env$const.mat <- tmp.const.mat
  lp.env$const.rhs <- tmp.const.rhs 
  lp.env$const.dir <- tmp.const.dir 
  lp.env$const.description <- tmp.const.description
  UpdateFractionalObjective()
  
  tmp.sign <- ifelse(lp.env$direction=="max", 1, -1)
  ret <- data.frame(desc = unique.description, 
                    objective = objval.vec, 
                    cost = tmp.sign * (objval.vec - target.objval), 
                    message = status.message.vec)
  ret <- ret[order(ret$objective * tmp.sign, decreasing = TRUE),]
  return(ret)
}

#' @export 
#' @rdname lp.tools
GetLastSolution <- function(){
  return(lp.env$last.solution)
}

#' @export 
#' @rdname lp.tools
RemoveConstraint <- function(description, rerun = TRUE, feedback = TRUE){
  f <- lp.env$const.description!=description
  lp.env$const.mat <- lp.env$const.mat[f, , drop=FALSE]
  lp.env$const.rhs <- lp.env$const.rhs[f]
  lp.env$const.dir <- lp.env$const.dir[f]
  lp.env$const.description <- lp.env$const.description[f]
  UpdateFractionalObjective()
  if(feedback)
    cat(lp.env$GetTimeStamp(),
        "Number of inequalities removed for ", 
        paste(unique(description), collapse = ", "), ": ", 
        sum(!f), ". ", sep = "")
  if(rerun){
    ret <- RunLP(dont.stop = TRUE)
    if (ret$status > 0){
      if(feedback)
        cat(ret$status.message)
    }else{
      if(feedback)
        cat("Optimal ", lp.env$obj.description, ": ", ret$objval, sep = "")
    }
  }
  if(feedback)
    cat("\r\n")
}

#' @export 
#' @rdname lp.tools
CheckConstraints <- function(solution, exclude = character(0),
                             tolerance = 1E-6){
  unique.description <- unique(lp.env$const.description)
  unique.description <- unique.description[!unique.description %in% exclude]
#   if(length(unique.description) == 0){
#     cat("No constraints to check.")
#     return(data.frame())
#   }
  if (!all(solution[lp.env$lock.filter]==lp.env$locked.vec)){
    cat("Locked variables not reflected in this solution.\r\n")
  }
  lhs <- as.vector(lp.env$const.mat %*% solution[lp.env$unlock.filter])
  rhs <- lp.env$const.rhs
#   tmp.ret <- 
#     ifelse(lp.env$const.dir=="<=", lhs <= rhs + tolerance * abs(rhs),
#            ifelse(lp.env$const.dir=="=", abs(lhs - rhs) < tolerance * abs(rhs),# lhs  == rhs,
#                   ifelse(lp.env$const.dir==">=", lhs  >= rhs - tolerance * abs(rhs), NA)))
  tmp.ret <- 
    ifelse(lp.env$const.dir=="<=", lhs <= rhs + tolerance,
    ifelse(lp.env$const.dir=="=", abs(lhs - rhs) < tolerance,
    ifelse(lp.env$const.dir==">=", lhs  >= rhs - tolerance, NA)))
  if(sum(is.na(tmp.ret)) > 0) stop("Unknown constraint direction.")
  ret.tab <- data.frame(description = character(0),
                        num.inequality = integer(0),
                        num.violation = integer(0))
  for(i in seq(length.out = length(unique.description))){
    f <- lp.env$const.description == unique.description[i]
    num.viol <- sum(!tmp.ret[f])
    inc.tab <- data.frame(description = unique.description[i],
                          num.inequality = sum(f),
                          num.violation = num.viol)
#                           ,
#                           tmp.out1 = 1E6 * (lhs[f] - rhs[f])[1],
#                           tmp.out2 = lp.env$const.dir[f][1],
#                           rhs = rhs[f][1],
#                           lhs = lhs[f][1],
#                           tmp.out3 = (lhs <= rhs + tolerance * abs(rhs))[f][1],
#                           tmp.out4 = (lhs <= rhs + tolerance)[f][1])
    ret.tab[i, ] <- inc.tab
  }
  f <- lp.env$const.description %in% unique.description
  tmp.detail <- data.frame(description = lp.env$const.description, lhs, rhs, 
                           dir = lp.env$const.dir, ok = tmp.ret)
  return(list(summary = ret.tab, 
              detail = tmp.detail[f,]))
}
