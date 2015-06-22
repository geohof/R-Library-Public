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
#' 4 + 5



#' @export 
#' @rdname lp.tools
Initialize <- function(num.v, optimizer, direction){
  lp.env$num.v <- num.v
  lp.env$const.description <- character(0)
  lp.env$const.mat <- matrix(data=0, nrow=0, ncol=num.v)
  lp.env$const.rhs <- numeric(0)
  lp.env$const.dir <- character(0)
  if (!missing(optimizer)){
    lp.env$optimizer <- optimizer
  }
  if (!missing(direction)){
    lp.env$direction <- direction
  }
}

#' @export 
#' @rdname Backup
#' @title Backup

Backup <- function(){
  backup.lp.env <- lp.env
}

#' @export 
#' @rdname lp.tools
Restore <- function(){
  lp.env <- backup.lp.env
}


#' @export 
#' @rdname lp.tools
AddConstraint <- function(description = "", mat, rhs, dir){
  if (length(is.na(mat))==1 & is.na(mat)[1]){
      cat("No constriant added for ", paste(unique(description), collapse = ", "), "\r\n")
  }else{
    if (class(mat)=="dgCMatrix"){
      mat <- as.matrix(mat)
    }
    if(!is.matrix(mat)){
      mat <- matrix(data = mat, nrow = 1)
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
      # norm.matrix <- Diagonal(x = abs(1 / rhs))
      norm.matrix <- Diagonal(x = 1 / rowSums(abs(mat)))
      mat <- as.matrix(norm.matrix %*% mat) 
      rhs <- as.vector(norm.matrix %*% rhs)
      lp.env$const.description <- c(lp.env$const.description, description)
      lp.env$const.mat <- rBind(lp.env$const.mat, mat)
      lp.env$const.rhs <- c(lp.env$const.rhs, rhs)
      lp.env$const.dir <- c(lp.env$const.dir, dir)
    }
    UpdateFractionalObjective()
    ret <- RunLP(dont.stop = TRUE)
    cat("Added ", sum(which.valid), " inequalites for ", paste(unique(description), collapse = ", "), "\r\n")
    if (ret$status > 0){
      cat(ret$status.message, "\r\n")
    }else{
      cat("Current objective value:", ret$objval, "\r\n")
    }
  }
}  

UpdateFractionalObjective <- function(){
  if (lp.env$fractional == TRUE){
    lp.env$trans.const.mat <- cbind(lp.env$const.mat, -lp.env$const.rhs)
    lp.env$trans.const.mat <- 
      rbind(lp.env$trans.const.mat, 
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
                         constant.denom = 0, objective.denom, scale.factor = 1){
  if(!missing(objective)){
    lp.env$fractional <- FALSE
    lp.env$objective <- objective
  }else{
    lp.env$fractional <- TRUE
    lp.env$constant.numer <- constant.numer / scale.factor
    lp.env$objective.numer <- objective.numer / scale.factor
    lp.env$constant.denom <- constant.denom / scale.factor
    lp.env$objective.denom <- objective.denom / scale.factor
  
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
                  const.mat = const.mat,
                  const.dir = const.dir,
                  const.rhs = const.rhs
                  #,scale = 0
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
      ret <- list(objval = tmp.sol$objval, solution = tmp.sol$solution,
                  status = tmp.sol$status, status.message = status.message)
      if(lp.env$fractional){
        ret$t <- tmp.sol$solution[lp.env$num.v + 1L]
        ret$solution <- tmp.sol$solution[1:lp.env$num.v] / ret$t
      }
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
  return(ret)
}

#' @export 
#' @rdname lp.tools
GetValue <- function(object.name){
  return(lp.env[[object.name]])
}


#' @export 
#' @rdname lp.tools
GetCostOfConstraint <- function(){
  tmp.sol <- RunLP(dont.stop = TRUE)
  target.objval <- tmp.sol$objval
  tmp.const.mat <- lp.env$const.mat
  tmp.const.rhs <- lp.env$const.rhs
  tmp.const.dir <- lp.env$const.dir
  tmp.const.description <- lp.env$const.description
  
  unique.description <- unique(tmp.const.description)
  num.const <- length(unique.description)
  
  objval.vec <- numeric(0)
  status.message.vec <- numeric(0)
  for(i in 1:num.const){
    f <- tmp.const.description!=unique.description[i]
    lp.env$const.mat <- as.matrix(tmp.const.mat[f,], nrow = sum(f))
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
  
  ret <- data.frame(desc = unique.description, 
                    objective = objval.vec, 
                    cost = objval.vec - target.objval, 
                    message = status.message.vec)
  ret <- ret[order(ret$cost, decreasing = TRUE),]
  return(ret)
}

#' @export 
#' @rdname lp.tools
RemoveConstraint <- function(description){
  f <- lp.env$description!=description
  lp.env$const.mat <- lp.env$const.mat[f, , drop=FALSE]
  lp.env$const.rhs <- lp.env$const.rhs[f]
  lp.env$const.dir <- lp.env$const.dir[f]
  lp.env$const.description <- lp.env$const.description[f]
  UpdateFractionalObjective()
  ret <- RunLP(dont.stop = TRUE)
  cat("Added ", sum(which.valid), " inequalites for ", paste(unique(description), collapse = ", "), "\r\n")
  if (ret$status > 0){
    cat(ret$status.message, "\r\n")
  }else{
    cat("Current objective value:", ret$objval, "\r\n")
  }
}