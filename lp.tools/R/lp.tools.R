#' Tools to solve LP problems
#' 
#' Helps formulating constraints and obectives for 
#' Linear Programming (LP) problems.
#' 
"_PACKAGE"


#' @name lp.tools
#' @title
#' Constraints
#' @description
#' Todo: add description
#' @param description A description of the constraint.
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

#' @name Constraints
#' @title
#' Linear constraints.
#' @description
#' TODO.

#' @export 
#' @rdname Constraints
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



#' @name Other
#' @title
#' TODO: Divi up
#' @description
#' TODO.


#' @export 
#' @rdname Other
SetLPSolveScale <- function(lpsolve.scale){
  lp.env$lpsolve.scale <- lpsolve.scale
}

#' @export 
#' @rdname Other
Initialize <- function(num.v, optimizer, direction, lock.variables){
  if("quad.const.list" %in% names(lp.env)){
    rm(list = c("quad.const.list", "quad.const.desc"), envir = lp.env)
  }
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
#' @rdname Other
Restore <- function(){
  for(var.name in ls(backup.lp.env, all.names=TRUE)){
    lp.env[[var.name]] <- backup.lp.env[[var.name]]
  }
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
#' @rdname Other
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
#' @rdname Other
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
    if("quad.const.list" %in% names(lp.env)){
      warning("Quadratic constraints ignored when optimizer is lpsolve.")
    }

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
    
  }else if (lp.env$optimizer == "gurobi"){
    model <- list()
    model$A <- const.mat
    model$obj <- objective
    model$sense <- const.dir
    model$rhs <- const.rhs
    model$modelsense <- lp.env$direction
    if("quad.const.list" %in% names(lp.env)){
      if(lp.env$fractional == TRUE){
        stop("Currently, fractional objectives can't be compined with quadratic constaints.")
      }
      model$quadcon <- lp.env$quad.const.list
    }
    sink(lp.env$gurobi.output)
    tmp.sol <- gurobi(model, lp.env$gurobi.params)
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
  # max(const.mat %*% tmp.sol.l$solution - const.rhs)
  # max(const.mat %*% tmp.sol.g$x - const.rhs)
  lp.env$last.solution <- ret
  return(ret)
}

#' @export 
#' @rdname Other
GetValue <- function(object.name){
  return(lp.env[[object.name]])
}
#' @export 
#' @rdname Other
SetValue <- function(object.name, value){
  lp.env[[object.name]] <- value
}
#' @export 
#' @rdname Other
GetThreeCols <- function(mat){
  mat <- as(mat, Class = "dgCMatrix")
  df <- data.frame(
    i = mat@i + 1, 
    j = rep(1:length(mat@p[-1]), diff(mat@p)), 
    x = mat@x)
  return(as.matrix(df))
}

#' @export 
#' @rdname Other
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
#' @rdname Other
GetLastSolution <- function(){
  return(lp.env$last.solution)
}

#' @export 
#' @rdname Other
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
#' @rdname Other
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

#' @export 
#' @rdname Other
AddQuadConstraint <- function(description = "", mat, rhs = 0, rerun = TRUE, 
                          feedback = TRUE){
  # TODO: Build in intelligence about parameters
  if(!"quad.const.list" %in% names(lp.env)){
    lp.env$quad.const.list <- list()
    lp.env$quad.const.desc <- character(0)
  }
  l <- length(lp.env$quad.const.list)
  quad.const <- list()
  quad.const$Qc <- mat
  quad.const$rhs <- rhs
  lp.env$quad.const.list[[l + 1L]] <- quad.const
  lp.env$quad.const.desc[l + 1L] <- description
  
  
  
  if(feedback){
    cat(lp.env$GetTimeStamp(),
      "Added quadratic constraint for ",description, ". ", sep = "")
  }
  if(rerun){
    ret <- RunLP(dont.stop = TRUE)        
    if (ret$status > 0){
      if(feedback){
        cat(ret$status.message)
      }
    }else{
      if(feedback){
        cat("Optimal ", lp.env$obj.description, ": ", 
            ret$objval, sep = "")
      }
    }
  }
  if(feedback){
    cat("\r\n")
  }
}  


lp.env <- new.env()
backup.lp.env <- new.env()
#require(lpSolve)
#require(Matrix)

assign(x = "optimizer", value = "lpsolve", envir = lp.env)  
assign(x = "direction", value = "max", envir = lp.env)  
assign(x = "lpsolve.scale", value = 0, envir = lp.env)  
assign(x = "gurobi.params", value = list(), envir = lp.env)  
assign(x = "gurobi.output", value = "gurobi.txt", envir = lp.env)  
assign(x = "GetTimeStamp", 
       value = function()paste(format(Sys.time(), "%H-%M-%S"), ": ", sep=""),
       envir = lp.env)  



.Random.seed <-
c(403L, 186L, -99621547L, 881978876L, 1539897559L, -590288460L, 
1724806629L, 1466198665L, -950264494L, 2117877957L, 690898096L, 
1952933295L, -432228335L, 1677602320L, -396467060L, 1551189345L, 
-463021799L, 131369369L, 714659189L, 988812028L, 772700713L, 
471264982L, 211817444L, -1385359328L, -683078826L, -436550605L, 
-636934970L, 1470428124L, -1104448765L, 1146236694L, -1838700031L, 
1864410078L, -897563563L, -1851847386L, -1301320415L, 1464629043L, 
430497839L, 1396144146L, 467920397L, 2080319552L, 991010667L, 
-2036518246L, 1215687996L, 656152725L, -1415425726L, -2060680164L, 
1571958893L, -484684929L, 600220301L, -1372585648L, -1693146005L, 
-2037231825L, -1718991005L, 644821190L, 1206315761L, 284933050L, 
-660072200L, 1306075811L, -701486157L, -1780116324L, 1730137201L, 
155656656L, 711542579L, -1775592233L, -1558734945L, -5729342L, 
-1693288404L, 1712849546L, 1583077728L, 197502589L, -1187267870L, 
-748403203L, -640097145L, -1399804668L, 1354795550L, 1075784038L, 
1148180253L, -1238337473L, -729494362L, -1990522566L, -1295836536L, 
-232373611L, -2077609492L, 2137503706L, 1436187269L, 1745109786L, 
567032166L, 491296010L, -541641143L, -586467251L, -526093454L, 
352259128L, -931037943L, -1918899132L, -1114518414L, 1609769820L, 
-1456028357L, -1398430839L, 890826901L, 1765161835L, 2005884951L, 
448357932L, 701566716L, -1721107861L, 68997398L, -701811069L, 
583815017L, 275748654L, 201441115L, 1162692676L, 664403980L, 
1144272119L, -2143950652L, -1164542568L, 992422185L, -1404994063L, 
1835321231L, 816341425L, 2090495742L, 2136918117L, 369514213L, 
1700975439L, 266492321L, 481045314L, 923862515L, -1724050007L, 
1154872429L, 612137286L, -1572812020L, 2129366606L, 1722611479L, 
-23818695L, 1060911669L, -1610776400L, 242992205L, 309169021L, 
-1523954355L, -1858321229L, -59945503L, 16490647L, -92105843L, 
-2059893948L, -1508349476L, 389890923L, -802960570L, 1903118058L, 
1289490432L, -137164181L, 1526718226L, -549124997L, 1451065970L, 
115237536L, -2099388231L, -1011926083L, -999962210L, -835085597L, 
1869010185L, -1367398608L, -737019080L, 1904293301L, 1760344066L, 
1209628209L, 2069894421L, 130767812L, 1889947714L, -2072002030L, 
-660828327L, -1715671825L, 535213777L, 57957805L, -2043996676L, 
691233442L, -1953239165L, -963796120L, 1982824855L, -472921107L, 
-294995222L, 769827148L, -1100536681L, 1460196176L, 61226258L, 
-62999755L, 1213342426L, 206007274L, 183666718L, -742216429L, 
1611067240L, -2118409440L, 1092062017L, 2070147234L, 1863660910L, 
-851301386L, 1912301654L, 702703630L, 1211665279L, -1232013864L, 
503545776L, 1226864796L, 414484224L, 1517084746L, 1341907238L, 
-653545127L, -176451080L, -500532753L, 1166401217L, 1767796262L, 
2081768775L, -65941968L, -1234102421L, 344736265L, -1990369477L, 
119569882L, -1491539184L, 1747980617L, -1115273846L, 1629561199L, 
866641088L, -1514879359L, 1057535406L, -1399864115L, -461139636L, 
-218415406L, 2124597361L, 1169529650L, -724215245L, -2089635031L, 
365318910L, 629061241L, 228045701L, 186722482L, 863244405L, 887719542L, 
272574082L, -2090651445L, -472200131L, 1926647212L, -1797319269L, 
269280135L, 2123617159L, 1856863530L, 432590891L, 694636486L, 
1976033706L, 826480362L, 625266753L, 1373793364L, 1642529138L, 
1329557495L, 2059222576L, -1350327341L, -287312193L, -1460828061L, 
969984146L, -27725348L, 1748975226L, -1915881790L, 1456233715L, 
-1618992083L, -844151317L, 471009921L, 1870310668L, 904239040L, 
757691315L, 797836389L, 1822125193L, 931126556L, 69277608L, -488199553L, 
-1316799846L, -877820130L, 922548905L, -190585333L, 17753401L, 
811451152L, 601289101L, -1904951059L, 195090882L, 1134945945L, 
817521503L, -1435622978L, -2076537187L, -815179385L, -557194305L, 
1621176527L, 1668859600L, -1669525942L, 1366779459L, -417816757L, 
-455693608L, -1608584793L, 757799161L, 1120733142L, -1393631857L, 
-1808053921L, 75894827L, -1028398639L, -396873961L, -1240883229L, 
-1951061899L, 1868257111L, 173296638L, 1022222073L, 1833710095L, 
-1455100623L, -1345897547L, -754575180L, -146078426L, 284420841L, 
1013232892L, -1036595606L, -543201325L, -1761437990L, -1200434787L, 
1853080782L, -122079193L, 1957846277L, -9297018L, 2036450320L, 
-1522294751L, -699550202L, -691849285L, 1203385033L, 713546885L, 
-2115080818L, -593173217L, 1259095110L, -1809785029L, -1832850266L, 
1765873905L, 1454930089L, 441998865L, -890197041L, -844605669L, 
-1001793312L, -270428624L, 2008300165L, -1422472045L, 900048402L, 
863402374L, 825687882L, 2037833393L, -1255335571L, -581358794L, 
1409329441L, 1061204250L, -935803214L, 1671326030L, -931297476L, 
-472630234L, 118218513L, 341496790L, -1324245978L, -1444009112L, 
-973589917L, 318509038L, -717046022L, 809770043L, -985808254L, 
761000561L, 907759450L, 1178118194L, -1335994773L, 1078483402L, 
1318982907L, 2001930896L, 1862040584L, 2041479850L, 1331290456L, 
-338815210L, 1177567972L, -1468967224L, 67607190L, -1846577143L, 
1447075823L, -753419060L, 137138012L, -1072703530L, -256456719L, 
-864891976L, -21547620L, 656356736L, 823673442L, 1244303894L, 
-374363312L, 1741567645L, -960950561L, 121969793L, -886337543L, 
-408775666L, 1163759238L, -2083991108L, -414063388L, -1176204205L, 
1499844992L, -1390307985L, -1999696679L, -150011805L, 332373714L, 
-1764263109L, 127572928L, 33149686L, 1199806756L, -995214174L, 
1675229580L, 1667255389L, 608643421L, 84148235L, 768911697L, 
447703022L, 1177145726L, 497926551L, -1259952923L, -1127711371L, 
2110479918L, -1569888511L, 2053370712L, -596380036L, -285466590L, 
-1165969555L, 2054499492L, 590971444L, -1082733476L, 1716902119L, 
1396455748L, -859805716L, -547933059L, 806317967L, -143819917L, 
871022758L, 1766679308L, 1197517660L, 1560581611L, -1135228037L, 
-756689139L, -396599415L, -152289187L, 1317421298L, 1612272700L, 
2120030826L, 1760221867L, -141831056L, 416458384L, 1835532135L, 
-1909236008L, -2126342713L, -1015826040L, 1787596795L, -1553658895L, 
1516715118L, -188755250L, 1752745926L, 734535544L, 1241542025L, 
924278115L, 270585154L, -1040285652L, -1702832787L, 1357151764L, 
832721832L, 2011061740L, 370513879L, 1912190970L, -1214915446L, 
-1372619039L, -642697553L, 1154131891L, 2099845673L, 804181308L, 
987940289L, -371828352L, -1592842952L, 309732861L, 186602853L, 
1682009519L, 435071504L, 601133354L, 1380721561L, 2010802558L, 
-666760012L, -427435632L, 2126396002L, -971931204L, 577315593L, 
-1181460850L, 961063244L, -246449847L, -1986170687L, 408776555L, 
9869431L, -409687092L, 1753072L, 1179623419L, -874471002L, 335802271L, 
-1810475020L, -192935587L, 490351905L, -330870531L, 1816776711L, 
1424650459L, 2056413343L, -842407372L, 1834126287L, 961244808L, 
-798080516L, 1569530745L, -1225973700L, 1797119437L, 1048719502L, 
-801600589L, 1402209096L, -1252070826L, -673763141L, 1746861562L, 
-48330223L, -1466125520L, 104853730L, -1608031233L, -1504074605L, 
552834880L, -1530036888L, -2031438681L, 53943444L, 571456554L, 
-1592353245L, 1034154753L, 583051460L, -2122777120L, -1364316984L, 
1800690893L, 368298053L, 709035732L, 1602112578L, 2118729427L, 
-1618404404L, 1441985791L, -2091507640L, 912692430L, 266183098L, 
1828103320L, 147218301L, -1974354208L, -979250459L, -886302068L, 
1126758495L, 970671122L, -1514950304L, 398729380L, -2115335796L, 
1757394408L, -1578983299L, 1665520652L, -632949340L, 402223113L, 
1208581113L, 629004862L, 1812268064L, -781935585L, -1285566311L, 
-665032107L, -926236012L, 295067737L, -1043984809L, -97519452L, 
-298831995L, -1743790969L, -624512822L, 988627517L, 893873709L, 
592659763L, 579182408L, 1462883674L, 21536935L, -1634295508L, 
-2041586016L, 146585337L, -1747212799L, 161153480L, -1676438962L, 
1280685856L, 1951310076L, -1326967065L, 561626486L, 1928146584L, 
1702543097L, 535405177L, -2012088708L, 506752262L, 845257812L, 
-136303902L, -1604630900L, 2098645807L, 1007271297L, -989874461L, 
338337951L, -1602993199L, -350350360L, -2002702685L, -75610816L, 
2056991546L, -542369774L, 1722214332L, 200477967L, -1492151846L, 
-1791311770L, -1697869079L, 1356456662L, -319810414L, -1649992994L, 
1406267991L, -1385896526L, 83392217L, -1093722595L, 1172060204L, 
-30251772L, 1303304582L, 1353684520L, 1546213919L, 491489475L, 
-1995669062L, 117814753L, 869976638L, 1895385935L, 933910407L, 
-1249120789L, 1926773070L)
