
# ==================================================================================================

# cover a binary set of varibles using gurobi

cover_gurobi = function(mat, alpha=0.05, maxsol=100, J=1){
  
  if(! "gurobi" %in% names(sessionInfo()$otherPkgs)){
    
    # ERROR
    stop("Cannot run optimization; unable to load gurobi")
    
  }
  
  n = ncol(mat)
  m = nrow(mat)
  
  A = t(mat)
  B = diag(n) * J
  C = cbind(rbind(A, rep(0, ncol(A))), 
            rbind(B, rep(1, ncol(B))) 
  )
  
  model = list()
  
  delta = rep(0, ncol(B))
  
  model$A = C
  model$obj = c(rep(1, ncol(A)), delta)
  model$modelsense = 'min'
  
  model$rhs = c(rep(J, nrow(model$A)-1), alpha * n)
  model$sense = c(rep('>=', length(model$rhs)-1), '<=')
  model$vtype = 'B'
  
  params <- list()
  params$PoolSearchMode <-2
  params$PoolSolutions <- maxsol
  params$PoolGap <-0
  
  result = gurobi(model, params=params)
  
  r = lapply(result$pool, function(x){
    list(v=x$objval,
         lsum=sum(x$xn[-c(1:m)]),
         xsum=sum(x$xn[1:m]),
         g=rownames(mat)[which(x$xn[1:m] > .5)])
  })
  
  obj = sapply(r, function(x) sum(colSums(mat[x$g, , drop=FALSE]) >= J) )
  
  sol = sapply(r, function(x) x$g )
  
  if(is.vector(sol))
    sol = matrix(sol, nrow=1)
  
  list(obj=obj, sol=sol, r=r, result=result)
  
}

# ==================================================================================================

# cover a binary set of varibles using lpSolve

cover_lpSolve = function(mat, alpha=0.05, maxsol=1, J=1){
  
  if(! "lpSolve" %in% names(sessionInfo()$otherPkgs)){
    
    # ERROR
    stop("Cannot run optimization; unable to load lpSolve")
    
  }
  
  n = ncol(mat)
  m = nrow(mat)
  
  A = t(mat)
  B = diag(n) * J
  C = cbind(rbind(A, rep(0, ncol(A))), 
            rbind(B, rep(1, ncol(B))) 
  )
  
  delta = rep(0, ncol(B))
  obj = c(rep(1, ncol(A)), delta)
  
  rhs = c(rep(J, nrow(C)-1), alpha * n)
  sense = c(rep('>=', length(rhs)-1), '<=')
  
  if(maxsol > 1){
    print("WARNING: Returning multiple solutions with lpSolve may take a long time")
  }
  
  result = lp(direction="min",
         objective.in=obj,
         const.mat=C,
         const.dir=sense,
         const.rhs=rhs,
         all.bin=TRUE,
         num.bin.solns=maxsol)
  
  k = m+n
  
  r = lapply(1:maxsol, function(j){
    
    a = ((j-1) * k) + 1
    b = j * k
    u = result$solution[a:b]
    lsum=sum(u[-c(1:m)])
    xsum=sum(u[1:m])
    g=rownames(mat)[which(u[1:m] > .5)]
    
    list(lsum=lsum, xsum=xsum, g=g)
    
  })
  
  obj = sapply(r, function(x) sum(colSums(mat[x$g, , drop=FALSE]) >= J) )
  
  sol = sapply(r, function(x) x$g )
  
  if(is.vector(sol))
    sol = matrix(sol, nrow=1)
  
  list(obj=obj, sol=sol, r=r, result=result)
  
}


# ==================================================================================================

# main function

getCovering = function(mat=mat, alpha=alpha, maxsol=maxsol, J=J, solver=""){
  
  # decide which solver to use
  
  # if a solver is not specified, use gurobi if available, lpSolve if not
  if(solver == ""){
    
    # try to use gurobi
    if("gurobi" %in% rownames(installed.packages())){
    
      load_package = require("gurobi")
      if(load_package)
        solver = "gurobi"
   
    }
    
    # if that didn't work, try lpSolve
    if(solver != "gurobi"){
      if("lpSolve" %in% rownames(installed.packages())){
      
        load_package = require("lpSolve")
        if(load_package)
          solver = "lpSolve"
      
      }
      
    }
    
  }else if(solver == "gurobi"){
    # try to use gurobi
    if("gurobi" %in% rownames(installed.packages())){
      
      load_package = require("gurobi")
      if(load_package)
        solver = "gurobi"
      else
        solver = ""
      
    }
  }else if(solver == "lpSolve"){
    if("lpSolve" %in% rownames(installed.packages())){
      
      load_package = require("lpSolve")
      if(load_package)
        solver = "lpSolve"
      else
        solver = ""
      
    }
  }
  
  
  if(solver == "gurobi"){
    
    cover_gurobi(mat=mat, alpha=alpha, maxsol=maxsol, J=J)
    
  }else if(solver == "lpSolve"){
    
    cover_lpSolve(mat=mat, alpha=alpha, maxsol=maxsol, J=J)
    
  }else{
    
    stop("Could not load either gurobi or lpSolve for running optimization procedure")
    
  }
  
  
}

