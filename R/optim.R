
# ==================================================================================================

# cover a binary set of varibles using gurobi

cover_gurobi = function(mat, alpha=0.05, maxsol=100, J=1){
  
  if(! "gurobi" %in% names(sessionInfo()$otherPkgs)){
    
    # ERROR
    
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
  
  list(obj=obj, sol=sol, r=r, result=result)
  
}

# ==================================================================================================

# cover a binary set of varibles using lpSolve

cover_lpSolve = function(mat, alpha=0.05, maxsol=100, J=1){
  
  if(! "lpSolve" %in% names(sessionInfo()$otherPkgs)){
    
    # ERROR
    
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
  
  R = lp(direction="min",
         objective.in=obj,
         const.mat=C,
         const.dir=sense,
         const.rhs=rhs,
         all.bin=TRUE)
         
  list(obj=sum(R$solution[1:ncol(A)]), sol=sol, result=R)
  
}


# ==================================================================================================

getCovering = function(mat=mat, alpha=alpha, maxsol=maxsol, J=J){
  
  # decide which solver to use
  
  solver = ""
  
  if("gurobi" %in% rownames(installed.packages())){
    
   load_package = require("gurobi")
   if(load_package)
    solver = "gurobi"
   
  }
  
  if(solver == ""){
    
    if("lpSolve" %in% rownames(installed.packages())){
      
      load_package = require("lpSolve")
      if(load_package)
        solver = "lpSolve"
      
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

