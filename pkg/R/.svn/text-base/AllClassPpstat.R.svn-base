setClass("Family",
         representation(
           name = "character",
           link = "character",       ## Name of the link, e.g. log if phi = exp
           phi = "function",         ## The function that may be called the inverse link
           Dphi = "function",        ## First derivative
           D2phi= "function"         ## Second derivative
           )
         )

setClass("PointProcess",
         representation(
           call = "call",

           ## The equidistant spacing between the g-evaluations.
           Delta = "numeric",      

           delta = "numeric",
           family = "Family",
           formula = "formula",
           response = "character",
           loss = "character",

           ## Results from call to 'optim' goes here.
           optimResult = "list",
           
           ## The process data.
           processData = "MarkedPointProcess",
           
           ## A vector c(a,b) with the support, [a,b], of the g-functions.
           support = "numeric",

           ## The (effective) degrees of freedom.
           df = "numeric",

           "VIRTUAL")
         )

setClass("PointProcessModel",
         representation(
           ## Evaluations of basis functions in support
           ## at Delta-grid values are in the list
           ## 'basis' in this environment.
           basisEnv = "environment",
           
           ## The 'basisPoints' contains the evaluation
           ## points (the grid) for the basis functions.
           basisPoints = "numeric",
           
           coefficients = "numeric",
           filterTerms = "numeric",
           
           ## The 'active' columns. Set in update, used
           ## in getModelMatrix and reset in
           ## computeModelMatrix
           modelMatrixCol = "numeric",
           
           ## The modelMatrix of class 'modelMatrix' is
           ## in this environment. So is the formula used
           ## to create the model matrix and an 'assign'
           ## vector. Locked after computation.
           modelMatrixEnv = "environment",

           responseMatrix = "Matrix",
           crossProd = "list",

           lambda = "numeric",
           var = "matrix",
           
           ## Which method is used to compute the
           ## estimate of the
           ## variance. 'pointProcessModel' has default
           ## 'Fisher'.
           varMethod = "character"      
           ),
         contains = "PointProcess",
         validity = function(object) {
           if(isTRUE(object@support[2] - object@support[1] <= 0))
             stop("Variable 'support' has to be an interval.")
           if(isTRUE(object@Delta > object@support[2] - object@support[1]))
             stop("Variable 'Delta' has to be smaller than the length of the support.")
           return(TRUE)
         }
         )

setClass("PointProcessSmooth",
         representation(
           ## The terms in the formula that are smooth terms.
           smoothTerms = "numeric", 
           ## The V matrix is a block diagonal matrix
           ## used for internal reparametrization of the 
           ## smoothing terms.
           V = "Matrix"
           ),
         contains = "PointProcessModel"
         )

setClass("PointProcessKernel",
         representation(
           ## Non-parametric components
           g = "numeric",
           
           ## The terms in the formula that are kernel terms.
           kernelTerms = "numeric", 
                     
           ## The 'active' kernel columns. Set in update, used
           ## in getKernelMatrix and reset in
           ## computeKernelMatrix
           kernelMatrixCol = "numeric",
           
           ## The 'kernelMatrixEnv' contains the
           ## kernelMatrix used for computations with
           ## non-parametric kernel filters.
           kernelMatrixEnv = "environment",
           
           responseKernelMatrix = "Matrix",
           
           ## The U matrix is a block diagonal matrix
           ## stored as a sparse matrix which encodes
           ## the reparametrization of the kernel
           ## expansion.
           U = "Matrix"           
           ),
         contains = "PointProcessModel")

setClass("MultivariatePointProcess",
         representation(
           models = "list",
           adjMat = "matrix"
           ),
         validity = function(object) {
           if(any(sapply(object@models, function(m) !isClass(m, "PointProcessModel"))))
             stop("Objects in model list are not all of class 'PointProcessModel'.")
         }
         )
