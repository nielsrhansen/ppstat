
explin.lambda <- function(x, 
                          y, 
                          weights, 
                          sampleGrouping = factor(rep(1, nrow(x))), 
                          covariateGrouping = factor(1:ncol(x)), 
                          groupWeights = c(sqrt(length(levels(sampleGrouping)) * table(covariateGrouping))), 
                          parameterWeights =  matrix(1, nrow = length(levels(sampleGrouping)), ncol = ncol(x)), 
                          alpha = 0.5, 
                          d = 100L, 
                          lambda.min, 
                          algorithm.config = sgl.standard.config) 
{
	# cast
	covariateGrouping <- factor(covariateGrouping)
	sampleGrouping <- factor(sampleGrouping)
	
	# create data
	data <- create.sgldata(x, y, weights, sampleGrouping)
	
	# call SglOptimizer function
	if(data$sparseX) {
		lambda <- sgl_lambda_sequence("explin_sparse", 
                                  "ppstat", 
                                  data, 
                                  covariateGrouping, 
                                  groupWeights, 
                                  parameterWeights, 
                                  alpha = alpha, 
                                  d = d, 
                                  lambda.min, 
                                  algorithm.config)
	} else {
		lambda <- sgl_lambda_sequence("explin_dense", 
                                  "ppstat", 
                                  data, 
                                  covariateGrouping, 
                                  groupWeights, 
                                  parameterWeights, 
                                  alpha = alpha, 
                                  d = d, 
                                  lambda.min, 
                                  algorithm.config)
	}

	return(lambda)
}


explin <- function(x, 
                   y, 
                   weights, 
                   sampleGrouping = factor(rep(1, nrow(x))), 
                   covariateGrouping = factor(1:ncol(x)), 
                   groupWeights = c(sqrt(length(levels(sampleGrouping)) * table(covariateGrouping))), 
                   parameterWeights =  matrix(1, nrow = length(levels(sampleGrouping)), ncol = ncol(x)), 
                   alpha = 0.5, 
                   lambda, 
                   algorithm.config = sgl.standard.config) 
{
	# cast
	covariateGrouping <- factor(covariateGrouping)
	sampleGrouping <- factor(sampleGrouping)
	
	# create data
	data <- create.sgldata(x, y, weights, sampleGrouping)
	
	# call SglOptimizer function
	if(data$sparseX) {
		res <- sgl_fit("explin_sparse", 
                   "ppstat", 
                   data, 
                   covariateGrouping, 
                   groupWeights, 
                   parameterWeights, 
                   alpha, 
                   lambda, 
                   return = seq_along(lambda), 
                   algorithm.config)
	} else {
		res <- sgl_fit("explin_dense", 
                   "ppstat", 
                   data, 
                   covariateGrouping, 
                   groupWeights, 
                   parameterWeights, 
                   alpha, 
                   lambda, 
                   return = seq_along(lambda), 
                   algorithm.config)
	}
	
	return(res)
}