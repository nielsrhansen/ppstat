ppstat
======

The package provides a framework for analyzing data from multivariate point processes in time or one-dimensional space, aka marked point processes with discrete marks, based on a specification of the conditional intensity process.

## Installation of the development version from source

The current development version can be installed from the source in this
repository using the `devtools` package.

	install.packages("devtools")  ## if 'devtools' not installed
	library(devtools)
	install_github("nielsrhansen/ppstat/pkg", build_vignettes = TRUE)

## Getting started

The best way to get started using ppstat is by working through the tutorial 
vignette.

  library("ppstat")
  vignette("Tutorial", package = "ppstat")

To see examples of how univariate and multivariate point processes can be 
fitted, you can run a demo.

  demo("toyModels", package = "ppstat")
  
One of the most important things to understand is that models can only be fitted
to data that are contained in an object of class MarkedPointProcess. Such objects 
can be constructed via the markedPointProcess function. The following vignette 
gives an introduction to the data structures that ppstat relies on.

  vignette("Introduction", package = "ppstat")
  
There is also a demo
  
  demo("dataContainers", package = "ppstat")

that illustrates some of the functionalities. 