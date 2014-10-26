setMethod("plot", c("ProcessPlotData", "missing"),
          function(x, y, ...) {

            ### Setting and extracting additional arguments to be
            ### passed to the relevant layers.
            factorArgs <- list(size = 3)
            pointArgs <- list()
            args <- list(...)
            factorIndex <- grep("factor_", names(args))
            factorNames <- gsub("factor_", "", names(args)[factorIndex])
            factorArgs[factorNames] <- args[factorIndex]
            pointIndex <- grep("point_", names(args))
            pointNames <- gsub("point_", "", names(args)[pointIndex])
            pointArgs[pointNames] <- args[pointIndex]
            
            ### Setting up y-values for discrete variables
            value <- factor()
            factorpoint <- numeric()
            
            if(dim(x@factorPlotData)[1] > 0) {
              value <- x@factorPlotData$value
              factorpoint <- rep(1, length(value))
            }

            if(dim(x@pointPlotData)[1] > 0) {
              if(is.factor(x@pointPlotData$value)) {
                levels <- unique(c(levels(value), levels(x@pointPlotData$value)))
                value <- factor(c(as.character(value), as.character(x@pointPlotData$value)), levels = levels)
                factorpoint <- c(factorpoint, rep(2, length(x@pointPlotData$value)))
              }
            }

            factorLevels <- levels(value)
            factorRange <-  length(factorLevels)*(x@limits[2] - x@limits[1])/(length(factorLevels) + 3)
            if(length(x@breaks) == 0) {
              offset <- x@limits[2]
            } else {
              offset <- max(x@breaks)
            }
            if(x@position == "bottom") {
              factorRange <- -factorRange
              if(length(x@breaks) == 0) {
                offset <- x@limits[1]
              } else {
                offset <- min(x@breaks)
              }
            }
            addBreaks <-  offset + factorRange*seq_along(factorLevels)/(length(factorLevels) + 1)

            if(length(addBreaks) > 0) {
              x@breaks <- c(x@breaks, addBreaks) 
              x@labels <- c(x@labels, factorLevels)
              x@limits <- range(c(x@breaks, x@limits))

            }
                  
            if(length(factorpoint) > 0) {
              if(factorpoint[1] == 1){
                x@factorPlotData$value <- offset + factorRange*as.numeric(value[factorpoint == 1])/(length(factorLevels) + 1)
              }
              
              if(factorpoint[length(factorpoint)] == 2){
                x@pointPlotData$value <- offset + factorRange*as.numeric(value[factorpoint == 2])/(length(factorLevels) + 1)
              }
            }
            
            p <- ggplot(x@continuousPlotData, aes_string(x = "position",
                                                         y = "value",
                                                         colour = "variable")) +
                scale_x_continuous(x@positionVar) + 
                guides(color = guide_legend(override.aes = list(alpha = 1)))

            if(length(levels(x@continuousPlotData[, x@idVar])) > 1) {
              facetFormula <- as.formula(paste(x@idVar, "~ ."))
              p <- p + facet_grid(facetFormula)
            }
            
            if("value" %in% names(x@continuousPlotData)){
              group <- paste(x@idVar, ":variable", sep = "")
              p <- p + geom_line(aes_string(group = group)) +
                scale_y_continuous(breaks = x@breaks,
                                   name = "",
                                   labels = x@labels,
                                   limits = x@limits)
            } else {
              p <- p + scale_y_continuous(breaks = x@breaks,
                                          name = "",
                                          labels = x@labels,
                                          expand = c(0, abs(factorRange)/(length(factorLevels) + 1)))
            }

            if(dim(x@factorPlotData)[1] > 0) {
              p <- p + do.call(geom_line, c(list(data = x@factorPlotData,
                                                 aes_string(group = "group")),
                                            factorArgs))
              
            }

            if(dim(x@pointPlotData)[1] > 0) {
              p <- p + do.call(geom_point, c(list(data = x@pointPlotData),
                                         pointArgs))
            }
                                                                         
            return(p)
          }
)

