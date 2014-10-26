### Implementation of the multivariate Ogata thinning algorithm.

Ogata <- function(n = 1, lambda, h, A = Inf, seed = NULL, tLim = Inf, ...) {
  if (!is.null(seed))
    set.seed(seed)
  T <- matrix(0, n, 2)
  ## Computing bounds for the Ogata algorithm
  M <- length(h)
  hMax <- vector("list", M)
  for (m in seq_len(M)) {
    hMax[[m]] <- vector("list", M)  
    for (k in seq_len(M)) {
      if (!is.null(h[[m]][[k]])) {
        hMax[[m]][[k]] <- rev(cummax(rev(h[[m]][[k]])))
      }
    }
  }  
  colnames(T) <- c("time", "markType")
  t <- 0
  i <- 0
  lamb <- lambda(0, h = h, ...)  ## Vector
  K <- sum(lamb)
  M <- length(lamb)
  Tlocal <- vector("list", M)
  repeat {
    while(i < n) {
      S <- rexp(1, K)
      U <- runif(1)
      t <- t + S
      if (t > tLim)
        break
      tmA <- t - A
      for (m in seq_len(M)) {
        j <- 1
        while (j <= length(Tlocal[[m]]) && Tlocal[[m]][j] < tmA)
          j <- j + 1
        if(j > 1)
          Tlocal[[m]] <- Tlocal[[m]][-(1:(j-1))]
      }
      lamb <- lambda(t = t, T = Tlocal, h = h, ...)  ## Vector
      if(U < sum(lamb)/K) {
        i <- i + 1
        ## ALternatively, the next sampling step could be done
        ## using the U already simulated. 
        m <- sample.int(M, 1, prob = lamb)
        T[i, ] <- c(t, m)
        Tlocal[[m]] <- c(Tlocal[[m]], t)
      }
      K <- sum(lambda(t = t, T = Tlocal, h = hMax, predict = FALSE, ...))
    }
    if(tLim == Inf || t > tLim)
      break
    T <- rbind(T, T)
    n <- nrow(T)
  }
  as.data.frame(T[1:i, , drop = FALSE])
} 

hawkesRate <- function(t, T = list(), h, predict = TRUE, Delta = 1, 
                       beta0 = rep(1, M), phi = function(x) pmax(x, 0), 
                       warn = TRUE, ...) {
  if (!is.list(T))
    stop("Argument 'T' must be a list.")
  t <- t[1]
  if (!is.list(h) && length(h) != length(T)) 
    stop("Argument 'h' is not a list of the same length as 'T'.")
  M <- length(h)
  lamb <- beta0
  for (k in seq_len(M)) {
    if (length(T) == 0 || length(T[[k]]) == 0) {
      i <- FALSE
    } else {
      if (t < T[[k]][length(T[[k]])] || (t == T[[k]][length(T[[k]])] && predict)) 
        stop("Time argument 't' smaller than largest 'T'. Possible explosion.")
      i <- floor((t - T[[k]])/Delta + 1.5)
    }
    for (m in seq_len(M)) {
      if (!is.null(h[[m]][[k]])) {
        ii <- i
        iMax <- length(h[[m]][[k]])
        if (i[length(i)] > iMax) {
          if (warn)
            warning("Point history truncated.", call. = FALSE)
          ii <- i[i <= iMax]
        }
        lamb[m] <- lamb[m] + sum(h[[m]][[k]][ii])
      }
    }
  }
  phi(lamb)
}

setMethod("simulate", "MultivariatePointProcess",
          function(object, nsim = 1, seed = NULL, ...) {
            Delta <- object@models[[1]]@Delta
            A <- max(object@models[[1]]@support)
            linearFilters <- getLinearFilter(object)
            responseNames <- names(linearFilters)
            M <- length(linearFilters) 
            h <- vector("list", M)
            beta0 <- numeric(M)
            for(m in seq_len(M)) {
              h[[m]] <- vector("list", M)
              names(h[[m]]) <- responseNames
              for(k in seq_len(M)) {
                if(responseNames[k] %in% names(linearFilters[[m]])) 
                  h[[m]][[responseNames[k]]] <- linearFilters[[m]][, responseNames[k]]
              }
              beta0[m] <- coefficients(object@models[[m]])["(Intercept)"]
            }
            sim <- Ogata(nsim, 
                         lambda = hawkesRate, 
                         h = h,
                         A = A,
                         Delta = Delta,
                         beta0 = beta0,
                         seed = seed,
                         phi = object@models[[1]]@family@phi,
                         ...)
            sim[, 'markType'] <- factor(responseNames[sim[, 'markType']])
            sim
          }
)
