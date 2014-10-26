### Implementation of the multivariate Ogata thinning algorithm.

Ogata <- function(n = 1, lambda, seed = NULL, tLim = Inf, adapK = FALSE, ...) {
  if (!is.null(seed))
    set.seed(seed)
  T <- matrix(0, n, 2)
  t <- 0
  i <- 0
  lamb <- lambda(0, bound = TRUE, ...)     ## Vector
  K <- sum(lamb)
  M <- length(lamb)
  Tlocal <- vector("list", M)
  Nlocal <- numeric(M)
  for (m in 1:M) 
    Tlocal[[m]] <- numeric(n)
  repeat {
    while(i < n) {
      S <- rexp(1, K)
      U <- runif(1)
      t <- t + S
      if (t > tLim)
        break
      lamb <- lambda(t = t, T = Tlocal, N = Nlocal, ...)  ## Vector
      accept <- U < sum(lamb)/K
      if(accept) {  ## accepts suggested point
        i <- i + 1
        if (M > 1) {
          m <- sample.int(M, 1, prob = lamb)
        } else {
          m <- 1
        }
        T[i, ] <- c(t, m)
        Nlocal[m] <- Nlocal[m] + 1
        Tlocal[[m]][Nlocal[m]] <- t
      }
      ## K must be updated if point is accepted. It can be adaptively updated,
      ## but it may be too expensive a computation to be worthwhile.
      if (accept || adapK) 
        K <- sum(lambda(t = t, T = Tlocal, N = Nlocal, bound = TRUE, ...))
      if (K == 0)
        break
    }
    if (tLim == Inf || t >= tLim || K == 0)
      break
    T <- rbind(T, T)
    n <- nrow(T)
    Tlocal <- lapply(Tlocal, function(T) c(T, T))    
  }
  if (M > 1) {
    idx <- seq_len(i)
    marks <- paste("M", 1:M, sep = "")
    return(data.frame(time = T[idx, 1],
                    markType = marks[T[idx, 2]])
    )
  } else {
    return(T[seq_len(i), 1])
  }
} 

hawkesRate <- function(h,
                       Delta = 1, 
                       beta0 = rep(1, M), 
                       phi = function(x) pmax(x, 0), 
                       A = Inf,
                       warn = TRUE,
                       ...) {
  ## Argument h can be numeric, which will generate a univariate point process,
  ## but it needs to be converted to a list of lists for the algorithm.
  if (is.numeric(h)) {
    hh <- list()
    hh[[1]] <- list()
    hh[[1]][[1]] <- h
    h <- hh
  }
  if (!is.list(h))
    stop("Argument 'h' must be a list'.")
  ## Computing bounds for the Ogata algorithm
  M <- length(h)
  hMax <- vector("list", M)
  for (m in seq_len(M)) {
    hMax[[m]] <- vector("list", M)  
    for (k in seq_len(M)) {
      if (!is.null(h[[m]][[k]])) {
        hMax[[m]][[k]] <- rev(pmax(cummax(rev(h[[m]][[k]])), 0))
      }
    }
  }  
  lambda <- function(t, T = list(), N = numeric(), bound = FALSE, ...) {
    if (is.numeric(T))
      T <- list(T)
    if (!is.list(T))
      stop("Argument 'T' must be a list.")
    if (bound)
      h <- hMax
    t <- t[1]
    lamb <- beta0
    if (length(T) != 0) {
      if (length(h) != length(T) || length(h) != length(N)) 
        stop("Arguments 'T' and 'N' must be of the same length as 'h'.")
      for (k in seq_len(M)) {
        tk <- T[[k]][seq_len(N[k])]
        tk <- tk[t - tk < A]
        if (length(T) == 0 || length(tk) == 0) {
          i <- FALSE
        } else {
          if (t < tk[length(tk)] || (t == tk[length(tk)] && !bound)) 
            stop("Time argument 't' smaller than or equal to the largest 'T'. Possible explosion.")
          i <- floor((t - tk)/Delta + 1.5)
        }
        for (m in seq_len(M)) {
          if (!is.null(h[[m]][[k]])) {
            ii <- i
            iMax <- length(h[[m]][[k]])
            if (i[1] > iMax) {
              if (warn)
                warning("Point history truncated.", call. = FALSE)
              ii <- i[i <= iMax]
            }
            lamb[m] <- lamb[m] + sum(h[[m]][[k]][ii])
          }
        }
      }
    }
    phi(lamb)
  }
  lambda
}

invHaz <- function(n = 1, LambdaInv, seed = NULL, tLim = Inf, ...) {
  if (!is.null(seed))
    set.seed(seed)
  T <- matrix(0, n, 2)
  S <- rexp(n)
  i <- 0
  t <- 0
  tmp <- LambdaInv(1, ...)  ## List with entries 't' and a vector 'p'
  M <- length(tmp$p)
  Tlocal <- vector("list", M)
  Nlocal <- numeric(M)
  for (m in 1:M) 
    Tlocal[[m]] <- numeric(n)
  repeat {  
    while(i < n) {
      tmp <- LambdaInv(S[i + 1], Tlocal, Nlocal, t = t, ...)
      t <- tmp$t
      if (t >= tLim)
        break
      i <- i + 1
      if (M > 1) {
        m <- sample.int(M, 1, prob = tmp$p)
      } else {
        m <- 1
      }
      T[i, ] <- c(t, m)
      Nlocal[m] <- Nlocal[m] + 1
      Tlocal[[m]][Nlocal[m]] <- t
    }
    if (tLim == Inf || t >= tLim)
      break
    T <- rbind(T, T)
    S <- c(S, rexp(n))
    n <- nrow(T)
    Tlocal <- lapply(Tlocal, function(T) c(T, T))    
  }
  if (M > 1) {
    idx <- seq_len(i)
    marks <- paste("M", 1:M, sep = "")
    return(data.frame(time = T[idx, 1],
                      markType = marks[T[idx, 2]])
    )
  } else {
    return(T[seq_len(i), 1])
  }
}
  
weibullInv <- function(scale, gamma = 1, ...) {
  gammainv <- 1/gamma
  p <- 1
  function(s, T = list(), N = numeric(), t = 0, ...) {
    alpha <- scale(T, N, t, ...)
    M <- length(alpha)
    lamb <- numeric(M)
    t <- (s / sum(alpha))^gammainv + t
    if (M > 1) {
      backt <- numeric(M)
      for(m in 1:M) {
        if (length(N) > 0 && N[m] > 0) {
          backt[m] <- t - T[[m]][N[m]]
        } else {
          backt[m] <- t
        }
      }
      lamb <- alpha * gamma * (backt)^(gamma - 1)
      p <- lamb / sum(lamb)
    } 
    return(list(t = t, p = p))
  }
}

expFilterScale <- function(alpha0 = 0, beta = 1, alpha = 1, ...) {
  function(T = list(), 
           N = numeric(), 
           t = 0, 
           ...) {
    if (is.numeric(T))
      T <- list(T)
    if (!is.list(T))
      stop("Argument 'T' must be a list.")
    M <- length(alpha0)
    alpha <- matrix(alpha, M, M)
    beta <- matrix(beta, M, M) 
    if (length(T) > 0 && (length(T) != M || length(N) != M))
      stop("Arguments 'T' and 'N' must have the the same length as 'alpha0'.")
    scale <- alpha0
    if (length(T) > 0) {
      for (k in seq_len(M)) {
        if (N[k] > 0) {
          if (t < T[[k]][N[k]])
            stop("Argument 't' smaller than some history event times.")
          tt <- t - T[[k]][seq_len(N[k])]
          for (m in seq_len(M)) 
            scale[k] <- scale[k] + alpha[k, m] * sum(exp(- beta[k, m] * tt))
        }
      }
    }
    return(scale)
  }
}

setMethod("simulate", "PointProcessModel",
          function(object, nsim = 1, seed = NULL, ...) {
            Delta <- object@Delta
            A <- max(object@support)
            h <- getLinearFilter(object)[, response(object)]
            beta0 <- coefficients(object)["(Intercept)"]
            Ogata(nsim, 
                         lambda = hawkesRate( 
                           h = h, 
                           Delta = Delta,
                           beta0 = beta0,
                           phi = object@family@phi,
                           A = A,
                           ...),
                         seed = seed
            )
          }
)

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
                         lambda = hawkesRate( 
                           h = h, 
                           Delta = Delta,
                           beta0 = beta0,
                           phi = object@models[[1]]@family@phi,
                           A = A,
                           ...),
                         seed = seed
            )
            sim[, 'markType'] <- factor(responseNames[sim[, 'markType']])
            sim
          }
)
