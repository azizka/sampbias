#'@importFrom raster rasterize
#'@importFrom stats dgamma dpois rnorm runif

#Occurrence raster
.OccRast <- function(x, ras){
  rast <- raster::rasterize(x, ras, fun = "count")
  return(rast)
}

#get number of decimal places
.DecimalPlaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# get_lambda_ij_old <- function(q, w, X) {
#   if (class(X) == "numeric") {
#     return(q * exp(-w * X))
#   } else {
#     return(q * exp(-apply(FUN = sum, w * X, 2)))
#   }
# }


get_lambda_ij <- function(q, w, X) {
  if (class(X) == "numeric") {
    return(q * exp(-w * X))
  } else {
    return(as.vector(q * exp(-(w %*% t(X)))))
  }
}

get_poi_likelihood <- function(Xcounts, lambdas) {
  return(dpois(Xcounts, lambdas, log = TRUE))
}

multiplier_proposal <- function(i, d = 1.2) {
  u <- runif(length(i))
  l <- 2 * log(d)
  m <- exp(l * (u - 0.5))
  ii <- i * m
  hastings_ratio <- sum(log(m))
  return(list(ii, hastings_ratio))
}

get_post_rate <- function(w,alpha){
  a0 <- 1
  b0 <- 0.001
  rate <- rgamma(1, a0 + length(w) * alpha, rate = b0 + sum(w))
  return(rate)
}

.RunSampBias <- function(x,
                         rescale_counts = 1,
                         rescale_distances = 1000,
                         iterations = 1e+05,
                         burnin = 0,
                         post_samples = NULL,
                         outfile = NULL,
                         prior_q = c(1, 0.01),
                         prior_w = c(1, 1),
			 run_null_model = FALSE) {

  indx <- c(3:ncol(x))

  # X is the distance matrix with 1 row for each cell and 1 col for each predictor
  X <- x[, indx]/rescale_distances
  # if (length(indx) > 1) {
  #   X <- t(X)
  # }

  Xcounts <- x$record_count

  # coefficient, i.e. Poisson rate at zero distance from all
  qA <- mean(Xcounts)

  # init weigths of predictors
  wA <- abs(rnorm(length(indx), 0.01, 0.01))
  if (run_null_model == TRUE){
    wA = rep(0, length(indx))
  }

  lambdas <- get_lambda_ij(qA, wA, X)

  likA <- sum(get_poi_likelihood(Xcounts, lambdas))
  
  priorA <- dgamma(qA, prior_q[1], rate=prior_q[2], log = TRUE)
  if (run_null_model == FALSE){
    priorA <- priorA + sum(dgamma(wA, prior_w[1], rate=prior_w[2], log = TRUE))
  }

  names_b <- paste("w", names(x[, indx]), sep = "_")


  out <- data.frame()

  if (!is.null(outfile)){
    outfile <- paste("mcmc", paste(names_b[indx - 2], collapse = "_"), ".log", sep = "_")
    cat(c("it", "likA", "priorA", "q", names_b[indx - 2], "hp_rate", "\n"), file = outfile, sep = "\t")
  }

  # print to screen
  message(paste(c("it", "likA", "priorA", "q", names_b[indx - 2]), collapse = " "))

  for (it in 1:iterations) {
    w <- wA
    q <- qA
    mh <- TRUE
    
    if (runif(1) < 0.01 & run_null_model == TRUE) {
      prior_w[2] <- get_post_rate(wA, prior_w[1])
      mh <- FALSE
      hastings <- 0
    
    } else if (runif(1) < 0.3 | run_null_model == TRUE) {
      update <- multiplier_proposal(qA, d = 1.1)
      q <- update[[1]]
      hastings <- update[[2]]
    } else {
      update <- multiplier_proposal(wA, d = 1.05)
      w <- update[[1]]
      hastings <- update[[2]]
    }

    lambdas <- get_lambda_ij(q, w, X)
    if (mh == TRUE){
    	lik <- sum(get_poi_likelihood(Xcounts, lambdas))
    }else{
	lik <- likA
    }
    
    prior <- dgamma(q, prior_q[1], rate=prior_q[2], log = TRUE)
    if (run_null_model == FALSE){
      prior <- prior + sum(dgamma(w, prior_w[1], rate=prior_w[2], log = TRUE))
    }
    

    if (mh == FALSE | (lik + prior) - (likA + priorA) + hastings >= log(runif(1))) {
      likA <- lik
      priorA <- prior
      wA <- w
      qA <- q
    }

    if (it > burnin & it%%100 == 0) {
      message(paste(round(c(it, likA, priorA, qA, wA), 3), collapse = " "))

      out <- rbind(out, c(it, likA, priorA, qA, wA, prior_w[2]))

      if (!is.null(outfile)){
        cat(c(it, likA, priorA, qA, wA, prior_w[2], "\n"), file = outfile, sep = "\t", append = TRUE)
      }
    }
  }

  if (is.null(outfile)){
    names(out) <- (c("it", "likA", "priorA", "q", names_b[indx - 2],"hp_rate"))
    return(out)
  }
}
