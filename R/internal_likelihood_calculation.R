#'@importFrom stats dexp median
#'@importFrom graphics par plot
#'@importFrom grDevices dev.off pdf

.RunSampBias <- function(dists, distmats, biasdist = c(0, 10), plotextra = F, plotextrafile = "samp_bias_extra_plots.pdf") {

  names.dists <- names(dists)
  if(is.null(names.dists)){
    names(dists) <- paste("Unnamed", seq(1, length(dists)), sep = "")
    names.dists <- names(dists)
  }

  if (plotextra == T) {
    pdf(file = plotextrafile, width = 5 * 4 * 0.75, height = 5 * 4 * 0.75)
    par(mfrow = c(length(names.dists), 4))
  }

  res <- list()
  for (i in 1:length(dists)) {
    res[[i]] <- .CalcExpBias(dists = dists[[i]], Dmatrix = distmats[[i]], names.dists[i],
                             rescale = 0, distvec = biasdist, plotextra = plotextra)
  }

  for (i in 1:length(names.dists)) {
    b.matrix <- res[[i]][[3]]
    if (i == 1) {
      average.bias <- b.matrix
    } else {
      average.bias <- average.bias + b.matrix
    }
  }

  average.bias.matrix <- average.bias/length(names.dists)

  res[[length(dists) + 1]] <- list(bias_matrix = average.bias.matrix)
  names(res) <- c(names.dists, "average_bias_matrix")


  if (plotextra == T) {
    n <- dev.off()
  }
  return(res)

}

# EXPONENTIAL MODEL
# construct probability vector for all poits based on the empirical probaility distribution
.ConstructPxVector <- function(x, X0, nP0, rescale, Px, plotextra) {
  x <- sort(x)
  if (plotextra == T) {
    h.obs <- hist(x, breaks = c(0, X0), main = "Sampled distribution", xlab = "Rescaled distance")
  } else {
    h.obs <- hist(x, breaks = c(0, X0), plot = F)
  }
  obs.counts <- h.obs$counts  # length(obs.counts) = length(P0) = length(X0)
  Px <- c()
  for (i in 1:length(obs.counts)) {
    Px <- c(Px, rep(nP0[i], obs.counts[i]))
  }  # length(Px) = length(x)
  Px[Px == 0] <- min(nP0[nP0 > 0])
  return(Px)
}

.bEXP <- function(d, l = 1) {
  return(dexp(d, rate = l, log = F))
}

.CalcProbSampleEXP <- function(x, X0, nP0, rescale, Px, l) {
  num <- Px * .bEXP(x, l)
  max.dist <- max(X0)
  int.discrete <- X0
  step <- max.dist/length(X0)
  den <- sum((nP0 * .bEXP(int.discrete, l)) * step)
  return(c(sum(log(num) - log(den))))
}

.PlotLikSurfaceEXP <- function(x, X0, nP0, rescale, Px, m = 0.01, M = 5, step = 100,
                               plotextra) {
  # PLOT LIKELIHOOD SURFACE
  plot(nP0 ~ X0, type = "l", ylab = "Probability", xlab = "Rescaled distance",
       main = "Empirical distribution")
  z <- c()
  lambda <- seq(m, M, length.out = step)
  ML <- c(-Inf, 0)
  for (i in 1:length(lambda)) {
    lik <- .CalcProbSampleEXP(x, X0, nP0, rescale, Px, lambda[i])
    z[i] <- lik
  }
  print(c(max(z), 1/lambda[which(z == max(z))]))
  ML.scale <- 1/lambda[which(z == max(z))]
  ML.rate <- lambda[which(z == max(z))]
  plot(z ~ lambda, type = "l", ylab = "Likelihood", main = "Likelihood surface",
       #main=paste("scale:",round(ML.scale,4),"rate:",round(ML.rate,4)),
       xlab = "Rate")
  return(ML.rate)
}


.CalcExpBias <- function(dists, Dmatrix, biasname = "bias", rescale = 0, plotextra = T,
                         distvec) {
  if (rescale == 0) {
    rescale <- 5 * max(dists$X0)
    cat(c("\nrescale factor:", rescale, max(dists$X0), "\n"))
  }
  X0 <- dists$X0/rescale  # binned distances - unit is set to 10,000 Km
  cell_size <- min(X0)
  X0 <- X0 + cell_size
  P0 <- dists$P0  # prob of binned distances # THEY SHOULD SUM UP TO 1 (maybe instead of $density we should take $counts/sum($counts))
  # normalized P0
  nP0 <- P0/sum(P0)
  x <- dists$X/rescale  # samples (observed distances) - unit is set to 10,000 Km
  x <- x[!is.na(x)]
  #x <- sort(x) + cell_size
  #x[x<min(X0)] <- min(X0) # to avoid x values < than min X0

  # NOTES: SMALLER RATE (=GREATER SCALE) -> SMALLER BIAS
  Px <- .ConstructPxVector(x = x, X0 = X0, nP0 = nP0, rescale = rescale, Px = Px, plotextra = plotextra)
  if (plotextra == T) {
    ML.rate <- .PlotLikSurfaceEXP(x, X0, nP0, rescale, Px)
  } else {
    ML.rate <- 1
  }

  .CalcProbSampleEXPOptim <- function(sc) {
    l <- 1/sc
    num <- Px * .bEXP(x, l)
    max.dist <- max(X0)
    int.discrete <- X0
    step <- max.dist/length(X0)
    den <- sum((nP0 * .bEXP(int.discrete, l)) * step)
    return(-sum(log(num) - log(den)))
  }

  OPT <- stats::optim(par = ML.rate, fn = .CalcProbSampleEXPOptim, method = "Brent",
               lower = 1e-02, upper = 10000)
  ML.rate <- 1/OPT$par
  print(ML.rate)
  bias1 <- stats::dexp(0, rate = ML.rate)
  bias2 <- stats::dexp(median(X0), rate = ML.rate)
  bias3 <- stats::dexp(max(X0), rate = ML.rate)
  # rescaled bias curve (at 0 distance -> 0 bians, hence sampling prob = 1)
  dExp <- stats::dexp(X0, rate = ML.rate)/bias1
  bias.vec <- 1 - dExp
  true.dist <- dists$X0/1000
  if (plotextra == T) {
    graphics::plot(bias.vec ~ true.dist, main = paste("Effect of", biasname), xlab = "Distance (Km)",
         ylab = "Sampling bias", type = "l", ylim = c(0, 1))
  }
  d1 <- Dmatrix
  bias.matrix <- 1 - stats::dexp(d1/rescale, rate = ML.rate)/bias1

  dist.vec.bias <- 1 - stats::dexp(distvec/rescale, rate = ML.rate)/bias1
  names(dist.vec.bias) <- distvec

  res <- list(dists$X0, bias.vec, bias.matrix, dist.vec.bias)
  names(res) <- c("distances", "bias", "bias_matrix", "bias_vector")

  return(res)

}
