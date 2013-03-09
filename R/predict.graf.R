predict.graf <-
function(object, newdata = NULL, type = c('response', 'latent'),
                         CI = 0.95, maxn = NULL, ...) {
  type = match.arg(type)
  if (is.null(maxn)) maxn <- round(nrow(object$x) / 10)
  # set up data
  if (is.null(newdata)) {
    # use already set up inference data if not specified
    newdata <- object$x
	# get mean on raw data
	mn <- object$mnfun(object$obsx)

  } else {
    if (is.data.frame(newdata) & all(sapply(object$obsx, class) == sapply(newdata, class))) {
	
	  # get mean on raw data
	  mn <- object$mnfun(newdata)

      k <- ncol(newdata)
      # numericize factors
      for (fac in object$facs) {
        newdata[, fac] <- as.numeric(newdata[, fac])
      }
      # convert to a matrix
      newdata <- as.matrix(newdata)
      # scale, if needed
      if (!is.null(object$scaling)) {
        notfacs <- (1:k)
		if(length(object$facs) > 0) notfacs <- notfacs[-object$facs]
        for(i in 1:k) {
          if (i %in% notfacs) {
            newdata[, i] <- (newdata[, i] - object$scaling[1, i]) / object$scaling[2, i]
          }
        }
      }
    } else {
      stop('newdata must be either a dataframe with the same elements as used for inference, or NULL')
    }
  }

  # check CI
  if (!is.null(CI)) {
    if (CI >= 1 | CI <= 0) {
      stop("CI must be a number between 0 and 1, or NULL")
    }
    err <- qnorm( 1 - (1 - CI) / 2 )
  }
  # latent case
  if (type == 'latent') {

    if (is.null(CI)) {
      # if CIs aren't wanted
      ans <- pred(newdata, object, mn, std = FALSE, maxn = maxn)
      colnames(ans) <- "posterior mean"
    } else {
      # if they are
      pred <- pred(newdata, object, mn, std = TRUE, maxn = maxn)
      upper <- pred[, 1] + err * pred[, 2]
      lower <- pred[, 1] - err * pred[, 2]
      ans <- cbind(pred[, 1], lower, upper)
      colnames(ans) <- c("posterior mean", paste("lower ", round(100 * CI), "% CI", sep = ""),
                         paste("upper ", round(100 * CI), "% CI", sep = ""))
    }
  } else {
    # response case
    if (is.null(CI)) {
      # if CIs aren't required
      ans <- pnorm(pred(newdata, object, mn, std = FALSE, maxn = maxn))
      colnames(ans) <- "posterior mode"
    } else {
      # if CIs are required
      pred <- pred(newdata, object, mn, std = TRUE, maxn = maxn)
      upper <- pred[, 1] + err * pred[, 2]
      lower <- pred[, 1] - err * pred[, 2]
      ans <- pnorm(cbind(pred[, 1], lower, upper))
      colnames(ans) <- c("posterior mode", paste("lower ", round(100 * CI), "% CI", sep = ""),
                         paste("upper ", round(100 * CI), "% CI", sep = ""))
    }
  }
  ans
}