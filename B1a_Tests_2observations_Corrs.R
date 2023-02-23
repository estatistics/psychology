

cr_tst(dt_years1, use = "pairwise", method = "spearman", 
       adjust = "none", alpha = 0.1, ci = F, minlength = 5 )$p



corr_tst_2obs <-function (x, y = NULL, use = "pairwise", method = "pearson", 
                   adjust = "holm", alpha = 0.05, ci = TRUE, minlength = 5) 
{
  cl <- match.call()
  if (is.null(y)) {
    r <- cor(x, use = use, method = method)
    sym <- TRUE
    n <- t(!is.na(x)) %*% (!is.na(x))
  }
  else {
    r <- cor(x, y, use = use, method = method)
    sym = FALSE
    n <- t(!is.na(x)) %*% (!is.na(y))
  }
  if ((use == "complete") | (min(n) == max(n))) 
    n <- min(n)
  r <- r/1.001
  t <- (r * sqrt(n - 1))/sqrt(1 - r^2)
  p <- -2 * expm1(pt(abs(t), (n - 1), log.p = TRUE))
  se <- sqrt((1 - r * r)/(n - 2))
  nvar <- ncol(r)
  p[p > 1] <- 1
  if (adjust != "none") {
    if (is.null(y)) {
      lp <- upper.tri(p)
      pa <- p[lp]
      pa <- p.adjust(pa, adjust)
      p[upper.tri(p, diag = FALSE)] <- pa
    }
    else {
      p[] <- p.adjust(p, adjust)
    }
  }
  z <- fisherz(r[lower.tri(r)])
  if (ci) {
    if (min(n) < 4) {
      warning("Number of subjects must be greater than 3 to find confidence intervals.")
    }
    if (sym) {
      ncors <- nvar * (nvar - 1)/2
    }
    else ncors <- prod(dim(r))
    if (adjust != "holm") {
      dif.corrected <- qnorm(1 - alpha/(2 * ncors))
    }
    else {
      ord <- order(abs(z), decreasing = FALSE)
      dif.corrected <- qnorm(1 - alpha/(2 * order(ord)))
    }
    alpha <- 1 - alpha/2
    dif <- qnorm(alpha)
    if (sym) {
      if (is.matrix(n)) {
        sef <- 1/sqrt(n[lower.tri(n)] - 3)
      }
      else {
        sef <- 1/sqrt(n - 3)
      }
      lower <- fisherz2r(z - dif * sef)
      upper <- fisherz2r(z + dif * sef)
      lower.corrected <- fisherz2r(z - dif.corrected * 
                                     sef)
      upper.corrected <- fisherz2r(z + dif.corrected * 
                                     sef)
      ci <- data.frame(lower = lower, r = r[lower.tri(r)], 
                       upper = upper, p = p[lower.tri(p)])
      ci.adj <- data.frame(lower.adj = lower.corrected, 
                           upper.adj = upper.corrected)
      cnR <- abbreviate(colnames(r), minlength = minlength)
      k <- 1
      for (i in 1:(nvar - 1)) {
        for (j in (i + 1):nvar) {
          rownames(ci)[k] <- paste(cnR[i], cnR[j], sep = "-")
          k <- k + 1
        }
      }
    }
    else {
      n.x <- NCOL(x)
      n.y <- NCOL(y)
      z <- fisherz(r)
      if (adjust != "holm") {
        dif.corrected <- qnorm(1 - (1 - alpha)/(n.x * 
                                                  n.y))
      }
      else {
        ord <- order(abs(z), decreasing = FALSE)
        dif.corrected <- qnorm(1 - (1 - alpha)/(order(ord)))
      }
      sef <- 1/sqrt(n - 3)
      lower <- as.vector(fisherz2r(z - dif * sef))
      upper <- as.vector(fisherz2r(z + dif * sef))
      lower.corrected <- fisherz2r(z - dif.corrected * 
                                     sef)
      upper.corrected <- fisherz2r(z + dif.corrected * 
                                     sef)
      ci <- data.frame(lower = lower, r = as.vector(r), 
                       upper = upper, p = as.vector(p))
      ci.adj <- data.frame(lower.adj = as.vector(lower.corrected), 
                           r = as.vector(r), upper.adj = as.vector(upper.corrected))
      cnR <- abbreviate(rownames(r), minlength = minlength)
      cnC <- abbreviate(colnames(r), minlength = minlength)
      k <- 1
      for (i in 1:NCOL(y)) {
        for (j in 1:NCOL(x)) {
          rownames(ci)[k] <- paste(cnR[j], cnC[i], sep = "-")
          k <- k + 1
        }
      }
    }
  }
  else {
    ci <- sef <- ci.adj <- NULL
  }
  result <- list(r = r, n = n, t = t, p = p, se = se, sef = sef, 
                 adjust = adjust, sym = sym, ci = ci, ci.adj = ci.adj, 
                 Call = cl)
  class(result) <- c("psych", "corr.test")
  return(result)
}