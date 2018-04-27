# Slider simulator for use in Shiny app
runSimulation <- function(fault_dist,
                          n_segments,
                          kexp,
                          static_base,
                          rseed,
                          vp = 0.02,
                          scale_length = 20) {
  # Get segment length distribution
  rseed <- as.integer(rseed)
  if (is.na(rseed) | is.null(rseed))
    rseed <- 12345678
  set.seed(rseed)
  n <- n_segments
  f_scale <- scale_length * 200 / n
  f_min_length <- f_scale / 50
  A <- switch(fault_dist,
              "1" = rep(f_scale, n),                       # Equal
              "2" = runif(n, f_min_length, 2 * f_scale),   # Uniform
              "3" = rnorm(n, f_scale, f_scale / 3),        # Gaussian
              "4" = rexp(n, 1 / f_scale)                   # Exponential
       )

  A[A < f_min_length] <- f_min_length

  # Establish run parameters
  static <- static_base / mean(A)
  kexp <- 4 / kexp
  k1 <- 2
  k2 <- 150
  k3 <- k2 / kexp
  k4 <- k2 / kexp ^ 2

  # Simulation duration
  t_steps <- 1505
  t_off <- 400
  t <- seq(1, t_steps + t_off, 1)
  nt <- length(t)

  # Initial block displacement (include some noise and a offset to strain blocks)
  doff <- static * max(A) * k1 * 2
  X <- matrix(0, ncol = n, nrow = nt)
  X[1, ] <- rnorm(n, -doff, doff * 0.02)

  # Some useful indices
  j <- 1:n
  l <- j + 3
  i <- 1

  # The guts
  while (i <= nt) {
    x <- X[i, ]
    px <- vp * t[i]
    xl <- c(x[n - 2], x[n - 1], x[n], x, x[1], x[2], x[3])
    Fs <- xl[l - 1] + xl[l + 1] - 2 * xl[l]
    Fl <- xl[l - 2] + xl[l + 2] - 2 * xl[l]
    Fd <- xl[l - 3] + xl[l + 3] - 2 * xl[l]
    Fp <- px - x
    Feff <- k1 * Fp + k2 * Fs + k3 * Fl + k4 * Fd - A * static
    lb <- which(Feff > 0)
    if (length(lb) == 0) {
      i <- i + 1
      if (i <= nt)
        X[i, ] <- X[i - 1, ]
      next
    }
    dx <- (k2 * (xl[lb + 2] + xl[lb + 4]) +
             k3 * (xl[lb + 1] + xl[lb + 5]) +
             k4 * (xl[lb] + xl[lb + 6]) +
             k1 * px) / (2 * (k2 + k3 + k4) + k1) - x[lb]
    dx[dx < 0] <- 0
    X[i, lb] <-
      X[i, lb] + dx * (1 + rexp(n = length(lb), rate = 10))
  }

  # Clip off the front toff time slices (reaching "equilibrium" )
  X <- X[-(1:t_off), ]
  nt <- t_steps
  return(list(X = X, A = A))
}
