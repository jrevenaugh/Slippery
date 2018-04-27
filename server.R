server <- function(input, output) {
  # Reactive to indicate that model has run and to hold computed stats
  isRun <- reactiveValues(yes = FALSE)
  modelStats <- reactiveValues(
    nEQ = 0,
    nMax = 0,
    mMax = 0,
    mMin = 0,
    longest = 0,
    shortest = 0,
    slip = 0,
    maxOffset = 0,
    minOffset = 0,
    expOffset = 30,
    fMax = 0,
    fMin = 0,
    gapLongest = 0,
    gapShortest = 0
  )

  # Tab Panel 1 ----------------------------------------------------------------
  # Render the time series ggplot
  output$plot1 <- renderPlot({
    rseed <- as.integer(input$random_seed)
    if (is.na(rseed) | is.null(rseed))
      rseed <- 12345678
    set.seed(rseed)
    n <- input$n_segments
    f_scale <- scale_length * 200 / n
    f_min_length <- f_scale / 50
    A <- switch(input$fault_dist,
                "1" = rep(f_scale, n),                              # Equal
                "2" = runif(n, f_min_length, 2 * f_scale),          # Uniform
                "3" = rnorm(n, f_scale, f_scale / 3),               # Gaussian
                "4" = rexp(n, 1 / f_scale)                          # Exponential
    )
    A[A < f_min_length] <- f_min_length
    A <- A * 4000 / cumsum(A)[input$n_segments]

    # Plot the distribution of block lengths
    # Show fault segment lengths as factor of 8 of actual. Attempt to get numbers that are "cleaner"
    df <- data.frame(A = A / 8)
    ghist <- ggplot(df, aes(A)) +
      geom_histogram(bins = floor(15 * n / 50),
                     fill = "lightsteelblue",
                     color = "black") +
      labs(x = "Fault Length", y = "Count")

    ghist
  }, res = 100)

  # Tab Panel 2 ----------------------------------------------------------------
  # Render the cumulative offset plot
  output$plot2 <- renderPlot({
    if (!isRun$yes)
      hide("plot2")
    else
      show("plot2")

    # Plot parameters
    deci <- 5
    nt <- dim(X)[1]
    n <- dim(X)[2]
    dX <- X[seq(1, nt, by = deci), ]
    dt <- seq(1, nt, by = deci)
    y <- cumsum(A) - A[1]
    y <- y / max(y) * 500
    hx <- 0.5 * (y[-1] + y[-n])

    # Cumulative offset plot
    pal = colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))(256)

    par(mar = c(1, 4, 4, 1))
    image(
      x = dt,
      y = y,
      z = dX,
      col = pal,
      axes = FALSE,
      ann = FALSE
    )
    axis(3)
    axis(2)
    mtext("Time", 3, 3)
    mtext("Along Fault Position", 2, 3)
    box()

    abline(h = hx, col = "grey30", lwd = 0.2)
  }, res = 120)

  # Tab Panel 3 ---------------------
  # Render the rupture/slip plot
  output$plot3 <- renderPlot({
    if (!isRun$yes)
      hide("plot3")
    else
      show("plot3")

    # Plot parameters
    deci <- 5
    nt <- dim(X)[1] - 1
    n <- dim(X)[2]
    dX <- X[seq(1, nt, by = deci), ]
    dt <- seq(1, nt, by = deci)
    y <- cumsum(A) - A[1]
    y <- y / max(y) * 500
    hx <- 0.5 * (y[-1] + y[-n])

    # Fault slip plot
    pal = colorRampPalette(
      c("white", "grey90", "grey85", "grey80", "khaki", "orange","steelblue3"),
      bias = 1,
      space = "rgb")(256)

    par(mar = c(1, 4, 4, 1))
    image(x = dt[-1], y = y, z  = log10(0.01 + diff(dX)),
          col = pal,
          axes = FALSE,
          ann = FALSE
    )
    axis(3)
    axis(2)
    mtext("Time", 3, 3)
    mtext("Along Fault Position", 2, 3)
    box()

    abline(h = hx, col = "grey50", lwd = 0.15)
  }, res = 120)

  # Tab Panel 4 ----------------------------------------------------------------
  # Render the stats table
  output$table <- renderTable({
    if (!isRun$yes)
      hide("table")
    else
      show("table")

    snames <- c(
      "Number of earthquakes",
      "Maximum magnitude",
      "Minimum magnitude",
      "Most quakes in unit time",
      "Longest rupture",
      "Greatest slip",
      "Maximum final offset",
      "Minimum final offset",
      "Mean final offset",
      "Most events on segment",
      "Least events on segment",
      "Longest quiescence",
      "Shortest quiescence"
    )

    snums <- c(
      as.integer(modelStats$nEQ),
      round(modelStats$mMax, 2),
      round(modelStats$mMin, 2),
      as.integer(modelStats$nMax),
      round(modelStats$longest, 2),
      round(modelStats$slip, 2),
      round(modelStats$maxOffset, 2),
      round(modelStats$minOffset, 2),
      round(modelStats$expOffset, 2),
      as.integer(modelStats$fMax),
      as.integer(modelStats$fMin),
      as.integer(modelStats$gapLongest),
      as.integer(modelStats$gapShortest)
    )

    df <-
      data.frame(Statistic = snames, Value = as.character(snums))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "lr")

  output$logn <- renderPlot({
    # Compute magnitude statistics.  Begin with magnitude list
    if (!isRun$yes)
      hide("logn")
    else
      show("logn")

    nt <- dim(X)[1] - 1
    n <- dim(X)[2]
    mag <- vector("double", nt * n)
    dX <- diff(X)
    k <- 1
    nEQ <- 0
    nMax <- 0
    fMax <- 0
    fMin <- 1e6
    longest <- 0
    gapLongest <- 0
    gapShortest <- 1e6

    fLength <- 500 / cumsum(A)[input$n_segments]

    # Compile statistics
    # rle here is used to separate distinct ruptures during same time step.
    for (i in 1:nt) {
      M0 <- as.vector(t(dX[i, ]) * A)
      M1 <- ifelse(M0 > 0, 1, 0)
      if (any(M1 > 0)) {
        segs <- rle(M1)
        l <- which(segs$values == 1)
        nMax <- max(nMax, length(l))
        nEQ <- nEQ + length(l)
        ends <- c(0, cumsum(segs$lengths))
        for (j in seq_along(l)) {
          mag[k] <- sum(M0[seq(ends[l[j]] + 1, ends[l[j] + 1])])
          k <- k + 1
          longest <-
            max(longest, sum(A[seq(ends[l[j]] + 1, ends[l[j] + 1])]) * fLength)
        }
      }
    }
    mag <- mag[mag > 0]
    mag <- log10(mag)

    modelStats$nEQ <- nEQ
    modelStats$mMax <- max(mag)
    modelStats$mMin <- min(mag)
    modelStats$nMax <- nMax
    modelStats$longest <- longest
    modelStats$slip <- max(dX)
    modelStats$maxOffset <- max(X[nt, ] - X[1, ])
    modelStats$minOffset <- min(X[nt, ] - X[1, ])

    for (i in 1:n) {
      M1 <- ifelse(dX[, i] > 0, 1, 0)
      if (any(M1)) {
        segs <- rle(M1)
        l <- which(segs$values == 1)
        fMax <- max(fMax, length(l))
        fMin <- min(fMin, length(l))
        l <- which(segs$values == 0)
        ends <- c(0, cumsum(segs$lengths))
        for (j in seq_along(l)) {
          gapLongest <- max(gapLongest, ends[l[j] + 1] - ends[l[j]] + 1)
          gapShortest <-
            min(gapShortest, ends[l[j] + 1] - ends[l[j]] + 1)
        }
      }
    }
    modelStats$fMax <- fMax
    modelStats$fMin <- fMin
    modelStats$gapLongest <- gapLongest
    modelStats$gapShortest <- gapShortest

    # Log( N ) > mag plot
    mag <- sort(mag)
    N <- length(mag):1
    df <- data.frame(N = log10(N), Mag = mag)
    lims <- quantile(mag)[c(2, 4)]
    df %<>% filter(mag >= lims[1] & mag <= lims[2])

    glogn <- ggplot(df, aes(Mag, N)) +
      geom_smooth(method = "lm", color = "lightsteelblue") +
      geom_line() +
      labs(x = "Magnitude",
           y = "Log(N)",
           title = "Gutenberg-Richter Law")

    glogn
  }, res = 100)

  # Run the simulation ---------------------------------------------------------
  observeEvent(input$runIt, {
    isRun$yes <- FALSE
    x <<- runSimulation(
      input$fault_dist,
      input$n_segments,
      input$kexp,
      input$static_base,
      input$random_seed,
      vp,
      scale_length
    )
    X <<- x$X
    A <<- x$A
    isRun$yes <- TRUE
  })
}
