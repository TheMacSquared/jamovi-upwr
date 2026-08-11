# Pure computational core of jRISK. Everything here is UI-independent and
# covered by tests/testthat; the .b.R files only translate options to these
# calls and render the results.

# ---- lifetime models -------------------------------------------------------
# dist: 'exponential' (rate), 'gamma' (shape, rate), 'weibull' (shape, scale)

riskLtDensity <- function(t, dist, par) {
  switch(dist,
    exponential = dexp(t, par$rate),
    gamma       = dgamma(t, par$shape, par$rate),
    weibull     = dweibull(t, par$shape, par$scale))
}

riskLtCdf <- function(t, dist, par) {
  switch(dist,
    exponential = pexp(t, par$rate),
    gamma       = pgamma(t, par$shape, par$rate),
    weibull     = pweibull(t, par$shape, par$scale))
}

# reliability R(t) computed as the upper tail directly for stability
riskLtReliability <- function(t, dist, par) {
  switch(dist,
    exponential = pexp(t, par$rate, lower.tail = FALSE),
    gamma       = pgamma(t, par$shape, par$rate, lower.tail = FALSE),
    weibull     = pweibull(t, par$shape, par$scale, lower.tail = FALSE))
}

# hazard h(t) = f(t)/R(t); closed forms where they exist, log scale for gamma
riskLtHazard <- function(t, dist, par) {
  switch(dist,
    exponential = rep(par$rate, length(t)),
    weibull     = (par$shape / par$scale) * (t / par$scale)^(par$shape - 1),
    gamma       = exp(dgamma(t, par$shape, par$rate, log = TRUE) -
                      pgamma(t, par$shape, par$rate, lower.tail = FALSE,
                             log.p = TRUE)))
}

riskLtMTTF <- function(dist, par) {
  switch(dist,
    exponential = 1 / par$rate,
    gamma       = par$shape / par$rate,
    weibull     = par$scale * gamma(1 + 1 / par$shape))
}

riskLtMedian <- function(dist, par) {
  switch(dist,
    exponential = qexp(0.5, par$rate),
    gamma       = qgamma(0.5, par$shape, par$rate),
    weibull     = qweibull(0.5, par$shape, par$scale))
}

riskLtQuantile <- function(p, dist, par) {
  switch(dist,
    exponential = qexp(p, par$rate),
    gamma       = qgamma(p, par$shape, par$rate),
    weibull     = qweibull(p, par$shape, par$scale))
}

# 'constant' / 'increasing' / 'decreasing', by the shape parameter
riskLtHazardCharacter <- function(dist, par) {
  shape <- switch(dist,
    exponential = 1,
    gamma       = par$shape,
    weibull     = par$shape)
  if (shape == 1) 'constant'
  else if (shape > 1) 'increasing'
  else 'decreasing'
}

# ---- system reliability ----------------------------------------------------
# A system is a structure function phi: {0,1}^n -> {0,1}, monotone for
# coherent systems. n <= 8 keeps full state enumeration exact and cheap.

riskPhiSeries <- function(n)
  function(x) as.integer(all(x == 1))

riskPhiParallel <- function(n)
  function(x) as.integer(any(x == 1))

riskPhiKofN <- function(n, k)
  function(x) as.integer(sum(x) >= k)

# m blocks in series, each block npb components in parallel;
# component index = (block - 1) * npb + position
riskPhiSeriesParallel <- function(m, npb)
  function(x) {
    blocks <- matrix(x, nrow = npb, ncol = m)
    as.integer(all(colSums(blocks) >= 1))
  }

# m branches in parallel, each branch npb components in series
riskPhiParallelSeries <- function(m, npb)
  function(x) {
    branches <- matrix(x, nrow = npb, ncol = m)
    as.integer(any(colSums(branches) == npb))
  }

# 5-component bridge: e1: S-A, e2: A-T, e4: S-B, e5: B-T, e3: A-B (crossover);
# minimal paths {1,2}, {4,5}, {1,3,5}, {4,3,2}
riskPhiBridge <- function()
  function(x) {
    as.integer((x[1] & x[2]) | (x[4] & x[5]) |
               (x[1] & x[3] & x[5]) | (x[4] & x[3] & x[2]))
  }

riskAllStates <- function(n)
  as.matrix(expand.grid(rep(list(0:1), n)))[, n:1, drop = FALSE]

# exact system reliability by enumeration over all 2^n states
riskSystemReliability <- function(phi, r) {
  n <- length(r)
  states <- riskAllStates(n)
  total <- 0
  for (i in seq_len(nrow(states))) {
    x <- states[i, ]
    if (phi(x) == 1)
      total <- total + prod(ifelse(x == 1, r, 1 - r))
  }
  total
}

# state table: every state vector with phi and its probability
riskStateTable <- function(phi, r) {
  n <- length(r)
  states <- riskAllStates(n)
  data.frame(
    state = apply(states, 1, paste, collapse = ""),
    phi = apply(states, 1, function(x) phi(x)),
    prob = apply(states, 1, function(x) prod(ifelse(x == 1, r, 1 - r))))
}

# minimal path sets: minimal component sets whose joint working makes phi = 1
riskMinimalPaths <- function(phi, n) {
  sets <- list()
  for (i in seq_len(2^n - 1)) {
    members <- which(bitwAnd(i, 2^(0:(n - 1))) > 0)
    x <- integer(n); x[members] <- 1
    if (phi(x) != 1) next
    minimal <- TRUE
    for (j in members) {
      y <- x; y[j] <- 0
      if (phi(y) == 1) { minimal <- FALSE; break }
    }
    if (minimal) sets[[length(sets) + 1]] <- members
  }
  sets
}

# minimal cut sets: minimal component sets whose joint failure makes phi = 0
riskMinimalCuts <- function(phi, n) {
  sets <- list()
  for (i in seq_len(2^n - 1)) {
    members <- which(bitwAnd(i, 2^(0:(n - 1))) > 0)
    x <- rep(1L, n); x[members] <- 0
    if (phi(x) != 0) next
    minimal <- TRUE
    for (j in members) {
      y <- x; y[j] <- 1
      if (phi(y) == 0) { minimal <- FALSE; break }
    }
    if (minimal) sets[[length(sets) + 1]] <- members
  }
  sets
}

# reliability block diagram layout: boxes (component rectangles with labels)
# and edges (connector segments) in abstract coordinates for ggplot
riskDiagramLayout <- function(structure, n, m = NULL, npb = NULL, r = NULL) {
  boxW <- 0.9
  boxH <- 0.55
  boxes <- NULL
  edges <- NULL

  addBox <- function(x, y, i)
    rbind(boxes, data.frame(
      x = x, y = y,
      label = if (is.null(r)) as.character(i)
              else paste(i, "\n", format(r[i], digits = 3), sep = "")))
  addEdge <- function(x, y, xend, yend)
    rbind(edges, data.frame(x = x, y = y, xend = xend, yend = yend))

  if (structure %in% c("series")) {
    for (i in seq_len(n)) {
      boxes <- addBox(1.5 * i, 0, i)
      if (i > 1)
        edges <- addEdge(1.5 * (i - 1) + boxW / 2, 0, 1.5 * i - boxW / 2, 0)
    }
    edges <- addEdge(1.5 - boxW / 2 - 0.6, 0, 1.5 - boxW / 2, 0)
    edges <- addEdge(1.5 * n + boxW / 2, 0, 1.5 * n + boxW / 2 + 0.6, 0)

  } else if (structure %in% c("parallel", "koutofn")) {
    ys <- rev(seq_len(n)) - (n + 1) / 2
    xL <- 0.4
    xR <- 2.6
    for (i in seq_len(n)) {
      boxes <- addBox(1.5, ys[i], i)
      edges <- addEdge(xL, ys[i], 1.5 - boxW / 2, ys[i])
      edges <- addEdge(1.5 + boxW / 2, ys[i], xR, ys[i])
    }
    edges <- addEdge(xL, min(ys), xL, max(ys))
    edges <- addEdge(xR, min(ys), xR, max(ys))
    edges <- addEdge(xL - 0.6, 0, xL, 0)
    edges <- addEdge(xR, 0, xR + 0.6, 0)

  } else if (structure == "seriesParallel") {
    # m blocks in series, npb parallel components per block
    idx <- 0
    for (b in seq_len(m)) {
      xC <- 2.6 * b
      xL <- xC - 1.1
      xR <- xC + 1.1
      ys <- rev(seq_len(npb)) - (npb + 1) / 2
      for (j in seq_len(npb)) {
        idx <- idx + 1
        boxes <- addBox(xC, ys[j], idx)
        edges <- addEdge(xL, ys[j], xC - boxW / 2, ys[j])
        edges <- addEdge(xC + boxW / 2, ys[j], xR, ys[j])
      }
      edges <- addEdge(xL, min(ys), xL, max(ys))
      edges <- addEdge(xR, min(ys), xR, max(ys))
      if (b > 1)
        edges <- addEdge(2.6 * (b - 1) + 1.1, 0, xL, 0)
    }
    edges <- addEdge(2.6 - 1.1 - 0.6, 0, 2.6 - 1.1, 0)
    edges <- addEdge(2.6 * m + 1.1, 0, 2.6 * m + 1.1 + 0.6, 0)

  } else if (structure == "parallelSeries") {
    # m parallel branches, npb series components per branch
    ys <- rev(seq_len(m)) - (m + 1) / 2
    xL <- 0.4
    xR <- 1.5 * npb + 1.1
    idx <- 0
    for (b in seq_len(m)) {
      for (j in seq_len(npb)) {
        idx <- idx + 1
        xC <- 1.5 * j + 0.5
        boxes <- addBox(xC, ys[b], idx)
        if (j > 1)
          edges <- addEdge(1.5 * (j - 1) + 0.5 + boxW / 2, ys[b], xC - boxW / 2, ys[b])
      }
      edges <- addEdge(xL, ys[b], 2 - boxW / 2, ys[b])
      edges <- addEdge(1.5 * npb + 0.5 + boxW / 2, ys[b], xR, ys[b])
    }
    edges <- addEdge(xL, min(ys), xL, max(ys))
    edges <- addEdge(xR, min(ys), xR, max(ys))
    edges <- addEdge(xL - 0.6, 0, xL, 0)
    edges <- addEdge(xR, 0, xR + 0.6, 0)

  } else if (structure == "bridge") {
    # nodes: S(0,0), A(2,1), B(2,-1), T(4,0); e3 is the A-B crossover
    boxes <- addBox(1, 1, 1)     # e1: S-A
    boxes <- addBox(3, 1, 2)     # e2: A-T
    boxes <- addBox(2, 0, 3)     # e3: A-B
    boxes <- addBox(1, -1, 4)    # e4: S-B
    boxes <- addBox(3, -1, 5)    # e5: B-T
    edges <- addEdge(0, 0, 0.4, 1)                       # S up
    edges <- addEdge(0.4, 1, 1 - boxW / 2, 1)
    edges <- addEdge(1 + boxW / 2, 1, 3 - boxW / 2, 1)   # A rail
    edges <- addEdge(3 + boxW / 2, 1, 3.6, 1)
    edges <- addEdge(3.6, 1, 4, 0)                       # to T
    edges <- addEdge(0, 0, 0.4, -1)                      # S down
    edges <- addEdge(0.4, -1, 1 - boxW / 2, -1)
    edges <- addEdge(1 + boxW / 2, -1, 3 - boxW / 2, -1) # B rail
    edges <- addEdge(3 + boxW / 2, -1, 3.6, -1)
    edges <- addEdge(3.6, -1, 4, 0)
    edges <- addEdge(2, 1, 2, boxH / 2)                  # A to e3
    edges <- addEdge(2, -boxH / 2, 2, -1)                # e3 to B
    edges <- addEdge(-0.6, 0, 0, 0)
    edges <- addEdge(4, 0, 4.6, 0)
  }

  list(boxes = boxes, edges = edges, boxW = boxW, boxH = boxH)
}

# coherence: phi monotone in every argument and every component relevant
riskCoherence <- function(phi, n) {
  states <- riskAllStates(n)
  monotone <- TRUE
  relevant <- rep(FALSE, n)
  for (i in seq_len(nrow(states))) {
    x <- states[i, ]
    fx <- phi(x)
    for (j in seq_len(n)) {
      if (x[j] == 0) {
        y <- x; y[j] <- 1
        fy <- phi(y)
        if (fy < fx) monotone <- FALSE
        if (fy != fx) relevant[j] <- TRUE
      }
    }
  }
  list(monotone = monotone, relevant = relevant,
       coherent = monotone && all(relevant))
}
