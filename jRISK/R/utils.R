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

# ---- censored maximum likelihood (right censoring) -------------------------
# t: observed times, status: 1 = failure observed, 0 = right-censored.
# Parameters are optimized on the log scale so positivity needs no constraint;
# CIs come from the log-scale Hessian and are exponentiated back.

riskLtLogLik <- function(logpar, t, status, dist) {
  if (dist == "exponential") {
    rate <- exp(logpar[1])
    sum(status * dexp(t, rate, log = TRUE) +
        (1 - status) * pexp(t, rate, lower.tail = FALSE, log.p = TRUE))
  } else if (dist == "gamma") {
    shape <- exp(logpar[1]); rate <- exp(logpar[2])
    sum(status * dgamma(t, shape, rate, log = TRUE) +
        (1 - status) * pgamma(t, shape, rate, lower.tail = FALSE, log.p = TRUE))
  } else {
    shape <- exp(logpar[1]); scale <- exp(logpar[2])
    sum(status * dweibull(t, shape, scale, log = TRUE) +
        (1 - status) * pweibull(t, shape, scale, lower.tail = FALSE, log.p = TRUE))
  }
}

riskLtFit <- function(t, status, dist) {
  n <- length(t)
  events <- sum(status)
  fail <- function(msg)
    list(ok = FALSE, message = msg, dist = dist)
  if (events == 0)
    return(fail("noEvents"))

  if (dist == "exponential") {
    # closed-form MLE with censoring; var(log rate) = 1/events
    rate <- events / sum(t)
    logpar <- log(rate)
    seLog <- 1 / sqrt(events)
    singular <- FALSE
    k <- 1
  } else {
    tev <- t[status == 1]
    m <- mean(tev)
    v <- if (length(tev) > 1) var(tev) else NA
    start <- if (dist == "gamma") {
      if (is.na(v) || v <= 0) c(0, log(1 / max(m, 1e-6)))
      else log(c(max(m^2 / v, 0.1), max(m / v, 1e-6)))
    } else {
      log(c(1.2, max(m, 1e-6)))
    }
    opt <- try(suppressWarnings(
      optim(start, riskLtLogLik, t = t, status = status, dist = dist,
            control = list(fnscale = -1, maxit = 500),
            method = "BFGS", hessian = TRUE)), silent = TRUE)
    if (inherits(opt, "try-error") || opt$convergence != 0)
      return(fail("noConvergence"))
    logpar <- opt$par
    vc <- try(solve(-opt$hessian), silent = TRUE)
    if (inherits(vc, "try-error") || any(!is.finite(diag(vc))) || any(diag(vc) < 0)) {
      seLog <- rep(NA_real_, length(logpar))
      singular <- TRUE
    } else {
      seLog <- sqrt(diag(vc))
      singular <- FALSE
    }
    k <- length(logpar)
  }

  est <- exp(logpar)
  lower <- exp(logpar - 1.96 * seLog)
  upper <- exp(logpar + 1.96 * seLog)
  parNames <- switch(dist,
    exponential = "rate",
    gamma = c("shape", "rate"),
    weibull = c("shape", "scale"))
  names(est) <- names(lower) <- names(upper) <- parNames

  ll <- riskLtLogLik(logpar, t, status, dist)
  list(ok = TRUE, dist = dist, par = as.list(est),
       lower = as.list(lower), upper = as.list(upper),
       singular = singular, events = events, censored = n - events,
       logLik = ll, AIC = -2 * ll + 2 * k, BIC = -2 * ll + k * log(n))
}

# Kaplan-Meier curve via the survival package (right censoring)
riskKaplanMeier <- function(t, status) {
  sf <- survival::survfit(survival::Surv(t, status) ~ 1)
  med <- unname(quantile(sf, probs = 0.5)$quantile)
  list(time = sf$time, surv = sf$surv, nEvents = sum(sf$n.event),
       median = med)
}

# ---- fault tree (two-level AND/OR) -----------------------------------------
# Basic events with occurrence probabilities `probs` grouped into branches
# (`branch` a vector of branch ids); `innerGate`/`topGate` are "and"/"or".
# All events are assumed independent and distinct.

riskFtaBranchProb <- function(p, gate)
  if (gate == "and") prod(p) else 1 - prod(1 - p)

riskFtaTopProb <- function(probs, branch, innerGate, topGate) {
  bp <- vapply(split(probs, branch), riskFtaBranchProb, 0, gate = innerGate)
  riskFtaBranchProb(bp, topGate)
}

# occurrence function of the top event over basic-event indicators;
# minimal CUT sets of the tree are the minimal PATH sets of this function
riskFtaOccurrence <- function(branch, innerGate, topGate) {
  idx <- split(seq_along(branch), branch)
  function(x) {
    b <- vapply(idx, function(i)
      if (innerGate == "and") as.integer(all(x[i] == 1))
      else as.integer(any(x[i] == 1)), 0L)
    if (topGate == "and") as.integer(all(b == 1)) else as.integer(any(b == 1))
  }
}

# importance: drop in P(top) when event i is made impossible (p_i = 0)
riskFtaImportance <- function(probs, branch, innerGate, topGate) {
  top <- riskFtaTopProb(probs, branch, innerGate, topGate)
  vapply(seq_along(probs), function(i) {
    p0 <- probs
    p0[i] <- 0
    top - riskFtaTopProb(p0, branch, innerGate, topGate)
  }, 0)
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

# two-level structure with arbitrary group sizes (components assigned to
# groups consecutively): inner gate within each group, outer gate across
# groups; "series" = all must work, "parallel" = at least one must work
riskPhiTwoLevel <- function(groupSizes, inner, outer) {
  ends <- cumsum(groupSizes)
  starts <- c(1, head(ends, -1) + 1)
  function(x) {
    g <- vapply(seq_along(groupSizes), function(j) {
      xs <- x[starts[j]:ends[j]]
      if (inner == "series") as.integer(all(xs == 1))
      else as.integer(any(xs == 1))
    }, 0L)
    if (outer == "series") as.integer(all(g == 1)) else as.integer(any(g == 1))
  }
}

# closed-form reliability of the two-level structure (independent components);
# unlike the enumeration this scales to any component count
riskTwoLevelReliability <- function(r, groupSizes, inner, outer) {
  ends <- cumsum(groupSizes)
  starts <- c(1, head(ends, -1) + 1)
  gRel <- vapply(seq_along(groupSizes), function(j) {
    rs <- r[starts[j]:ends[j]]
    if (inner == "series") prod(rs) else 1 - prod(1 - rs)
  }, 0)
  if (outer == "series") prod(gRel) else 1 - prod(1 - gRel)
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

# block-diagram layout for the two-level structure with arbitrary group
# sizes and optional custom component labels (data mode of relsystem)
riskDiagramLayoutTwoLevel <- function(groupSizes, inner, outer,
                                      r = NULL, labels = NULL) {
  n <- sum(groupSizes)
  if (is.null(labels))
    labels <- as.character(seq_len(n))
  boxW <- 0.9
  boxH <- 0.55
  mkLabel <- function(i)
    if (is.null(r)) labels[i]
    else paste(labels[i], "\n", format(r[i], digits = 3), sep = "")
  boxes <- NULL
  edges <- NULL
  addBox <- function(x, y, i)
    rbind(boxes, data.frame(x = x, y = y, label = mkLabel(i)))
  addEdge <- function(x, y, xend, yend)
    rbind(edges, data.frame(x = x, y = y, xend = xend, yend = yend))

  # degenerate cases collapse to a flat series / parallel of all components
  if (inner == "series" && outer == "series") {
    for (i in seq_len(n)) {
      boxes <- addBox(1.5 * i, 0, i)
      if (i > 1)
        edges <- addEdge(1.5 * (i - 1) + boxW / 2, 0, 1.5 * i - boxW / 2, 0)
    }
    edges <- addEdge(1.5 - boxW / 2 - 0.6, 0, 1.5 - boxW / 2, 0)
    edges <- addEdge(1.5 * n + boxW / 2, 0, 1.5 * n + boxW / 2 + 0.6, 0)
  } else if (inner == "parallel" && outer == "parallel") {
    ys <- rev(seq_len(n)) - (n + 1) / 2
    xL <- 0.4; xR <- 2.6
    for (i in seq_len(n)) {
      boxes <- addBox(1.5, ys[i], i)
      edges <- addEdge(xL, ys[i], 1.5 - boxW / 2, ys[i])
      edges <- addEdge(1.5 + boxW / 2, ys[i], xR, ys[i])
    }
    edges <- addEdge(xL, min(ys), xL, max(ys))
    edges <- addEdge(xR, min(ys), xR, max(ys))
    edges <- addEdge(xL - 0.6, 0, xL, 0)
    edges <- addEdge(xR, 0, xR + 0.6, 0)
  } else if (outer == "series") {
    # parallel groups chained in series, group j drawn as a stacked block
    idx <- 0
    xPos <- 0
    for (j in seq_along(groupSizes)) {
      sz <- groupSizes[j]
      xC <- xPos + 1.3
      xL <- xC - 1.1; xR <- xC + 1.1
      ys <- rev(seq_len(sz)) - (sz + 1) / 2
      for (k in seq_len(sz)) {
        idx <- idx + 1
        boxes <- addBox(xC, ys[k], idx)
        edges <- addEdge(xL, ys[k], xC - boxW / 2, ys[k])
        edges <- addEdge(xC + boxW / 2, ys[k], xR, ys[k])
      }
      edges <- addEdge(xL, min(ys), xL, max(ys))
      edges <- addEdge(xR, min(ys), xR, max(ys))
      if (j > 1)
        edges <- addEdge(xPos - 0.2, 0, xL, 0)
      xPos <- xR + 0.2
    }
    edges <- addEdge(0.2 - 0.6, 0, 0.2, 0)
    edges <- addEdge(xPos - 0.2, 0, xPos + 0.4, 0)
  } else {
    # series branches stacked in parallel, branch j has its own length
    ys <- rev(seq_along(groupSizes)) - (length(groupSizes) + 1) / 2
    maxLen <- max(groupSizes)
    xL <- 0.4
    xR <- 1.5 * maxLen + 1.1
    idx <- 0
    for (j in seq_along(groupSizes)) {
      sz <- groupSizes[j]
      for (k in seq_len(sz)) {
        idx <- idx + 1
        xC <- 1.5 * k + 0.5
        boxes <- addBox(xC, ys[j], idx)
        if (k > 1)
          edges <- addEdge(1.5 * (k - 1) + 0.5 + boxW / 2, ys[j], xC - boxW / 2, ys[j])
      }
      edges <- addEdge(xL, ys[j], 2 - boxW / 2, ys[j])
      edges <- addEdge(1.5 * sz + 0.5 + boxW / 2, ys[j], xR, ys[j])
    }
    edges <- addEdge(xL, min(ys), xL, max(ys))
    edges <- addEdge(xR, min(ys), xR, max(ys))
    edges <- addEdge(xL - 0.6, 0, xL, 0)
    edges <- addEdge(xR, 0, xR + 0.6, 0)
  }

  list(boxes = boxes, edges = edges, boxW = boxW, boxH = boxH)
}

# Birnbaum importance: B_j = dR/dr_j = R(component j perfect) - R(j failed);
# relFun maps a reliability vector to the system reliability, so the same
# helper serves enumeration-based and closed-form structures
riskBirnbaum <- function(relFun, r) {
  vapply(seq_along(r), function(j) {
    r1 <- r; r1[j] <- 1
    r0 <- r; r0[j] <- 0
    relFun(r1) - relFun(r0)
  }, 0)
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
