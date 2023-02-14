interventionSim <- function(outcome,
                            intervention,
                            outcomeVars = NULL,
                            interventionVars = NULL,
                            data,
                            n = 1000,
                            r = 1000,
                            bias = c(10, 20, 30, 40)) {
  if (is.null(outcomeVars)) {
    outcomeVars <-
      names(data)[-which(names(data) %in% c(outcome, intervention))]
  }
  if (is.null(interventionVars)) {
    interventionVars <- outcomeVars
  }
  sim_results <-
    replicate(
      r,
      interventionBias(
        outcome = outcome,
        intervention = intervention,
        outcomeVars = outcomeVars,
        interventionVars = interventionVars,
        data = data
      )
    )
  coefMat <-
    list(
      intervention = matrix(
        nrow = length(sim_results[[1]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[1]]$coefficients), c(1:r))
      ),
      outcome = matrix(
        nrow = length(sim_results[[2]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[2]]$coefficients), c(1:r))
      ),
      outcomeIntervention = matrix(
        nrow = length(sim_results[[3]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[3]]$coefficients), c(1:r))
      ),
      risk = list()
    )
  for (j in 1:length(bias)) {
    coefMat$risk[[j]] <-
      matrix(
        nrow = length(sim_results[[4, 1]][[j]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[4, 1]][[j]]$coefficients), c(1:r))
      )
  }
  for (i in 1:r) {
    coefMat$intervention[, i] <- sim_results[[1, i]]$coefficients
    coefMat$outcome[, i] <- sim_results[[2, i]]$coefficients
    coefMat$outcomeIntervention[, i] <-
      sim_results[[3, i]]$coefficients
    for (j in 1:length(bias)) {
      coefMat$risk[[j]][, i] <-
        sim_results[[4, i]][[j]]$coefficients
    }
  }
  ### Something to average the coefficients across rows of each completed table.
  coef <-
    matrix(
      nrow = nrow(coefMat$outcomeIntervention),
      ncol = 2 + length(bias),
      dimnames = list(
        rownames(coefMat$outcomeIntervention),
        c(
          'Outcome',
          'Outcome.With.Intervention',
          paste0('Risk', bias)
        )
      )
    )
  coef[, 1] <- c(rowMeans(coefMat$outcome), NA)
  coef[, 2] <- rowMeans(coefMat$outcomeIntervention)
  for (i in 1:length(bias)) {
    coef[, 2 + i] <- c(rowMeans(coefMat$risk[[i]]), NA)
  }
  coef.intervention <- rowMeans(coefMat$intervention)
  coef <- merge(coef.intervention, coef, by = 'row.names', all = TRUE, sort = FALSE)
  rownames(coef) <- coef$Row.names
  coef <- coef[, -which(names(coef) %in% 'Row.names')]
  colnames(coef)[1] <- 'Intervention'
  sd <-
    matrix(
      nrow = nrow(coefMat$outcomeIntervention),
      ncol = 2 + length(bias),
      dimnames = list(
        rownames(coefMat$outcomeIntervention),
        c(
          'Outcome',
          'Outcome.With.Intervention',
          paste0('Risk', bias)
        )
      )
    )
  sd[, 1] <- c(matrixStats::rowSds(coefMat$outcome), NA)
  sd[, 2] <- matrixStats::rowSds(coefMat$outcomeIntervention)
  for (i in 1:length(bias)) {
    sd[, 2 + i] <- c(matrixStats::rowSds(coefMat$risk[[i]]), NA)
  }
  sd.intervention <- matrixStats::rowSds(coefMat$intervention)
  names(sd.intervention) <- rownames(coefMat$intervention)
  sd <- merge(sd.intervention, sd, by = 'row.names', all = TRUE, sort = FALSE)
  rownames(sd) <- sd$Row.names
  sd <- sd[, -which(names(sd) %in% 'Row.names')]
  colnames(sd)[1] <- 'Intervention'
  out <- list(
    coefficients = coef,
    se = sd,
    coefMat = coefMat,
    results = sim_results
  )
  out
}
