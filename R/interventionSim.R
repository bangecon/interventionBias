interventionSim <- function(outcome,
                            intervention,
                            outcomeVars = NULL,
                            interventionVars = NULL,
                            data,
                            n = 1000,
                            r = 1000,
                            effect = c(10, 20, 30, 40)) {
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
  names(sim_results) <-
    c("Intervention", "Outcome", "OutcomeIntervention", "Risk")
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
  seMat <-
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
  for (j in 1:length(effect)) {
    coefMat$risk[[j]] <-
      matrix(
        nrow = length(sim_results[[4, 1]][[j]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[4, 1]][[j]]$coefficients), c(1:r))
      )
    seMat$risk[[j]] <-
      matrix(
        nrow = length(sim_results[[4, 1]][[j]]$coefficients),
        ncol = r,
        dimnames = list(names(sim_results[[4, 1]][[j]]$coefficients), c(1:r))
      )
  }
  for (i in 1:r) {
    coefMat$intervention[, i] <- sim_results[[1, i]]$coefficients
    seMat$intervention[, i] <- summary(sim_results[[1, i]])$coefficients[,2]
    coefMat$outcome[, i] <- sim_results[[2, i]]$coefficients
    seMat$outcome[, i] <- summary(sim_results[[2, i]])$coefficients[,2]
    coefMat$outcomeIntervention[, i] <-
      sim_results[[3, i]]$coefficients
    seMat$outcomeIntervention[, i] <-
      summary(sim_results[[3, i]])$coefficients[,2]
    for (j in 1:length(effect)) {
      coefMat$risk[[j]][, i] <-
        sim_results[[4, i]][[j]]$coefficients
      seMat$risk[[j]][, i] <-
        summary(sim_results[[4, i]][[j]])$coefficients[2]
      }
  }
  ### Something to average the coefficients across rows of each completed table.
  coef <-
    matrix(
      nrow = nrow(coefMat$outcomeIntervention),
      ncol = 2 + length(effect),
      dimnames = list(
        rownames(coefMat$outcomeIntervention),
        c(
          'Outcome',
          'Outcome.With.Intervention',
          paste0('EffectSize', effect)
        )
      )
    )
  coef[, 1] <- c(rowMeans(coefMat$outcome), NA)
  coef[, 2] <- rowMeans(coefMat$outcomeIntervention)
  for (i in 1:length(effect)) {
    coef[, 2 + i] <- c(rowMeans(coefMat$risk[[i]]), NA)
  }
  coef.intervention <- rowMeans(coefMat$intervention)
  coef <-
    merge(
      coef.intervention,
      coef,
      by = 'row.names',
      all = TRUE,
      sort = FALSE
    )
  rownames(coef) <- coef$Row.names
  coef <- coef[,-which(names(coef) %in% 'Row.names')]
  colnames(coef)[1] <- 'Intervention'
  se <-
    matrix(
      nrow = nrow(coefMat$outcomeIntervention),
      ncol = 2 + length(effect),
      dimnames = list(
        rownames(coefMat$outcomeIntervention),
        c(
          'Outcome',
          'Outcome.With.Intervention',
          paste0('EffectSize', effect)
        )
      )
    )
  se[, 1] <- c(rowMeans(seMat$outcome), NA)
  se[, 2] <- rowMeans(seMat$outcomeIntervention)
  for (i in 1:length(effect)) {
    se[, 2 + i] <- c(rowMeans(seMat$risk[[i]]), NA)
  }
  se.intervention <- rowMeans(seMat$intervention)
  names(se.intervention) <- rownames(coefMat$intervention)
  se <-
    merge(se.intervention,
          se,
          by = 'row.names',
          all = TRUE,
          sort = FALSE)
  rownames(se) <- se$Row.names
  se <- se[,-which(names(se) %in% 'Row.names')]
  colnames(se)[1] <- 'Intervention'
  out <- list(
    coefficients = coef,
    se = se,
    coefMat = coefMat,
    results = sim_results
  )
  class(out) <- c('biasSim', class(out))
  out
}
