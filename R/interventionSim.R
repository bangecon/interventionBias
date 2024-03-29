interventionSim <- function(outcome,
                            intervention,
                            outcomeVars,
                            interventionVars = NULL,
                            clusterVar = NULL,
                            data,
                            n = 1000,
                            r = 1000,
                            effect = c(10, 20, 30, 40),
                            margins = FALSE,
                            parallel = TRUE) {
  if (is.null(outcomeVars)) {
    outcomeVars <-
      names(data)[-which(names(data) %in% c(outcome, intervention))]
  }
  if (is.null(interventionVars)) {
    interventionVars <- outcomeVars
  }
  if (parallel == TRUE) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    parallel::clusterEvalQ(cl, library(interventionBias))
    parallel::clusterExport(
      cl,
      list(
        'outcome',
        'intervention',
        'outcomeVars',
        'interventionVars',
        'clusterVar',
        'data',
        'n',
        'effect',
        'margins'
      ),
      envir = environment()
    )
    sim_results <- parallel::parSapply(cl,
                                       1:r,
                                       function(outcome,
                                                intervention,
                                                outcomeVars,
                                                interventionVars,
                                                clusterVar,
                                                data,
                                                n,
                                                effect) {
                                         interventionBias(
                                           outcome = outcome,
                                           intervention = intervention,
                                           outcomeVars = outcomeVars,
                                           interventionVars = interventionVars,
                                           clusterVar = clusterVar,
                                           data = data,
                                           n = n,
                                           effect = effect
                                         )
                                       })
    # clusterVar and effect possibly not clusterExport-ing correctly?
    parallel::stopCluster(cl)
  } else {
    sim_results <-
      replicate(
        r,
        interventionBias(
          outcome = outcome,
          intervention = intervention,
          outcomeVars = outcomeVars,
          interventionVars = interventionVars,
          clusterVar = clusterVar,
          data = data,
          effect = effect
        )
      )
  }
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
  for(j in 1:length(effect)) {
    coefMat$risk[[j]] <- matrix(
      nrow = length(sim_results[[2]]$coefficients),
      ncol = r,
      dimnames = list(names(sim_results[[2]]$coefficients), c(1:r))
    )
    seMat$risk[[j]] <- matrix(
      nrow = length(sim_results[[2]]$coefficients),
      ncol = r,
      dimnames = list(names(sim_results[[2]]$coefficients), c(1:r))
    )
  }
  for (i in 1:r) {
    coefMat$intervention[, i] <- sim_results[[1, i]]$coefficients
    coefMat$outcome[, i] <- sim_results[[2, i]]$coefficients
    coefMat$outcomeIntervention[, i] <-
      sim_results[[3, i]]$coefficients
    seMat$intervention[, i] <- sim_results[5, ][[i]]
    seMat$outcome[, i] <- sim_results[6, ][[i]]
    seMat$outcomeIntervention[, i] <- sim_results[7, ][[i]]
    for (j in 1:length(effect)) {
      coefMat$risk[[j]][, i] <-
        sim_results[[4, i]][[j]]$coefficients
      seMat$risk[[j]][, i] <- sim_results[8,][[i]][[j]]
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
  mfx <-
    matrix(
      nrow = nrow(mfxMat$outcomeIntervention),
      ncol = 2 + length(effect),
      dimnames = list(
        make.names(rownames(mfxMat$outcomeIntervention)),
        c(
          'Outcome',
          'Outcome.With.Intervention',
          paste0('EffectSize', effect)
        )
      )
    )
  mfx[, 1] <- c(rowMeans(mfxMat$outcome), NA)
  mfx[, 2] <- rowMeans(mfxMat$outcomeIntervention)
  for (i in 1:length(effect)) {
    mfx[, 2 + i] <- c(rowMeans(mfxMat$risk[[i]]), NA)
  }
  mfx.intervention <- matrix(rowMeans(mfxMat$intervention),
                             dimnames = list(make.names(rownames(
                               mfxMat$intervention
                             )), NULL))
  # mfx <-
  #   merge(mfx.intervention,
  #         mfx,
  #         by = 'row.names',
  #         all = TRUE,
  #         sort = FALSE)
  # rownames(mfx) <-
  #   rownames(coef)[-which(rownames(coef) %in% "(Intercept)")]
  # mfx <- mfx[,-which(names(mfx) %in% 'Row.names')]
  # colnames(mfx)[1] <- 'Intervention'
  # mfxse <-
  #   matrix(
  #     nrow = nrow(mfxMat$outcomeIntervention),
  #     ncol = 2 + length(effect),
  #     dimnames = list(
  #       rownames(mfxMat$outcomeIntervention),
  #       c(
  #         'Outcome',
  #         'Outcome.With.Intervention',
  #         paste0('EffectSize', effect)
  #       )
  #     )
  #   )
  # mfxse[, 1] <- c(rowMeans(mfxseMat$outcome), NA)
  # mfxse[, 2] <- rowMeans(mfxseMat$outcomeIntervention)
  # for (i in 1:length(effect)) {
  #   mfxse[, 2 + i] <- c(rowMeans(mfxseMat$risk[[i]]), NA)
  # }
  # mfxse.intervention <- matrix(rowMeans(mfxseMat$intervention),
  #                              dimnames = list(make.names(rownames(
  #                                mfxseMat$intervention
  #                              )), NULL))
  # names(mfxse.intervention) <- rownames(mfxMat$intervention)
  # mfxse <-
  #   merge(
  #     mfxse.intervention,
  #     mfxse,
  #     by = 'row.names',
  #     all = TRUE,
  #     sort = FALSE
  #   )
  # rownames(mfxse) <-
  #   rownames(coef)[-which(rownames(coef) %in% "(Intercept)")]
  # mfxse <- mfxse[,-which(names(mfxse) %in% 'Row.names')]
  # colnames(mfxse)[1] <- 'Intervention'
  #
  out <- list(
    coefficients = coef,
    se = se,
    # margins = mfx,
    # marginsSE = mfxse,
    coefMat = coefMat,
    results = sim_results
  )
  class(out) <- c('biasSim', class(out))
  out
}
