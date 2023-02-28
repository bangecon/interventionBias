interventionBias <-
  function(outcome,
           intervention,
           outcomeVars = NULL,
           interventionVars = NULL,
           data,
           n = 1000,
           effect = c(10, 20, 30, 40)) {
    if (is.null(outcomeVars)) {
      outcomeVars <-
        names(data)[-which(names(data) %in% c(outcome, intervention))]
    }
    if (is.null(interventionVars)) {
      interventionVars <- outcomeVars
    }
    if (max(effect) > 1) {
      effect <- effect / 100
    }
    if (is.numeric(data[[outcome]])) {
      data[[outcome]] <- as.factor(data[[outcome]])
    }
    pi <-
      min(summary(data[[outcome]])) / length(na.omit(data[[outcome]]))
    bias <- (pi / (1 - pi)) * 1 / (1 - effect)
    fit.outcome <- NULL
    fit.intervention <- NULL
    # Randomly sample the data and "inflate" risk for HHC patients
    bs <- data[sample(nrow(data), 1000), which(names(data) %in% c(outcome, intervention, outcomeVars, interventionVars))]
    bs$adjust <- runif(nrow(bs))
    outcome.frame <-
      cbind(bs[outcome], bs[, which(names(bs) %in% outcomeVars)])
    outcomeFormula <- formula(outcome.frame)
    fit.outcome <-
      suppressWarnings(glm(outcomeFormula, data = bs, family = binomial))
    #margins.outcome <- margins(fit.outcome)
    intervention.frame <-
      cbind(bs[intervention], bs[, which(names(bs) %in% interventionVars)])
    interventionFormula <- formula(intervention.frame)
    fit.intervention <-
      suppressWarnings(glm(interventionFormula, data = bs, family = binomial))
    #margins.intervention <- margins(fit.intervention)
    outcomeWIntervention.frame <-
      cbind(bs[outcome], bs[, which(names(bs) %in% outcomeVars)], bs[intervention])
    outcomeWInterventionFormula <- formula(outcomeWIntervention.frame)
    fit.outcomeIntervention <-
      suppressWarnings(glm(outcomeWInterventionFormula, data = bs, family = binomial))
    fit <-
      list(
        fit.intervention = fit.intervention,
        fit.outcome = fit.outcome,
        fit.outcomeIntervention = fit.outcomeIntervention,
        fit.risk = NULL
      )
    for (i in 1:length(effect)) {
      bs$risk <- factor(
        ifelse(bs[[intervention]] == levels(bs[[intervention]])[2] &
                 bs[[outcome]] == levels(bs[[outcome]])[1] &
                 bs$adjust < effect[i], 2, bs[[outcome]]), labels = c('Not At-Risk', 'At-Risk'))
      risk.frame <-
        cbind(bs['risk'], bs[, which(names(bs) %in% outcomeVars)])
      riskFormula <- formula(risk.frame)
      fit$fit.risk[[i]] <-
        suppressWarnings(glm(riskFormula, data = bs, family = binomial))
    }
    out <-
      list(
        fit.intervention = fit.intervention,
        fit.outcome = fit.outcome,
        fit.outcomeIntervention = fit.outcomeIntervention,
        fit.risk = fit$fit.risk
      )
    out
  }
