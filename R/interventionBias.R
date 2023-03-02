interventionBias <-
  function(outcome,
           intervention,
           outcomeVars = NULL,
           interventionVars = NULL,
           clusterVar = NULL,
           data,
           n = 1000,
           effect = c(10, 20, 30, 40),
           margins = TRUE) {
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
      summary(data[[outcome]])[2] / length(na.omit(data[[outcome]]))
    bias <- (pi / (1 - pi)) * 1 / (1 - effect)
    # Randomly sample the data and "inflate" risk for HHC patients
    bs <-
      data[sample(nrow(data), 1000), which(names(data) %in% c(outcome, intervention, outcomeVars, interventionVars))]
    bs$adjust <- runif(nrow(bs))
    outcome.frame <-
      cbind(bs[outcome], bs[, which(names(bs) %in% outcomeVars)])
    outcomeFormula <- formula(outcome.frame)
    intervention.frame <-
      cbind(bs[intervention], bs[, which(names(bs) %in% interventionVars)])
    interventionFormula <- formula(intervention.frame)
    outcomeWIntervention.frame <-
      cbind(bs[outcome], bs[, which(names(bs) %in% outcomeVars)], bs[intervention])
    outcomeWInterventionFormula <-
      formula(outcomeWIntervention.frame)
    fit <-
      list(
        fit.intervention = NULL,
        fit.outcome = NULL,
        fit.outcomeIntervention = NULL,
        fit.risk = NULL
      )
    mfx <-
      list(
        mfx.intervention = NULL,
        mfx.outcome = NULL,
        mfx.outcomeIntervention = NULL,
        mfx.risk = NULL
      )
    if(is.null(clusterVar)) {
      fit$fit.outcome <-
        suppressWarnings(glm(outcomeFormula, data = bs, family = binomial))
      fit$fit.intervention <-
        suppressWarnings(glm(interventionFormula, data = bs, family = binomial))
      fit$fit.outcomeIntervention <-
        suppressWarnings(glm(
          outcomeWInterventionFormula,
          data = bs,
          family = binomial
        ))
      for (i in 1:length(effect)) {
        bs$risk <- factor(
          ifelse(
            bs[[intervention]] == levels(bs[[intervention]])[2] &
              bs[[outcome]] == levels(bs[[outcome]])[1] &
              bs$adjust < bias[i],
            2,
            bs[[outcome]]
          ),
          labels = c('Not At-Risk', 'At-Risk')
        )
        risk.frame <-
          cbind(bs['risk'], bs[, which(names(bs) %in% outcomeVars)])
        riskFormula <- formula(risk.frame)
        fit$fit.risk[[i]] <-
          suppressWarnings(glm(riskFormula, data = bs, family = binomial))
      }
    } else {
      cluster <- formula(data[,which(names(data) %in% clusterVar)])
      fit$glm.outcome <-
        suppressWarnings(glm(outcomeFormula, data = bs, family = binomial))
      fit$fit.outcome <-
        coeftest(fit$glm.outcome, vcov = vcovCL, cluster = cluster)
      fit$glm.intervention <-
        suppressWarnings(glm(interventionFormula, data = bs, family = binomial))
      fit$fit.intervention <-
        coeftest(fit$glm.intervention, vcov = vcovCL, cluster = cluster)
      fit$glm.outcomeIntervention <-
        suppressWarnings(glm(
          outcomeWInterventionFormula,
          data = bs,
          family = binomial))
      fit$fit.outcomeIntervention <-
        coeftest(fit$glm.outcomeIntervention, vcov = vcovCL, cluster = cluster)
      for (i in 1:length(effect)) {
        bs$risk <- factor(
          ifelse(
            bs[[intervention]] == levels(bs[[intervention]])[2] &
              bs[[outcome]] == levels(bs[[outcome]])[1] &
              bs$adjust < bias[i],
            2,
            bs[[outcome]]
          ),
          labels = c('Not At-Risk', 'At-Risk')
        )
        risk.frame <-
          cbind(bs['risk'], bs[, which(names(bs) %in% outcomeVars)])
        riskFormula <- formula(risk.frame)
        fit$glm.risk[[i]] <-
          suppressWarnings(glm(riskFormula, data = bs, family = binomial))
        fit$fit.risk[[i]] <- coeftest(fit$glm.risk[[i]], vcov = vcovCL, cluster = cluster)
      }
    }
    if(margins == TRUE){
      if (is.null(clusterVar)) {
        mfx$mfx.outcome <-
          marginaleffects(fit$fit.outcome, newdata = bs)
        mfx$mfx.intervention <-
          marginaleffects(fit$fit.intervention, newdata = bs)
        mfx$mfx.outcomeIntervention <-
          marginaleffects(fit$fit.outcomeIntervention, newdata = bs)
        for (i in 1:length(effect)) {
          bs$risk <- factor(
            ifelse(
              bs[[intervention]] == levels(bs[[intervention]])[2] &
                bs[[outcome]] == levels(bs[[outcome]])[1] &
                bs$adjust < bias[i],
              2,
              bs[[outcome]]
            ),
            labels = c('Not At-Risk', 'At-Risk')
          )
          risk.frame <-
            cbind(bs['risk'], bs[, which(names(bs) %in% outcomeVars)])
          riskFormula <- formula(risk.frame)
          mfx$mfx..risk[[i]] <-
            marginaleffects(fit$fit.risk[[1]], newdata = bs)
        }
      } else {
        mfx$mfx.outcome <-
          marginaleffects(fit$glm.outcome, vcov = cluster, newdata = bs)
        mfx$mfx.intervention <-
          marginaleffects(fit$glm.intervention, vcov = cluster, newdata = bs)
        mfx$mfx.outcomeIntervention <-
          marginaleffects(fit$glm.outcomeIntervention,
                          vcov = cluster,
                          newdata = bs)
        for (i in 1:length(effect)) {
          bs$risk <- factor(
            ifelse(
              bs[[intervention]] == levels(bs[[intervention]])[2] &
                bs[[outcome]] == levels(bs[[outcome]])[1] &
                bs$adjust < bias[i],
              2,
              bs[[outcome]]
            ),
            labels = c('Not At-Risk', 'At-Risk')
          )
          risk.frame <-
            cbind(bs['risk'], bs[, which(names(bs) %in% outcomeVars)])
          riskFormula <- formula(risk.frame)
          mfx$mfx.risk[[i]] <-
            marginaleffects(fit$glm.risk[[1]], vcov = cluster, newdata = bs)
        }
      }
    }
    out <-
      list(
        fit.intervention = fit$fit.intervention,
        fit.outcome = fit$fit.outcome,
        fit.outcomeIntervention = fit$fit.outcomeIntervention,
        fit.risk = fit$fit.risk,
        mfx.intervention = mfx$mfx.intervention,
        mfx.outcome = mfx$mfx.outcome,
        mfx.outcomeIntervention = mfx$mfx.outcomeIntervention,
        mfx.risk = mfx$mfx$mfx.risk
      )
    out
  }
