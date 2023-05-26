interventionBias <-
  function(outcome,
           intervention,
           outcomeVars,
           interventionVars = NULL,
           clusterVar = NULL,
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
      summary(data[[outcome]])[2] / length(na.omit(data[[outcome]]))
    bias <- (pi / (1 - pi)) * 1 / (1 - effect)
    bs <-
      data[sample(nrow(data), n), which(names(data) %in% c(outcome, intervention, outcomeVars, interventionVars))]
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
    glm.intervention = NULL
    glm.outcome = NULL
    glm.outcomeIntervention = NULL
    glm.risk = NULL
    se.intervention = NULL
    se.outcome = NULL
    se.outcomeIntervention = NULL
    se.risk = NULL
    glm.outcome <-
      suppressWarnings(glm(outcomeFormula, data = bs, family = binomial))
    glm.intervention <-
      suppressWarnings(glm(interventionFormula, data = bs, family = binomial))
    glm.outcomeIntervention <-
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
      glm.risk[[i]] <-
        suppressWarnings(glm(riskFormula, data = bs, family = binomial))
    }
    if (is.null(clusterVar)) {
      se.outcome <- summary(glm.outcome)$coefficients[, 2]
      se.intervention <- summary(glm.intervention)$coefficients[, 2]
      se.outcomeIntervention <-
        summary(glm.outcomeIntervention)$coefficients[, 2]
      for (i in 1:length(effect)) {
        se.risk[[i]] <- summary(glm.risk[[i]])$coefficients[, 2]
      }
    } else {
      se.outcome <- glm.outcome |>
        sandwich::vcovCL(cluster = bs[, clusterVar]) |>
        diag()
      se.intervention <- glm.intervention |>
        sandwich::vcovCL(cluster = bs[, clusterVar]) |>
        diag()
      se.outcomeIntervention <- glm.outcomeIntervention |>
        sandwich::vcovCL(cluster = bs[, clusterVar]) |>
        diag()
      for (i in 1:length(effect)) {
        se.risk[[i]] <- glm.risk[[i]] |>
          sandwich::vcovCL(cluster = bs[, clusterVar]) |>
          diag()
      }
      
    }
    out <-
      list(
        glm.intervention,
        glm.outcome,
        glm.outcomeIntervention,
        glm.risk,
        se.intervention,
        se.outcome,
        se.outcomeIntervention,
        se.risk
      )
    out
  }
