plot.biasSim <- function(biasSim,
                         variable,
                         title = "Distribution of the effect of X",
                         xlab = "Effect",
                         ylab = "Density",
                         beta0 = NULL,
                         labels = NULL,
                         alpha = 0.1) {
  if (is.null(beta0)) {
    beta0 = 0
  }
  if (is.null(labels)) {
    labels <- c("Outcome",
                "Outcome with Intervention",
                paste0("Risk", c(1:length(biasSim$coefMat$risk))))
  }
  temp <-
    as.data.frame(cbind(
      t(hhcSim$coefMat$outcome)[, variable],
      t(hhcSim$coefMat$outcomeIntervention)[, variable],
      t(hhcSim$coefMat$risk[[1]])[, variable],
      t(hhcSim$coefMat$risk[[2]])[, variable],
      t(hhcSim$coefMat$risk[[3]])[, variable],
      t(hhcSim$coefMat$risk[[4]])[, variable]
    ))
  temp_gather <- gather(temp)
  g <- ggplot(data = temp_gather, aes(x = value, fill = key)) +
    geom_density(alpha = alpha) +
    geom_vline(xintercept = beta0) +
    theme(text = element_text(size = 12, family = "serif")) +
    ggtitle(title) +
    labs(x = xlab, y = ylab) +
    scale_fill_discrete(labels = labels)
  g
}
