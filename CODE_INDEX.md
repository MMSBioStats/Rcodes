# Index

A searchable index for all R scripts in this repository.

---

## [`Bayesian Modeling`](BayesianModeling)

- [`BayesLogBinomialRESAmpleCode.R`](BayesianModeling/BayesLogBinomialRESAmpleCode.R)  
  Bayesian analysis for binary primary outcome
  Using log binomial with one random effect

- [`BayesLogisticRRSampleCode.R`](BayesianModeling/BayesLogisticRRSampleCode.R)
  sample code to run bayes logistic and calculate RR
  prior for log OR is based on prior for RR
  using rstanarm package

- [`BayesLogisticRRSampleCodeBRMS.R`](BayesianModeling/BayesLogisticRRSampleCodeBRMS.R)
  sample code to run bayes logistic and calculate RR
  prior for log OR is based on prior for RR
  using brm package

- [`BayesLogisticRelRisk.R`](BayesianModeling/BayesLogisticRelRisk.R)
  Calculating RR from binomial logistic reg
  https://discourse.mc-stan.org/t/binomial-regression-with-a-log-link/4102/5

- [`BayesMarginalMeansExample.RData`](BayesianModeling/BayesMarginalMeansExample.RData)
  data sample for bayes marginal means

- [`BayesMarginalPostMeans.R`](BayesianModeling/BayesMarginalPostMeans.R)
  Analysis of repeat grade status using a Bayesian GLM with posterior predictions, emmeans, and marginal effects

- [`BayesMarginalPostMeansvsAME.R`](BayesianModeling/BayesMarginalPostMeansvsAME.R)
  Uses Bayesian GLM via brms to model grade repetition status based on gender and parental education.
  Posterior predictions, emmeans-based estimates, and average marginal effects are computed

- [`BayesRRAprox.R`](BayesianModeling/BayesRRAprox.R)
  Uses neutral and expert priors to calculate posterior RRs for STOP-IT trial outcomes.
  Demonstrates combining prior and observed log(RR) estimates, deriving posterior estimates, and computing posterior probabilities.

- [`ECMOBayesRRAprox.R`](BayesianModeling/ECMOBayesRRAprox.R)
  Re-analysis of the ECMO trial using Bayesian methods.
  Calculates posterior risk ratios (RR) and hazard ratios (HR) for 60-day mortality and adverse events (bleeding) under neutral and CESAR-derived priors.
  Provides posterior probabilities for efficacy and harm.

- [`PosteriorPlotsBRMS_Rstanarm.R`](BayesianModeling/PosteriorPlotsBRMS_Rstanarm.R)
  Creating pretty plots from rstanarm or brms fitted models

- [`PosteriorPriorPlots.R`](BayesianModeling/PosteriorPriorPlots.R)
  Generates posterior and prior distributions for a Bayesian hazard ratio analysis using a neutral prior with shaded treatment benefit/harm regions.
  Produces visualizations comparing prior and posterior curves with emphasis on interpretability in clinical trial context.

- [`PosteriorPriorPlotsClass09_2024.R`](BayesianModeling/PosteriorPriorPlotsClass09_2024.R)
  Visualizes prior and posterior distributions for relative risk using a neutral prior and observed data with interpretation regions shaded.
  Outputs multiple plots to illustrate treatment effect, credible intervals, and probability of benefit vs. harm.

- [`PosteriorPriorPlotsClass12_2021.R`](BayesianModeling/PosteriorPriorPlotsClass12_2021.R)
  Plots prior and posterior hazard ratio distributions with shaded regions for treatment benefit and harm using a neutral prior.
  Outputs multiple visuals comparing distributions and highlighting areas of clinical interest.

## [`Cost Analysis`](CostAnalysis)

- [`CompCareCostAnalysis.R`](CostAnalysis/CompCareCostAnalysis.R)
  Compares costs by treatment assignment using linear, Gamma, and Bayesian regression models adjusted for follow-up time.
  Estimates the probability of cost reduction with comprehensive care using Bayesian inference.

---
