# Index
A searchable index for all R scripts in this repository.

---

## [`Bayesian Modeling`](BayesianModeling)
- [`BayesLogBinomialRESAmpleCode.R`](BayesianModeling/BayesLogBinomialRESAmpleCode.R)
  - Bayesian analysis for binary primary outcome.
  - Using log binomial with one random effect.

- [`BayesLogisticRRSampleCode.R`](BayesianModeling/BayesLogisticRRSampleCode.R)
  - sample code to run bayes logistic and calculate RR.
  - prior for log OR is based on prior for RR.
  - using rstanarm package.

- [`BayesLogisticRRSampleCodeBRMS.R`](BayesianModeling/BayesLogisticRRSampleCodeBRMS.R)
  - sample code to run bayes logistic and calculate RR.
  - prior for log OR is based on prior for RR.
  - using brm package.

- [`BayesLogisticRelRisk.R`](BayesianModeling/BayesLogisticRelRisk.R)
  - Calculating RR from binomial logistic reg.
  - https://discourse.mc-stan.org/t/binomial-regression-with-a-log-link/4102/5.

- [`BayesMarginalMeansExample.RData`](BayesianModeling/BayesMarginalMeansExample.RData)
  - data sample for bayes marginal means.

- [`BayesMarginalPostMeans.R`](BayesianModeling/BayesMarginalPostMeans.R)
  - Analysis of repeat grade status using a Bayesian GLM with posterior predictions, emmeans, and marginal effects.

- [`BayesMarginalPostMeansvsAME.R`](BayesianModeling/BayesMarginalPostMeansvsAME.R)
  - Uses Bayesian GLM via brms to model grade repetition status based on gender and parental education.
  - Posterior predictions, emmeans-based estimates, and average marginal effects are computed

- [`BayesRRAprox.R`](BayesianModeling/BayesRRAprox.R)
  - Uses neutral and expert priors to calculate posterior RRs for STOP-IT trial outcomes.
  - Demonstrates combining prior and observed log(RR) estimates, deriving posterior estimates, and computing posterior probabilities.

- [`ECMOBayesRRAprox.R`](BayesianModeling/ECMOBayesRRAprox.R)
  - Re-analysis of the ECMO trial using Bayesian methods.
  - Calculates posterior risk ratios (RR) and hazard ratios (HR) for 60-day mortality and adverse events (bleeding) under neutral and CESAR-derived priors.
  - Provides posterior probabilities for efficacy and harm.

- [`PosteriorPlotsBRMS_Rstanarm.R`](BayesianModeling/PosteriorPlotsBRMS_Rstanarm.R)
  - Creating pretty plots from rstanarm or brms fitted models.

- [`PosteriorPriorPlots.R`](BayesianModeling/PosteriorPriorPlots.R)
  - Generates posterior and prior distributions for a Bayesian hazard ratio analysis using a neutral prior with shaded treatment benefit/harm regions.
  - Produces visualizations comparing prior and posterior curves with emphasis on interpretability in clinical trial context.

- [`PosteriorPriorPlotsClass09_2024.R`](BayesianModeling/PosteriorPriorPlotsClass09_2024.R)
  - Visualizes prior and posterior distributions for relative risk using a neutral prior and observed data with interpretation regions shaded.
  - Outputs multiple plots to illustrate treatment effect, credible intervals, and probability of benefit vs. harm.

- [`PosteriorPriorPlotsClass12_2021.R`](BayesianModeling/PosteriorPriorPlotsClass12_2021.R)
  - Plots prior and posterior hazard ratio distributions with shaded regions for treatment benefit and harm using a neutral prior.
  - Outputs multiple visuals comparing distributions and highlighting areas of clinical interest.

## [`Cost Analysis`](CostAnalysis)
- [`CompCareCostAnalysis.R`](CostAnalysis/CompCareCostAnalysis.R)
  - Compares costs by treatment assignment using linear, Gamma, and Bayesian regression models adjusted for follow-up time.
  - Estimates the probability of cost reduction with comprehensive care using Bayesian inference.
 
## [`Modeling`](Modeling)
- [`BetaAnalysis.R`](Modeling/BetaAnalysis.R)
  - Performs a Bayesian odds ratio analysis using beta-binomial models with weak priors and visualizes prior vs. posterior distributions on the log-odds scale.        - Calculates posterior probabilities and credible intervals for odds and risk ratios between two groups.
 
- [`LinearCombCI.R`](Modeling/LinearCombCI.R)
  - linear combination to calculate HR and 95%CI.
  - Fits a Cox proportional hazards model with a time-varying effect for age using the tt() function.
 
- [`PoissonBinomSims.txt`](Modeling/PoissonBinomSims.txt)
  - Runs a simulation study to assess Bayesian and frequentist estimation of treatment effects across subgroups in a three-way interaction Poisson model.
  - Estimates posterior log odds ratios, their variances, coverage, and probability of benefit using shrinkage priors.
 
- [`RegressionAnalysis_TableCreation.R`](Modeling/RegressionAnalysis_TableCreation.R)
  - Performs univariate and multivariable ordinal regression analyses of standardized lab predictors on GOS-E outcome at discharge.
  - Outputs formatted regression tables as Word document using gtsummary and flextable.
 
- [`betaparamsfunctions.R`](Modeling/betaparamsfunctions.R)
  - function to calculate beta parameters implied by 95% CI of 0.5-2 for RR
  - Defines two functions to estimate the shape parameters of a beta distribution that correspond to assumed 95% confidence intervals for relative risks.
  - These are used to construct informative priors for Bayesian analysis based on control group event rates and upper CI limits.

- [`modelOptim.tx`](Modeling/modelOptim.txt)
  - Specifies a Bayesian logistic regression model in BUGS syntax with bounded probabilities and informative priors on the intercept and treatment effect (log RR).
  - The model uses a Bernoulli likelihood and priors centered on expected values from prior knowledge.

## [`Tables and Others`](Others)
- [`DSMBSafetyOct2012.R`](Others/DSMBSafetyOct2012.R)
  - Performs Bayesian estimation of proportions for three treatment groups using beta-binomial models with weak priors.
  - Computes posterior probabilities and credible intervals for absolute differences and threshold exceedance (≥ 0.15).
 
- [`TablesCreation.R`](Others/TablesCreation.R)
  - Generates descriptive and outcome tables for a clinical trial using gtsummary, and compares early vs. late intervention groups across multiple outcomes.
  -  Conducts both frequentist and Bayesian primary outcome analyses, including logistic regression, GEE models.
  -  and Bayesian mixed models with posterior inference on risk and odds ratios.

- [`TsaoPrePostStudy.R`](Others/TsaoPrePostStudy.R)
  - Implements Bayesian comparison of proportions between two groups using beta priors informed by specified means and coefficients of variation.
  - Computes posterior probabilities and credible intervals for treatment group differences.

## [`Plotting`](Plot)
- [`ForestPlots.R`](Plot/ForestPlots.R)
  - Draw a forestplot of cross-sectional, linear associations
 
- [`NonInfPlots_NONOTrial.R`](Plot/NonInfPlots_NONOTrial.R)
  - Simulates observed treatment differences in proportions and applies a Bayesian normal approximation to estimate the posterior distribution of the risk difference.
  - Visualizes the posterior distribution and summarizes central estimates with 95% credible intervals using ggplot2.
 
- [`NormalExponentPlotsShade.R`](Plot/NormalExponentPlotsShade.R)
  - Plots log-normal curves to visualize posterior and prior distributions of relative risk on both log and exponentiated scales.
  - Uses shaded areas to highlight treatment benefit and harm regions and overlays prior for visual comparison.
 
- [`NormalPlots.R`](Plot/NormalPlots.R)
  - Plots posterior normal distributions for Bayley score group differences with shaded regions highlighting clinically meaningful thresholds.
  - Visualizes cumulative probabilities beyond different cutoffs to support Bayesian interpretation of treatment effect magnitude.
    
- [`NormalPlotsShade.R`](Plot/NormalPlotsShade.R)
  - Creates multiple normal distribution plots with shaded regions to illustrate posterior probabilities, risk differences, and standard normal coverage (e.g., 68%, 95%, 99.7%).
  -  Uses custom colorArea functions to highlight specific intervals and save visuals for teaching or reporting.
    
- [`Prior_Post_plot.R`](Plot/Prior_Post_plot.R)
  - plotting posterior distribution of RR and prior distribution
  - Visualizes the posterior distribution of the log risk ratio with overlaid neutral prior and shaded regions for treatment benefit or harm.
  - Annotates posterior probabilities and 95% credible intervals to aid interpretation of Bayesian results.
    
- [`SplinesTutorial.R`](Plot/SplinesTutorial.R)
  - Demonstrates flexible regression modeling using linear, cubic, and natural splines in R, comparing their fits to a simulated nonlinear trend.
  - Also contrasts quadratic polynomial regression with a quadratic spline to illustrate advantages of spline-based methods for capturing nonlinearity.
    
- [`StandardNormalPlot.R`](Plot/StandardNormalPlot.R)
  - Illustrates the standard normal curve with shaded regions corresponding to 1, 2, and 3 standard deviation intervals to visualize the empirical rule.
  - Compares histograms of random samples with increasing sizes to demonstrate how sampling distribution approximates normality as sample size grows.
    
- [`contourplot.R`](Plot/contourplot.R)
  - Samples from two beta distributions and visualizes their joint density using 2D kernel density estimation.
  - A custom function draw.contour is defined to plot confidence contours (e.g., 95%) over the density, offering insights into bivariate uncertainty or correlation between posterior samples.

- [`plot.beta.R`](Plot/plot.beta.R) and [`plot.beta.txt`](Plot/plot.beta.txt)
  - This R script compares the prior, likelihood, and posterior beta distributions for a beta-binomial model using different prior assumptions and sample sizes.
  - It includes visualization of density curves and a helper function (plot.beta2) to zoom in on a specific theta range, useful for exploring how data updates prior beliefs.
 
## [`Power Analysis`](PowerAnalysis)
- [`AriasPower2022.R`](PowerAnalysis/AriasPower2022.R)
  - Simulates 5,000 datasets to estimate the power of detecting a significant interaction between antibiotic type (cef) and MIC status (mic) on mortality using logistic regression.
  - Fits a logistic model to observed data from Lee 2018 and defines a function to calculate the odds ratio from two probabilities.
    
- [`BayesLMPower.R`](PowerAnalysis/BayesLMPower.R)
  - Bayesian power calculations comparing means between >=2 groups
  - Simulates Bayesian power for detecting group mean differences across three treatment groups using 3,000 iterations with skeptical priors.
  - Estimates probabilities of achieving high posterior support or statistical significance for treatment effects using both Bayesian and frequentist methods.

- [`BayesNegBinLongitudinalPower.R`](PowerAnalysis/BayesNegBinLongitudinalPower.R)
  - Performs 250 Bayesian and frequentist simulations to evaluate treatment effects on repeated count outcomes using Poisson mixed-effects models.
  - Estimates the power and posterior probabilities for detecting clinically meaningful treatment effects across two timepoints and grouped subjects.
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
- [``]()
---
