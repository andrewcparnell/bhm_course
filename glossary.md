---
title: "Glossary"
author: "Andrew Parnell"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

A glossary of terms for the course _Introduction to Bayesian Hierarchical Modelling_

- __Posterior distribution__: A probability distribution of the parameters given the data. This is usually represented as a matrix where the rows represent each sample and the columns represent each parameter.
- __Likelihood__: The probability of observing the data given some parameter values. For a given set of parameters, the likelihood can be represented as a single numerical value
- __Prior distribution__: The probability distribution of the parameters obtained externally from the data (i.e. before you have seen the data, or more practically obtained from information separately to the current experiment).
- __Prior predictive distribution__: A pseudo-data set obtained (possibly repeatedly) by simulating first from the prior distribution of the parameters, and subsequently the likelihood. If you have used good priors then this pseudo-data should 'look like' real data.
- __Posterior predictive distribution__: A pseudo-data set obtained (usually repeatedly) by simulating first from the posterior distribution of the parameters, and subsequently from the likelihood. If the model is a good fit to the data, the pseudo-data set should be similar to the real data set.
- __Generalised Linear Model (GLM)__: A type of statistical model where a response variable is related to an explanatory variable via a probability distribution (through the likelihood), and a link function. 
- __Link functions__: A function which transforms a set of parameters in a probability distribution from a restricted to an unrestricted range. The unrestricted range can then be used to incorporate the covariates.
- __Directed Acyclic Graph (DAG)__: A graphical display of a model used to identify the links between the parameters and the data. Circles are used to display parameters, squares for data, and arrows are used to identify links. Dashed lines are sometimes also used to display indexed variables (e.g. observations)
- __Information criteria__: A set of tools used to compare between models. The attempt to penalise the deviance (minus twice the log-likelihood) by a measure of the complexity of the model, with the idea that the 'best' models will represent the data well and be relatively simple. Common information criteria for Bayesian models are the Deviance Information Criterion (DIC), and the Widely Applicable Information Criterion (WAIC). Smaller values of the IC tend to indicate 'better' models.
- __Deviance__: A measure of the fit of the model using only the likelihood score. The Deviance is calculated as minus twice the log-likelihood score. It is popular because, for normally distributed data, the Deviance is equivalent to the Mean Square Error. JAGS reports the deviance as it is used as part of the Deviance Information Criterion, whereas Stan reports the log-posterior score (see definition below)
- __Cross validation (CV)__: A technique whereby parts of the data are removed before fitting the model, and subsequently predicted from the new model output. CV is valuable because it tests the model performance on data the model has not seen. Common versions of CV include k-fold CV whereby the data are split into k groups and each is left out (and subsequently predicted) in turn, or leave-one-out (LOO-CV) where each observation is left out (and subsequently predicted) in turn.
- __Hierarchical model__: A model where some parameters are given further prior distributions that depend on other parameters.
- __Markov chain Monte Carlo__: A method for simulating values from a posterior distribution given a likelihood and a prior (and a data set). The method works by guessing initial values for the parameters, and scoring them against the likelihood and the prior. It subsequently updates the parameters and compares them with the previous score. Over thousands of iterations the model should eventually  s
- __Hyper-parameter__: A lower level parameter (i.e. further removed from the data) that depends on no other parameters in a hierarchical model
- __Rhat__: A measure of how well a parameter has converged to the posterior distribution. Values less than 1.1 are usually considered acceptable. Will only work when for runs with multiple different starting values (multiple chains)
- __Convergence monitoring__: The means by which a set of parameters is considered to have converged to the true posterior distribution. The convergence is controlled (in JAGS and Stan) by the number of iterations required, the total burn-in period (the number of iterations removed at the start), and the amount of thinning (the number of successive iterations removed). For models which haven't converged, it is common to increase one or more of these values.
- __Mixed effects models__: A Frequentist model containing both _fixed effects_ (parameters which do not borrow strength across groups), and _random effects_ (parameters which do borrow strength across groups). In a hierarchical models the aim is always to use the random effects approach.
- __Marginal distribution__: A probability distribution which does not depend on further (usually unknown) parameter values. 
- __Conditional distributions__: A probability distribution that depends on further parameters and/or data.
- __Conditional independence__: A situation that occurs when two parameters are not linked directly in a DAG. 
- __Latent parameters__: A parameter which is hidden in the data and can be estimated by using a statistical model (such as a hierarchical model)
- __`lp__`__: Part of the output of a Stan model that represents the _log posterior_ score. This is the log of the likelihood times the prior (or equivalently the sum of the log-likelihood plus the sum of the log-priors). It represents the overall score that Stan is trying to maximise (and subsequently hover around) to create the poserior distribution


