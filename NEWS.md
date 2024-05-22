NEWS.md
=====


### 0.0.5

Use group-cv to ensure that, when tuning the effect_wf, if multiple pseudo-outcomes are estimated for a single individual then those pseudo-outcomes should always be assigned to the same split. This can only happen in multi-way cross-fitting. (https://github.com/aaronjfisher/wpor/pull/11)

### 0.0.4

Make some nuisance workflows optional, so computations can be faster when users do not want to compute multiple CATE estimators (https://github.com/aaronjfisher/wpor/pull/9).

### 0.0.3

This version changed the outcome_obs_wf, outcome_1_wf, and outcome_0_wf arguments to outcome_marginal_wf, outcome_1_separate_wf  and
outcome_0_separate_wf, in order to disambiguate from a new argument 
outcome_single_wf which fits a single model for E(Y|X,A), rather than
two models for E(Y|X,A=1) and E(Y|X,A=0).

### 0.0.2

This model adds a lightgbm model, and renames the `tuneflow` class as `tunefit`.

