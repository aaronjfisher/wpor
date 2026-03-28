NEWS.md
=====

### 0.0.7

* (**Breaking**) cvboost_spec.R removed from package and placed in separate script under `experiments-for-paper`, in preparation for CRAN submission. These functions depend on the `rlearner` installation package which is not yet available on CRAN, and were primarily intended for simulations supporting the publication associated with this package.
* (**Breaking**) Minimum R version requirement increased from 3.5.0 to 4.1.0.
* Replaced deprecated pryr with lobstr in Imports.
* Fixed S3 method exports for get_x and get_y methods
* Minor documentation changes in response to CRAN submission checks.

### 0.0.6

* (**Breaking**) Replace `getOption("verbose")` with `getOption("wpor.verbose")` to avoid overlap with other packages. Users should now set `options(wpor.verbose = TRUE)` to enable verbose output.
* Replacing internal use of `dials::grid_latin_hypercube` with `dials::grid_space_filling`, as the former is deprecated.
* Adding LICENSE files, updating DESCRIPTION file.
* Many changes to documentation and internal code to prepare for CRAN submission, including:
  * Updating S3 methods expanded to include `...` argument.
  * Adding @param documentation for all arguments.
  * Removed unused xgboost_spec.R, which was conslidated into cvboost_spec.R but was accidentally left in the package.
  * Removed unused `get_params()` functions.
  * Explicit namespace prefixes added (e.g., `dplyr::`)
  * Add use of `.data$` within dplyr functions to avoid R CMD check notes about undefined variables.

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

