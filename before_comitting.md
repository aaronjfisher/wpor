# Manual checks to run before pushing to the main branch


Before pushing or making a pull request to the main branch, run the following code to make sure everything still works
```r
stopifnot(c('DESCRIPTION', 'NAMESPACE') %in% dir())
styler::style_pkg()
devtools::document()

system.time({
	no_errors <- FALSE
	devtools::run_examples()
	devtools::test()
	devtools::build_vignettes()
	no_errors <- TRUE
})
stopifnot(no_errors)
warnings()
```

Before submitting a more involved pull request (e.g., > 100 lines or > 3 files changed), run a full devtools::check. Also run lint_dir and manually implement the changes.
```r
devtools::check(cran = TRUE)
lintr::lint_dir()
```


## Scratch

Some other helpful debugging shortcuts, stored here for convenience. 

These are especially helpful for CRAN submissions.

```r
rm(list=ls())
unloadNamespace('wpor')
devtools::load_all()

devtools::install(build_vignettes = FALSE)
devtools::install(build_vignettes = TRUE)
browseVignettes('wpor')
help(package='wpor')
devtools::build()
```

or in terminal

```
R CMD build wpor
R CMD check --as-cran wpor_0.0.7.tar.gz
```

For cran, you should also run

```r
# Compile package on windows via http://win-builder.r-project.org/
# These functions email results to package maintainer email within ~30min
# The functions will each check your email before continuing,
# and must be run interactively.
devtools::check_win_devel()
devtools::check_win_release()
```
