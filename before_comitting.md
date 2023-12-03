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
```

Before submitting a more involved pull request (e.g., > 100 lines or > 3 files changed), run a full devtools::check. Also run lint_dir and manually implement the changes.
```r
lintr::lint_dir()
```


## Scratch

Some other helpful debugging shortcuts, stored here for convenience:
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