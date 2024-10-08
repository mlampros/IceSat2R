# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks(build_args = c('--compact-vignettes="gs+qpdf"', '--resave-data'), error_on = "never")

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  do_pkgdown()
}
