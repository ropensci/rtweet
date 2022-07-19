This is a major update of the package as there are some breaking changes and a change in maintainers (already communicated via private email on 2022/07/10. 

# Reverse dependencies

All current CRAN dependencies were tested a couple of months ago and notified of any problem.
Packages were updated and as far as I could test there are currently no breaking changes. 

# Test environments

* local R installation, R 4.1.2
* win-builder (olrelease): There it detects a (spurious?) problem with the testing framework rtweet uses.
* win-builder (release)
* win-builder (devel)
* macOS builder (release)
* R-hub (multiple): Inconsistent behavior. 

# local R CMD check results

There is a note when using --as-cran about changing maintainer, the old maintainer has publicly approved the changes of the package. See also the communications to cran@r-project.org

