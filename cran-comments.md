## Test environments
* local ubuntu 17.04 install, R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Notes

* One function of the script uses `:::` for a piece of code where it needs to
  mock a devtools function. 'testthis' integrates tightly with devtools and
  this was the cleanest way that I could find to solve that particular problem.
  
* Most functions in this package modify the file system or are designed to be
  run interactively. That is why most code examples in the documentation are
  wrapped in `\dontrun{}`

---
