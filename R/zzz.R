.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mesowest <- list(
    mesowest.base_url = "http://api.mesowest.net/v2/"
  )
  toset <- !(names(op.mesowest) %in% names(op))
  if(any(toset)) options(op.mesowest[toset])

  invisible()
}
