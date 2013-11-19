.onLoad <- function(libname, pkgname) {
  Sys.setenv(NOAWT= "true")
  .jpackage(pkgname, lib.loc = libname)
}