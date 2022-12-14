.onLoad <- function (libname, pkgname) { # nocov start
  
  #default fonts
  sysfonts::font_add(family = "Arial Unicode MS",
                     regular = system.file("extdata/fonts", "Arial Unicode MS Font.ttf", package = "mapit"),
                     bold = system.file("extdata/fonts", "Arial-Unicode-Bold.ttf", package = "mapit"))
  sysfonts::font_add(family = "FuturaStd", 
                     regular =  system.file("extdata/fonts", "FuturaStd-Medium.otf", package = "mapit"),
                     bold = system.file("extdata/fonts", "FuturaStd-Heavy.otf", package = "mapit"))
  #cache
  assign(".mapit.cache", new.env(), envir= asNamespace(pkgname))
  
} # nocov end
