

.onLoad = function(libname, pkgname){

  options("vmd.tol"           = 1e-6,
          "vmd.init"          = 0,
          "vmd.DC"            = TRUE,
          "vmd.K"             = 3,
          "vmd.tau"           = 0,
          "vmd.alpha"         = 2000,
          "vmd.theme.default" = list(theme_bw(),
                                     theme(panel.grid        = element_blank(),
                                           legend.background = element_rect(fill=alpha('white', 0.4)))),
          "vmd.N"             = 500,
          "vmd.NMin"          = 1,
          "vmd.NMax"          = 10000,
          "vmd.orderModes"    = TRUE
  )

}
