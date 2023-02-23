install.packages( 
    lib  = lib <- .libPaths()[1],
    pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package,
) 
