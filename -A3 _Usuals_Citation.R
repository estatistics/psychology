
library(devtools)
library(stringr)

# To cite automatically packages in R
cite_pkgs <- function(){
  
  #Full citation per package
  pkg_list = list()
  load_pkgs <- lapply( loaded_packages()[,1], citation)
  ln_pkg    <- length(loaded_packages()[,1])
  
  
  for (y in 1:ln_pkg) {
    
    # Getting the names of authors
    pkgs              <- unlist( load_pkgs[[y]] )
    mtx               <- as.matrix(pkgs)    
    firstname_pkg     <- mtx[grepl("author.given", rownames(mtx))]   
    firstletter       <- substring(firstname_pkg, 1, 1)
    secname_pkg       <- mtx[grepl("author.family", rownames(mtx))]   
    names_pkg         <- paste0(paste(secname_pkg, firstletter), ".")
    
    # print(names_pkg)
    
    # Making the citation
    pst_pkg <- paste(paste(names_pkg, collapse=", "),
                     paste0("(",pkgs["year"][[1]],")." ),
                     paste0(pkgs["title"][[1]], "."),
                     paste0(pkgs["note"][[1]], ","),
                     paste0(pkgs["url"][[1]], ".")
    )
    
    # Citing for packages
    pkg_list[y] <-  unlist(pst_pkg)
    
  }
  
  
  # Citing Rstudio
  unlRS       <- unlist(RStudio.Version())
  citerstudio <- paste(paste0(unlRS["citation.author.given"][[1]], "."),
                       paste0("(",unlRS["citation.year"][[1]],")." ),
                       paste0(unlRS["citation.title"][[1]], "."),
                       paste0(unlRS["citation.address"][[1]], ","),
                       paste0(unlRS["citation.url"][[1]], ".")
  )
  
  # Citing R
  unlR        <- unlist(citation())
  cite_r      <- paste(paste0(unlR["author.given"][[1]], "."),
                       paste0("(",unlR["year"][[1]],")." ),
                       paste0(unlR["title"][[1]], "."),
                       paste0(unlR["address"][[1]], ","),
                       paste0(unlR["url"][[1]], ".")
  )
  
  # All citings
  unl_cites  <- c( unlist(cite_r), unlist(citerstudio), unlist(pkg_list))
  sort_cites <- unl_cites[order(substring(unl_cites, 1, 6))]
  
  # Printing them as "text".
  cat(paste(sort_cites, "||"))
  
  print("--------------------------------------------------------------------------------------------------------------------------------------------------")
  cat( "In librewriter, in replace menu (or ctr+H) select -Current Selection- and -Regular Expressions- and then - Replace this '\\|\\|' with this '\\n'. " )
  print("---------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  
}

