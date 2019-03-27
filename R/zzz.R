#' @importFrom magrittr %>%

.onAttach<- function(libname, pkgname) 
{
 packageStartupMessage('

 ## occupar package

 ')
} 

.onAttach<- function(libname, pkgname) 
{
 packageStartupMessage('

 ## occupar package

 ')
} 

.onAttach<- function(libname, pkgname) 
{
 packageStartupMessage('

 ## occupar package

 ')
} 

## global varibles for dplyr (used only so that the check ignores it, it does not actually creates global variables)
## -------------------------
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "isco08"
                                                        ))
