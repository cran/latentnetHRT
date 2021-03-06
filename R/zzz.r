######################################################################
#
# zzz.r
#
# copyright (c) 2003, Mark S. Handcock, University of Washington
#                     David R. Hunter, Penn State University
#                     Carter T. Butts, University of California - Irvine
#                     Martina Morris, University of Washington
# written December 2003
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the statnet suite of packages
#
# .First.lib is run when the package is loaded with library(latentnetHRT)
#
######################################################################

.conflicts.OK <- "0.7-4"

.First.lib <- function(lib, pkg){
#   if(substr(as.character(Sys.Date()),start=1,stop=4) > "2005"){
#    stop('Your version of the "latentnetHRT" package is out-of-date.\nGo to http://www.csde.washington.edu/statnet for an update.')
#   }
    library.dynam("latentnetHRT", pkg, lib)
    if(R.version$major=="1"){
     ehelp <- help(package="latentnetHRT")$info[[2]][[2]]
     cat(paste("'",ehelp[4],"'\n",
               "Version ",ehelp[2],
               " created on ",ehelp[3],".\n", sep=""))
    }else{
     if(R.version$minor < "1.0"){
      ehelp <- library(help="latentnetHRT",lib.loc=NULL,character.only=TRUE)$info[[2]]
     }else{
      ehelp <- library(help="latentnetHRT",lib.loc=NULL,character.only=TRUE)$info[[1]]
     }
     cat(paste(substring(ehelp[4],first=16),"\n",
               "Version ",substring(ehelp[2],first=16),
               " created on ",
                substring(ehelp[3],first=16),".\n", sep=""))
    }
    cat(paste("copyright (c) 2003, Mark S. Handcock, University of Washington\n",
"                    Susan Shortreed, University of Washington\n",
"                    Jeremy Tantrum, University of Washington\n",
"                    Peter Hoff, University of Washington\n",sep=""))
    cat('See http://statnetproject.org/latentnetHRT\n')
    cat('Type help(package="latentnetHRT") to get started.\n')
    cat('To cite, see citation("latentnetHRT")\n')
}
