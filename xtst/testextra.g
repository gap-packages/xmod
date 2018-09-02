##############################################################################
##
#W  testextra.g                    XMod Package                  Chris Wensley 
##
#Y  Copyright (C) 2000-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

LoadPackage( "xmod" ); 
dir := DirectoriesPackageLibrary("xmod","xtst");
TestDirectory(dir, rec(testOptions:=rec(compareFunction := "uptowhitespace")));
