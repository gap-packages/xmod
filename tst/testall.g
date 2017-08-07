#############################################################################
##
#W  testall.g                 GAP4 package `XMod'               Chris Wensley
## 
#Y  Copyright (C) 2001-2017, Chris Wensley et al 
##
#############################################################################

LoadPackage( "xmod" ); 
dir := DirectoriesPackageLibrary("xmod","tst");
TestDirectory(dir, rec(exitGAP := true,
    testOptions:=rec(compareFunction := "uptowhitespace")));
FORCE_QUIT_GAP(1);
