#############################################################################
##
#W  testall.g                 GAP4 package `XMod'               Chris Wensley
## 
#Y  Copyright (C) 2001-2020, Chris Wensley et al 
##
#############################################################################

LoadPackage( "xmod" ); 
TestDirectory( 
    [ DirectoriesPackageLibrary( "xmod", "tst/manual" ), 
      DirectoriesPackageLibrary( "xmod", "tst/extra" ) ], 
    rec( exitGAP := true,
         testOptions := rec(compareFunction := "uptowhitespace") ) 
    );
FORCE_QUIT_GAP(1);
