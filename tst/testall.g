#############################################################################
##
#W  testall.g                 GAP4 package `XMod'               Chris Wensley
## 
#Y  Copyright (C) 2001-2016, Chris Wensley et al 
##
#############################################################################

LoadPackage( "xmod" ); 

TestXMod := function( pkgname )
    local  pkgdir, testfiles, testresult, ff, fn;
    LoadPackage( pkgname );
    pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );
    # Arrange chapters as required
    testfiles := 
        [ "gp2obj.tst", "gp2map.tst", "gp2up.tst", "gp2act.tst", "gp2ind.tst", 
          "isoclinic.tst", "gp3objmap.tst", "gpd2obj.tst", "util.tst" ];
    testresult := true;
    for ff in testfiles do
        fn := Filename( pkgdir, ff );
        Print( "#I  Testing ", fn, "\n" );
        if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
            testresult := false;
        fi;
    od;
    if testresult then
        Print("#I  No errors detected while testing package ", pkgname, "\n");
    else
        Print("#I  Errors detected while testing package ", pkgname, "\n");
    fi;
end;

##  Set the name of the package here
TestXMod( "xmod" );
