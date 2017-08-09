##############################################################################
##
#W  testing.g                   XMod Package                     Chris Wensley
##   
#Y  Copyright (C) 1999-2017, Chris Wensley et al 
##

LoadPackage( "xmod" );

TestXMod := function( pkgname )
    local  pkgdir, testfiles, testresult, ff, fn;
    LoadPackage( pkgname );
    pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );
    # Arrange chapters as required
    testfiles := 
        [ "01-gp2obj.tst", "02-gp2map.tst", "03-gp2up.tst", "04-gp2act.tst", 
          "05-gp2ind.tst", "06-isoclinic.tst", "07-gp3objmap.tst", 
          "08-gpd2obj.tst", "09-util.tst", "10-coprod.tst", 
          "11-gpnobjmap.tst" ];
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
