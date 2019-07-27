##############################################################################
##
#W  testing.g                   XMod Package                     Chris Wensley
##   
#Y  Copyright (C) 1999-2019, Chris Wensley et al 
#Y  School of Computer Science, Bangor University, U.K. 

LoadPackage( "xmod" );

pkgname := "xmod";
pkgdir := DirectoriesPackageLibrary( pkgname, "tst/manual" ); 
## testing manual examples 
testmanual := 
    [ "gp2obj.tst",  "gp2map.tst",    "gp2up.tst",     "gp2act.tst", 
      "gp2ind.tst",  "isoclinic.tst", "gp3objmap.tst", "gp4objmap.tst", 
      "gpd2obj.tst", "util.tst",      "gpnobjmap.tst", "apps.tst" ];
testresult := true;
for ff in testmanual do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print( "#I  No errors detected while testing manual examples in package ", 
           pkgname, "\n" );
else
    Print( "#I  Errors detected while testing manual examples in package ", 
           pkgname, "\n" );
fi; 
## testing extra examples
pkgdir := DirectoriesPackageLibrary( pkgname, "tst/extra" );
testextra := 
    [ "allxmods.tst",  "cat1data.tst",  "cat1mor.tst",  "coprod.tst", 
      "d24.tst",       "induced.tst",   "issue9.tst",   "loops.tst", 
      "others.tst" ];
testresult := true;
for ff in testextra do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print( "#I  No errors detected while testing extra examples in package ", 
           pkgname, "\n" );
else
    Print( "#I  Errors detected while testing extra examples in package ", 
           pkgname, "\n" );
fi; 
