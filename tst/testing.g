##############################################################################
##
#W  testing.g                   XMod Package                     Chris Wensley
##   
#Y  Copyright (C) 1999-2020, Chris Wensley et al 

LoadPackage( "xmod" );

pkgname := "xmod";
pkgdir := DirectoriesPackageLibrary( pkgname, "tst/manual" ); 
## testing manual examples 
testmanual := 
    [ "gp2obj.tst",    "gp2map.tst",    "gp2up.tst",   "gp2act.tst", 
      "gp2ind.tst",    "isoclinic.tst", "gp3xsq.tst",  "gp3cat2.tst",  
      "gp4objmap.tst", "gpd2obj.tst",   "util.tst",    "apps.tst" ];
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
    [ "allxmods.tst",  "cat1mor.tst",  "coprod.tst",    ## "cat1data.tst", 
      "d24.tst",       "gpnobjmap.tst", "induced.tst",  "issue9.tst", 
      "loops.tst",     "others.tst",    "smallids.tst"  ];
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
