##############################################################################
##
#W  testing.g                   XMod Package                     Chris Wensley
##   
#Y  Copyright (C) 1999-2018, Chris Wensley et al 
#Y  School of Computer Science, Bangor University, U.K. 

LoadPackage( "xmod" );

pkgname := "xmod";
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );
testfiles := 
    [ "gp2obj.tst", "gp2map.tst",    "gp2up.tst",     "gp2act.tst", 
      "gp2ind.tst", "isoclinic.tst", "gp3objmap.tst", "gpd2obj.tst", 
      "util.tst",   "gpnobjmap.tst", "apps.tst" ];
testresult := true;
for ff in testfiles do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print("#I  No errors detected while testing package ", pkgname, "\n");
else
    Print("#I  Errors detected while testing package ", pkgname, "\n");
fi; 
