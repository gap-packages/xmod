##  makedoc.g for the package XMod,
##  This builds the documentation of the XMod package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 
##  
##  latex_header_file := "mylatexhead.tex", 

LoadPackage( "GAPDoc" );

if fail = LoadPackage("AutoDoc", ">= 2017.09.08") then
    Error("AutoDoc is required: version at least 2017.09.08");
fi;

AutoDoc( rec( 
    scaffold := rec(
        ## MainPage := false, 
        includes := [ "intro.xml",     "gp2obj.xml",    "gp2map.xml", 
                      "isoclinic.xml", "gp2up.xml",     "gp2act.xml",
                      "gp2ind.xml",    "gp3objmap.xml", "gpd2obj.xml",
                      "util.xml",      "history.xml"
                    ],
        gapdoc_latex_options := rec( 
            EarlyExtraPreamble := "\\usepackage[all]{xy}" 
        ),  
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>",
            Act     := "\mathop{\textrm{Act}\rm}",
            Aut     := "\mathop{\textrm{Aut}\rm}", 
            Disp    := "\mathop{\textrm{Disp}\rm}", 
            Fix     := "\mathop{\textrm{Fix}\rm}", 
            Stab    := "\mathop{\textrm{Stab}\rm}" 
        )
    )
));

# Create VERSION file for "make towww"
PrintTo( "VERSION", GAPInfo.PackageInfoCurrent.Version );

QUIT;
