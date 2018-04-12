##  makedoc.g for the package XMod,
##  This builds the documentation of the XMod package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 

LoadPackage( "GAPDoc" );
LoadPackage( "AutoDoc" );

AutoDoc( rec( 
    scaffold := rec(
        ## MainPage := false, 
        includes := [ "intro.xml",     "gp2obj.xml",    "gp2map.xml", 
                      "isoclinic.xml", "gp2up.xml",     "gp2act.xml",
                      "gp2ind.xml",    "gp3objmap.xml", "gpd2obj.xml",
                      "apps.xml",      "util.xml",      "history.xml"
                    ],
        bib := "bib.xml", 
        gapdoc_latex_options := rec( EarlyExtraPreamble := """
            \usepackage[all]{xy} 
            \newcommand{\Act} {\mathrm{Act}}
            \newcommand{\Aut} {\mathrm{Aut}}
            \newcommand{\Disp}{\mathrm{Disp}}
            \newcommand{\Fix} {\mathrm{Fix}}
            \newcommand{\Inn} {\mathrm{Inn}}
            \newcommand{\Stab}{\mathrm{Stab}}
        """ ),  
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>"
        )
    )
));

QUIT;
