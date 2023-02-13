##  makedoc.g for the package XMod,
##  This builds the documentation of the XMod package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 

LoadPackage( "GAPDoc" );
LoadPackage( "AutoDoc" );

AutoDoc( rec( 
    gapdoc := rec( 
        LaTeXOptions := rec( EarlyExtraPreamble := """
            \usepackage[all]{xy} 
            \newcommand{\Act} {\mathrm{Act}}
            \newcommand{\Aut} {\mathrm{Aut}}
            \newcommand{\Disp}{\mathrm{Disp}}
            \newcommand{\Fix} {\mathrm{Fix}}
            \newcommand{\Inn} {\mathrm{Inn}}
            \newcommand{\Stab}{\mathrm{Stab}}
            \newcommand{\calA}{\mathcal{A}}
            \newcommand{\calB}{\mathcal{B}}
            \newcommand{\calC}{\mathcal{C}}
            \newcommand{\calD}{\mathcal{D}}
            \newcommand{\calE}{\mathcal{E}}
            \newcommand{\calF}{\mathcal{F}}
            \newcommand{\calG}{\mathcal{G}}
            \newcommand{\calL}{\mathcal{L}}
            \newcommand{\calM}{\mathcal{M}}
            \newcommand{\calN}{\mathcal{N}}
            \newcommand{\calP}{\mathcal{P}}
            \newcommand{\calQ}{\mathcal{Q}}
            \newcommand{\calS}{\mathcal{S}}
            \newcommand{\calW}{\mathcal{W}}
            \newcommand{\calX}{\mathcal{X}}
            \newcommand{\calY}{\mathcal{Y}}
        """ )
    ),  
    scaffold := rec(
        ## MainPage := false, 
        includes := [ "intro.xml", "gp2obj.xml", "gp2map.xml",  "isoclinic.xml", 
                            "gp2up.xml", "gp2act.xml", "gp2ind.xml", "gp3xsq.xml",    
                            "gp3cat2.xml", "gp4objmap.xml", "gpd2obj.xml", "double.xml", 
                            "apps.xml", "hap.xml", "util.xml", "history.xml"  ], 
        bib := "bib.xml", 
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>"
        )
    )
));
