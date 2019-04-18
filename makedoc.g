##  makedoc.g for the package XMod,
##  This builds the documentation of the XMod package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 

LoadPackage( "GAPDoc" );
LoadPackage( "AutoDoc" );

AutoDoc( rec( 
    gapdoc := rec( 
        GAPDocLaTeXOptions := rec( EarlyExtraPreamble := """
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
            \newcommand{\calF}{\mathcal{F}}
            \newcommand{\calG}{\mathcal{G}}
            \newcommand{\calL}{\mathcal{L}}
            \newcommand{\calM}{\mathcal{M}}
            \newcommand{\calN}{\mathcal{N}}
            \newcommand{\calP}{\mathcal{P}}
            \newcommand{\calS}{\mathcal{S}}
            \newcommand{\calW}{\mathcal{W}}
            \newcommand{\calX}{\mathcal{X}}
            \newcommand{\calY}{\mathcal{Y}}
        """ )
    ),  
    scaffold := rec(
        ## MainPage := false, 
        gapodoc_latex_options := rec( EarlyExtraPreamble := """
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
            \newcommand{\calF}{\mathcal{F}}
            \newcommand{\calG}{\mathcal{G}}
            \newcommand{\calL}{\mathcal{L}}
            \newcommand{\calM}{\mathcal{M}}
            \newcommand{\calN}{\mathcal{N}}
            \newcommand{\calP}{\mathcal{P}}
            \newcommand{\calS}{\mathcal{S}}
            \newcommand{\calW}{\mathcal{W}}
            \newcommand{\calX}{\mathcal{X}}
            \newcommand{\calY}{\mathcal{Y}}
        """ ),
        includes := [ "intro.xml",     "gp2obj.xml",    "gp2map.xml", 
                      "isoclinic.xml", "gp2up.xml",     "gp2act.xml",
                      "gp2ind.xml",    "gp3objmap.xml", "gpd2obj.xml",
                      "apps.xml",      "util.xml",      "history.xml"
                    ],
        bib := "bib.xml", 
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>"
        )
    )
));
