#############################################################################
##
#W  cpcpcp.g                  GAP4 package `XMod'               Chris Wensley
## 
#Y  Copyright (C) 2001-2021, Chris Wensley et al 
##
#############################################################################

CpCpCpCat2Groups := function( p ) 

    local  G, genG, o, a, b, c, pooo, pooc, pocc, pcoc, 
           pobo, pbbo, pabb, pabo, paoc, pobc, pbbc, pabc, 
           C1, i, A, j, B, iso, C2, pos, ok; 

    G := CyclicGroup( IsPermGroup, p ); 
    G := DirectProduct( G, G, G ); 
    genG := GeneratorsOfGroup( G ); 
    o := One( G ); 
    a := genG[1]; 
    b := genG[2]; 
    c := genG[3]; 
    SetName( G, "G" ); 
    ## construct the 12 projections needed for the cat1-groups 
    pooo := GroupHomomorphismByImages( G, G, [a,b,c], [o,o,o] );
    pooc := GroupHomomorphismByImages( G, G, [a,b,c], [o,o,c] );
    pocc := GroupHomomorphismByImages( G, G, [a,b,c], [o,c,c] );
    pcoc := GroupHomomorphismByImages( G, G, [a,b,c], [c,o,c] );
    pobo := GroupHomomorphismByImages( G, G, [a,b,c], [o,b,o] );
    pbbo := GroupHomomorphismByImages( G, G, [a,b,c], [b,b,o] );
    pabb := GroupHomomorphismByImages( G, G, [a,b,c], [a,b,b] );
    pabo := GroupHomomorphismByImages( G, G, [a,b,c], [a,b,o] );
    paoc := GroupHomomorphismByImages( G, G, [a,b,c], [a,o,c] );
    pobc := GroupHomomorphismByImages( G, G, [a,b,c], [o,b,c] );
    pbbc := GroupHomomorphismByImages( G, G, [a,b,c], [b,b,c] );
    pabc := GroupHomomorphismByImages( G, G, [a,b,c], [a,b,c] );
    ## construct 14 cat1-groups (there are 6 isomorphism classses) 
    Print( "14 cat1-groups constructed:\n" ); 
    C1 := [ Cat1Group( pooo, pooo ),  ##  1
            Cat1Group( pooc, pooc ),  ##  2
            Cat1Group( pooc, pocc ),  ##  3
            Cat1Group( pabo, pabo ),  ##  4
            Cat1Group( pabo, pabb ),  ##  5
            Cat1Group( pabc, pabc ),  ##  6
            Cat1Group( pobo, pobo ),  ##  7 ~ 2
            Cat1Group( pobc, pobc ),  ##  8 ~ 4
            Cat1Group( pobo, pbbo ),  ##  9 ~ 3
            Cat1Group( pooc, pcoc ),  ## 10 ~ 3
            Cat1Group( pbbc, pbbc ),  ## 11 ~ 4
            Cat1Group( pbbc, pobc )   ## 12 ~ 5
          ]; 
    Print( C1, "\n" ); 
    ## when p is small check the 6 isomorphisms listed above 
    if ( p < 4 ) then 
        for i in [7..12] do 
            A := C1[i]; 
            Print( i, " : " ); 
            for j in [1..6] do 
                B := C1[j];  
                iso := IsomorphismPreCat1Groups( A, B ); 
                if ( iso <> fail ) then 
                    Print( " ~ ", j ); 
                fi; 
            od;
            Print( "\n" ); 
        od; 
    fi;
    ## construct representatives of the 23 classes of cat2-groups on G 
    C2 := [ Cat2Group( C1[1], C1[1] ),   ##   1
            Cat2Group( C1[1], C1[2] ),   ##   2
            Cat2Group( C1[1], C1[3] ),   ##   3
            Cat2Group( C1[1], C1[4] ),   ##   4
            Cat2Group( C1[1], C1[5] ),   ##   5
            Cat2Group( C1[1], C1[6] ),   ##   6
            Cat2Group( C1[2], C1[6] ),   ##   7
            Cat2Group( C1[3], C1[6] ),   ##   8
            Cat2Group( C1[4], C1[6] ),   ##   9
            Cat2Group( C1[5], C1[6] ),   ##  10
            Cat2Group( C1[6], C1[6] ),   ##  11
            Cat2Group( C1[2], C1[2] ),   ##  12
            Cat2Group( C1[2], C1[7] ),   ##  13
            Cat2Group( C1[4], C1[4] ),   ##  14
            Cat2Group( C1[4], C1[8] ),   ##  15
            Cat2Group( C1[2], C1[4] ),   ##  16
            Cat2Group( C1[2], C1[9] ),   ##  17
            Cat2Group( C1[10], C1[9] ),  ##  18
            Cat2Group( C1[2], C1[11] ),  ##  19 
            Cat2Group( C1[2], C1[12] ),  ##  20 
            Cat2Group( C1[3], C1[8] ),   ##  21 
            Cat2Group( C1[5], C1[11] ),  ##  22 
            Cat2Group( C1[5], C1[12] )   ##  23
          ]; 
    ## verify that there are no failures in these cat2-groups 
    pos := Position( C2, fail ); 
    if not ( pos = fail ) then 
        Print( "failure at pos = ", pos, "\n" ); 
    fi; 
    ## verify that no two of these cat2-groups are isomorphic 
    ok := true; 
    for i in [1..22] do 
        A := C2[i]; 
        for j in [i+1..23] do 
            B := C2[j]; 
            iso := IsomorphismCat2Groups( A, B ); 
            if ( iso <> fail ) then 
                Print( [i,j], " are isomorphic\n" ); 
                ok := false; 
            fi; 
        od;
    od;
    if ok then 
        Print( "no 2 of the 23 cat2-groups are isomorphic:\n" ); 
    fi; 
    return C2; 
end; 
