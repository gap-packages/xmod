##############################################################################
##
#W  apps.gi                    GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
InstallMethod( LoopsXMod, "for a crossed module and an element", true, 
    [ IsPreXMod, IsObject ], 0, 
function( X0, a ) 

    local M0, genM0, lenM0, P0, bdy0, act0, aut0, acta, elPa, p, c, m, 
          nPa, C0, G0, e1, e2, Pa, genPa, posPa, i, e, e2m, e1p, g, ngPa, 
          imda, j, pa, ma, bdy1, imact1, act1;

    if not IsXMod( X0 ) then 
        Error( "X0 should be a crossed module" ); 
    fi; 
    M0 := Source( X0 ); 
    genM0 := GeneratorsOfGroup( M0 );
    lenM0 := Length( genM0 ); 
    P0 := Range( X0 ); 
    if not ( a in P0 ) then 
        Error( "element a not in the range group P0" ); 
    fi; 
    bdy0 := Boundary( X0 ); 
    act0 := XModAction( X0 ); 
    aut0 := Range( act0 ); 
    acta := ImageElm( act0, a ); 
    elPa := [ ]; 
    for p in P0 do 
        c := Comm( p, a ); 
        for m in M0 do 
            if ( ImageElm( bdy0, m ) = c ) then 
                Add( elPa, [p,m] );
            fi;
        od; 
    od; 
    nPa := Length( elPa );
    C0 := Cat1GroupOfXMod( X0 ); 
    G0 := Source( C0 ); 
    e1 := Embedding( G0, 1 ); 
    e2 := Embedding( G0, 2 );
    Pa := Subgroup( G0, [ ] ); 
    genPa := [ ];
    posPa := [ ];
    for i in [1..nPa] do 
        e := elPa[i]; 
        p := e[1]; 
        m := e[2]; 
        e2m := ImageElm( e2, m ); 
        e1p := ImageElm( e1, p ); 
        g := e1p * e2m;
        if not ( g in Pa ) then 
            Add( genPa, g );
            Add( posPa, i );
            Pa := Subgroup( G0, genPa ); 
        fi;
    od;
    ngPa := Length( genPa ); 
    ## construct the boundary for X1 
    imda := ListWithIdenticalEntries( lenM0, 0 ); 
    for j in [1..lenM0] do 
        m := genM0[j];  
        pa := ImageElm( bdy0, m ); 
        ma := m^-1 * ImageElm( acta, m ); 
        e2m := ImageElm( e2, ma ); 
        e1p := ImageElm( e1, pa ); 
        g := e1p * e2m;
        if not ( g in Pa ) then 
            Error( "element g not in Pa" ); 
        fi; 
        imda[j] := g; 
    od; 
    bdy1 := GroupHomomorphismByImages( M0, Pa, genM0, imda ); 
    if ( bdy1 = fail ) then 
        Error( "bdy1 fails to be a homomorphism" ); 
    fi;
    ## construct the action map for X1 
    imact1 := ListWithIdenticalEntries( ngPa, 0 ); 
    for i in [1..ngPa] do  
        p := elPa[ posPa[i] ][1]; 
        imact1[i] := ImageElm( act0, p ); 
    od; 
    act1 := GroupHomomorphismByImages( Pa, aut0, genPa, imact1 ); 
    ## now we can construct the crossed module 
    return XModByBoundaryAndAction( bdy1, act1 );  
end ); 

##############################################################################
##
InstallMethod( AllLoopsXMod, "default method for a crossed module", true, 
    [ IsPreXMod ], 0, 
function( X0 ) 

    local bdy0, act0, M0, genM0, P0, genP0, igenP0, imgenM0, imbdy0, Q0, 
          conjP0, repsP0, eltsP0, nclP0, i, eclP0, relP0, c, p, genimbdy0, 
          len, class, j, r, k, q, pos, d, h, eqreps, numreps, a, X1, allX; 

    bdy0 := Boundary( X0 ); 
    act0 := XModAction( X0 ); 
    M0 := Source( X0 ); 
    genM0 := GeneratorsOfGroup( M0 ); 
    P0 := Range( X0 ); 
    genP0 := GeneratorsOfGroup( P0 ); 
    igenP0 := List( genP0, g -> ImageElm( act0, g ) ); 
    imgenM0 := List( genM0, g -> List( igenP0, p -> ImageElm(p,g) ) ); 
    imbdy0 := ImagesSource( bdy0 ); 
    Q0 := P0/imbdy0; 
    SetName( P0, "P0" ); 
    conjP0 := ConjugacyClasses( P0 ); 
    repsP0 := List( conjP0, c -> Representative( c ) ); 
    eltsP0 := List( conjP0, c -> Elements(c) ); 
    nclP0 := Length( conjP0 ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "#I  Size(P0) = ", Size(P0), "\n" ); 
        Print( "#I  imgenM0 = ", imgenM0, "\n" );   #? this is not used 
        Print( "#I  cokernel of boundary has IdGroup ", IdGroup(Q0), "\n" ); 
        Print( "#I  conjugacy class sizes, reps and orders:classes for P :-\n" ); 
        Print( "#I  ", List( conjP0, c -> Size(c) ), "\n" ); 
        Print( "#I  repsP0 = ", repsP0, "\n" ); 
    fi;
    if ( InfoLevel( InfoXMod ) > 2 ) then 
        Print( "\n#I  orders and elements in conjugacy classes:\n" ); 
        for i in [1..nclP0] do 
            Print( "#I  ", i, " : ", Order(repsP0[i]), " > ", eltsP0[i], "\n" ); 
        od; 
    fi; 
    eltsP0 := Elements( P0 ); 
    eclP0 := ListWithIdenticalEntries( Size(P0), 1 ); 
    relP0 := [1..nclP0]; 
    for i in [2..nclP0] do 
        c := conjP0[i]; 
        for p in c do 
            eclP0[ Position( eltsP0, p ) ] := i; 
        od;
    od; 
    Info( InfoXMod, 3, "eclP0 = ", eclP0 ); 
    genimbdy0 := GeneratorsOfGroup( imbdy0 ); 
    for i in [1..nclP0] do 
        if ( relP0[i] = i ) then 
            len := 1;
            class := [ i ];
            j := 0; 
            while ( j < len  ) do 
                j := j+1; 
                c := class[j]; 
                r := repsP0[c]; 
                for k in imbdy0 do    ##? genimbdy do 
                    q := r*k; 
                    pos := Position( eltsP0, q ); 
                    d := eclP0[pos];  ## the class of q 
                    if ( d <> c ) then 
                        Info( InfoXMod, 3, "[c,r,k,q,d] = ", [c,r,k,q,d] ); 
                        for h in [1..nclP0] do 
                            if ( relP0[h] = d ) then 
                                relP0[h] := c; 
                            fi; 
                        od;
                        Add( class, d ); 
                        Info( InfoXMod, 3, "class = ", class ); 
                    fi; 
                od; 
            od; 
        fi; 
    od; 
    eqreps := [ ]; 
    for i in [1..nclP0] do 
        if ( i = relP0[i] ) then 
            Add( eqreps, repsP0[i] ); 
        fi;
    od; 
    Info( InfoXMod, 2, "eqreps = ", eqreps ); 
    ## now run LoopsXMod on on the list eqreps 
    numreps := Length( eqreps ); 
    allX := ListWithIdenticalEntries( numreps, 0 );
    for i in [1..numreps] do
        a := eqreps[i]; 
        X1 := LoopsXMod( X0, a ); 
        Info( InfoXMod, 1, "LoopsXMod with a = ", a, ",  ", 
                           "IdGroup = ", IdGroup( X1 ) );  
        if ( InfoLevel( InfoXMod ) > 1 ) then 
            Display( X1 ); 
        fi; 
        allX[i] := X1; 
    od; 
    return allX; 
end ); 

#############################################################################
##
#E  apps.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
