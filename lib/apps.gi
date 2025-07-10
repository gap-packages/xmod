############################################################################
##
#W  apps.gi                    GAP4 package `XMod'             Chris Wensley
##
#Y  Copyright (C) 2001-2024, Chris Wensley et al,  

############################################################################
##
InstallMethod( LoopClass, "for a crossed module and an element", true, 
    [ IsPreXMod, IsObject ], 0, 
function( X0, a ) 

    local M0, P0, bdy0, Q0, class, p, c, q, b, len; 

    if not IsXMod( X0 ) then 
        Error( "X0 should be a crossed module" ); 
    fi; 
    M0 := Source( X0 ); 
    P0 := Range( X0 ); 
    if not ( a in P0 ) then 
        Error( "element a not in the range group P0" ); 
    fi; 
    bdy0 := Boundary( X0 ); 
    Q0 := Image( bdy0 ); 
    class := [ ]; 
    for p in P0 do 
        c := a^p; 
        for q in Q0 do 
            b := c*q; 
            if not ( b in class ) then 
                Add( class, b );
            fi;
        od; 
    od; 
    len := Length( class );
    Print( "class has length ", len, " with elements\n", class, "\n" );
    return class;
end ); 


############################################################################
##
InstallMethod( LoopClasses, "for a crossed module", true, [ IsPreXMod ], 0, 
function( X0 ) 

    local M0, P0, bdy0, Q0, classes, nc, class, a, found, i, p, c, q, b, len; 

    if not IsXMod( X0 ) then 
        Error( "X0 should be a crossed module" ); 
    fi; 
    M0 := Source( X0 ); 
    P0 := Range( X0 ); 
    bdy0 := Boundary( X0 ); 
    Q0 := Image( bdy0 ); 
    classes := [ ]; 
    nc := 0; 
    for a in P0 do 
        found := false; 
        for i in [1..nc] do 
            if ( a in classes[i] ) then 
                found := true; 
            fi; 
        od; 
        if not found then 
            class := [ ]; 
            for p in P0 do 
                c := a^p; 
                for q in Q0 do 
                    b := c*q; 
                    if not ( b in class ) then 
                        Add( class, b );
                    fi;
                od; 
            od; 
            len := Length( class );
            Info( InfoXMod, 1, "class has length ", len, 
                               " with elements\n", class );
            Add( classes, class ); 
            nc := nc+1; 
        fi; 
    od;
    return classes; 
end ); 


InstallMethod( LoopClassOld, "for a crossed module and an element", true, 
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
    Print( "elPa has length ", nPa, "\n" ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( " with elements ", elPa, "\n" );
    fi; 
    C0 := PreCat1GroupRecordOfPreXMod( X0 ).precat1; 
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
    Info( InfoXMod, 2, "Pa has size ", Size( Pa ) ); 
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
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "the boundary map:\n" ); 
        Display( bdy1 ); 
    fi; 
    ## construct the action map for X1 
    imact1 := ListWithIdenticalEntries( ngPa, 0 ); 
    for i in [1..ngPa] do  
        p := elPa[ posPa[i] ][1]; 
        imact1[i] := ImageElm( act0, p ); 
    od; 
    act1 := GroupHomomorphismByImages( Pa, aut0, genPa, imact1 ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "the action map:\n" ); 
        Display( act1 ); 
    fi; 
    ## now we can construct the crossed module 
    return XModByBoundaryAndAction( bdy1, act1 );  
end ); 

############################################################################
## 
InstallMethod( LoopClassesNew, "for a crossed module", true, 
    [ IsPreXMod ], 0, 
function( X0 ) 

    local M0, iterM0, P0, iterPa, iterPb, iterPp, lenP, elP0, 
          act0, bdy0, Q0, found, L, i, a, p, m, b, 
          classes, class, conj, numj, pos, c, c1, j, cj; 

    if not IsXMod( X0 ) then 
        Error( "X0 should be a crossed module" ); 
    fi; 
    M0 := Source( X0 ); 
    P0 := Range( X0 ); 
    bdy0 := Boundary( X0 ); 
    Q0 := Image( bdy0 ); 
    if IsAbelian( P0 ) then 
        return List( RightCosets( P0, Q0 ), Representative ); 
    fi;
    iterM0 := Iterator( M0 ); 
##    elM0 := Elements( M0 ); 
    iterPa := Iterator( P0 );
    iterPp := Iterator( P0 ); 
    elP0 := Elements( P0 ); 
    lenP := Size( P0 ); 
    act0 := XModAction( X0 ); 
##    elQ0 := Elements( Q0 ); 
    found := ListWithIdenticalEntries( lenP, 0 ); 
    L := [ ]; 
    classes := [ ]; 
    while not IsDoneIterator( iterPa ) do 
        a := NextIterator( iterPa ); 
        if ( found[i] = 0 ) then 
            found[i] := 1; 
            class := [ ];
            while not IsDoneIterator( iterPp ) do 
                p := NextIterator( iterPp ); 
                while not IsDoneIterator( iterM0 ) do 
                    m := NextIterator( iterM0 );  
                    b := a^p * ImageElm( bdy0, m );  ## a' = a^p + delta(m) 
                    if not ( b in class ) then  
                        found[ Position( elP0, b ) ] := 1; 
                        Add( class, b ); 
                    fi; 
                od; 
            od; 
            Add( classes, class );
        fi; 
    od; 
    if not IsAbelian( P0 ) then 
        conj := List( ConjugacyClasses( P0 ), Elements ); 
        numj := Length( conj ); 
        pos := ListWithIdenticalEntries( numj, 0 ); 
        for i in [1..numj] do 
            c := conj[i]; 
            c1 := c[1];
            cj := First( classes, j -> c1 in j ); 
            if ForAll( c, g -> g in cj ) then 
                pos[i] := Position( classes, cj ); 
            fi; 
        od; 
        Print( "positions of conjugacy classes in loop classes: ", pos, "\n" ); 
    fi; 
    return classes; 
end ); 

############################################################################
## 
InstallMethod( LoopClassRepresentatives, "for a crossed module", true, 
    [ IsPreXMod ], 0, 
function( X0 ) 

    local M0, iterM0, P0, iterPa, iterPb, iterPp, lenP, 
          act0, bdy0, Q0, found, L, i, a, p, m, b, 
          reps, conj, numj, pos, c, c1, j, cj; 

    if not IsXMod( X0 ) then 
        Error( "X0 should be a crossed module" ); 
    fi; 
    M0 := Source( X0 ); 
    P0 := Range( X0 ); 
    bdy0 := Boundary( X0 ); 
    Q0 := Image( bdy0 ); 
    if IsAbelian( P0 ) then 
        return List( RightCosets( P0, Q0 ), Representative ); 
    fi;
    iterM0 := Iterator( M0 ); 
##    elM0 := Elements( M0 ); 
    iterPa := Iterator( P0 );
    iterPp := Iterator( P0 ); 
##    elP0 := Elements( P0 ); 
    lenP := Size( P0 ); 
    act0 := XModAction( X0 ); 
##    elQ0 := Elements( Q0 ); 
    found := ListWithIdenticalEntries( lenP, 0 ); 
    L := [ ]; 
    reps := [ ]; 
    while not IsDoneIterator( iterPa ) do 
        a := NextIterator( iterPa ); 
        if ( found[i] = 0 ) then 
            found[i] := 1; 
            Add( reps, a );
            while not IsDoneIterator( iterPp ) do 
                p := NextIterator( iterPp ); 
                while not IsDoneIterator( iterM0 ) do 
                    m := NextIterator( iterM0 );  
                    b := a^p * ImageElm( bdy0, m );  ## a' = a^p + delta(m) 
                od; 
            od; 
        fi; 
    od; 
    return reps; 
end ); 

############################################################################
## 
InstallMethod( LoopsXMod, "for a crossed module and an element", true, 
    [ IsPreXMod, IsObject ], 0, 
function( X0, a ) 

    local M0, genM0, lenM0, P0, bdy0, act0, aut0, acta, elPa, p, c, m, rec0, 
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
    Info( InfoXMod, 3, "M0: ", Elements(M0) ); 
    Info( InfoXMod, 3, List( Elements(M0), Order ) );
    Info( InfoXMod, 3, "P0: ", Elements(P0) ); 
    Info( InfoXMod, 3, List( Elements(P0), Order ) );
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
    Info( InfoXMod, 2, "elPa has length ", nPa );
    Info( InfoXMod, 3, "with elements ", elPa );
    C0 := Cat1GroupOfXMod( X0 ); 
    G0 := Source( C0 ); 
    if HasSemidirectProductInfo( G0 ) then 
        e1 := Embedding( G0, 1 );
        e2 := Embedding( G0, 2 );
    elif HasPreCat1GroupRecordOfPreXMod( X0 ) then 
        rec0 := PreCat1GroupRecordOfPreXMod( X0 ); 
        e1 := rec0!.xmodRangeEmbeddingIsomorphism; 
        e2 := rec0!.xmodSourceEmbeddingIsomorphism; 
    else 
        Error( "no way of defining e1, e2" ); 
    fi; 
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
    Info( InfoXMod, 2, "Pa has size ", Size( Pa ) ); 
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
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "the boundary map:\n" ); 
        Display( bdy1 ); 
    fi; 
    ## construct the action map for X1 
    imact1 := ListWithIdenticalEntries( ngPa, 0 ); 
    for i in [1..ngPa] do  
        p := elPa[ posPa[i] ][1]; 
        imact1[i] := ImageElm( act0, p ); 
    od; 
    act1 := GroupHomomorphismByImages( Pa, aut0, genPa, imact1 ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "the action map:\n" ); 
        Display( act1 ); 
    fi; 
    ## now we can construct the crossed module 
    return XModByBoundaryAndAction( bdy1, act1 );  
end ); 

############################################################################
##
InstallMethod( AllLoopsXMod, "default method for a crossed module", true, 
    [ IsPreXMod ], 0, 
function( X0 ) 

    local bdy0, act0, M0, genM0, P0, genP0, igenP0, imbdy0, Q0, 
          conjP0, repsP0, eltsP0, nclP0, i, eclP0, relP0, c, p, genimbdy0, 
          len, class, j, r, k, q, pos, d, h, eqreps, numreps, a, X1, allX; 

    bdy0 := Boundary( X0 ); 
    act0 := XModAction( X0 ); 
    M0 := Source( X0 ); 
    genM0 := GeneratorsOfGroup( M0 ); 
    P0 := Range( X0 ); 
    genP0 := GeneratorsOfGroup( P0 ); 
    igenP0 := List( genP0, g -> ImageElm( act0, g ) ); 
    imbdy0 := ImagesSource( bdy0 ); 
    Q0 := P0/imbdy0; 
    conjP0 := ConjugacyClasses( P0 ); 
    repsP0 := List( conjP0, Representative ); 
    eltsP0 := List( conjP0, Elements ); 
    nclP0 := Length( conjP0 ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "#I  Size(P0) = ", Size(P0), "\n" ); 
        Print( "#I  cokernel of boundary has IdGroup ", IdGroup(Q0), "\n" ); 
        Print( "#I  conjugacy class sizes, reps and orders:classes for P :-\n" ); 
        Print( "#I  ", List( conjP0, Size ), "\n" ); 
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
    Info( InfoXMod, 3, "\neclP0 = ", eclP0 ); 
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
    numreps := Length( eqreps ); 
    Info( InfoXMod, 2, "there are ", numreps, 
                       " equivalence classes with representatives:" ); 
    Info( InfoXMod, 2, eqreps ); 
    ## now run LoopsXMod on on the list eqreps 
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
