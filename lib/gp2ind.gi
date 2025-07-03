#############################################################################
##
#W  gp2ind.gi                      XMOD Package                Chris Wensley
##
#Y  Copyright (C) 2001-2024, Chris Wensley et al,  
##  
##  This file implements functions for induced crossed modules. 

#############################################################################
##
#M  CoproductXMod( <xmod>, <xmod> ) . . . . . . . . . coproduct of two xmods
##
InstallMethod( CoproductXMod, "for two crossed modules", true,
    [ IsXMod, IsXMod ], 0,
function( X1, X2 )

    local f, S1, S2, R, act1, act2, aut2, gen1, gen2, bdy1, bdy2, idR, ok,
          orb12, act12, hom12, imu, mu, sdp, proj, mgi1, mgi2, emb1, emb2,
          emor1, emor2, gens, lens, fgens, gens1, gens2, imbdy, i, j, g, r,
          genR, lenR, cbdy, a1, a2, alpha, imalpha, cact, imcact, caut,
          prexmod, peiffer, pmor, pmor1, pmor2, info, coprod, m;

    ##  function to split a semidirect product element into two parts 
    f := function( g ) 
        local x, x1, y1, y;
        x := ImageElm( proj, g );
        x1 := ImageElm( emb1, x );
        y1 := x1^-1 * g;
        y := PreImagesRepresentativeNC( emb2, y1 );
        if not ( g = x1 * ImageElm( emb2, y ) ) then 
            Error( "problem with factoring g" );
        fi;
        return [x,y];
    end;

    S1 := Source( X1 );
    S2 := Source( X2 );
    if ( Size(S1) = 1 ) then 
        return X2;
    elif ( Size(S2) = 1 ) then 
        return X1;
    fi;
    R := Range( X1 );
    if not ( Range( X2 ) = R ) then 
        Error( "X1 and X2 do not have the same range" );
    fi;
    idR := IdentityMapping( R );
    gen1 := GeneratorsOfGroup( S1 );
    gen2 := GeneratorsOfGroup( S2 );
    genR := GeneratorsOfGroup( R );
    lenR := Length( genR );
    bdy1 := Boundary( X1 );
    bdy2 := Boundary( X2 );
    act1 := XModAction( X1 );
    act2 := XModAction( X2 );
    aut2 := Range( act2 );
    imu := List( gen1, g -> ImageElm( act2, ImageElm( bdy1, g ) ) );
    mu := GroupHomomorphismByImages( S1, aut2, gen1, imu );
    sdp := SemidirectProduct( S1, mu, S2 );
    proj := Projection( sdp );
    mgi1 := MappingGeneratorsImages( Embedding( sdp, 1 ) );
    emb1 := GroupHomomorphismByImages( S1, sdp, mgi1[1], mgi1[2] );
    mgi2 := MappingGeneratorsImages( Embedding( sdp, 2 ) );
    emb2 := GroupHomomorphismByImages( S2, sdp, mgi2[1], mgi2[2] );
    gens := GeneratorsOfGroup( sdp );
    lens := Length( gens );
    fgens := List( gens, g -> f(g) );
    gens1 := List( fgens, z -> z[1] );
    gens2 := List( fgens, z -> z[2] );
    ## now construct the pre-xmod boundary (sdp->R)
    imbdy := ListWithIdenticalEntries( lens, 0 );
    for i in [1..lens] do 
        imbdy[i] := ImageElm(bdy1,gens1[i]) * ImageElm(bdy2,gens2[i]);
    od;
    cbdy := GroupHomomorphismByImages( sdp, R, gens, imbdy );
    ## now construct the pre-xmod action 
    imcact := ListWithIdenticalEntries( lenR, 0 );
    for j in [1..lenR] do 
        r := genR[j]; 
        imalpha := ListWithIdenticalEntries( lens, 0 );
        a1 := ImageElm( act1, r );
        a2 := ImageElm( act2, r );
        for i in [1..lens] do 
            imalpha[i] := ImageElm( emb1, ImageElm(a1,gens1[i]) ) 
                          * ImageElm( emb2, ImageElm(a2,gens2[i]) );
        od;
        imcact[j] := GroupHomomorphismByImages( sdp, sdp, gens, imalpha );
    od;
    caut := Group( imcact );
    cact := GroupHomomorphismByImages( R, caut, genR, imcact );
    prexmod := PreXModByBoundaryAndAction( cbdy, cact );
    m := Size( Source( prexmod ) );
    if ( ( m <= 1000 ) and ( m <> 512 ) ) then 
        Info( InfoXMod, 1, "prexmod is ", IdGroup( prexmod ) );
    fi;
    emor1 := Make2DimensionalGroupMorphism( [ X1, prexmod, emb1, idR ] );
    ok := IsPreXModMorphism( emor1 );
    emor2 := Make2DimensionalGroupMorphism( [ X2, prexmod, emb2, idR ] );
    ok := IsPreXModMorphism( emor2 );
    peiffer := PeifferSubgroup( prexmod );
    Info( InfoXMod, 1, "peiffer subgroup is ", 
        StructureDescription( peiffer:nice ), ", ", IdGroup( peiffer ) );
    coprod := XModByPeifferQuotient( prexmod );
    m := Size( Source( coprod ) );
    if ( ( m <= 1000 ) and ( m <> 512 ) ) then 
        Info( InfoXMod, 1, "the coproduct is ", 
            StructureDescription( coprod:nice ), ", ", IdGroup( coprod ) );
    fi;
    if HasProjectionOfFactorPreXMod( coprod ) then 
        pmor := ProjectionOfFactorPreXMod( coprod );
        ok := IsPreXModMorphism( pmor );
        pmor1 := emor1 * pmor;
        pmor2 := emor2 * pmor;
    else 
        pmor1 := emor1;
        pmor2 := emor2;
    fi;
    SetCoproductInfo( coprod,
        rec( embeddings := [ pmor1, pmor2 ], xmods := [ X1, X2 ] ) );
    return coprod;
end );

InstallMethod( CoproductXMod, "for a list of crossed modules", true, 
    [ IsList ], 0,
function( LX )

    local n, emb, C1, C2, info, e1, e2, i, j, k;

    n := Length( LX );
    if not ForAll( LX, Y -> IsXMod( Y ) ) then 
        Error( "LX is not a list of crossed modules" );
    fi;
    if ( n = 2 ) then 
        return CoproductXMod( LX[1], LX[2] );
    fi;
    emb := ListWithIdenticalEntries( n, 0 );
    C1 := LX[n];
    for i in [1..n-1] do 
        j := n-i;
        C2 := CoproductXMod( LX[j], C1 );
        info := CoproductInfo( C2 );
        e1 := info!.embeddings[1];
        e2 := info!.embeddings[2];
        if ( i = 1 ) then 
            emb[n-1] := e1;
            emb[n] := e2;
        else 
            emb[j] := e1;
            for k in [j+1..n] do 
                emb[k] := emb[k] * e2;
            od;
        fi;
        C1 := C2;
    od;
    info!.xmods := LX;
    info!.embeddings := emb;
    return C2;
end );

############################################################################
##
#F  InducedXMod( <xmod>, <hom> [, <trans>] )          crossed module induced
#F  InducedXMod( <grp>, <grp>, <grp> [, <trans>] )     by group homomorphism 
##
InstallGlobalFunction( InducedXMod, function( arg )

    local usage, nargs, X0, M, P, Q, iota, ires, T, iP, X1, inc, IX;

    usage := function( u )
        Print("\nUsage: InducedXMod( X, iota [, T] );");
        Print("\n where X is a crossed module and iota is a homomorphism");
        Print("\n   or: InducedXMod( Q, P, M [, T] );");
        Print("\n where Q >= P |>= M and T is a transversal for Q/P\n\n");
    end;
    nargs := Length( arg );
    if ( ( nargs < 2 ) or ( nargs > 4 ) ) then
        Info( InfoXMod, 2, "expecting 2, 3 or 4 arguments" );
        usage( 0 );
        return fail;
    fi;
    T := [ ];
    if ( Is2DimensionalDomain( arg[1] ) and IsXMod( arg[1] ) ) then
        X0 := arg[1];
        M := Source( X0 );
        P := Range( X0 );
        iota := arg[2];
        if not ( IsGroupGeneralMapping(iota) and ( Source(iota) = P ) ) then
            usage( 0 );
        fi;
        Q := Range( iota );
        if ( ( nargs = 3 ) and IsList( arg[3] ) ) then
            T := arg[3];
        fi;
    elif ( IsGroup( arg[1] ) and ( nargs >= 3 ) ) then
        Q := arg[1];
        P := arg[2];
        M := arg[3];
        if not ( IsSubgroup( Q, P ) and IsNormal( P, M ) ) then
        Info( InfoXMod, 2, "expecting Q >= P and P |> M" );
            usage( 0 );
            return fail;
        fi;
        X0 := XModByNormalSubgroup( P, M );
        iota := InclusionMappingGroups( Q, P );
        if ( ( nargs = 4 ) and IsList( arg[4] ) ) then
            T := arg[4];
        fi;
    fi;
    Info( InfoXMod, 2, "X0, iota, M, P, Q all defined" );
    if ( T <> [ ] ) then
        Info( InfoXMod, 2, "T specified: ", T );
    fi;
    ## we have now defined X0, iota, M, P, Q in both cases ##
    if ( Size( M ) = 1 ) then 
        Info( InfoXMod, 3, "using induced xmod with trivial source" );
        IX := InducedXModFromTrivialSource( X0, iota );
    elif ( Size( P ) = 1 ) then 
        Info( InfoXMod, 3, "using induced xmod with trivial range" );
        IX := InducedXModFromTrivialRange( X0, iota );
    elif IsSurjective( iota ) then
        Info( InfoXMod, 3, "iota is surj" );
        IX := InducedXModBySurjection( X0, iota );
    elif IsInjective( iota ) then
        Info( InfoXMod, 3, "iota is mono" );
        IX := InducedXModByCopower( X0, iota, T );
    else  ## split in two ##
        Info( InfoXMod, 3, "splitting into surjective and injective cases" );
        iP := ImagesSource( iota );
        ires := GeneralRestrictedMapping( iota, P, iP );
        Info( InfoXMod, 3, "iota splits: ires =", ires );
        X1 := InducedXModBySurjection( X0, ires );
        if ( InfoLevel( InfoXMod ) > 1 ) then
            Print( "surjective induced xmod:\n" );
            Display( X1 );
        fi;
        inc := InclusionMappingGroups( Q, iP );
        IX := InducedXModByCopower( X1, inc, [ ] );
    fi;
    if HasName( X0 ) then
        SetName( IX, Concatenation( "i*(", Name( X0 ), ")" ) );
    elif HasName(M) and HasName(P) and HasName(Q) then
        SetName( IX, 
             Concatenation( "i*(", [Name(Q),Name(P),Name(M)], ")" ) );
    fi;
    return IX;
end );

############################################################################
##
#M  InducedXModFromTrivialSource( <xmod>, <hom> ) . . . . . . . induced xmod
##
InstallMethod( InducedXModFromTrivialSource, "for an xmod and an inclusion",
    true, [ IsXMod, IsGroupHomomorphism ], 0,
function( X0, iota )

    local M, Q, I, IX, morsrc, mor, ok, sdpr;

    Info( InfoXMod, 2, "calling InducedXModFromTrivialSource" );
    Q := Range( iota );
    I := Subgroup( Q, [ One(Q) ] );
    IX := XModByNormalSubgroup( Q, I );
    M := Source( X0 );
    if not ( Size( M ) = 1 ) then 
        Error( "M is not a trivial group" );
    fi;
    morsrc := GroupHomomorphismByImages( M, I, [ One(M) ], [ One(I) ] );
    mor := PreXModMorphism( X0, IX, morsrc, iota );
    if ( mor = fail ) then 
        Error( "mor fails to be a precrossed module morphism" );
    else 
        ok := IsXModMorphism( mor );
    fi;
    if IsPermGroup( IX ) then 
        sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup( IX );
        if not ( sdpr = fail ) then 
            IX := Range( sdpr );
            mor := mor * sdpr;
        fi;
    fi;
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

############################################################################
##
#M  InducedXModFromTrivialRange( <xmod>, <hom> ) . . . . . . .  induced xmod
##
InstallMethod( InducedXModFromTrivialRange, "for an xmod and a monomorphism",
    true, [ IsXMod, IsGroupHomomorphism ], 0,
function( X0, iota )

    local P, Q, oQ, genQ, lenQ, regQ, M, genM, lenM, I, genI, lenI, info,
          morsrc, Ibdy, n, q, rq, L, i, j, k, imact, Iact, Iaut, IX, mor,
          ok, sdpr;

    Info( InfoXMod, 2, "calling InducedXModFromTrivialRange" );
    P := Range( X0 );
    if not ( Size( P ) = 1 ) then 
        Error( "P is not the trivial group" );
    fi;
    Q := Range( iota );
    oQ := Size( Q );
    genQ := GeneratorsOfGroup( Q );
    lenQ := Length( genQ );
    regQ := RegularActionHomomorphism( Q );
    M := Source( X0 );
    genM := GeneratorsOfGroup( M );
    lenM := Length( genM );
    I := DirectProduct( ListWithIdenticalEntries( oQ, M ) );
    if HasName( M ) then 
        SetName( I, Concatenation( Name( M ), "^", String( oQ ) ) );
    fi;
    genI := GeneratorsOfGroup( I );
    lenI := Length( genI );
    if not ( lenI = oQ * lenM ) then 
        Error( "genI has unexpected length" );
    fi;
    info := DirectProductInfo( I );
    morsrc := Embedding( I, 1 );
    Ibdy := MappingToOne( I, Q );
    imact := ListWithIdenticalEntries( lenQ, 0 );
    for n in [1..lenQ] do 
        q := genQ[n];
        rq := ImageElm( regQ, q );
        L := ListWithIdenticalEntries( lenI, 0 );
        for i in [1..oQ] do 
            j := i^rq;
            for k in [1..lenM] do 
                L[(i-1)*lenM+k] := genI[(j-1)*lenM+k];
            od;
        od;
        imact[n] := GroupHomomorphismByImages( I, I, genI, L );
    od;
    Iaut := Group( imact );
    Iact := GroupHomomorphismByImages( Q, Iaut, genQ, imact );
    IX := XModByBoundaryAndAction( Ibdy, Iact );
    mor := PreXModMorphism( X0, IX, morsrc, iota );
    if ( mor = fail ) then 
        Error( "mor fails to be a precrossed module morphism" );
    else 
        ok := IsXModMorphism( mor );
    fi;
    if IsPermGroup( IX ) then 
        sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup( IX );
        if not ( sdpr = fail ) then 
            IX := Range( sdpr );
            mor := mor * sdpr;
        fi;
    fi;
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

############################################################################
##
#M  InducedXModByCopower( <xmod>, <hom>, <trans> ) . . . . . .  induced xmod
##
InstallMethod( InducedXModByCopower,
    "for an xmod, an inclusion, and a transversal", true,
    [ IsXMod, IsGroupHomomorphism, IsList ], 0,
function( X0, iota, trans )

    local CopowerAction, 
          Q, genQ, oQ, elQ, ngQ, q, q1, P, genP, oP, elP, p, iP, eliP,
          M, genM, oM, elM, ngM, m, m1, m2, N, genN, ngN, n1, n2, posn,
          act0, bdy0, comp, mgiota, iP2P, indQP,
          Minfo, FM, genFM, ngFM, g2fpM, fp2gM, mgiM, presFM, relFM, nrFM,
          freeM, genfreeM, genFN, fN, genfN, subfN, defN, relFN, nrFN, FN,
          i, i1, i2, j, j1, j2, k, l1, l2, diff, c1, c2,
          T, t, t1, t2, ok, qP, qT, pos, iN, geniN,
          xgM, ngI, nxI, free1, one1, genfree1, free2, genfree2,
          genFI, ngFI, fgenFI, nfgFI, g1, g2, h1, h2, ag2, u, v,
          ofpi, actQ, imFIQ, imD, gimD, genD, ind, relFI,
          FI1, presFI1, gensFI1, tietze, total,
          FI2, genFI2, ngFI2, oFI2, gensFI2,
          info, ispc, I, f2p, degI, prenew, imold,
          homFIQ, imIQ, FK, genFK, genK, K, oK, mgiFIQ, words, imrem, imM,
          idI, genI, genpos, imact, imI, genim, aut, mor, morsrc,
          bdy, act, ishom, IX, series, idseries, sdpr;

    CopowerAction := function( i, j, q )
    ## calculates (s,u)^q where (s,u) corresponds generator genFI[i][j] 
    ## and returns generator genFI[k][l] corresponding to (m,v)
        local s, u, pos, ip, p, v, k, l;
        s := genN[j];
        u := T[i];
        pos := Position( elQ, u*q );
        ip := qP[pos];
        p := ImageElm( iP2P, ip );
        v := qT[pos];
        m := ImageElm( ImageElm( act0, p ), s );
        l := Position( genN, m );
        k := Position( T, v );
        if ( ( l = fail ) or ( k = fail ) ) then 
            Error( "position failure in CopowerAction" );
        fi;
        return genFI[k][l];
    end;

    M := Source( X0 );
    oM := Size( M );
    genM := GeneratorsOfGroup( M );
    if ( oM = 1 ) then 
        return InducedXModFromTrivialSource( X0, iota );
    fi;
    Info( InfoXMod, 2, "calling InducedXModByCopower" );
    Q := Range( iota );
    genQ := GeneratorsOfGroup( Q );
    oQ := Size( Q );
    ngQ := Length( genQ );
    elQ := Elements( Q );
    P := Range( X0 );;
    genP := GeneratorsOfGroup( P );
    oP := Size( P );
    elP := Elements( P );
    ## image of P under iota
    iP := ImagesSource( iota );
    eliP := List( elP, p -> ImageElm( iota, p ) );
    mgiota := MappingGeneratorsImages( iota );
    iP2P := GroupHomomorphismByImages( iP, P, mgiota[2], mgiota[1] );
    indQP := oQ/oP;
    act0 := XModAction( X0 );
    bdy0 := Boundary( X0 );
    comp := GroupHomomorphismByImages( M, Q, genM, 
                List( genM, m -> ImageElm( iota, ImageElm( bdy0, m ) ) ) ); 
    Minfo := IsomorphismFpInfo( M );
    FM := Minfo!.fp;
    fp2gM := Minfo!.fp2g;
    g2fpM := Minfo!.g2fp;
    mgiM := MappingGeneratorsImages( g2fpM );
    Info( InfoXMod, 2, "MappingGeneratorsImages for M :-" );
    Info( InfoXMod, 2, mgiM, "\n" );
    genM := mgiM[1];
    genFM := mgiM[2];
    ngM := Length( genM );
    ngFM := Length( genFM );
    presFM := PresentationFpGroup( FM );
    TzInitGeneratorImages( presFM );
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "presentation of FM :-\n" );
        TzPrint( presFM );
    fi;
    freeM := FreeGroupOfFpGroup( FM );
    genfreeM := GeneratorsOfGroup( freeM );
    relFM := RelatorsOfFpGroup( FM );
    nrFM := Length( relFM );

    # determine  genN = closure of  genM  under conjugation by  P 
    Info( InfoXMod, 2, 
        "finding closure of GeneratorsOfGroup(M) by P-action");
    genN := ShallowCopy( genM );
    ngN := Length( genN );
    genFN := ShallowCopy( genFM );
    i := 0;
    while ( i < ngN ) do
        i := i + 1;
        n1 := genN[i];
        for p in genP do
            n2 := ImageElm( ImageElm( act0, p ), n1 );
            posn := Position( genN, n2 );
            if ( posn = fail ) then
                Add( genN, n2 );
                m2 := ImageElm( g2fpM, n2 ); ## is this better?
                if not ( n2 in M ) then 
                    Error( "M is not a normal subgroup of P" );
                fi;
                Add( genFN, m2 );
                ngN := ngN + 1;
            fi;
        od;
    od;
    Info( InfoXMod, 2, "genN = P-closure of GeneratorsOfGroup(M) :- ");
    Info( InfoXMod, 2, genN );

    # prepare enlargement FN of FM with more generators
    fN := FreeGroup( ngN, "fN" );
    genfN := GeneratorsOfGroup( fN );
    subfN := genfN{[1..ngFM]};
    Info( InfoXMod, 3, "genFM = ", genFM );
    Info( InfoXMod, 3, "subfN = ", subfN );
    Info( InfoXMod, 3, "genFN = ", genFN );
    defN := List( genFN, g -> MappedWord( g, genFM, subfN ) );
    relFN := List( relFM, r -> MappedWord( r, genfreeM, subfN ) );
    for i in [ (ngFM+1)..ngN ] do
        n1 := defN[i];
        n2 := genfN[i]^(-1);
        Add( relFN, n1*n2 );
    od;
    if ( InfoLevel( InfoXMod ) > 2 ) then
        Print( "FN iso to FM but with generators closed under P-action\n" );
        Print( "Extended set of relators for N :- \n", relFN, "\n" );
    fi;
    nrFN := Length( relFN );
    
    genFN :=  [ ];
    for n1 in genN do
        n2 := PreImagesRepresentativeNC( fp2gM, n1 );
        m1 := ImageElm( g2fpM, n1 );
        l1 := Length( m1 );
        l2 := Length( n2 );
        if ( l2 < l1 ) then
            Add( genFN, n2 );
        else
            Add( genFN, m1 );
        fi;
    od;
    Info( InfoXMod, 2, "revised genFN = ", genFN );
    FN := Subgroup( FM, genFN );
    diff := ngN - ngFM;
    N := Subgroup( M, genN );
    geniN := List( genN, n -> ImageElm( iota, ImageElm( bdy0, n ) ) );
    iN := Subgroup( Q, geniN );
    if ( ( InfoLevel( InfoXMod ) > 1 ) and ( diff > 0 ) ) then
        Print( "Closed generating set for iN :-\n", geniN, "\n" );
    fi;

    ## process transversal
    if ( trans <> [ ] ) then
        T := trans;
    else
        T := CommonTransversal( Q, iP );
    fi;
    ok := IsCommonTransversal( Q, iP, T );
    if not ok then
        Error( "T fails to be a common transversal" );
    fi;
    Info( InfoXMod, 2, "Using transversal :- \n", T );
    #? these lists grow large so perhaps better to avoid them? 
    ## express each  q in Q  as  p.t with  p in iP, t in T
    qP := 0 * [1..oQ];
    qT := 0 * [1..oQ];
    for t in T do
        for p in eliP do
            q := p*t;
            pos := Position( elQ, q );
            qP[pos] := p;
            qT[pos] := t;
        od;
    od;
    Info( InfoXMod, 2, "qP = ", qP );
    Info( InfoXMod, 2, "qT = ", qT );
    Info( InfoXMod, 3, "\nstarting InducedXModByCopower here" );
    xgM := ngN-ngM;
    ngI := ngN*indQP;
    nxI := xgM*indQP;
    free1 := FreeGroup( ngI, "f1" );
    one1 := One( free1 );
    genfree1 := GeneratorsOfGroup( free1 );
    genFI := [ ];
    for i in [1..indQP] do
        j := (i-1) * ngN + 1;
        k := i*ngN;
        Add( genFI, genfree1{[j..k]} );
    od;
    ngFI := Length( genFI );
    ofpi := 0 * [1..ngI];
    actQ := 0 * [1..ngQ];
    for i in [1..ngQ] do
        actQ[i] := ShallowCopy( ofpi );
    od;
    for i in [1..ngN] do
        ofpi[i] := Order( genN[i] );
    od;
    for i in [(ngN+1)..ngI] do
        ofpi[i] := ofpi[i-ngN];
    od;
    Info( InfoXMod, 2, "Orders of the generators of I :- \n", ofpi );
    # Images of the generators of I in Q
    imFIQ := 0 * [1..ngI];
    for i in [1..indQP] do
        t := T[i];
        for j in [1..ngN] do
            n1 := geniN[j];
            n2 := n1^t;
            k := (i-1)*ngN + j;
            imFIQ[k] := n2;
        od;
    od;

    Info( InfoXMod, 2, "  ngQ = ", ngQ );
    Info( InfoXMod, 2, "indQP = ", indQP );
    Info( InfoXMod, 2, "  ngN = ", ngN );
    Info( InfoXMod, 2, " genQ = ", genQ );
    Info( InfoXMod, 2, "geniN = ", geniN );
    Info( InfoXMod, 2, "  elQ = ", elQ );
    Info( InfoXMod, 2, "imFIQ = images in Q of the gens of I: ", imFIQ );
    # Action of the generators of Q on the generators of I
    for j1 in [1..ngQ] do
        q1 := genQ[j1];
        for i2 in [1..indQP] do
            n2 := (i2-1)*ngN;
            for j2 in [1..ngN] do 
                actQ[j1][n2+j2] := CopowerAction( i2, j2, q1 );
            od;
        od;
    od;
    Info( InfoXMod, 2, "\nactQ = ", actQ );
    fgenFI := Flat( genFI );
    nfgFI := Length( fgenFI );
    Info( InfoXMod, 2, "Action of Q on the generators of I :-" );
    for i in [1..ngQ] do 
        for j in [1..nfgFI] do 
            actQ[i][j] := Position( fgenFI, actQ[i][j] );
        od;
        Info( InfoXMod, 2, genQ[i], " : ", PermList( actQ[i] ) );
    od;

    relFI := [ ];
    ## add in the copower relators for FI1 
    for i in [1..indQP] do
        for j in [1..nrFN] do
            u := relFN[j];
            v := MappedWord( u, genfN, genFI[i] );
            Add( relFI, v );
            Info( InfoXMod, 3, "u,v = ", u, " -> ", v );
        od;
    od;
    gimD := [ ];
    ## add in the Peiffer commutators for FI1
    for i1 in [1..indQP] do
        t1 := T[i1];
        n1 := (i1-1)*ngN;
        Info( InfoXMod, 4, "[i1,t1] = ", [i1,t1] );
        for j1 in [1..ngM] do
            m1 := genM[j1];
            t := ImageElm( comp, m1 )^t1;
            if not ( t in gimD ) then 
                Add( gimD, t );
            fi;
            g1 := genFI[i1][j1];
            c1 := n1 + j1;
            if ( ofpi[c1] > 2 ) then
                h1 := g1^-1;
            else
                h1 := g1;
            fi;
            ## \delta(r,t) = t^-1(iota mu r)t 
            q1 := ImageElm( comp, m1 )^t1;
            for i2 in [1..indQP] do
                t2 := T[i2];
                n2 := (i2-1)*ngN;
                Info( InfoXMod, 4, "[i2,t2] = ", [i2,t2] );
                for j2 in [1..ngM] do         # no longer [1..ngN]
                    g2 := genFI[i2][j2];
                    c2 := n2 + j2;
                    if ( ofpi[c2] > 2 ) then
                        h2 := g2^-1;
                    else 
                        h2 := g2;
                    fi;
                    ag2 := CopowerAction( i2, j2, q1 );
                    pos := PositionMaximum( [g1, g2, ag2 ] );
                    if ( pos = 1 ) then 
                        v := h1 * h2 * g1 * ag2;
                    elif( pos = 2 ) then 
                        v := g2 * g1 * ag2^-1 * h1;
                    else 
                        v := ag2 * h1 * h2 * g1;
                    fi;
                    Info( InfoXMod, 3,  "v = ", v );
                    if ( v <> one1 ) then 
                        pos := Position( relFI, v );
                        if ( pos = fail ) then 
                            Add( relFI, v );           # new relator! 
                        fi;
                    fi;
                od;
            od;
        od;
    od;
    imD := Subgroup( Q, gimD );
    ind := Index( Q, imD );
    Info( InfoXMod, 2, "\nImage of I has index ", ind, 
                       " in Q, and is generated by" );
    Info( InfoXMod, 2, gimD );
    FI1 := free1 / relFI;
    presFI1 := PresentationFpGroup( FI1 );
    gensFI1 := GeneratorsOfPresentation( presFI1 );
    TzOptions( presFI1 ).printLevel := InfoLevel( InfoXMod );
    TzInitGeneratorImages( presFI1 );
    presFI1!.protected := ngM;
    ##  presFI1!.oldGenerators := ShallowCopy( presFI1!.generators );??
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\n#I protecting the first ", ngM, " generators\n" );
        TzPrint( presFI1 );
        Print( "\n#I Full set of relators for FI1 :- \n", relFI, "\n" );
        Print( "\n#I Applying PresentationFpGroup, TzPartition & TzGo ", 
               "to FI1 :- \n" );
    fi;
    i := 0;
    repeat  ##??  why 9 times ?? 
        i := i + 1;
        Info( InfoXMod, 3, "TzGo interation number ", i );
        tietze := presFI1!.tietze;
        total := tietze[TZ_TOTAL];
        TzGo( presFI1 );
        if ( InfoLevel( InfoXMod ) > 2 ) then 
            TzPrint( presFI1 );
        fi;
    until ( ( total = tietze[TZ_TOTAL] ) or ( i > 9 ) );
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "\nSimplified pres. for induced group:\n", presFI1, "\n" );
        TzPrint( presFI1 );
    fi;
    FI2 := FpGroupPresentation( presFI1 );
    genFI2 := GeneratorsOfGroup( FI2 );
    Info( InfoXMod, 3, "genFI2 = ", genFI2 );
    ngFI2 := Length( genFI2 );
    oFI2 := Size( FI2 );
    free2 := FreeGroupOfFpGroup( FI2 );
    genfree2 := GeneratorsOfGroup( free2 );
    gensFI2 := GeneratorsOfPresentation( presFI1 );   ## ????? 
    Info( InfoXMod, 3, "gensFI2 = ", gensFI2 );
    Info( InfoXMod, 3, "genFI2 = gensFI2 ? ", genFI2 = gensFI2 );
    Info( InfoXMod, 1, "#I induced group has size: ", oFI2 );
    #? (19/07/11) : example InducedXMod( s4, s3b, s3b ) fails 
    #? because of a pc isomorphism instead of a perm isomorphism,
    #? so revert, for now, to the perm case only: 
    if ( oFI2 >= 1 ) then 
        #?  info := IsomorphismPermOrPcInfo( FI2 );
        info := IsomorphismPermInfo( FI2 );
        #?  ispc := ( info!.type = "pc" );
        ispc := false;
        if ispc then 
            I := info!.pc;
            f2p := info!.g2pc;
        else 
            I := info!.perm;
            degI := NrMovedPoints( I );
            f2p := info!.g2perm;
        fi;
        Info( InfoXMod, 2, "IsomorphismPermOrPcInfo: ", info );
    else 
        Print( "\n#I  unexpected order(I) = 1\n" );
        I := Group( () );
        ##I := Subgroup( Group( () ), [ ] );
    fi;

    # now identify I (if possible)
    if IsAbelian( I ) then
        Info( InfoXMod, 2, "#I factor ", i, 
                  " is abelian with invariants", AbelianInvariants( I ) );
    elif ( ( oFI2 < 2000 ) and ( oFI2 <> 1024 ) ) then 
        series := CompositionSeries( I );
        if ( InfoLevel( InfoXMod ) > 1 ) then
            DisplayCompositionSeries( series );
        fi;
        ## (21/01/10, 06/07/10) changed this condition - also changed below 
        if ( Size( series[1] ) < 2000 ) then 
            if ( RemInt( Size(series[1]), 128 ) <> 0 ) then 
                idseries := List( series, StructureDescription );
            else 
                idseries := List( series, IdGroup );
            fi;
            Info( InfoXMod, 2, "CompositionSeries for induced group:" );
            Info( InfoXMod, 2, idseries, "\n" );
        fi;
    fi;
    if HasName( M ) then
        SetName( I, Concatenation( "i*(", Name( M ), ")" ) );
    else
        SetName( I, "i*M" );
    fi;
    Info( InfoXMod, 2, "presFI1!.oldGenerators = ",presFI1!.oldGenerators);
    Info( InfoXMod, 2, "genFI2 = ", genFI2 );
    imold := TzImagesOldGens( presFI1 );
    prenew := TzPreImagesNewGens( presFI1 );
    Info( InfoXMod, 2, "   ImagesOldGens: ", imold );
    Info( InfoXMod, 2, "PreImagesNewGens: ", prenew );       
    genD := 0 * [1..ngFI2];
    for i in [1..ngFI2] do
        g1 := prenew[i];
        ##  change made as a test (14/01/04)
        ##  j := Position( gensFI1, g1 );
        j := Position( imold, g1 );
        genD[i] := imFIQ[j];
    od;
    Info( InfoXMod, 2, "genD = ", genD );
    homFIQ := GroupHomomorphismByImages( FI2, Q, genFI2, genD );
    FK := Kernel( homFIQ );
    genFK := GeneratorsOfGroup( FK );
    genK := List( genFK, k -> ImageElm( f2p, k ) );
    Info( InfoXMod, 2, "genFK = ", genFK );
    Info( InfoXMod, 2, " genK = ", genK );
    K := Subgroup( I, genK );
    oK := Size( K );
    if ( ( InfoLevel( InfoXMod ) > 2 ) and ( oK > 1 ) ) then
        Print( "K has size: ", oK, "\n" );
        if ( oK <= 4000 ) then
            Print( "K has IdGroup\n", IdGroup( K ), "\n" );
        fi;
    fi;
    mgiFIQ := MappingGeneratorsImages( f2p );
    Info( InfoXMod, 2, "mgiFIQ[1] = genFI2 ?? ", mgiFIQ[1] = genFI2 );
    words := List( imold, w -> MappedWord( w, gensFI2, genFI2 ) );
    Info( InfoXMod, 2, "words = ", words );
    imrem := List( words, w -> ImageElm( f2p, w ) );
    Info( InfoXMod, 2, "imrem = ", imrem );

    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\nInitial generators in terms of final generators :- \n" );
        for i in [ 1..Length(imold) ] do
            Print( gensFI1[i]," : ",imold[i]," --> ",imrem[ i ],"\n" );
        od;
    Print( "\n" );
    fi;

    idI := IdentityMapping( I );
    genI := GeneratorsOfGroup( I );
    genpos := List( genI, g -> Position( imrem, g ) );
    Info( InfoXMod, 2, "genpos = ", genpos );
    imI := 0 * [1..ngQ];
    imact := 0 * [1..ngQ];
    for i in [1..ngQ] do
        imI[i] := List( actQ[i], j -> imrem[j] );
        ## imI[i] := List( actQ[i], j -> mgicomp[2][j] );
        genim := List( genpos, p -> imI[i][p] );
        imact[i] := GroupHomomorphismByImages( I, I, genI, genim );
    od;
    imIQ := List( genpos, p -> imFIQ[p] );
    Info( InfoXMod, 2, " imFIQ = ", imFIQ );
    Info( InfoXMod, 2, "  imIQ = ", imIQ );
    bdy := GroupHomomorphismByImages( I, Q, genI, imIQ );
    ishom := IsGroupHomomorphism( bdy );
    imM := imrem{[1..ngM]};
    Info( InfoXMod, 2, [ M, I, genM, imM ] );
    Info( InfoXMod, 2, "------------------------------------------------" );
    morsrc := GroupHomomorphismByImages( M, I, genM, imM );
    if ( morsrc = fail ) then 
        Error( "morsrc fails to be a group homomorphism" );
    fi;
    Info( InfoXMod, 2, "morsrc: ", morsrc, "\n" );
    aut := GroupWithGenerators( imact, idI );
    SetName( aut, "aut(i*)" );
    act := GroupHomomorphismByImages( Q, aut, genQ, imact );
    IX := XModByBoundaryAndAction( bdy, act );
    SetIsInducedXMod( IX, true );
    ## IX!.xmod := X0;
    SetName( IX, Concatenation( "i*(", Name( X0 ), ")" ) );
    mor := PreXModMorphism( X0, IX, morsrc, iota );
    if not IsXModMorphism( mor ) then
        Print( "mor: X0 -> IX  not an xmod morphism!\n" );
    fi;
    if IsPermGroup( IX ) then 
        sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup(IX);
        if not ( sdpr = fail ) then 
            IX := Range( sdpr );
            mor := mor * sdpr;
        fi;
    fi;
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

###############################################################################
##
#M  InducedXModBySurjection( <xmod>, <hom> ) . . . induced xmod, surjective map
##
InstallMethod( InducedXModBySurjection, "for xmod and surjective homomorphism",
    true, [ IsXMod, IsGroupHomomorphism ], 0,
function( X0, iota )

    local ispc, S, genS, R, bdy, act, K, genK, s, r, a, x,
          H, genH, rcos, reps, Q, lenQ, genQ, preQ, PI, actPI,
          isoI, I, genI, imi, istar, acthom, imb, bdystar, i,
          autgen, imI, imS, actstar, autstar, idI, IX, ok, mor, sdpr;

    Info( InfoXMod, 2, "calling InducedXModBySurjection" );
    R := Range( X0 );
    S := Source( X0 );
    if IsBijective( iota ) then 
        Info( InfoXMod, 2, "constructing isomorphic xmod" );
        istar := IdentityMapping( S );
        mor := IsomorphismByIsomorphisms( X0, [ istar, iota ] );
        IX := Image( mor );
        if IsPermGroup( IX ) then 
            sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup(IX);
            if not ( sdpr = fail ) then 
                IX := Range( sdpr );
                mor := mor * sdpr;
            fi;
        fi;
        SetMorphismOfInducedXMod( IX, mor );
        return IX;
    fi;
    ispc := IsPc2DimensionalGroup( X0 );
    genS := GeneratorsOfGroup( S );
    bdy := Boundary( X0 );
    act := XModAction( X0 );
    K := Kernel( iota );
    genK := GeneratorsOfGroup( K );
    H := DisplacementGroup( X0, K, S );
    genH := GeneratorsOfGroup( H );
    Info( InfoXMod, 2, "displacement group generators: ", genH );
    Q := Range( iota );
    genQ := GeneratorsOfGroup( Q );
    preQ := List( genQ, q -> PreImagesRepresentativeNC( iota, q ) );
    rcos := RightCosets( S, H );
    reps := List( rcos, Representative );
    Info( InfoXMod, 2, "reps = ", reps );
    imb := List( genS, r -> ImageElm( iota, ImageElm( bdy, r ) ) );
    PI := Action( S, rcos, OnRight );
    actPI := ActionHomomorphism( S, PI );
    if ispc then 
        isoI := IsomorphismPcGroup( PI );
        ispc := not ( isoI = fail );
    fi;
    if ispc then 
        I := ImagesSource( isoI );
        acthom := actPI * isoI;
    else 
        I := PI;
        acthom := actPI;
    fi;
    Info( InfoXMod, 2, "acthom = ", MappingGeneratorsImages( acthom ) );
    genI := GeneratorsOfGroup( I );
    Info( InfoXMod, 2, "genI = ", genI );
    if HasName( S ) then
        SetName( I, Concatenation( Name( S ), "/ker" ) );
    fi;
    imi := List( genS, s -> ImageElm( acthom, s ) );
    istar := GroupHomomorphismByImages( S, I, genS, imi );
    bdystar := GroupHomomorphismByImages( I, Q, imi, imb );
    Info( InfoXMod, 3, "bdystar = ", bdystar );
    lenQ := Length( genQ );
    autgen := 0 * [1..lenQ];
    for i in [1..lenQ] do
        a := ImageElm( act, preQ[i] );
        imS := List( genS, s -> ImageElm( a, s ) );
        imI := List( imS, s -> ImageElm( acthom, s ) );
        autgen[i] := GroupHomomorphismByImages( I, I, imi, imI );
    od;
    idI := InclusionMappingGroups( I, I );
    autstar := Group( autgen, idI );
    actstar := GroupHomomorphismByImages( Q, autstar, genQ, autgen );
    Info( InfoXMod, 3, "actstar = ", actstar );
    IX := XMod( bdystar, actstar );
    SetIsInducedXMod( IX, true );
    if HasName( X0 ) then
        SetName( IX, Concatenation( "i*(", Name( X0 ), ")" ) );
    fi;
    if ( HasIsCentralExtension2DimensionalGroup( X0 ) 
         and IsCentralExtension2DimensionalGroup( X0 ) ) then 
        ok := IsCentralExtension2DimensionalGroup( IX );
    fi;
    mor := XModMorphism( X0, IX, istar, iota );
    if IsPermGroup( IX ) then 
        sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup( IX );
        if not ( sdpr = fail ) then 
            IX := Range( sdpr );
            mor := mor * sdpr;
        fi;
    fi;
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

#############################################################################
##
#M  AllInducedXMods( <grp> ) given Q, finds all XMods induced as  M <= P <= Q
##
InstallGlobalFunction( AllInducedXMods, function( arg )

    local nargs, rrange,nrange, usage, L, lenL, reps, nreps, r, i, j, k,
          a, b, norm, nnorm, n, sizes, keep, coll, Q, P, M, id, XQ, SQ,
          num, line, all, descrip, Msd, Psd, Qsd, SQsd, Ksd;

    all := [ ];
    descrip := [ ];
    nargs := Length( arg );
    Q := arg[1];
    Qsd := StructureDescription( Q );
    Info( InfoXMod, 2, "Induced crossed modules with Q = ", Qsd );
    L := LatticeSubgroups( Q );
    norm := NormalSubgroups( Q );
    Info( InfoXMod, 2, "normal subgroups of Q: ", norm );
    reps := Reversed( List( ConjugacyClassesSubgroups( L ),  
                            Representative ) );
    nreps := Length( reps );
    Info( InfoXMod, 2, "non-trivial reps = ", [2..nreps-1] );
    for r in [ 1 .. nreps-1 ] do
        Info( InfoXMod, 2, StructureDescription( reps[r] ), " = ", reps[r] );
    od;
    num := 0;
    line := "--------------------------------------";
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\nAll induced crossed modules  M --> IM" );
        Print( "\n                             |     | " );
        Print( "\n                             P --> Q\n");
        Print( "\ngenQ = ", GeneratorsOfGroup( Q ), "\n" );
        Print( "\n", line, line, "\n\n" );
    fi;
    if ( nargs > 1 ) then 
        rrange := arg[2];
    else 
        rrange := [2..nreps-1];
    fi;
    for r in rrange do
        P := reps[r];
        Psd := StructureDescription( P );
        norm := NormalSubgroups( P );
        # find representatives of conjugacy classes in Q
        sizes := List( norm, Size );
        coll := Collected( sizes );
        keep := List( norm, n -> true );
        k := 1;
        for i in [ 2 .. ( Length( coll ) - 1 ) ] do
            j := k + 1;
            k := k + coll[i][2];
            for a in [ j .. k-1 ] do
                if keep[a] then
                    for b in [ a+1 ..k ] do
                        if IsConjugate( Q, norm[a], norm[b] ) then
                            keep[b] := false;
                        fi;
                    od;
                fi;
            od;
        od;
        nnorm := Length( norm );
        Info( InfoXMod, 2, "nnorm = ", nnorm );
        ##  ??  (16/01/04)
        ##  norm[ nnorm ] := P;
        if ( ( nargs > 2 ) and ( Length(rrange) = 1 ) ) then 
            nrange := arg[3];
        else 
            nrange := [1..nnorm];
        fi;
        for n in nrange do
        ## for n in [1..nnorm-1] do
            if keep[n] then
                M := norm[n];
                Msd := StructureDescription( M );
                Info( InfoXMod, 2, "[ ", Msd, " -> ", Psd, " ]" );
                num := num + 1;
                Info( InfoXMod, 2, num, ". : " );
                Info( InfoXMod, 2, "genM = ", GeneratorsOfGroup( M ) );
                Info( InfoXMod, 2, "genP = ", GeneratorsOfGroup( P ) );
                XQ := InducedXMod( Q, P, M );
                Add( all, XQ );
                SQ := Source( XQ );
                SQsd := StructureDescription( SQ );
                if ( InfoLevel( InfoXMod ) > 1 ) then
                    Display( XQ );
                    Print( "SQ has structure description: ", SQsd, "\n\n" );
                    Print( line, "\n\n" );
                fi;
                Ksd := StructureDescription( Kernel( Boundary( XQ ) ) );
                Add( descrip, [ Msd, Psd, SQsd, Qsd, Ksd ] );
            fi;
        od;
    od;
    Info( InfoXMod, 1, 
              "Number of induced crossed modules calculated = ", num );
    if ( InfoLevel( InfoXMod ) > 0 ) then 
        Print( "#I induced crossed modules [M->P] -> [iM->Q] where:\n" );
        Print( "#I groups [ M, P, iM, Q, ker(bdy) ] are:\n" );
        Perform( descrip, Display );
    fi;
    return all;
end );

#############################################################################
##
#M  InducedCat1Data( <cat1>, <hom>, <trans> )
##
##  ?? do we really want the trans ???
##
InstallMethod( InducedCat1Data, "for cat1-group, homomorphism, list",
    true, [ IsCat1Group, IsGroupHomomorphism, IsList ], 0,
function( C, iota, trans )

local Q, R, G,                    # 3 permutation groups
      Qinfo, Rinfo, Ginfo,        # IsomorphismFpInfos
      # C, iota,                  # C = [G ==iota==> R]
      ICGinfo,                    # InducedCat1Info
      nargs,                      # number of arg
      FQ, FR, FG,                 # Fin. Pres. Group
      elQ, elR, elG,              # elements of groups
      genQ, genR, genG,           # generating sets of groups
      oQ, oR, oG,                 # size of groups
      ngG,                        # number of generating set for G
      indQ,                       # oQ/oR
      degQ,                       # NrMovedPoints( Q )
      genFG,                      # generating set for Fin.Pres. Group FG
      ngFG,                       # number of generating set for FG
      relFG,                      # relators for FG
      nrFG,                       # number of all relators of FG
      elFG,                       # elements of FG
      presFG,                     # PresentationViaCosetTable for FG
      fp2gG,                      # record field for G
      qR, qT,                     # record field for Q
      posg2f, posf2g,             # positions of isomorphic images G <-> FG
      pos,                        # position variable
      T,                          # Tinfo
      ok,                         # checking variable
      t, i, q, p, m, rm;          # variables

    G := Source( C );
    Ginfo := IsomorphismFpInfo( G );
    R := Range( C );
    Rinfo := IsomorphismFpInfo( R );
    Q := ImagesSource( iota );
    Qinfo := IsomorphismFpInfo( Q );

    oQ := Size( Q );
    elQ := Elements( Q );
    genQ := GeneratorsOfGroup( Q );
    if IsPermGroup( Q ) then
        degQ := NrMovedPoints( Q );
    fi;

   # if not IsSubgroup( Q, R ) then
   #     Error( " R not a subgroup of Q" );
   # fi;
    oR := Size( R );
    elR := Elements( R );
    genR := GeneratorsOfGroup( R );
    indQ := oQ/oR;
    
    #if not IsNormal( R, G ) then
    #    Error( " R not a normal subgroup of G" );
    #fi;
    oG := Size( G );
    elG := Elements( G );
    genG := GeneratorsOfGroup( G );
    ngG := Length( genG );
    
    presFG := PresentationFpGroup( FG );
    TzOptions( presFG ).printLevel := InfoLevel( InfoXMod );
    TzInitGeneratorImages( presFG );
    if ( InfoLevel( InfoXMod ) > 2 ) then
        Print( "initial presentation for G :-\n\n" );
        TzPrint( presFG );
    fi;
    relFG := RelatorsOfFpGroup( FG );
    nrFG := Length( relFG );
    elFG := Elements( FG );  

    # Determine the positions of isomorphic images G <-> FG
    posf2g := 0 * [1..oG];
    posg2f := 0 * [1..oG];
    fp2gG := Ginfo!.fp2g;
    for i in [1..oG] do
        m := elFG[i];
        rm := ImageElm( fp2gG, m );
        pos := Position( elG, rm );
        posf2g[i] := pos;
        posg2f[pos] := i;
    od;
    Info( InfoXMod, 2, "posf2g = ", posf2g );
    Info( InfoXMod, 2, "posg2f = ", posg2f );

    ICGinfo := rec( 
      Qinfo := Qinfo,
      Rinfo := Rinfo,
      Ginfo := Ginfo,
      cat1 := C,
      iota := iota,
      isInducedCat1Info := true );
    return ICGinfo;
end );

#############################################################################
##
#M  InducedCat1GroupByFreeProduct( <list> )
##
InstallMethod( InducedCat1GroupByFreeProduct, "for a list", true, [ IsList ], 0,
function( info )

    local FQ,               # Fin. Pres. Group
          Q,                # Perm Group 
          Qinfo,            # Record field 
          oQ,               # Size of Q
          genQ,             # generating set for Perm group Q 
          ngQ,              # number of generating set of Perm Group Q
          genG,             # generating set perm group G
          ngG,              # number of generating set of perm G
          ngI,              # total length of ngPG+ngPQ
          C,                # Cat1Group 
          Csrc,             # Cat1Group source 
          Crng,             # Cat1Group range 
          t, h, e,          # tail, head, embedding 
          genCsrc,          # generating set of sourxe group
          genCrng,          # generating set of range group
          fI,               # free group 
          genfI,            # generating set of free group 
          imG, imQ, 
          relI,             # all relators
          Gfp,              # IsomorphismFpInfo 
          genGfp,           # generating set  
          FGrel,            # relators
          len,              # Length of relators
          Qfp,              # IsomorphismFpInfo for Q
          genQfp,           # generating set 
          FQrel,            # relators
          iota,             # inclusion map from Crng to PQ
          imembed,          # relations produced by embedding
          imiota,           # relations produced by iota
          uuQ,              # List variable
          wQ, uQ, wG, uG, uQG,
          kert, kerh,       # kernel of tail and head
          genkert,          # generating set of kert
          genkerh,          # generating set of kerh
          imt, imh, 
          tG, hG, 
          com,              # Commutator subgroup
          Yh, Yt,           # conjugations for tail and head
          YYt, YYh,         # List of conjugations
          I, genI,          # new free group and its generating set
          presFI,           # Presentation
          newFIfp,          # IsomorphismFpInfo
          PI,               # new permutational group
          oFI2, genPI,      # Size and generating set of new perm group
          iotastar,         # homomorphism from Csrc to nep perm group
          imh1, imh2, 
          hstar,            # new head homomorphism for induced cat1-group
          imt1, imt2, 
          tstar,            # new tail homomorphism for induced cat1-group
          imm, imag, images, 
          estar,            # new embed homomorphism for Ind.cat1
          IC,               # Induced Cat1-group variable
          mor,              # Cat1GroupMorphism from C to IC 
          u, v, j, x, i, g; # using variables

    Q := info!.Qinfo!.perm;
    C := info!.cat1;
    iota := info!.iota;
    Csrc := C!.source;
    Crng := C!.range;
    t := C!.tailMap;
    h := C!.headMap;
    e := C!.embedRange;
    genQ := GeneratorsOfGroup( Q );
    genCsrc := Csrc!.generators;
    genCrng := Crng!.generators;
    Info( InfoXMod, 2, "genCrng = ", genCrng );
    ngG := Length( genCsrc );
    ngQ := Length( genQ );
    ngI := ngG+ngQ;
    fI := FreeGroup( ngI, "fI" );
    genfI := fI!.generators;
    imQ := genfI{[ngG+1..ngI]};
    imG := genfI{[1..ngG]};
    relI := [ ];
    # Creating the relations of G
    Gfp := IsomorphismFpInfo( Csrc );
    genGfp := GeneratorsOfGroup( Gfp!.fp );
    FGrel := RelatorsOfFpGroup( Gfp!.fp );
    len := Length( FGrel );
    for j in [1..len] do
        u := FGrel[j];
        v := MappedWord( u, genGfp, imG );
        Add( relI, v );
    od;
    # Adding extra relations from Q
    Qfp := IsomorphismFpInfo( Q );
    genQfp := GeneratorsOfGroup( Qfp!.fp );
    FQrel := RelatorsOfFpGroup( Qfp!.fp );
    len := Length( FQrel );
    for j in [1..len] do
        u := FQrel[j];
        v := MappedWord( u, genQfp, imQ );
        Add( relI, v );
    od;
    # Adding extra relations from embedding and iota
    uuQ := [ ];
    imembed := List( genCrng, x -> ImageElm( e, x ) );
    wG := List( imembed, x -> ImageElm( Gfp!.p2f, x ) );
    uG := List( wG, g -> MappedWord( g, genGfp, imG ) );
    imiota := List( genCrng, x -> ImageElm( iota, x ) );
    wQ := List( imiota, x -> ImageElm( Qfp!.p2f, x ) );
    uQ := List( wQ, u -> MappedWord( u, genQfp, imQ ) );
    for i in [1..Length(uG)] do     
        uQG := uG[i]*uQ[i]^-1;
        Add( uuQ, uQG );
    od;
    relI := Concatenation( relI, uuQ );
    # Finding the Peiffer subgroup
    YYt := [ ];
    YYh := [ ];
    kert := Kernel( t );
    kerh := Kernel( h );
    genkert := kert!.generators;
    genkerh := kerh!.generators;
    imt := List( genkert, x -> ImageElm( Gfp!.p2f, x ) );
    tG := List( imt, i -> MappedWord( i, genGfp, imG ) );
    imh := List( genkerh, x -> ImageElm( Gfp!.p2f, x ) );
    hG := List( imh, i -> MappedWord( i, genGfp, imG ) );
    for u in genfI do
    Yt := List( tG, x -> x^u );
    YYt := Concatenation( YYt, Yt );
    Yh := List( hG, x -> x^u );
    YYh := Concatenation( YYh, Yh );
    od;
    for i in YYt do
        for j in YYh do
            com := Comm( i, j );
            Add( relI, com );
        od;
    od;
    I := fI / relI;
    genI := I!.generators;
    presFI := PresentationFpGroup( I );
    TzInitGeneratorImages( presFI );
    presFI!.protected := Length( genI );
    Print( "#I  Protecting the first ", ngG, " generators!.\n" );
    presFI!.oldGenerators := ShallowCopy( presFI!.generators );
     if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\nFull set of relators for I :- \n", relI, "\n" );
        Print( "\nApplying PresentationFpGroup to I :- \n" );
        TzPrint( presFI );
        Print( "\nApplying TzPartition & TzGo to I :- \n" );
    fi;
    TzPrint( presFI );
    TzGoGo( presFI );
    TzPrint( presFI );
    imQ := genI{[ngG+1..ngI]};
    imG := genI{[1..ngG]};
    newFIfp := IsomorphismFpInfo( I );
    PI := newFIfp!.perm;
    oFI2 := Size( PI );
    genPI := PI!.generators;
    Print("new perm group size ", oFI2, "\n");
    Print("******************** \n");
    imG := genPI{[1..ngG]};
    iotastar := GroupHomomorphismByImages( Csrc, PI, genCsrc, imG );
    imh1 := List( genCsrc, x -> ImageElm( h, x ) );
    imh2 := List( imh1, x -> ImageElm( iota, x ) );
    imh := Concatenation( imh2, genQ );
    hstar := GroupHomomorphismByImages( PI, Q, genPI, imh );
    imt1 := List( genCsrc, x -> ImageElm( t, x ) );
    imt2 := List( imt1, x -> ImageElm( iota, x ) );
    imt := Concatenation( imt2, genQ );
    tstar := GroupHomomorphismByImages( PI, Q, genPI, imt );
    imm := List( genQ, x -> ImageElm( Qfp!.p2f, x ) );
    imag := List( imm, x -> MappedWord( x, genQfp, imQ ) );
    images := List( imag, x -> ImageElm( newFIfp!.f2p, x ) );
    estar := GroupHomomorphismByImages( Q, PI, genQ, images );
    IC := Cat1Group( PI, tstar, hstar, estar );
    IC!.isCat1 := IsCat1Group( IC );
    mor := Cat1GroupMorphism( C, IC, [ iotastar, iota ] );
    if not ( IsCat1GroupMorphism( mor ) ) then
        Print( " mor : C --> IC not a cat1-group morphism \n" );
    fi;
    IC := rec( 
      morphism := mor,
      name := Concatenation( "<ICG(", Name( C ), ")>" ),
      cat1 := C,
      isInducedCat1Group := true );
    return IC;
end );

############################################################################
##
#F  InducedCat1Group( <arg> ) . . . . . . . . . . . . .  induced cat1-groups
##
InstallGlobalFunction( InducedCat1Group, function( arg )

    local nargs, info, Q, Qinfo, P, Pinfo, G, Ginfo, C, iota, IC;

    nargs := Length( arg );
    if ( nargs > 2 ) then
        return false;
    fi;
##    if not IsRecord( arg[1] ) then
##        return false;
##    fi;
    if IsGroup( arg[1] ) then
        if ( ( nargs < 3 ) or not IsNormal( arg[2], arg[1] ) ) then
            return false;
        fi;
        Qinfo := arg[1];
        Pinfo := arg[2];
        Ginfo := arg[3];
        C := Cat1Group( Pinfo, Ginfo );
        G := Source( C );
        Ginfo := IsomorphismFpInfo( G );
        iota := InclusionMappingGroups( Pinfo, Qinfo );
    elif IsCat1Group( arg[1] ) then
        C := arg[1];
        G := Source( C );
        Ginfo := IsomorphismFpInfo( G );
        P := Range( C );
        Pinfo := IsomorphismFpInfo( P );
        iota := arg[2];
        #if ( ( nargs > 2 ) or ( iota!.source <> Pinfo )
        #                   or not IsInjective( iota ) ) then
        #    return false;
        #fi;
        Q := ImagesSource( iota );
        Qinfo := IsomorphismFpInfo( Q );
    fi;

    info := InducedCat1Data( C, iota );
    IC := InducedCat1GroupByFreeProduct( info );
    return IC;
end );

#############################################################################
##
#M  AllInducedCat1Groups( <grp> ) . . . . . . . . . . . . induced cat1-groups
##
InstallGlobalFunction( AllInducedCat1Groups, function( Q )

    local rrange, nrange, L, lenL, reps, nreps, r, i, j, k, a, b,
          norm, nnorm, n, sizes, keep, coll, P, M, id, info,
          IC, num, line, C, iota;

    L := LatticeSubgroups( Q );
    reps := Reversed( List( ConjugacyClassesSubgroups(L), 
                      Representative ) );
    nreps := Length( reps );
    Print( "non-trivial reps = ", [2..nreps-1], "\n" );
    for r in [ 2 .. nreps-1 ] do
        Print( reps[r], "\n" );
    od;
    num := 0;
    Print( "\n All induced cat1-groups       M --> IM" );
    Print( "\n                              ||    || " );
    Print( "\n                               P --> Q\n");
    Print( "\n genQ = ", GeneratorsOfGroup( Q ), "\n" );
    line := "--------------------------------------";
    Print( "\n", line, line, "\n\n" );
    rrange := [2..nreps-1];
    for r in rrange do
        P := reps[r];
        norm := NormalSubgroups( P );
        # find representatives of conjugacy classes in Q
        sizes := List( norm, Size );
        coll := Collected( sizes );
        keep := List( norm, n -> true );
        k := 1;
        for i in [ 2 .. ( Length( coll ) - 1 ) ] do
            j := k + 1;
            k := k + coll[i][2];
            for a in [ j .. k-1 ] do
                if keep[a] then
                    for b in [ a+1 ..k ] do
                        if IsConjugate( Q, norm[a], norm[b] ) then
                            keep[b] := false;
                        fi;
                    od;
                fi;
            od;
        od;
        nnorm := Length( norm );
        norm[ nnorm ] := P;
        nrange := [2..nnorm];
        for n in nrange do
            if keep[n] then
                M := norm[n];
                Print( "genM = ", GeneratorsOfGroup( M ), "\n" );
                Print( "genP = ", GeneratorsOfGroup( P ), "\n" );
                C := Cat1Group( P, M );
                iota := InclusionMappingGroups( P, Q );
                IC := InducedCat1Group( C, iota );
                Display( IC );
                num := num + 1;
                Print( line, line, "\n\n" );
            fi;
        od;
    od;
    Print( "Number of induced cat1-groups calculated = " );
    return num;
end );
