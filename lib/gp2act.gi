#############################################################################
##
#W  gp2act.gi                  GAP4 package `XMod'              Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2024, Chris Wensley et al,  
##
##  This filebimplements methods for actor crossed squares of crossed modules

#############################################################################
##
#M  AutomorphismPermGroup( <XM> )  subgroup of Aut(Source(XM))xAut(Range(XM))
##
InstallMethod( AutomorphismPermGroup, "automorphism perm group of xmod", 
    true, [ IsXMod ], 0, 
function( XM )

    local S, genS, ngS, R, genR, ngR, act, 
          AS, genAS, a2pS, PAS, genPAS, p2aS,
          AR, genAR, a2pR, PAR, genPAR, p2aR, 
          D, genD, emS, emR, emgenPAS, emgenPAR, emAS, emAR, infoD, 
          P, num, autogens, as, eas, ar, ear, mor, ispre, ismor, 
          genP, imsrc, imrng, projDS, projDR, projPS, projPR, ePAS, ePAR;

    Info( InfoXMod, 1, "using standard AutomorphismPermGroup method" );
    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    ngS := Length( genS );
    R := Range( XM );
    genR := GeneratorsOfGroup( R );
    ngR := Length( genR );
    act := XModAction( XM );
    AS := AutomorphismGroup( S ); 
    genAS := GeneratorsOfGroup( AS );
    a2pS := IsomorphismPermGroup( AS );    ### check if smaller possible
    PAS := Image( a2pS );
    genPAS := List( genAS, a -> ImageElm( a2pS, a ) );
    p2aS := GroupHomomorphismByImages( PAS, AS, genPAS, genAS ); 
    if ( p2aS = fail ) then 
        Error( "p2aS = fail" );
    else 
        SetAutoGroupIsomorphism( PAS, p2aS );
    fi;
    Info( InfoXMod, 1, "p2aS = ", p2aS );
    AR := AutomorphismGroup( R );
    genAR := GeneratorsOfGroup( AR );
    a2pR := IsomorphismPermGroup( AR );    ### ditto
    PAR := Image( a2pR );
    genPAR := List( genAR, a -> ImageElm( a2pR, a ) );
    p2aR := GroupHomomorphismByImages( PAR, AR, genPAR, genAR );
    if ( p2aR = fail ) then 
        Error( "p2aR = fail" );
    else 
        SetAutoGroupIsomorphism( PAR, p2aR );
    fi;
    Info( InfoXMod, 1, "p2aR = ", p2aR );
    D := DirectProduct( PAS, PAR );
    genD := GeneratorsOfGroup( D );
    if ( HasName( PAS ) and HasName( PAR ) ) then
        SetName( D, Concatenation( Name(PAS), "x", Name(PAR) ) );
    fi;
    emS := Embedding( D, 1 );
    emR := Embedding( D, 2 );
    emgenPAS := List( genPAS, a -> ImageElm( emS, a ) );
    emgenPAR := List( genPAR, a -> ImageElm( emR, a ) ); 
    emAS := GroupHomomorphismByImages( AS, D, genAS, emgenPAS );  
    emAR := GroupHomomorphismByImages( AR, D, genAR, emgenPAR ); 
    infoD := DirectProductInfo( D );
    P := Subgroup( D, [ ] );
    num := 0;
    autogens := [ ];
    for as in AS do 
        eas := ImageElm( emAS, as );
        for ar in AR do 
            ear := ImageElm( emAR, ar );
            if not ( eas*ear in P ) then
                mor := Make2DimensionalGroupMorphism( [ XM, XM, as, ar ] );
                ispre := ( not( mor = fail ) and IsPreXModMorphism( mor ) );
                if ispre then 
                    ismor := IsXModMorphism( mor );
                    if ismor then
                        num := num + 1;
                        Add( autogens, mor );
                        P := ClosureGroup( P, eas*ear );
                        Info( InfoXMod, 2, "size of P now ", Size(P) );
                    fi;
                fi;
            fi;
        od;
    od;
    genP := GeneratorsOfGroup( P );
    projDS := Projection( D, 1 );
    projDR := Projection( D, 2 );
    imsrc := List( genP, g -> ImageElm( projDS, g ) ); 
    imrng := List( genP, g -> ImageElm( projDR, g ) ); 
    projPS := GroupHomomorphismByImages( P, PAS, genP, imsrc );
    projPR := GroupHomomorphismByImages( P, PAR, genP, imrng );
    SetGeneratingAutomorphisms( XM, autogens );
    SetIsAutomorphismPermGroupOfXMod( P, true );
    ### 22/06/06, revised 30/07/18 ###
    ### these two functions could be changed as follows:  AS -> P 
    ### imePAS := List( genAS, a -> ImageElm( emS, ImageElm( a2pS, a ) ) ); 
    ### ePAS := GroupHomomorphismByImages( AS, D, genAS, imePAS ); 
    ### imePAR := List( genAR, a -> ImageElm( emR, ImageElm( a2pR, a ) ) ); 
    ### ePAR := GroupHomomorphismByImages( AR, D, genAR, imePAR ); 
    ePAS := GroupHomomorphismByImages( PAS, D, genPAS, emgenPAS ); 
    ePAR := GroupHomomorphismByImages( PAR, D, genPAR, emgenPAR ); 
    SetEmbedSourceAutos( P, ePAS );
    SetEmbedRangeAutos( P, ePAR );
    SetSourceProjection( P, projPS );
    SetRangeProjection( P, projPR );
    SetAutomorphismDomain( P, XM ); 
    return P;
end );

InstallMethod( AutomorphismPermGroup, "automorphism perm group of xmod", 
    true, [ IsXMod and IsNormalSubgroup2DimensionalGroup ], 0, 
function( XM )

    local S, genS, R, genR, autR, autS, AR, genAR, ngAR, AS, genAS, 
          ar, as, a2pR, PAR, genPAR, p2aR, a2pS, PAS, genPAS, p2aS, 
          restrict, D, genD, emS, emR, emgenPAS, emgenPAR, emAS, emAR, 
          infoD, P, genP, autogens, j, p2, p2i, newS, newR, oldS, oldR, 
          imsrc, imrng, projS, projR, filtS, ePAS, egenR, ePAR;

    Info( InfoXMod, 1, "using special AutomorphismPermGroup method" );
    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    R := Range( XM );
    genR := GeneratorsOfGroup( R );

    autR := AutomorphismGroup( R );
    autS := AutomorphismGroup( S );
    genAR := [ ];
    genAS := [ ]; 
    AR := Subgroup( autR, [ IdentityMapping( R ) ] );
    AS := Subgroup( autS, [ IdentityMapping( S ) ] );
    for ar in autR do
        as := GeneralRestrictedMapping( ar, S, S ); 
        if not ( fail in MappingGeneratorsImages(as)[2] ) then 
            if not ( ar in AR ) then 
                Add( genAR, ar );
                Add( genAS, as );
                AR := ClosureGroup( AR, ar );
                AS := ClosureGroup( AS, as );
            fi;
        fi;
    od;
    Info( InfoXMod, 2, " genAR = ", genAR );
    Info( InfoXMod, 2, " genAS = ", genAS );
    ngAR := Length( genAR );
    a2pR := IsomorphismPermGroup( AR );
    PAR := Image( a2pR );
    a2pR := a2pR * SmallerDegreePermutationRepresentation( PAR );
    PAR := ImagesSource( a2pR );
    genPAR := List( genAR, a -> ImageElm( a2pR, a ) );
    Info( InfoXMod, 2, "genPAR = ", genPAR );
    p2aR := GroupHomomorphismByImages( PAR, AR, genPAR, genAR );
    if ( p2aR = fail ) then 
        Error( "p2aR = fail" );
    else 
        SetAutoGroupIsomorphism( PAR, p2aR );
    fi; 
    a2pS := IsomorphismPermGroup( AS );
    PAS := Image( a2pS );
    a2pS := a2pS * SmallerDegreePermutationRepresentation( PAS );
    PAS := ImagesSource( a2pS );
    genPAS := List( genAS, a -> ImageElm( a2pS, a ) );
    Info( InfoXMod, 2, "genPAS = ", genPAS );
    p2aS := GroupHomomorphismByImages( PAS, AS, genPAS, genAS );
    if ( p2aS = fail ) then 
        Error( "p2aS = fail" );
    else 
        SetAutoGroupIsomorphism( PAS, p2aS );
    fi; 
    restrict := GroupHomomorphismByImages( PAR, PAS, genPAR, genPAS );

    D := DirectProduct( PAS, PAR );
    genD := GeneratorsOfGroup( D );
    if ( HasName( PAS ) and HasName( PAR ) ) then
        SetName( D, Concatenation( Name(PAS), "x", Name(PAR) ) );
    fi;
    emS := Embedding( D, 1 );
    emR := Embedding( D, 2 );
    ## this looks odd, but ngAR=ngAS
    filtS := Filtered( [1..ngAR], i -> not IsOne( genPAS[i] ) ); 
    Info( InfoXMod, 2,  "filtS = ", filtS ); 
    emgenPAS := List( genPAS, a -> ImageElm( emS, a ) );
    emgenPAR := List( genPAR, a -> ImageElm( emR, a ) );
    ##  (05/03/07)  allowed for the case that AS is trivial
    emAS := GroupHomomorphismByImages( AS,D,genAS{filtS},emgenPAS{filtS} );
    emAR := GroupHomomorphismByImages( AR,D,genAR,       emgenPAR );
    infoD := DirectProductInfo( D );

    genP := ListWithIdenticalEntries( ngAR, 0 );
    autogens := ListWithIdenticalEntries( ngAR, 0 );
    for j in [1..ngAR] do
        genP[j] := ImageElm( emAS, genAS[j] ) * ImageElm( emAR, genAR[j] );
        autogens[j] := XModMorphism( XM, XM, genAS[j], genAR[j] );
    od;
    P := Subgroup( D, genP );

    p2 := infoD.perms[2];
    p2i := p2^(-1);
    newS := infoD.news[1];  oldS := infoD.olds[1];
    newR := infoD.news[2];  oldR := infoD.olds[2];
    imsrc := List( genP, g -> 
                   MappingPermListList( oldS, List( newS, x->(x^g) ) ) );
    imrng := List( genP, g -> 
                   MappingPermListList( oldR, List( newR, x->(x^g)^p2i ) ) );
    projS := GroupHomomorphismByImages( P, PAS, genP, imsrc );
    projR := GroupHomomorphismByImages( P, PAR, genP, imrng ); 
    ePAS := GroupHomomorphismByImages( PAS,D,genPAS{filtS},genPAS{filtS} );
    egenR := List( genPAR, p -> p^p2 );
    ePAR := GroupHomomorphismByImages( PAR,D,genPAR,       egenR ); 
    SetGeneratingAutomorphisms( XM, autogens );
    SetIsAutomorphismPermGroupOfXMod( P, true );
    SetEmbedSourceAutos( P, ePAS );
    SetEmbedRangeAutos( P, ePAR );
    SetSourceProjection( P, projS );
    SetRangeProjection( P, projR );
    SetAutomorphismDomain( P, XM ); 
    return P;
end );

InstallMethod( AutomorphismPermGroup, 
    "automorphism perm group of a cat1-group", true, [ IsCat1Group ], 0, 
function( C1G )

    local G, genG, ngG, R, genR, ngR, 
          AG, genAG, a2pG, PAG, genPAG, p2aG,
          AR, genAR, a2pR, PAR, genPAR, p2aR, 
          D, genD, emG, emR, emgenPAG, emgenPAR, emAG, emAR, infoD, 
          P, num, autogens, ag, eag, ar, ear, mor, ispre, ismor, 
          genP, imsrc, imrng, projDG, projDR, projPG, projPR, ePAG, ePAR;

    Info( InfoXMod, 1, "using standard AutomorphismPermGroup method" );
    G := Source( C1G );
    genG := GeneratorsOfGroup( G );
    ngG := Length( genG );
    R := Range( C1G );
    genR := GeneratorsOfGroup( R );
    ngR := Length( genR );
    AG := AutomorphismGroup( G ); 
    genAG := GeneratorsOfGroup( AG );
    a2pG := IsomorphismPermGroup( AG );    ### check if smaller possible
    PAG := Image( a2pG );
    genPAG := List( genAG, a -> ImageElm( a2pG, a ) );
    p2aG := GroupHomomorphismByImages( PAG, AG, genPAG, genAG ); 
    if ( p2aG = fail ) then 
        Error( "p2aG = fail" );
    else 
        SetAutoGroupIsomorphism( PAG, p2aG );
    fi; 
    ## imbdy := Image( bdy );
    AR := AutomorphismGroup( R );
    genAR := GeneratorsOfGroup( AR );
    a2pR := IsomorphismPermGroup( AR );    ### ditto
    PAR := Image( a2pR );
    genPAR := List( genAR, a -> ImageElm( a2pR, a ) );
    p2aR := GroupHomomorphismByImages( PAR, AR, genPAR, genAR );
    if ( p2aR = fail ) then 
        Error( "p2aR = fail" );
    else 
        SetAutoGroupIsomorphism( PAR, p2aR );
    fi; 
    D := DirectProduct( PAG, PAR );
    genD := GeneratorsOfGroup( D );
    if ( HasName( PAG ) and HasName( PAR ) ) then
        SetName( D, Concatenation( Name(PAG), "x", Name(PAR) ) );
    fi;
    emG := Embedding( D, 1 );
    emR := Embedding( D, 2 );
    emgenPAG := List( genPAG, a -> ImageElm( emG, a ) );
    emgenPAR := List( genPAR, a -> ImageElm( emR, a ) ); 
    emAG := GroupHomomorphismByImages( AG, D, genAG, emgenPAG );  
    emAR := GroupHomomorphismByImages( AR, D, genAR, emgenPAR ); 
    infoD := DirectProductInfo( D );
    P := Subgroup( D, [ ] );
    num := 0;
    autogens := [ ];
    for ag in AG do 
        eag := ImageElm( emAG, ag );
        for ar in AR do 
            ear := ImageElm( emAR, ar );
            if not ( eag*ear in P ) then
                mor := Make2DimensionalGroupMorphism( [ C1G, C1G, ag, ar ] );
                ispre := ( not( mor = fail ) and 
                           IsPreCat1GroupMorphism( mor ) );
                if ispre then 
                    ismor := IsCat1GroupMorphism( mor );
                    if ismor then
                        num := num + 1;
                        Add( autogens, mor );
                        P := ClosureGroup( P, eag*ear );
                        Info( InfoXMod, 2, "size of P now ", Size(P) );
                    fi;
                fi;
            fi;
        od;
    od;
    genP := GeneratorsOfGroup( P );
    projDG := Projection( D, 1 );
    projDR := Projection( D, 2 );
    imsrc := List( genP, g -> ImageElm( projDG, g ) ); 
    imrng := List( genP, g -> ImageElm( projDR, g ) ); 
    projPG := GroupHomomorphismByImages( P, PAG, genP, imsrc );
    projPR := GroupHomomorphismByImages( P, PAR, genP, imrng );
    SetGeneratingAutomorphisms( C1G, autogens );
    SetIsAutomorphismPermGroupOfXMod( P, true );
    ePAG := GroupHomomorphismByImages( PAG, D, genPAG, emgenPAG ); 
    ePAR := GroupHomomorphismByImages( PAR, D, genPAR, emgenPAR ); 
    SetEmbedSourceAutos( P, ePAG );
    SetEmbedRangeAutos( P, ePAR );
    SetSourceProjection( P, projPG );
    SetRangeProjection( P, projPR );
    SetAutomorphismDomain( P, C1G ); 
    return P;
end );

#############################################################################
##
#M  PermAutomorphismAs2dGroupMorphism( <2d-gp>, <permaut> )
##
InstallMethod( PermAutomorphismAs2dGroupMorphism, 
    "xmod morphism coresponding to an element of the AutomorphismPermGroup",
    true, [ Is2DimensionalDomain, IsPerm ], 0, 
function( D, a )

    local APD, sp, rp, sa, ra, si, ri, smor, rmor, mor;

    APD := AutomorphismPermGroup( D );
    sp := SourceProjection( APD );
    sa := ImageElm( sp, a );
    si := AutoGroupIsomorphism( Range( sp ) );
    smor := ImageElm( si, sa );
    rp := RangeProjection( APD );
    ra := ImageElm( rp, a );
    ri := AutoGroupIsomorphism( Range( rp ) );
    rmor := ImageElm( ri, ra );
    if IsXMod( D ) then
        mor := XModMorphism( D, D, smor, rmor );
    elif IsCat1Group( D ) then
        mor := Cat1GroupMorphism( D, D, smor, rmor );
    else
        mor := fail;
    fi;
    return mor;
end );

#############################################################################
##
#M  ImageAutomorphismDerivation( <mor>, <chi> )
##
InstallMethod( ImageAutomorphismDerivation, 
    "image of derivation under action", true,
    [ IsXModMorphism, IsDerivation ], 0, 
function( mor, chi )

    local XM, R, stgR, imj, rho, imrho, sigma, invrho, rngR, k, r, rr, crr, chj;

    XM := Source( mor );
    sigma := SourceHom( mor );
    rho := RangeHom( mor );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    rngR := [ 1..Length( stgR ) ];
    imrho := List( stgR, r -> ImageElm( rho, r ) );
    invrho := GroupHomomorphismByImages( R, R, imrho, stgR );
    imj := 0 * rngR;
    for k in rngR do
        r := stgR[k];
        rr := ImageElm( invrho, r );
        crr := DerivationImage( chi, rr );
        imj[k] := ImageElm( sigma, crr );
    od;
    chj := DerivationByImages( XM, imj );
    return chj;
end );

#############################################################################
##
#M  WhiteheadXMod( <XM> )     (InnerSourceHom : Range(XM) -> Whitehead Group)
##
InstallMethod( WhiteheadXMod, "Whitehead crossed module", true, 
    [ IsXMod ], 0, 
function( XM )
    local  XP, iso; 
    iso := IsomorphismPerm2DimensionalGroup( XM ); 
    XP := Range( iso ); 
    return WhiteheadXMod( XP ); 
end ); 

InstallMethod( WhiteheadXMod, "Whitehead crossed module", true, 
    [ IsPermXMod ], 0, 
function( XM )
    local S, genS, W, posW, nposW, genW, iota, autS, genchi, j, chi, sigma, 
          ima, a, imact, act, WX, name;

    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    W := WhiteheadPermGroup( XM );
    posW := WhiteheadGroupGeneratorPositions( XM );
    nposW := Length( posW );
    genW := GeneratorsOfGroup( W );
    # determine the boundary map iota = PrincipalSourceHom
    iota := WhiteheadHomomorphism( XM ); 
    if ( InfoLevel( InfoXMod ) >= 2 ) then 
        Print( "iota in WhiteheadXMod:\n" ); 
        Display( iota ); 
    fi; 
    # now calculate the action homomorphism
    autS := AutomorphismGroup( S );
    if ( InfoLevel( InfoXMod ) >= 2 ) then 
        Print( "autS in WhiteheadXMod: ", StructureDescription(autS), "\n" ); 
    fi; 
    genchi := WhiteheadGroupGeneratingUpMappings( XM );
    if ( genchi = [ ] ) then
        imact := [ One( autS ) ];
    else 
        imact := [ 1..nposW ];
        for j in [1..nposW] do
            chi := genchi[j];
            sigma := SourceEndomorphism( chi );
            ima := List( genS, s -> ImageElm( sigma, s ) );
            a := GroupHomomorphismByImages( S, S, genS, ima );
            imact[j] := a;
        od;
    fi; 
    act := GroupHomomorphismByImages( W, autS, genW, imact );
    WX := XMod( iota, act );
    name := Name( XM );
    SetName( WX, Concatenation( "Whitehead", name ) );
    ## SetIsWhiteheadXMod( WX, true );
    return WX;
end );

############################################################################
##
#M  NorrieXMod( <XM> ) 
##
InstallMethod( NorrieXMod, "Norrie crossed module", true,
    [ IsXMod ], 0, 
function( XM )

    local S, R, genR, P, DX, genP, Prng, 
          AS, AR, a2pS, PAS, p2aS, a2pR, PAR, p2aR, 
          im, r, autr, psrc, emsrc, conjr, prng, emrng, bdy, ok,
          imact, p, projp, proja, ima, a, act, i, f, NX, name;

    Info( InfoXMod, 2, "now in NorrieXMod" ); 
    S := Source( XM );
    R := Range( XM );
    genR := GeneratorsOfGroup( R );
    P := AutomorphismPermGroup( XM );
    DX := Parent( P );
    genP := GeneratorsOfGroup( P );
    Prng := [ 1..Length( genP ) ];
    PAR := Image( RangeProjection( P ) ); 
    if HasAutoGroupIsomorphism( PAR ) then 
        p2aR := AutoGroupIsomorphism( PAR ); 
    elif ( HasParent( PAR ) and HasAutoGroupIsomorphism( Parent(PAR) ) ) then 
        p2aR := AutoGroupIsomorphism( Parent( PAR ) );
    else
        Error( "AutoGroupIsomorphism unavailable for PAR" );
    fi; 
    AR := Image( p2aR );
    a2pR := InverseGeneralMapping( p2aR );
    PAS := Image( SourceProjection( P ) ); 
    if HasAutoGroupIsomorphism( PAS ) then 
        p2aS := AutoGroupIsomorphism( PAS ); 
    elif ( HasParent( PAS ) and HasAutoGroupIsomorphism( Parent(PAS) ) ) then 
        p2aS := AutoGroupIsomorphism( Parent( PAS ) );
    else
        Error( "AutoGroupIsomorphism unavailable for PAS" );
    fi; 
    AS := Image( p2aS );
    a2pS := InverseGeneralMapping( p2aS );
    ######################################
    # determine the boundary map
    im := [ ]; 
    for r in genR do 
        autr := ImageElm( XModAction( XM ), r );
        psrc := ImageElm( a2pS, autr );
        emsrc := ImageElm( EmbedSourceAutos( P ), psrc );
        conjr := InnerAutomorphism( R, r );
        prng := ImageElm( a2pR, conjr );
        emrng := ImageElm( EmbedRangeAutos( P ), prng );
        Add( im, emrng * emsrc );  ### assumes direct product ###
    od;
    bdy := GroupHomomorphismByImages( R, P, genR, im );
    # determine the action
    imact := 0 * Prng;
    for i in Prng do
        p := genP[i];
        projp := ImageElm( RangeProjection( P ), p );
        proja := ImageElm( p2aR, projp );
        ima := List( genR, r -> ImageElm( proja, r ) );
        a := GroupHomomorphismByImages( R, R, genR, ima );
        imact[i] := a;
    od;
    act := GroupHomomorphismByImages( P, AR, genP, imact );
    for f in MappingGeneratorsImages( act )[2] do
        if ( f = IdentityMapping( R ) ) then
            f := InclusionMappingGroups( R, R );
        fi;
    od;
    ## create the crossed module
    NX := XMod( bdy, act );
    name := Name( XM );
    SetName( NX, Concatenation( "Norrie", name ) );
    return NX;
end );

############################################################################
##
#M  LueXMod( <XM> )
##
InstallMethod( LueXMod, "Lue crossed module", true,
    [ IsXMod ], 0, 
function( XM )

    local NX, Nbdy, Xbdy, Lbdy, P, genP, Prng, S, genS, AS, a2pS, PAS, 
          p2aS, imact, i, p, projp, proja, ima, a, act, f, LX, name;

    NX := NorrieXMod( XM );
    Nbdy := Boundary( NX );
    Xbdy := Boundary( XM );
    Lbdy := Xbdy * Nbdy;
    P := AutomorphismPermGroup( XM );
    genP := GeneratorsOfGroup( P );
    Prng := [ 1..Length( genP ) ];
    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    PAS := Image( SourceProjection( P ) ); 
    if HasAutoGroupIsomorphism( PAS ) then 
        p2aS := AutoGroupIsomorphism( PAS ); 
    elif ( HasParent( PAS ) and HasAutoGroupIsomorphism( Parent(PAS) ) ) then 
        p2aS := AutoGroupIsomorphism( Parent( PAS ) );
    else
        Error( "AutoGroupIsomorphism unavailable for PAS" );
    fi; 
    AS := Image( p2aS );
    a2pS := InverseGeneralMapping( p2aS );
    ######################################
    imact := 0 * Prng;
    for i in Prng  do
        p := genP[i];
        projp := ImageElm( SourceProjection( P ), p );
        proja := ImageElm( p2aS, projp );
        ima := List( genS, s -> ImageElm( proja, s ) );
        a := GroupHomomorphismByImages( S, S, genS, ima );
        imact[i] := a;
    od;
    act := GroupHomomorphismByImages( P, AS, genP, imact );
    for f in MappingGeneratorsImages( act )[2] do
        if ( f = IdentityMapping( S ) ) then
            f := InclusionMappingGroups( S, S );
        fi;
    od;
    LX := XMod( Lbdy, act );
    name := Name( XM );
    SetName( LX, Concatenation( "Lue", name ) );
    return LX;
end );

#############################################################################
##
#M  Actor( <obj> ) 
#M  InnerActor( <obj> ) 
##
InstallGlobalFunction( Actor, function( obj )
    if HasIsXMod( obj ) and IsXMod( obj ) then
        return ActorXMod( obj );
    elif ( HasIsCat1Group( obj ) and IsCat1Group( obj ) ) then 
        return ActorCat1Group( obj );
    else
        return fail;
    fi;
end );

InstallGlobalFunction( InnerActor, function( obj )
    if HasIsXMod( obj ) and IsXMod( obj ) then
        return InnerActorXMod( obj );
    else
        return fail;
    fi;
end );

#############################################################################
##
#M  ActorXMod( <XM> ) 
##
InstallMethod( ActorXMod, "actor crossed module", true, [ IsXMod ], 0, 
function( XM )
    local XP, iso; 
    iso := IsomorphismPerm2DimensionalGroup( XM ); 
    XP := Range( iso ); 
    return ActorXMod( XP ); 
end );

InstallMethod( ActorXMod, "actor crossed module", true, [ IsPermXMod ], 0, 
function( XM )

    local D, L, W, RW, isoW, eRW, P, genP, genpos, ngW, genRW, genW, invW,
          imdelta, S, R, AS, AR, PAS, p2aS, a2pS, PAR, p2aR, a2pR, emsrc,
          emrng, i, j, k, mor, imsrc, imrng, delta, GA, nGA, imact, rho,
          invrho, impos, chi, chj, imgen, phi, id, aut, act, ActX, name;

    D := RegularDerivations( XM );
    L := ImagesList( D );
    W := WhiteheadPermGroup( XM );
    RW := WhiteheadRegularGroup( XM );
    isoW := WhiteheadGroupIsomorphism( XM );
    eRW := Elements( RW );
    P := AutomorphismPermGroup( XM );
    genP := GeneratorsOfGroup( P );
    genpos := WhiteheadGroupGeneratorPositions( XM );
    ngW := Length( genpos );
    # determine the boundary map
    genRW := List( genpos, i -> eRW[i] );
    genW := List( genRW, g -> ImageElm( isoW, g ) );
    invW := List( genW, g -> g^-1 );
    imdelta := ListWithIdenticalEntries( ngW, 0 );
    S := Source( XM );
    R := Range( XM );
    PAR := Image( RangeProjection( P ) ); 
    if HasAutoGroupIsomorphism( PAR ) then 
        p2aR := AutoGroupIsomorphism( PAR ); 
    elif ( HasParent( PAR ) and HasAutoGroupIsomorphism( Parent(PAR) ) ) then 
        p2aR := AutoGroupIsomorphism( Parent( PAR ) );
    else
        Error( "AutoGroupIsomorphism unavailable for PAR" );
    fi; 
    AR := Image( p2aR );
    a2pR := InverseGeneralMapping( p2aR );
    PAS := Image( SourceProjection( P ) ); 
    if HasAutoGroupIsomorphism( PAS ) then 
        p2aS := AutoGroupIsomorphism( PAS ); 
    elif ( HasParent( PAS ) and HasAutoGroupIsomorphism( Parent(PAS) ) ) then 
        p2aS := AutoGroupIsomorphism( Parent( PAS ) );
    else
        Error( "AutoGroupIsomorphism unavailable for PAS" );
    fi; 
    AS := Image( p2aS );
    a2pS := InverseGeneralMapping( p2aS );
    ######################################
    emsrc := EmbedSourceAutos( P );
    emrng := EmbedRangeAutos( P );
    for i in [1..ngW] do
        j := genpos[i];
        chj := DerivationByImages( XM, L[j] );
        mor := Object2dEndomorphism( chj );
        imsrc := ImageElm( emsrc, ImageElm( a2pS, SourceHom( mor ) ) );
        imrng := ImageElm( emrng, ImageElm( a2pR, RangeHom( mor ) ) );
        imdelta[i] := imsrc * imrng;
    od;
    delta := GroupHomomorphismByImages( W, P, genW, imdelta );
    Info( InfoXMod, 3, "delta: ", MappingGeneratorsImages( delta ) );

    # determine the action
    GA := GeneratingAutomorphisms( XM );
    nGA := Length( GA );
    imact := ListWithIdenticalEntries( nGA, 0 );
    for k in [1..nGA] do
        mor := GA[k];
        rho := RangeHom( mor );
        invrho := rho^(-1);
        impos := ListWithIdenticalEntries( ngW, 0);
        for i in [1..ngW] do
            j := genpos[i];
            chi := DerivationByImages( XM, L[j] );
            chj := ImageAutomorphismDerivation( mor, chi );
            impos[i] := Position( L, UpGeneratorImages( chj ) );
        od;
        imgen := List( impos, i -> Image( isoW, eRW[i] ) );
        phi := GroupHomomorphismByImages( W, W, genW, imgen );
        imact[k] := phi;
    od;
    id := InclusionMappingGroups( W, W );
    aut := Group( imact, id );
    SetName( aut, "Aut(W)" );
    act := GroupHomomorphismByImages( P, aut, genP, imact );
    ActX := XMod( delta, act );
    name := Name( XM );
    SetName( ActX, Concatenation( "Actor", name ) );
    return ActX;
end );

#############################################################################
##
#M  ActorCat1Group( <C> )
#M  InnerActorCat1Group( <C> )
##
#? direct implementations might be better!
##
InstallMethod( ActorCat1Group, "actor cat1-group", true, [ IsCat1Group ], 0, 
function( C )

    local XC, AXC;

    XC := XModOfCat1Group( C );
    AXC := ActorXMod ( XC );
    return Cat1GroupOfXMod( AXC ); 
end );

InstallMethod( InnerActorCat1Group, "inner actor cat1-group", true,
    [ IsCat1Group ], 0, 
function( C )

    local XC, IAXC;

    XC := XModOfCat1Group( C );
    IAXC := InnerActorXMod ( XC );
    return Cat1GroupOfXMod( IAXC ); 
end );

#############################################################################
##
#M  InnerMorphism( <XM> )
##
InstallMethod( InnerMorphism, "inner morphism of xmod", true, [ IsXMod ], 0, 
function( XM )
    local XP, iso; 
    iso := IsomorphismPerm2DimensionalGroup( XM ); 
    XP := Range( iso ); 
    return InnerMorphism( XP ); 
end );

InstallMethod( InnerMorphism, "inner morphism of xmod", true,
    [ IsPermXMod ], 0, 
function( XM )

    local WX, NX, ActX, mor;

    WX := WhiteheadXMod( XM );
    NX := NorrieXMod( XM );
    ActX := ActorXMod( XM );
    mor := XModMorphismByGroupHomomorphisms(XM,ActX,Boundary(WX),Boundary(NX));
    return mor;
end );

#############################################################################
##
#M  XModCentre( <XM> )
##
InstallMethod( XModCentre, "centre of an xmod", true, [ IsXMod ], 0, 
function( XM )
    return Kernel( InnerMorphism( XM ) );
end );

#############################################################################
##
#M  InnerActorXMod( <XM> )
##
InstallMethod( InnerActorXMod, "inner actor crossed module", true, 
    [ IsXMod ], 0, 
function( XM )
    local XP, iso; 
    iso := IsomorphismPerm2DimensionalGroup( XM ); 
    XP := Range( iso ); 
    return InnerActorXMod( XP ); 
end );

InstallMethod( InnerActorXMod, "inner actor crossed module", true,
    [ IsPermXMod ], 0, 
function( XM )

    local InnX, mor, name, ActX;
    
    ActX := ActorXMod( XM );
    mor := InnerMorphism( XM );
    InnX := ImagesSource( mor );
    if ( InnX = ActX ) then
        InnX := ActX;
    else
        name := Name( XM );
        SetName( InnX, Concatenation( "InnerActor", name ) );
    fi;
    return InnX;
end );
