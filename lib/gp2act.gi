##############################################################################
##
#W  gp2act.gi                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##
##  This file implements methods for actor crossed squares of crossed modules. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  AutomorphismPermGroup( <XM> )  subgroup of Aut(Source(XM))xAut(Range(XM))
##
InstallMethod( AutomorphismPermGroup, "automorphism perm group of xmod", 
    true, [ IsXMod ], 0, 
function( XM )

    local S, genS, ngS, R, genR, ngR, bdy, act, ker, imbdy, 
          AS, genAS, a2pS, PAS, genPAS, p2aS,
          AR, genAR, a2pR, PAR, genPAR, p2aR, 
          D, genD, emS, emR, emgenPAS, emgenPAR, emAS, emAR, infoD, 
          P, num, autogens, as, eas, ar, ear, mor, ispre, ismor, 
          genP, p2, p2i, newS, oldS, newR, oldR, imsrc, imrng, 
          projS, projR, ePAS, egenR, ePAR;

    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    ngS := Length( genS );
    R := Range( XM );
    genR := GeneratorsOfGroup( R );
    ngR := Length( genR );
    bdy := Boundary( XM );
    act := XModAction( XM );

    ker := Kernel( bdy );
    AS := AutomorphismGroup( S ); 
    genAS := GeneratorsOfGroup( AS );
    a2pS := IsomorphismPermGroup( AS );    ### check if smaller possible
    PAS := Image( a2pS );
    genPAS := List( genAS, a -> Image( a2pS, a ) );
    p2aS := GroupHomomorphismByImages( PAS, AS, genPAS, genAS );
    SetAutoGroupIsomorphism( PAS, p2aS );

    imbdy := Image( bdy );
    AR := AutomorphismGroup( R );
    genAR := GeneratorsOfGroup( AR );
    a2pR := IsomorphismPermGroup( AR );    ### ditto
    PAR := Image( a2pR );
    genPAR := List( genAR, a -> Image( a2pR, a ) );
    p2aR := GroupHomomorphismByImages( PAR, AR, genPAR, genAR );
    SetAutoGroupIsomorphism( PAR, p2aR );

    D := DirectProduct( PAS, PAR );
    genD := GeneratorsOfGroup( D );
    if ( HasName( PAS ) and HasName( PAR ) ) then
        SetName( D, Concatenation( Name(PAS), "x", Name(PAR) ) );
    fi;
    emS := Embedding( D, 1 );
    emR := Embedding( D, 2 );
    emgenPAS := List( genPAS, a -> Image( emS, a ) );
    emgenPAR := List( genPAR, a -> Image( emR, a ) ); 
    emAS := GroupHomomorphismByImages( AS, D, genAS, emgenPAS );  
    emAR := GroupHomomorphismByImages( AR, D, genAR, emgenPAR ); 
    infoD := DirectProductInfo( D );
    P := Subgroup( D, [ ] );
    num := 0;
    autogens := [ ];
    for as in AS do 
        eas := Image( emAS, as );
        for ar in AR do 
            ear := Image( emAR, ar );
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
    ### 21/06/06 ### genPAS := GeneratorsOfGroup( PAS );
    ePAS := GroupHomomorphismByImages( PAS, D, genPAS, genPAS );
    ### 21/06/06 ### genPAR := GeneratorsOfGroup( PAR );
    egenR := List( genPAR, p -> p^p2 );
    ePAR := GroupHomomorphismByImages( PAR, D, genPAR, egenR );
    SetGeneratingAutomorphisms( XM, autogens );
    SetIsAutomorphismPermGroupOfXMod( P, true );
    ### 22/06/06 ###
    ### these two functions would better be  AS -> P
    SetEmbedSourceAutos( P, ePAS );
    SetEmbedRangeAutos( P, ePAR );
    SetSourceProjection( P, projS );
    SetRangeProjection( P, projR );
    SetAutomorphismDomain( P, XM ); 
    return P;
end );

#######  special version for XModByNormalSubgroup, 23/06/06  #######

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
    a2pR := IsomorphismSmallPermGroup( AR );
    PAR := Image( a2pR, AR );
    genPAR := List( genAR, a -> Image( a2pR, a ) );
    Info( InfoXMod, 2, "genPAR = ", genPAR );
    p2aR := GroupHomomorphismByImages( PAR, AR, genPAR, genAR );
    SetAutoGroupIsomorphism( PAR, p2aR );
    a2pS := IsomorphismSmallPermGroup( AS );
    PAS := Image( a2pS, AS );
    genPAS := List( genAS, a -> Image( a2pS, a ) );
    Info( InfoXMod, 2, "genPAS = ", genPAS );
    p2aS := GroupHomomorphismByImages( PAS, AS, genPAS, genAS );
    SetAutoGroupIsomorphism( PAS, p2aS );
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
    emgenPAS := List( genPAS, a -> Image( emS, a ) );
    emgenPAR := List( genPAR, a -> Image( emR, a ) );
    ##  (05/03/07)  allowed for the case that AS is trivial
    emAS := GroupHomomorphismByImages( AS,D,genAS{filtS},emgenPAS{filtS} );
    emAR := GroupHomomorphismByImages( AR,D,genAR,       emgenPAR );
    infoD := DirectProductInfo( D );

    genP := ListWithIdenticalEntries( ngAR, 0 );
    autogens := ListWithIdenticalEntries( ngAR, 0 );
    for j in [1..ngAR] do
        genP[j] := Image( emAS, genAS[j] ) * Image( emAR, genAR[j] );
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

#############################################################################
##
#M  PermAutomorphismAsXModMorphism( <xmod>, <permaut> )
##
InstallMethod( PermAutomorphismAsXModMorphism, 
    "xmod morphism coresponding to an element of the AutomorphismPermGroup",
    true, [ IsXMod, IsPerm ], 0, 
function( XM, a )

    local APXM, sp, rp, sa, ra, si, ri, smor, rmor, mor;

    APXM := AutomorphismPermGroup( XM );
    sp := SourceProjection( APXM );
    sa := Image( sp, a );
    si := AutoGroupIsomorphism( Range( sp ) );
    smor := Image( si, sa );
    rp := RangeProjection( APXM );
    ra := Image( rp, a );
    ri := AutoGroupIsomorphism( Range( rp ) );
    rmor := Image( ri, ra );
    mor := XModMorphism( XM, XM, smor, rmor ); 
    return mor;
end );

#############################################################################
##
#M  ImageAutomorphismDerivation( <mor>, <chi> )
##
InstallMethod( ImageAutomorphismDerivation, "image of derivation under action",
    true, [ IsXModMorphism, IsDerivation ], 0, 
function( mor, chi )

    local XM, R, stgR, imj, rho, imrho, sigma, invrho, rngR, k, r, rr, crr, chj;

    XM := Source( mor );
    sigma := SourceHom( mor );
    rho := RangeHom( mor );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    rngR := [ 1..Length( stgR ) ];
    imrho := List( stgR, r -> Image( rho, r ) );
    invrho := GroupHomomorphismByImages( R, R, imrho, stgR );
    imj := 0 * rngR;
    for k in rngR do
        r := stgR[k];
        rr := Image( invrho, r );
        crr := DerivationImage( chi, rr );
        imj[k] := Image( sigma, crr );
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
    local S, genS, reg, imreg, W, WT, posW, nposW, genW, imiota,
          s, chi, poschi, iota, autS, genchi, j, sigma, ima, a, 
          imact, act, WX, name;

    S := Source( XM );
    genS := GeneratorsOfGroup( S );
    reg := RegularDerivations( XM );
    imreg := ImagesList( reg );
    W := WhiteheadPermGroup( XM );
    WT := WhiteheadGroupTable( XM );
    posW := WhiteheadGroupGeneratorPositions( XM );
    nposW := Length( posW );
    genW := GeneratorsOfGroup( W );
    # determine the boundary map iota = PrincipalSourceHom
    imiota := [ ];
    for s in genS do
        chi := PrincipalDerivation( XM, s );
        poschi := Position( imreg, UpGeneratorImages( chi ) );
        Add( imiota, PermList( WT[poschi] ) );
    od;
    iota := GroupHomomorphismByImages( S, W, genS, imiota );
    ##  ????? should this be a general mapping ????????????????????
    if not IsGroupHomomorphism( iota ) then
        Error( "Whitehead boundary fails to be a homomorphism" );
    fi;
    # now calculate the action homomorphism
    autS := AutomorphismGroup( S );
    genchi := WhiteheadGroupGeneratingDerivations( XM );
    ##  (05/03/07)  allow for the case that W is trivial 
    if ( genchi = [ ] ) then
        imact := [ One( autS ) ];
    else 
        imact := [ 1..nposW ];
        for j in [1..nposW] do
            chi := genchi[j];
            sigma := SourceEndomorphism( chi );
            ima := List( genS, s -> Image( sigma, s ) );
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

#############################################################################
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
    ########## 23/06/06 revision ########
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
        autr := Image( XModAction( XM ), r );
        psrc := Image( a2pS, autr );
        emsrc := Image( EmbedSourceAutos( P ), psrc );
        conjr := InnerAutomorphism( R, r );
        prng := Image( a2pR, conjr );
        emrng := Image( EmbedRangeAutos( P ), prng );
        Add( im, emrng * emsrc );  ### assumes direct product ###
    od;
    bdy := GroupHomomorphismByImages( R, P, genR, im );
    # determine the action
    imact := 0 * Prng;
    for i in Prng do
        p := genP[i];
        projp := Image( RangeProjection( P ), p );
        proja := Image( p2aR, projp );
        ima := List( genR, r -> Image( proja, r ) );
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

#############################################################################
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
    ########## 23/06/06 revision ##########
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
        projp := Image( SourceProjection( P ), p );
        proja := Image( p2aS, projp );
        ima := List( genS, s -> Image( proja, s ) );
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
#M  ActorXMod( <XM> ) 
##
InstallMethod( ActorXMod, "actor crossed module", true, [ IsXMod ], 0, 
function( XM )

    local D, L, W, eW, P, genP, genpos, ngW, genW, invW, imdelta, 
          S, R, AS, AR, PAS, p2aS, a2pS, PAR, p2aR, a2pR, emsrc, emrng, 
          i, j, k, mor, imsrc, imrng, delta, GA, nGA, imact, rho, invrho, 
          impos, chi, chj, imgen, phi, id, aut, act, ActX, name;

    if not IsPermXMod( XM ) then 
        Error( "ActorXMod only implemented for permutation xmods" ); 
    fi;
    D := RegularDerivations( XM );
    L := ImagesList( D );
    W := WhiteheadPermGroup( XM );
    eW := Elements( W );
    P := AutomorphismPermGroup( XM );
    genP := GeneratorsOfGroup( P );
    genpos := WhiteheadGroupGeneratorPositions( XM );
    ngW := Length( genpos );
    # determine the boundary map
    genW := List( genpos, i -> eW[i] );
    invW := List( genW, g -> g^-1 );
    imdelta := ListWithIdenticalEntries( ngW, 0 );
    S := Source( XM );
    R := Range( XM );
    ########## 23/06/06 revision ##########
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
        imsrc := Image( emsrc, Image( a2pS, SourceHom( mor ) ) );
        imrng := Image( emrng, Image( a2pR, RangeHom( mor ) ) );
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
        imgen := List( impos, i -> eW[i] );
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
##
InstallMethod( ActorCat1Group, "actor cat1-group", true, [ IsCat1Group ], 0, 
function( C )
    return 0;
end );

#############################################################################
##
#M  InnerMorphism( <XM> )
##
InstallMethod( InnerMorphism, "inner morphism of xmod", true,
    [ IsPermXMod ], 0, 
function( XM )

    local WX, NX, ActX, mor;

    WX := WhiteheadXMod( XM );
    NX := NorrieXMod( XM );
    ActX := ActorXMod( XM );
    mor := XModMorphismByHoms( XM, ActX, Boundary(WX), Boundary(NX) );
    return mor;
end );

#############################################################################
##
#M  XModCentre( <XM> )
##
#?  InstallOtherMethod( Centre, "centre of an xmod", true, [ IsPermXMod ], 0, 
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


#############################################################################
##
#E  gp2act.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
