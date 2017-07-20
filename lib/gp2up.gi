##############################################################################
##
#W  gp2up.gi                    GAP4 package `XMod'              Chris Wensley
#W                                                                 & Murat Alp
##
##  This file contains implementations of UpMappings, Derivations and Sections
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  IsUp2DimensionalMapping  (19/07/11) : no longer needed now a category
##
##  InstallMethod( IsUp2DimensionalMapping, 
##  "generic method for derivations and sections", true, [IsGeneralMapping], 0,
##function( map )
##    return ( HasObject2d( map ) and HasUpGeneratorImages( map ) );
##end );

##############################################################################
##
#M  Up2DimensionalMappingObj( <obj>, <ims> ) . . . . . . . . . make up-mapping
##
InstallMethod( Up2DimensionalMappingObj, "for 2d-object and images", true,
    [ Is2DimensionalDomain, IsHomogeneousList ], 0,
function( obj, ims )

    local filter, fam, map, genR, stgR, invR, ok, src, rng, aut, name, hom;

    fam := FamilyObj( [ obj, ims ] );
    filter := IsUp2DimensionalMappingRep;
    src := Source( obj );
    rng := Range( obj );
    map := rec();
    ObjectifyWithAttributes( map, NewType( fam, filter ),
      Object2d, obj,
      UpGeneratorImages, ims,
      IsUp2DimensionalMapping, true );
    if IsPreXMod( obj ) then
        ok := IsDerivation( map );
        if ( ok and IsXMod( obj ) ) then
            SetIsDerivation( map, true );
        fi;
        stgR := StrongGeneratorsStabChain( StabChain( rng ) );
        invR := List( stgR, r -> r^(-1) );
    elif IsPreCat1Group( obj ) then
        genR := GeneratorsOfGroup( rng );
        hom := GroupHomomorphismByImages( rng, src, genR, ims );
        if RespectsMultiplication( hom ) then
            ####  SetIsGroupHomomorphism( map, true );
        else
            Error( "proposed section not a group homomorphism" );
        fi;
        ok := IsSection( map );
        if ( ok and IsCat1Group( obj ) ) then
            SetIsSection( map, true );
        fi;
    else
        Error( "2dobject neither an xmod nor a cat1-group" );
    fi;
    return map;
end );

##############################################################################
##
#M  Source( <map> )  . . . . . . . . . . . . . . . . . . source for up-mapping
#M  Range( <map> )  . . . . . . . . . . . . . . . . . . . range for up-mapping
##
InstallOtherMethod( Source, "generic method for an up-mapping",
    [ IsUp2DimensionalMapping ], 0,
function ( map )
    return Source( Object2d( map ) );
end );

InstallOtherMethod( Range, "generic method for an up-mapping",
    [ IsUp2DimensionalMapping ], 0,
function ( map )
    return Range( Object2d( map ) );
end );

##############################################################################
##
#M  \=( <u1>, <u2> )  . . . . . . . test if two derivations|sections are equal
##
InstallMethod( \=,
    "generic method for two upmappings",
    IsIdenticalObj, [ IsUp2DimensionalMapping, IsUp2DimensionalMapping ], 0,
function ( u1, u2 )
    return ( ( Object2d(u1) = Object2d(u2) )
         and ( UpGeneratorImages(u1) = UpGeneratorImages(u2) ) );
end );

#############################################################################
##
#M  IsDerivation
##
InstallMethod( IsDerivation, "generic method for derivation of pre-xmod",
    true, [ IsUp2DimensionalMapping ], 0,
function( chi )

    local im, inv, XM, bdy, R, stgR, invR, oneR, ord, S, oneS,
          genrng, rho, imrho, ok, g, i, j, r, s, t, u, v, w, aut, act, pair,
          iso, fp, pres, T, numrels, lenrels, rels, rel, len, triv;

    XM := Object2d( chi );
    im := UpGeneratorImages( chi );
    S := Source( XM );
    oneS := One( S );
    R := Range( XM );
    oneR := One( R );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    iso := IsomorphismFpGroupByGenerators( R, stgR );
    fp := Range( iso );
    Info( InfoXMod, 3, "iso to fp group : ", iso );
    invR := List( stgR, r -> r^(-1) );
    triv := List( stgR, r -> oneS );
    if ( UpGeneratorImages( chi ) = triv ) then
        return true;
    fi;
    genrng := [ 1..Length( stgR ) ];
    bdy := Boundary( XM );
    act := XModAction( XM );

    # calculate  chi(r^-1)  for each generator  r
    inv := 0 * genrng;
    for j in genrng do
        r := stgR[j];
        s := im[j];
        aut := Image( act, r^(-1) );
        inv[j] := Image( aut, s )^(-1);
    od;
    Info( InfoXMod, 3, "  Images = ", im, ",  inverses = ", inv );

    pres := PresentationFpGroup( fp );
    T := pres!.tietze;
    numrels := T[TZ_NUMRELS];
    rels := T[TZ_RELATORS];
    lenrels := T[TZ_LENGTHS];
    w := oneS;
    v := oneR;
    for i in [1..numrels] do
        rel := rels[i];
        len := lenrels[i];
        for j in Reversed( [1..len] ) do
            g := rel[j];
            if ( g > 0 ) then
                r := stgR[g];
                s := im[g];
            else
                r := invR[-g];
                s := inv[-g];
            fi;
            aut := Image( act, v );
            u := Image( aut, s );
            w := u * w;
            v := r * v;
        od;
        if ( w <> oneS ) then
            Info( InfoXMod, 3, "chi(rel) <> One(S)  when rel = ", rel );
            return false;
        fi;
    od;
    SetIsDerivation( chi, true );
    return true;
end );

##############################################################################
##
#M  IsSection                    tests the section axioms for a pre-cat1-group
##
InstallMethod( IsSection, "generic method for section of cat1-group",
    true, [ IsUp2DimensionalMapping ], 0,
function( xi )

    local hom, C, Crng, idrng, ok, reg;

    hom := UpHomomorphism( xi ); 
    if not IsGroupHomomorphism( hom ) then
        return false;
    fi;
    C := Object2d( xi );
    Crng := Range( C );
    if not ( ( Source(hom) = Crng ) and ( Range(hom) = Source(C) ) ) then
        Print( "<hom> not a homomorphism: Range( C ) -> Source( C )\n" );
        return false;
    fi;
    idrng := InclusionMappingGroups( Crng, Crng );   
    ok := ( hom * TailMap(C) = idrng );
    reg := IsInjective( hom * HeadMap(C) );
    ## SetIsRegularSection( xi, ok and reg );
    SetIsSection( xi, true );
    return ok;
end );

##############################################################################
##
#M  SourceEndomorphism                                        for an upmapping
##
InstallMethod( SourceEndomorphism, "method for an upmapping", true,
    [ IsUp2DimensionalMapping ], 0,
function( u )

    local XM, S, bdy, genS, ngenS, imsigma, i, s, r, s2, sigma;

    if IsDerivation( u ) then
        XM := Object2d( u );
        S := Source( XM );
        bdy := Boundary( XM );
        genS := GeneratorsOfGroup( S );
        ngenS := Length( genS );
        imsigma := [ 1..ngenS ];
        for i in [1..ngenS] do
            s := genS[i];
            r := Image( bdy, s );
            s2 := DerivationImage( u, r );
            imsigma[i] := s * s2;
        od;
        sigma := GroupHomomorphismByImages( S, S, genS, imsigma );
        return sigma;
    else
        Error( "SourceEndomorphism for sections not yet implemented" );
    fi;
end );

##############################################################################
##
#M  RangeEndomorphism                                         for an upmapping
##
InstallMethod( RangeEndomorphism, "method for an upmapping", true,
    [ IsUp2DimensionalMapping ], 0,
function( u )

    local XM, R, bdy, genR, ngenR, imrho, i, s, r, r2, rho;

    if IsDerivation( u ) then
        XM := Object2d( u );
        R := Range( XM );
        bdy := Boundary( XM );
        genR := GeneratorsOfGroup( R );
        ngenR := Length( genR );
        imrho := [ 1..ngenR ];
        for i in [1..ngenR] do
            r := genR[i];
            s := DerivationImage( u, r );
            r2 := Image( bdy, s );
            imrho[i] := r * r2;
        od;
        rho := GroupHomomorphismByImages( R, R, genR, imrho );
        return rho;
    else
        Error( "RangeEndomorphism for sections not yet implemented" );
    fi;
end );

#############################################################################
##
#M  Object2dEndomorphism                                     for an upmapping
##
InstallMethod( Object2dEndomorphism, "method for an upmapping", true,
    [ IsUp2DimensionalMapping ], 0,
function( u )

    local obj, sigma, rho, mor;

    if IsDerivation( u ) then
        obj := Object2d( u );
        sigma := SourceEndomorphism( u );
        rho := RangeEndomorphism( u );
        mor := XModMorphismByHoms( obj, obj, sigma, rho );
        return mor;
    else
        Error( "Object2dEndomorphism for sections not yet implemented" );
    fi;
end );

##############################################################################
##                               Derivations                                ##
##############################################################################

##############################################################################
##
#M  DerivationByImages                                     sets up the mapping
##
InstallMethod( DerivationByImages, "method for an xmod", true,
    [ IsXMod, IsHomogeneousList ], 0,
function( XM, im )

    local nargs, usage, isder, chi, R, S, stgR, ngR, ok;

    S := Source( XM );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngR := Length( stgR );
    if not ( IsList( im ) and ( Length( im ) = ngR )
                   and ForAll( im, x -> ( x in S ) ) ) then
        Error( "<im> must be a list of |stgR| elements in S" );
    fi;
    chi := Up2DimensionalMappingObj( XM, im );
    ok := IsDerivation( chi );
    if not ok then 
        return fail;
    else
        return chi;
    fi;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj . . . for a derivation  
##
InstallMethod( String, "for a derivation", true, [ IsDerivation ], 0, 
function( chi ) 

    local obj;

    obj := Object2d( chi );
    return( STRINGIFY( "derivation by images: ", String( Range(obj) ), 
                       " -> ", String( Source(obj) ) ) ); 
end );

InstallMethod( ViewString, "for a derivation", true, [ IsDerivation ], 
    0, String ); 

InstallMethod( PrintString, "for a derivation", true, [ IsDerivation ], 
    0, String ); 

InstallMethod( PrintObj, "method for a derivation", true, [ IsDerivation ], 0,
function( chi )

    local obj, iso, R, stgR;

    obj := Object2d( chi );
    R := Range( obj );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    iso := IsomorphismFpGroupByGenerators( R, stgR );
    Print( "DerivationByImages( ", R, ", ", Source(obj), ", ",
         stgR, ", ", UpGeneratorImages(chi), " )" );
end );

InstallMethod( ViewObj, "method for a derivation", true, [ IsDerivation ], 
    0, PrintObj );

##############################################################################
##
#M  DerivationImage                  image of  r \in R  by the derivation  chi
##
InstallMethod( DerivationImage, "method for a derivation and a group element",
    true, [ IsDerivation, IsObject ], 0,
function( chi, r )

    local XM, S, R, stgR, imchi, elR, elS, ngR, genrng, j, g, s, u, v,
          rpos, spos, ord, P, a, act;

    XM := Object2d( chi );
    S := Source( XM );
    R := Range( XM );
    if not ( r in R ) then
        Error( "second parameter must be an element of chi.source" );
    fi;
    if ( r = One( R ) ) then
        return One( S );
    fi;
    elR := Elements( R );
    elS := Elements( S );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    imchi := UpGeneratorImages( chi );
    rpos := Position( elR, r );
    if HasUpImagePositions( chi ) then
        spos := UpImagePositions( chi )[ rpos ];
        return elS[ spos ];
    fi;
    ord := GenerationOrder( R );
    P := GenerationPairs( R );
    j := P[rpos][1];
    g := P[rpos][2];
    if ( j = 1 ) then   # r is a generator
        return imchi[g];
    fi;
    act := XModAction( XM );
    u := imchi[g];
    v := stgR[g];
    while ( j > 1 ) do
        g := P[j][2];
        j := P[j][1];
        s := imchi[g];
        a := Image( act, v );
        u := Image( a, s ) * u;
        v := stgR[g] * v;
    od;
    return u;
end );

##############################################################################
##
#M  UpImagePositions                  returns list of positions of chi(r) in S
##
InstallMethod( UpImagePositions, "method for a derivation", true,
    [ IsDerivation ], 0,
function( chi )

    local XM, R, S, elR, stgR, ngR, ord, P, oR, elS, oneS,
          i, j, k, x, y, L, ok, imchi, act, agen, a;

    XM := Object2d( chi );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngR := Length( stgR );
    imchi := UpGeneratorImages( chi );
    act := XModAction( XM );
    agen := List( stgR, r -> Image( act, r ) );
    S := Source( XM );
    elR := Elements( R );
    elS := Elements( S );
    oneS := One( S );
    ord := GenerationOrder( R );
    P := GenerationPairs( R );
    if ( InfoLevel( InfoXMod ) > 2 ) then
        ok := CheckGenerationPairs( R );
        Print( "checking generation pairs: ", ok, "\n" );
    fi;
    oR := Size( R );
    L := 0 * [1..oR];
    L[1] := Position( elS, oneS );
    for k in [2..oR] do
        i := ord[k];
        j := P[i][2];
        x := elS[ L[ P[i][1] ] ];
        a := agen[j];
        y := Image( a, x ) * imchi[j]; 
        L[i] := Position( elS, y );
    od;
    return L;
end );

##############################################################################
##
#M  PrincipalDerivation            derivation determined by a choice of s in S
##
InstallMethod( PrincipalDerivation, "method for xmod and source element", 
    true, [ IsXMod, IsObject ], 0,
function( XM, s )

    local act, S, R, stgR, q, r, iota, imiota, ngen, a, j;

    S := Source( XM );
    R := Range( XM );
    act := XModAction( XM );
    if not ( s in S ) then
        Error( "Second parameter must be an element of the source of X" );
    fi;
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngen := Length( stgR );
    imiota := 0 * [1..ngen];
    for j in [1..ngen] do
        r := stgR[j];
        a := Image( act, r );
        q := Image( a, s^(-1) );
        imiota[j] := q * s;
    ##  Print( "[r,a,q,q*s] = ", [r,a,q,q*s], "\n" );
    ##  now using  r -> (s^-1)^r * s,  rather than  s^r * s^-1  ##
    od;
    iota := DerivationByImages( XM, imiota );
    return iota;
end );

##############################################################################
##
#M  PrincipalDerivations      list of principal derivations without duplicates
##
InstallMethod( PrincipalDerivations, "method for xmod",  true, [ IsXMod ], 0,
function( XM )

    local S, s, elS, size, i, L, iota, pos;

    S := Source( XM );
    elS := Elements( S );
    size := Length( elS );
    L := [ ];
    for i in [1..size] do
        iota := PrincipalDerivation( XM, elS[i] );
        pos := Position( L, iota );
        if ( pos = fail ) then
            Add( L, iota );
        fi;
    od;
    return L;
end );

##############################################################################
##
#M  CompositeDerivation                 Whitehead composite of two derivations
##
InstallMethod( CompositeDerivation, "method for two derivations", true,
    [ IsDerivation, IsDerivation ], 0,
function( chi, chj )

    local XM, imi, imj, R, stgR, numrng, rng, r, k,
          s, si, sj, bdy, bsj, imcomp, comp;

    XM := Object2d( chi );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    if not ( Object2d( chj ) = XM ) then
        Error( "<chi>,<chj> must be derivations of the SAME crossed module" );
    fi;
    numrng := Length( stgR );
    rng := [ 1..numrng ];
    imi := UpGeneratorImages( chi );
    imj := UpGeneratorImages( chj );
    bdy := Boundary( XM );
    imcomp := 0 * rng;
    for k in rng do
        r := stgR[k];
        sj := imj[k];
        si := imi[k];
        bsj := Image( bdy, sj );
        s := DerivationImage( chi, bsj );
        imcomp[k] := si * sj * s;
    od;
    comp := DerivationByImages( XM, imcomp );
    return comp;
end );

##############################################################################
##
#M  \*( <chi1>, <chi2> ) . . . . . . . . . . . . . . . . . . for 2 derivations
##
InstallOtherMethod( \*, "for two derivations of crossed modules",
    IsIdenticalObj, [ IsDerivation, IsDerivation ], 0,
function( chi1, chi2 )
    return CompositeDerivation( chi2, chi1 );
end );

##############################################################################
##
#M  IsRegularDerivation               test whether a derivation has an inverse
##
InstallMethod( IsRegularDerivation, "method for derivations", true,
    [ IsDerivation ], 0,
function( chi )

    local XM, S, bdy, R, stgR, genrng, im, imrho, rho, ok;

    XM := Object2d( chi );
    S := Source( XM );
    bdy := Boundary( XM );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    im := UpGeneratorImages( chi );
    genrng := [ 1..Length( stgR ) ];
    imrho := List( genrng, i -> stgR[i] * Image( bdy, im[i] ) );
    rho := GroupHomomorphismByImages( R, R , stgR, imrho);
    ok := ( ( rho <> fail ) and IsBijective( rho ) );
    return ok;
end );

#############################################################################
##
#M  InverseDerivations      Finds all semigroup inverses XJ for derivation Xi
##                                                i.e.  XiXjXi=Xi & XjXiXj=Xj
InstallMethod( InverseDerivations, "method for a derivation", true,
    [ IsDerivation ], 0,
function( chi )

    local XM, x, D, L, size, j, chj, chk, chl, chh, chg, inv;

    XM := Object2d( chi );
    D := AllDerivations( XM );
    L := ImagesList( D );
    size := Length( L );
    inv := [ ];
    for j in [1..size] do
        chj := DerivationByImages( XM, L[j] );
        chk := CompositeDerivation( chi, chj );
        chl := CompositeDerivation( chk, chi );
        chh := CompositeDerivation( chj, chi );
        chg := CompositeDerivation( chh, chj );
        if ( ( UpGeneratorImages( chi ) = UpGeneratorImages( chl ) )
            and ( UpGeneratorImages( chg ) = UpGeneratorImages( chj ) ) ) then
            Add( inv, Position( L, UpGeneratorImages( chj ) ) );
        fi;
    od;
    return inv;
end );

##############################################################################
##
#M  ListInverseDerivations               List all inverses for each derivation
##
InstallMethod( ListInverseDerivations, "method for a crossed module", true,
    [ IsXMod ], 0,
function( XM )

    local L, i, M, size, D, chi, inv;

    D := AllDerivations( XM );
    L := ImagesList( D );
    size := Length( L );
    M := 0 * [1..size];
    if IsBound( D.inverses ) then
        return D.inverses;
    fi;
    for i in [1..size] do
        chi := DerivationByImages( XM, L[i] );
        inv := InverseDerivations( chi );
        M[i] := inv;
    od;
    return M;
    end );

##############################################################################
##                                 Sections                                 ##
##############################################################################

##############################################################################
##
#M  SectionByImages                                   sets up GroupHomByImages
##
InstallMethod( SectionByImages, "method for a cat1-group", true,
    [ IsCat1Group, IsGroupHomomorphism ], 0,
function( C, hom )

    local fam, filter, R, G, stgR, ngR, nargs, usage, isect, im, xi;

    fam := Up2DimensionalMappingFamily; 
    filter := IsUp2DimensionalMappingRep; 
    G := Source( C );
    R := Range( C );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngR := Length( stgR );
    if not ( ( Source( hom ) = R ) and ( Range( hom ) = G ) ) then
        Error( "require  hom : Range(C) -> Source(C)" );
    fi;
    im := List( stgR, r -> Image( hom, r ) ); 
    xi := rec(); 
    ObjectifyWithAttributes( xi, 
        NewType( fam, filter ), 
        Object2d, C, 
        UpHomomorphism, hom, 
        UpGeneratorImages, im ); 
    isect := IsSection( xi );
    return xi;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj . . . . for a section 
##
InstallMethod( String, "for a section", true, [ IsSection ], 0, 
function( g2d ) 
    return( STRINGIFY( "[", String( Source(g2d) ), " -> ", 
                            String( Range(g2d) ), "]" ) ); 
end );

InstallMethod( ViewString, "for a section", true, [ IsSection ], 0, String ); 

InstallMethod( PrintString, "for a section", true, [ IsSection ], 0, String ); 

InstallMethod( PrintObj, "method for a section", true, [ IsSection ], 0,
function( xi )

    local obj;

    obj := Object2d( xi );
    Print( "SectionByImages( ", Range( obj ), ", ", Source( obj ), ", ",
        GeneratorsOfGroup( Range( obj ) ), ", ", UpGeneratorImages( xi ), 
        " )" );
end ); 

InstallMethod( ViewObj, "method for a section", true, [ IsSection ], 
    0, PrintObj ); 

##############################################################################
##
#M  SectionByDerivation . .  the cat1-group section determined by a derivation
##
InstallMethod( SectionByDerivation, "method for a derivation", true,
    [ IsDerivation ], 0,
function( chi )

    local XM, R, stgR, ngR, hom, xi, imchi, imhom, i, r, 
          er, eR, eK, eKchi, g, C;

    XM := Object2d( chi );
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngR := Length( stgR );
    C := Cat1GroupOfXMod( XM );
    eR := RangeEmbedding( C );
    eK := KernelEmbedding( C );
    imchi := UpGeneratorImages( chi );
    eKchi := List( imchi, s -> Image( eK, s ) );
    imhom := 0 * [ 1..ngR ];
    for i in [ 1..Length( stgR ) ] do
        r := stgR[i];
        er := Image( eR, r );
        g := er * eKchi[i];
        imhom[i] := g;
    od;
    ## xi := SectionByImages( C, imhom );   use  NC
    hom := GroupHomomorphismByImages( R, Source(C), stgR, imhom );
    xi := SectionByImages( C, hom );
    return xi;
end ); 

##############################################################################
##
#M  DerivationBySection      construct XMod derivation from cat1-group section
##
InstallMethod( DerivationBySection, "method for a section", true,
    [ IsSection ], 0,
function( xi )

    local C, imxi, eR, eK, R, stgR, ngR, S, XM, imchi, r, er, s, i, chi;

    if not IsSection( xi ) then
        Error( "Parameter must be a section of a cat1-group" );
    fi;
    C := xi.cat1;
    imxi := UpGeneratorImages( xi );
    XM := XModOfCat1Group( C );
    S := C.kernel;
    R := Range( C );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    ngR := Length( stgR );
    eR := C.embedRange;
    eK := C.embedKernel;
    imchi := 0 * [ 1..ngR ];
    for i in [ 1..Length( stgR ) ] do
        r := stgR[i];
        er := Image( eR, r );
        s := er^(-1) * imxi[i];
        imchi[i] := PreImagesRepresentative( eK, s );
        Info( InfoXMod, 2, "In xi->chi :- ", [ i, r, er, s] );
    od;
    chi := DerivationByImages( XM, imchi );
    chi.isDerivation := IsDerivation( chi );
    return chi;
end );

##############################################################################
##
#M  CompositeSection                       Whitehead composite of two sections
##
InstallMethod( CompositeSection, "method for two sections", true,
    [ IsSection, IsSection ], 0,
function( xi, xj )

    local C, R, stgR, h, e, hxj, ehxj, xihxj, r, im1, im2, im3,
          numrng, k, imcomp, comp;

    C := Object2d( xi );
    if not ( C = Object2d( xj ) ) then
        Error( "<xi>,<xj> must be sections of the SAME cat1-group" );
    fi;
    R := Range( C );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    h := HeadMap( C );
    e := RangeEmbedding( C );
    hxj := xj * h;
    ehxj := hxj * e;
    xihxj := hxj * xi;
    numrng := Length( stgR );
    imcomp := 0 * [1..numrng];
    for k in [1..numrng] do
        r := stgR[k];
        im1 := Image( xj, r );
        im2 := Image( ehxj, r )^(-1);
        im3 := Image( xihxj, r );
        imcomp[k] := im1 * im2 * im3;
    od;
    comp := SectionByImages( C, imcomp );
    return comp;
end );


###############################################################################
##                        Monoids of Derivations and Sections                ##
###############################################################################


###############################################################################
##
#M  MonoidOfUp2DimensionalMappingsObj( <obj>, <ims>, <str> )  construct upmappings record
##
InstallMethod( MonoidOfUp2DimensionalMappingsObj, "for a 2DimensionalDomain", true,
    [ Is2DimensionalDomain, IsHomogeneousList, IsString ], 0,
function( obj, images, str )

    local filter, fam, mon;

    fam := FamilyObj( [ obj, images, str ] );
    filter := IsMonoidOfUp2DimensionalMappingsObj;
    mon := rec(); 
    ObjectifyWithAttributes( mon, NewType( fam, filter ), 
      IsMonoidOfUp2DimensionalMappings, true,
      Object2d, obj,
      ImagesList, images,
      AllOrRegular, str );
    if IsXMod( obj ) then
        SetIsMonoidOfDerivations( mon, true );
    elif IsCat1Group( obj ) then
        SetIsMonoidOfSections( mon, true );
    else
        Error( "<obj> not an xmod nor a cat1" );
    fi;
    return mon;
end );

##############################################################################
##
#?  added 28/04/08, but does not seem to be 'applicable' ??
##
#M  PrintObj( <mon> )                  prints regular/all derivations/sections 
#M  ViewObj( <mon> )                    views regular/all derivations/sections 
##
InstallMethod( PrintObj, "for IsMonoidOfUp2DimensionalMappings", true,
    [ IsMonoidOfUp2DimensionalMappings ], 0,
function( mon ) 

    if IsMonoidOfDerivations( mon ) then 
        Print( "monoid of derivations with images list:\n" ); 
    elif IsMonoidOfSections( mon ) then  
        Print( "monoid of sections with images list:\n" ); 
    else 
        Error( "neither derivations nor sections" ); 
    fi; 
    PrintListOneItemPerLine( ImagesList( mon ) ); 
end ); 

InstallMethod( ViewObj, "for IsMonoidOfUp2DimensionalMappings", true,
    [ IsMonoidOfUp2DimensionalMappings ], 0, PrintObj ); 

##############################################################################
##
#M  ImagesTable                      returns list of lists of DerivationImages
##
InstallMethod( ImagesTable, "method for crossed module derivations", true,
    [ IsMonoidOfDerivations ], 0,
function( D )

    local str, T, i, chi, L, XM, size;

    XM := Object2d( D );
    L := ImagesList( D );
    size := Length( L );
    T := 0 * [1..size];
    for i in [1..size] do
        chi := DerivationByImages( XM, L[i] );
        T[i] := UpImagePositions( chi );
    od;
    return T;
end );

##############################################################################
##
#M  BacktrackDerivationsJ          recursive function for BacktrackDerivations
##
InstallMethod( BacktrackDerivationsJ, "method for a crossed module", true,
    [ IsXMod, IsHomogeneousList, IsHomogeneousList, IsHomogeneousList,
      IsInt, IsString ], 0,
function( XM, subs, imrho, imchi, j, str )
    
    local Xsrc, onesrc, Xrng, isorng, stgrng, derivgen, s, ok, rho, bdy,
          k, J, genJ, ord, r, aut, w, t, i, chi;

    Xsrc := Source( XM );
    Xrng := Range( XM );
    onesrc := One( Xsrc );
    stgrng := StrongGeneratorsStabChain( StabChain( Xrng ) );
    k := Length( stgrng );
    bdy := Boundary( XM );
    if ( k < j ) then
        # complete list of images found: if a derivation, add to genimagesList
        imchi := ShallowCopy( imchi );
        derivgen := [ imchi ];
        chi := DerivationByImages( XM, imchi );
        ok := ( ( chi <> fail ) and IsDerivation( chi ) );
        if ( ok and IsXMod( XM ) ) then
            SetIsDerivation( chi, true );
            if ( ok and ( str = "reg" ) ) then
                ok := IsRegularDerivation( chi );
            fi;
        fi;
        if not ok then
            derivgen := [ ];
        fi;
    else
        J := subs[j];
        genJ := GeneratorsOfGroup( J );
        derivgen := [ ];
        for s in Xsrc do
            imchi[ j ] := s;
            imrho[ j ] := genJ[j] * Image( bdy, s );
            rho := GroupHomomorphismByImages( J, Xrng, genJ, imrho );
            ok := IsGroupHomomorphism( rho );
            if ok then
                r := genJ[j];
                ord := Order( r );
                w := s;
                t := s;
                aut := Image( XModAction( XM ), r );
                for i in [1..ord-1] do
                    t := Image( aut, t );
                    w := t * w;
                od;
                if ( w <> onesrc ) then
                    ok := false;
                    Info( InfoXMod, 3, "test fails at j,r,s,w = ", [j,r,s,w] );
                fi;
            fi;
            if ok then
                Append( derivgen,
                   BacktrackDerivationsJ( XM, subs, imrho, imchi, j+1, str ) );
            fi;
            imrho := imrho{[1..j]};
        od;
    fi;
    return derivgen;
end );

##############################################################################
##
#M  BacktrackDerivations            recursive construction for all derivations
##
InstallMethod( BacktrackDerivations, "method for a crossed module", true,
    [ IsXMod, IsString ], 0,
function( XM, str )

    local R, len, stgR, subs, images, sorted, derivrec;

    if not ( str in [ "reg", "all" ] ) then
        Error( "Invalid reg|all string" );
    fi;
    R := Range( XM );
    stgR := StrongGeneratorsStabChain( StabChain( R ) );
    len := Length( stgR );
    subs := List( [1..len], i -> Subgroup( R, stgR{[1..i]} ) ); 
    images := BacktrackDerivationsJ( XM, subs, [], [], 1, str );
    derivrec := MonoidOfUp2DimensionalMappingsObj( XM, images, str );
    return derivrec;
end );

##############################################################################
##
#M  RegularDerivations    find all invertible derivations for a crossed module
##
InstallMethod( RegularDerivations, "method for a crossed module", true, 
    [ IsXMod ], 0,
function( XM )

    local how, ok;

    ok := true;
    return BacktrackDerivations( XM, "reg" );
end );

##############################################################################
##
#M  AllDerivations                   find all derivations for a crossed module
##
InstallMethod( AllDerivations, "method for a crossed module", true, 
    [ IsXMod ], 0,
function( XM )

    local how, ok;

    ok := true;
    return BacktrackDerivations( XM, "all" );
end );

##############################################################################
##
#M  WhiteheadMonoidTable( XM )                table of products of derivations
##
InstallMethod( WhiteheadMonoidTable, "method for a crossed module", true,
    [ IsXMod ], 0,
function( XM )

    local C, D, L, i, j, chi, images, J, M, size;

    D := AllDerivations( XM );
    L := ImagesList( D );
    size := Length( L );
    M := 0 * [1..size];
    C := 0 * [1..size];
    for i in [1..size] do
        C[i] := DerivationByImages( XM, L[i] );
    od;
    for i in [1..size] do
        J := 0 * [1..size];
        for j in [1..size] do
            chi := CompositeDerivation( C[i], C[j] );
            J[j] := Position( L, UpGeneratorImages( chi ) );
        od;
        M[i] := J;
    od;
    return M;
end );

###############################################################################
##
#M  WhiteheadGroupTable( XM )          table of products of regular derivations
##
InstallMethod( WhiteheadGroupTable, "method for a crossed module", true,
    [ IsXMod ], 0,
function( XM )

    local C, D, L, i, j, chi, images, J, M, reg, len;

    D := RegularDerivations( XM );
    reg := AllOrRegular( D );
    L := ImagesList( D );
    len := Length( L );
    M := 0 * [1..len];
    C := 0 * [1..len];
    for i in [1..len] do
        C[i] := DerivationByImages( XM, L[i] );
    od;
    for i in [1..len] do
        J := 0 * [1..len];
        for j in [1..len] do
            chi := CompositeDerivation( C[i], C[j] );
            J[j] := Position( L, UpGeneratorImages( chi ) );
        od;
        M[i] := J;
    od;
    return M;
end );

##############################################################################
##
#M  WhiteheadPermGroup( XM )                  perm rep for the Whitehead group
##
##  (08/01/04)  wish to use SmallerDegreePermutationRepresentation
##
InstallMethod( WhiteheadPermGroup, "method for a crossed module", true,
    [ IsXMod ], 0,
function( XM )

    local tab, reg, im, grp, gens, strgens, pos, genchi, grp1;

    reg := RegularDerivations( XM );
    tab := WhiteheadGroupTable( XM );
    im := ImagesList( reg );
    gens := List( tab, row -> PermList( row ) );
    grp := Group( gens );
    strgens := StrongGeneratorsStabChain( StabChain( grp ) );
    pos := List( strgens, g -> Position( gens, g ) );
    SetWhiteheadGroupGeneratorPositions( XM, pos ); 
    genchi := List( pos, p -> DerivationByImages( XM, im[p] ) );
    SetWhiteheadGroupGeneratingDerivations( XM, genchi );
    if ( pos = [ ] ) then
        grp1 := Group( () );
    else
        grp1 := Group( strgens );
    fi;
    return grp1;
end );

###############################################################################
##
#M  WhiteheadTransMonoid( XM )               trans rep for the Whitehead monoid
##
##  this needs some more work
##
InstallMethod( WhiteheadTransMonoid, "method for a crossed module", true,
    [ IsXMod ], 0,
function( XM )

    local tab, j, t, mon, gens, it;

    tab := WhiteheadMonoidTable( XM );
    it := Transformation( [ ] ); 
    gens := [ ];
    mon := Monoid( [ it ] ); 
    for j in [1..Length(tab)] do 
        t := Transformation( tab[j] ); 
        if not ( t in mon ) then 
            Add( gens, t ); 
            mon := Monoid( gens ); 
        fi;
    od;
    return mon;
end );

#############################################################################
##
#E  gp2up.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
