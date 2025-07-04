#############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'            Chris Wensley
##                                                              Alper Odabas
##  This file implements generic methods for (pre-)crossed squares 
##  and (pre-)cat2-groups.
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
    
#############################################################################
##
#M  IsPerm3DimensionalGroup . . check whether the 4 sides are perm 2d-groups
#M  IsFp3DimensionalGroup . . . . check whether the 4 sides are fp 2d-groups
#M  IsPc3DimensionalGroup . . . . check whether the 4 sides are pc 2d-groups
##
InstallMethod( IsPerm3DimensionalGroup, 
    "generic method for 3d-group objects", true, 
    [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsPermGroup( Up2DimensionalGroup(obj) ) 
             and IsPermGroup( Left2DimensionalGroup(obj) ) 
             and IsPermGroup( Right2DimensionalGroup(obj) ) 
             and IsPermGroup( Down2DimensionalGroup(obj) ) 
             and IsPermGroup( Diagonal2DimensionalGroup(obj) ) );
end );

InstallMethod( IsFp3DimensionalGroup, "generic method for 3d-group objects",
    true, [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsFpGroup( Up2DimensionalGroup(obj) ) 
             and IsFpGroup( Left2DimensionalGroup(obj) ) 
             and IsFpGroup( Right2DimensionalGroup(obj) ) 
             and IsFpGroup( Down2DimensionalGroup(obj) ) 
             and IsFpGroup( Diagonal2DimensionalGroup(obj) ) );
end );

InstallMethod( IsPc3DimensionalGroup, "generic method for 3d-group obj ects",
    true, [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsPcGroup( Up2DimensionalGroup(obj) ) 
             and IsPcGroup( Left2DimensionalGroup(obj) ) 
             and IsPcGroup( Right2DimensionalGroup(obj) ) 
             and IsPcGroup( Down2DimensionalGroup(obj) ) 
             and IsPcGroup( Diagonal2DimensionalGroup(obj) ) );
end );

#############################################################################
##
#M  CrossedPairingByCommutators( <grp>, <grp>, <grp> ) . . . . make an xpair
##
InstallMethod( CrossedPairingByCommutators, "for three groups", true,
    [ IsGroup, IsGroup, IsGroup ], 0,
function( N, M, L )

    local xp;

    if not IsSubgroup( L, CommutatorSubgroup(N,M) ) then 
        Error( "require CommutatorSubgroup(N,M) <= L" );
    fi;
    xp := function( n, m ) return Comm( n, m ); end ;
    return xp;
end );

#############################################################################
##
#M  CrossedPairingByConjugators( <grp> ) . . . make an xpair : Inn(M)^2 -> M 
##
InstallMethod( CrossedPairingByConjugators, "for inner automorphism group", 
    true, [ IsGroup ], 0,
function( innM )

    local gens, xp;

    gens := GeneratorsOfGroup( innM ); 
    if not ForAll( gens, HasConjugatorOfConjugatorIsomorphism ) then 
        Error( "innM is not a group of inner automorphisms" ); 
    fi;
    xp := function( n, m ) 
              local cn, cm;
              cn := ConjugatorOfConjugatorIsomorphism( n );
              cm := ConjugatorOfConjugatorIsomorphism( m );
              return Comm( cn, cm );
          end;
    return xp;
end );

#############################################################################
##
#M  CrossedPairingByDerivations( <xmod> ) . . make an actor crossed pairing
##
InstallMethod( CrossedPairingByDerivations, "for a crossed module", true,
    [ IsXMod ], 0,
function( X0 )

    local SX, RX, Winv, WR, eWR, WP, reg, imlist, xp;

    SX := Source( X0 );
    RX := Range( X0 );
    Winv := WhiteheadGroupInverseIsomorphism( X0 );
    WR := WhiteheadRegularGroup( X0 );
    eWR := Elements( WR );
    WP := WhiteheadPermGroup( X0 );
    reg := RegularDerivations( X0 );
    imlist := ImagesList( reg );
    xp := function( r, t ) 
              local pos, chi;
              pos := Position( eWR, Image( Winv, t ) ); 
              chi := DerivationByImages( X0, imlist[pos] ); 
              return DerivationImage( chi, r ); 
          end;
    return xp;
end );

#############################################################################
##
#M  CrossedPairingByPreImages( <xmod>, <xmod> ) . . inner autos -> x-pairing 
##
InstallMethod( CrossedPairingByPreImages, "for two crossed modules", 
    true, [ IsXMod, IsXMod ], 0,
function( up, lt )

    local L, M, N, kappa, lambda, xp;

    L := Source( up );
    if not ( Source( lt ) = L ) then 
        Error( "up and lt should have the same source" ); 
    fi;
    M := Range( up ); 
    N := Range( lt );
    kappa := Boundary( up ); 
    lambda := Boundary( lt );
    xp := function( n, m ) 
              local lm, ln;
              ln := PreImagesRepresentativeNC( lambda, n ); 
              lm := PreImagesRepresentativeNC( kappa, m ); 
              return Comm( ln, lm ); 
          end;
    return xp;
end );

#############################################################################
##
#M  CrossedPairingBySingleXModAction( <xmod>, <subxmod> ) 
##      . . . . . . . . . . . . . . . . action -> x-pairing 
#M  PrincipalCrossedPairing( <xmod > )   
##
InstallMethod( CrossedPairingBySingleXModAction, 
    "for xmod and normal subxmod", true, [ IsXMod, IsXMod ], 0,
function( rt, lt )

    local M, L, N, act, xp;

    if not IsNormalSub2DimensionalDomain( rt, lt ) then 
        Error( "lt not a normal subxmod of rt" ); 
    fi;
    M := Source( rt ); 
    L := Source( lt );
    N := Range( lt ); 
    act := XModAction( rt );
    xp := function( n, m ) 
              return ImageElm( ImageElm(act,n), m^(-1) ) * m;
          end;
    return xp;
end );

InstallMethod( PrincipalCrossedPairing, "for an xmod", true, [ IsXMod ], 0,
function( X0 )
    return CrossedPairingBySingleXModAction( X0, X0 ); 
end );

#############################################################################
##
#M  IsPreCrossedSquare . . . . . . . . . . . . check that the square commutes
##
InstallMethod( IsPreCrossedSquare, "generic method for a pre-crossed square",
    true, [ IsHigherDimensionalGroup ], 0,
function( PXS )

    local up, lt, rt, dn, dg, L, M, N, P, kappa, lambda, mu, nu, delta, 
          lambdanu, kappamu, ok, actrt, rngactrt, actdn, rngactdn, 
          genN, genM, imactNM, actNM, imactMN, actMN, morupdn, morltrt;

    if not IsPreCrossedSquareObj( PXS ) then
        return false;
    fi;
    up := Up2DimensionalGroup( PXS );
    lt := Left2DimensionalGroup( PXS );
    rt := Right2DimensionalGroup( PXS );
    dn := Down2DimensionalGroup( PXS );
    dg := Diagonal2DimensionalGroup( PXS );
    L := Source( up );
    M := Range( up );
    N := Source( dn );
    P := Range( dn );
    if not ( ( L = Source(lt) ) and ( N = Range(lt) ) and
             ( L = Source(dg) ) and ( P = Range(dg) ) and 
             ( M = Source(rt) ) and ( P = Range(rt) ) ) then
        Info( InfoXMod, 2, "Incompatible source/range" );
        return false;
    fi;
    ## checks for the diagonal 
    delta := Boundary( dg ); 
    lambda := Boundary( lt );
    nu := Boundary( dn );
    lambdanu := lambda * nu;
    kappa := Boundary( up );
    mu := Boundary( rt );
    kappamu := kappa * mu;
    if not ( lambdanu = delta ) and ( kappamu = delta ) then
        Info( InfoXMod, 2, "boundaries in square do not commute" );
        return false;
    fi;
    # construct the cross-diagonal actions 
    actrt := XModAction( rt ); 
    rngactrt := Range( actrt ); 
    actdn := XModAction( dn ); 
    rngactdn := Range( actdn );
    genM := GeneratorsOfGroup( M ); 
    genN := GeneratorsOfGroup( N ); 
    imactNM := List( genN, n -> ImageElm( actrt, ImageElm( nu, n ) ) ); 
    actNM := GroupHomomorphismByImages( N, rngactrt, genN, imactNM ); 
    imactMN := List( genM, m -> ImageElm( actdn, ImageElm( mu, m ) ) ); 
    actMN := GroupHomomorphismByImages( M, rngactdn, genM, imactMN ); 
    SetCrossDiagonalActions( PXS, [ actNM, actMN ] ); 
    #? compatible actions to be checked?
    morupdn := PreXModMorphism( up, dn, lambda, mu );
    morltrt := PreXModMorphism( lt, rt, kappa, nu );
    if not ( IsPreXModMorphism(morupdn) and IsPreXModMorphism(morltrt) ) then
        Info( InfoXMod, 2, "morupdn and/or modltrt not prexmod morphisms" );
        return false;
    fi;
    return true;
end );

############################################################################# 
##
#M  IsCrossedSquare . . . . . . . check all the axioms for a crossed square
##
InstallMethod( IsCrossedSquare, "generic method for a crossed square", 
    true, [ IsHigherDimensionalGroup ], 0,
function( XS )

    local up, lt, rt, dn, L, M, N, P, kappa, lambda, mu, nu, 
          autu, autl, actdg, dg, ok, morud, morlr, 
          genL, genM, genN, genP, actup, actlt, actrt, actdn, l, p, 
          xp, x, y, z, m, n, m2, n2, am, an, apdg, aprt, apdn, nboxm;

    if not ( IsPreCrossedSquare( XS ) and HasCrossedPairing( XS ) ) then
        return false;
    fi;
    up := Up2DimensionalGroup( XS );
    lt := Left2DimensionalGroup( XS );
    rt := Right2DimensionalGroup( XS );
    dn := Down2DimensionalGroup( XS );
    dg := Diagonal2DimensionalGroup( XS );
    L := Source( up );
    M := Range( up );
    N := Source( dn );
    P := Range( dn );
    kappa := Boundary( up );
    lambda := Boundary( lt );
    mu := Boundary( rt );
    nu := Boundary( dn );
    genL := GeneratorsOfGroup( L ); 
    genM := GeneratorsOfGroup( M ); 
    genN := GeneratorsOfGroup( N ); 
    genP := GeneratorsOfGroup( P ); 
    actup := XModAction( up ); 
    actlt := XModAction( lt );
    actrt := XModAction( rt );
    actdn := XModAction( dn ); 
    actdg := XModAction( dg ); 
    ## check that kappa,lambda preserve the action of P 
    for p in genP do 
        apdg := ImageElm( actdg, p );
        aprt := ImageElm( actrt, p );
        apdn := ImageElm( actdn, p );
        for l in genL do 
            if not ( ImageElm( kappa, ImageElm( apdg, l ) ) 
                     = ImageElm( aprt, ImageElm( kappa, l ) ) ) then 
                Info( InfoXMod, 2,  "action of P on up is not preserved" );
                return false; 
            fi;
            if not ( ImageElm( lambda, ImageElm( apdg, l ) ) 
                     = ImageElm( apdn, ImageElm( lambda, l ) ) ) then 
                Info( InfoXMod, 2, "action of P on lt is not preserved" ); 
                return false; 
            fi;
        od;
    od;
    ## check the axioms for a crossed pairing 
    xp := CrossedPairing( XS ); 
    for n in genN do 
        for n2 in genN do 
            for m in genM do 
                x := xp( n*n2, m ); 
                an := ImageElm( actlt, n2 ); 
                y := ImageElm( an, xp( n, m ) ); 
                z := xp( n2, m ); 
                if not x = y * z then 
                    Info( InfoXMod, 2, 
                          "n1,n2,m crossed pairing axiom fails" ); 
                    return false; 
                fi; 
            od;
       od;
    od;
    for n in genN do 
        for m in genM do 
            for m2 in genM do 
                x := xp( n, m*m2 ); 
                am := ImageElm( actup, m2 ); 
                y := ImageElm( am, xp( n, m ) ); 
                if not x = xp( n, m2 ) * y then
                    Info( InfoXMod, 2, 
                          "n,m1,m2 crossed pairing axiom fails" ); 
                    return false; 
                fi; 
            od;
       od;
    od;
    for p in genP do 
        apdg := ImageElm( actdg, p ); 
        aprt := ImageElm( actrt, p ); 
        apdn := ImageElm( actdn, p );
        for n in genN do 
            for m in genM do 
                if not ImageElm( apdg, xp( n, m ) ) 
                     = xp( ImageElm(apdn,n), ImageElm(aprt,m) ) then
                    Info( InfoXMod, 2, "n,m,p crossed pairing axiom fails" ); 
                    return false; 
                fi; 
            od;
       od;
    od;
    ## check that kappa,lambda correctly map (n box m) 
    for n in genN do 
        an := ImageElm( actrt, ImageElm( nu, n ) ); 
        for m in genM do 
            am := ImageElm( actdn, ImageElm( mu, m ) ); 
            nboxm := xp( n, m ); 
            if not ImageElm( lambda, nboxm ) = n^(-1) * ImageElm( am, n ) 
               and ImageElm( kappa, nboxm ) = ImageElm( an, m^(-1) ) * m then 
                Info( InfoXMod, 2,  
                      "kappa,lambda do not map nboxm correctly" ); 
                return false;
            fi;
        od;
    od;
    ## check crossed pairing on images of kappa,lambda 
    for m in genM do 
        ## am := ImageElm( actdg, ImageElm( mu, m ) ); 
        am := ImageElm( actup, m );
        for l in genL do 
            if not ( xp( ImageElm( lambda, l ), m ) 
                   = l^(-1) * ImageElm( am, l ) ) then 
                Info( InfoXMod, 2, "incorrect image for (lambda(l) box n)" ); 
                return false;
            fi;
        od;
    od;
    for n in genN do 
        ## an := ImageElm( actdg, ImageElm( nu, n ) ); 
        an := ImageElm( actlt, n );  
        for l in genL do 
            if not ( xp( n, ImageElm( kappa, l ) ) 
                   = ImageElm( an, l^(-1) ) * l ) then 
                Info( InfoXMod, 2, "incorrect image for (n box kappa(l))" ); 
                return false; 
            fi; 
        od;
    od;
    return true;
end );

#############################################################################
##
#M  PreCrossedSquareObj ( <up>, <left>, <right>, <down>, <diag>, <pair> ) 
##                                          . . . make a PreCrossedSquare
##
InstallMethod( PreCrossedSquareObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( up, lt, rt, dn, dg, xp )

    local PS;

    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, PreCrossedSquareObjType, 
      Up2DimensionalGroup, up, 
      Left2DimensionalGroup, lt,
      Right2DimensionalGroup, rt,
      Down2DimensionalGroup, dn,
      Diagonal2DimensionalGroup, dg,
      CrossedPairing, xp, 
      HigherDimension, 3, 
      Is3DimensionalGroup, true );
    if not IsPreCrossedSquare( PS ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed square." );
    fi; 
    return PS;
end );

#############################################################################
##
#F  PreCrossedSquare( <up>, <lt>, <rt>, <dn>, <dg>, <xp> ) 
##                    . . . . . . . . . . . comprising 5 prexmods + pairing
#F  PreCrossedSquare( <PC2> )  . . . . . . . . . . . . for a pre-cat2-group
##
InstallGlobalFunction( PreCrossedSquare, function( arg )

    local nargs, PXS, ok;

    nargs := Length( arg );
    ok := true; 
    if ( nargs = 1 ) then
        if ( HasIsPreCat2Group( arg[1] ) and IsPreCat2Group( arg[1] ) ) then 
            Info( InfoXMod, 1, "pre-crossed square of a pre-cat2-group" ); 
            PXS := PreCrossedSquareOfPreCat2Group( arg[1] );
        else 
            ok := false; 
        fi;
    elif ( nargs = 6  ) then
        PXS := PreCrossedSquareByPreXMods(
                   arg[1], arg[2], arg[3], arg[4], arg[5], arg[6] );
    else  
        ok := false; 
    fi; 
    if not ok then 
        Print( "standard usage for the function PreCrossedSquare:\n" );  
        Print( "    PreCrossedSquare(<up>,<lt>,<rt>,<dn>,<dg>,<xp>);\n" ); 
        Print( "    for 5 pre-crossed modules and a crossed pairing\n" );
        Print( "or: PreCrossedSquare( <PC2G> );  for a pre-cat2-group> );\n" );
        return fail;
    fi;
    return PXS;
end );


#############################################################################
##
#F  CrossedSquare( <up>, <lt>, <rt>, <dn>, <dg>, <xp> ) 5 xmods and a pairing
#F  CrossedSquare( <L>, <M>, <N>, <P> ) . . . . . . . . by normal subgroups
#F  CrossedSquare( <X0> ) . . . . . . . . . . . . . . . actor crossed square
#F  CrossedSquare( <X0>, <X1> ) . . . . . . . . . . . . by pullback 
#F  CrossedSquare( <X0>, <X1> ) . . . . . . . . . . . . by normal subxmod
#F  CrossedSquare( <C2G> )  . . . . . . . . . . . . . . for a cat2-group
##
InstallGlobalFunction( CrossedSquare, function( arg )

    local nargs, XS, ok;

    nargs := Length( arg );
    ok := true; 
    if ( nargs = 1 ) then
        if ( HasIsXMod( arg[1] ) and IsXMod( arg[1] ) ) then
            Info( InfoXMod, 1, "crossed square by splitting" );
            XS := CrossedSquareByXModSplitting( arg[1] ); 
        elif ( HasIsCat2Group( arg[1] ) and IsCat2Group( arg[1] ) ) then 
            Info( InfoXMod, 1, "crossed square of a cat2-group" ); 
            XS := PreCrossedSquareOfPreCat2Group( arg[1] );
        else 
            ok := false; 
        fi;
    elif ( nargs = 2 ) then
        if ( HasIsXMod( arg[1] ) and IsXMod( arg[1] ) 
             and HasIsXMod( arg[2] ) and IsXMod( arg[2] ) ) then 
            if Range( arg[1] ) = Range( arg[2] ) then 
                Info( InfoXMod, 1, "crossed square by pullback" );
                XS := CrossedSquareByPullback( arg[1], arg[2] ); 
            elif IsNormalSub2DimensionalDomain( arg[1], arg[2] ) then 
                Info( InfoXMod, 1, "crossed square by normal subxmod" );
                XS := CrossedSquareByNormalSubXMod( arg[1], arg[2] ); 
            else 
                ok := false; 
            fi; 
        else 
            ok := false; 
        fi;
    elif ( nargs = 4 ) and ForAll( arg, IsGroup ) then
        XS := CrossedSquareByNormalSubgroups(arg[1],arg[2],arg[3],arg[4]);
    elif ( nargs = 6  ) then
        XS := CrossedSquareByXMods(arg[1],arg[2],arg[3],arg[4],arg[5],arg[6]);
    else  
        ok := false; 
    fi; 
    if not ok then 
        Print( "standard usage for the function CrossedSquare:\n" );  
        Print( "    CrossedSquare( <up>, <lt>, <rt>, <dn>, <dg>, <xp> );\n" ); 
        Print( "             for 5 crossed modules and a crossed pairing\n" );
        Print( "or: CrossedSquare( <L>, <M>, <N>, <P> );  " ); 
        Print( "for 3 normal subgroups of P\n" );
        Print( "or: CrossedSquare( <X0>, <X1> );  for a pullback\n" );
        Print( "or: CrossedSquare( <X0>, <X1> );  for a normal subxmod\n" );
        Print( "or: CrossedSquare( <X0> );  for splitting an xmod\n" );
        Print( "or: CrossedSquare( <C2G> );  for a cat2-group> );\n" );
        return fail;
    fi;
    return XS;
end );

#############################################################################
##
#M  PreCrossedSquareByPreXMods . pre-crossed square from 5 pre-xmods + xpair
#M  CrossedSquareByXMods . . . . . . . . crossed square from 5 xmods + xpair
##
InstallMethod( PreCrossedSquareByPreXMods, "default pre-crossed square", 
    true, 
    [ IsPreXMod, IsPreXMod, IsPreXMod, 
      IsPreXMod, IsPreXMod, IsFunction ], 0,
function( up, left, right, down, diag, xp )

    local L, M, N, P, kappa, lambda, mu, nu, delta, n, m, PXS; 

    L := Source( up );
    M := Range( up );
    N := Source( down ); 
    P := Range( down ); 
    kappa := Boundary( up ); 
    lambda := Boundary( left );
    mu := Boundary( right ); 
    nu := Boundary( down ); 
    delta := Boundary( diag ); 
    ## checks 
    if not ( ( L = Source( left ) ) and ( N = Range( left ) ) 
         and ( M = Source( right ) ) and ( P = Range( right ) ) 
         and ( L = Source( diag ) ) and ( P = Range( diag ) ) ) then 
        Error( "sources and ranges not matching" ); 
    fi;
    for n in GeneratorsOfGroup( N ) do
        for m in GeneratorsOfGroup( M ) do
            if not ( xp( n, m ) in L ) then
                Error( "incorrect source/range for crossed pairing" ); 
            fi;
        od;
    od;
    PXS := PreCrossedSquareObj( up, left, right, down, diag, xp );
    if not IsPreCrossedSquare( PXS ) then 
        Error( "PXS fails to be a crossed square" ); 
    fi; 
    return PXS;
end );

InstallMethod( CrossedSquareByXMods, "default crossed square", true, 
    [ IsXMod, IsXMod, IsXMod, IsXMod, IsXMod, IsFunction ], 0,
function( up, left, right, down, diag, xp )

    local XS; 

    XS := PreCrossedSquareByPreXMods( up, left, right, down, diag, xp );
    if not IsCrossedSquare( XS ) then 
        Info( InfoXMod, 1, "XS fails to be a crossed square" ); 
        return fail; 
    fi; 
    return XS;
end );

#############################################################################
##
#M  CrossedSquareByNormalSubgroups . . crossed square from normal L,M,N in P
##
InstallMethod( CrossedSquareByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( L, M, N, P )

    local XS, up, lt, rt, dn, dg, xp, diag;

    if not ( IsNormal(P,M) and IsNormal(P,N) and IsNormal(L,P) ) then
        Error( "M,N,L fail to be normal subgroups of P" ); 
    fi;
    if not ( IsNormal( M, L ) and IsNormal( N, L ) ) then
        Error( "L fails to be a normal subgroup of both M and N" ); 
    fi;
    if not ( IsSubgroup( Intersection(M,N), L ) 
         and IsSubgroup( L, CommutatorSubgroup(M,N) ) ) then 
        Error( "require CommutatorSubgroup(M,N) <= L <= Intersection(M,N)" );
    fi;
    up := XModByNormalSubgroup( M, L );
    lt := XModByNormalSubgroup( N, L );
    rt := XModByNormalSubgroup( P, M );
    dn := XModByNormalSubgroup( P, N );
    dg := XModByNormalSubgroup( P, L );
    ##  define the pairing as a commutator
    xp := CrossedPairingByCommutators( N, M, L ); 
    XS := PreCrossedSquareObj( up, lt, rt, dn, dg, xp );
##    SetIsCrossedSquare( XS, true );
##    SetIs3DimensionalGroup( XS, true ); 
    SetDiagonal2DimensionalGroup( XS, dg ); 
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by normal subgroups" ); 
    fi;
    return XS;
end );

InstallMethod( CrossedSquareByNormalSubgroups, 
    "conjugation crossed square", true, [ IsGroup, IsGroup, IsGroup ], 0,
function( M, N, P )

    local XS, genP, genM, genN, L, genL, u, d, l, r, a, p, diag;

    if not ( IsNormal( P, M ) and IsNormal( P, N ) ) then
        return fail;
    fi;
    L := Intersection( M, N );
    return CrossedSquareByNormalSubgroups( L, M, N, P ); 
end );

#############################################################################
##
#M  CrossedSquareByNormalSubXMod . crossed square from xmod + normal subxmod 
##
InstallMethod( CrossedSquareByNormalSubXMod, 
    "for an xmod and normal subxmod", true, [ IsXMod, IsXMod ], 0,
function( rt, lt )

    local M, L, up, P, N, dn, xp, dg, XS;

    if not IsNormalSub2DimensionalDomain( rt, lt ) then 
        return fail; 
    fi;
    M := Source( rt ); 
    L := Source( lt );
    up := XModByNormalSubgroup( M, L ); 
    P := Range( rt ); 
    N := Range( lt );
    dn := XModByNormalSubgroup( P, N ); 
    xp := CrossedPairingBySingleXModAction( rt, lt ); 
    dg := SubXMod( rt, L, P );
    XS := PreCrossedSquareObj( up, lt, rt, dn, dg, xp );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by normal subxmod" ); 
    fi; 
    return XS;
end );

#############################################################################
##
#M  CrossedSquareByXModSplitting . . xsq by surjection followed by injection
##
InstallMethod( CrossedSquareByXModSplitting, "for an xmod", true, 
    [ IsXMod ], 0,
function( X0 )

    local S, R, bdy, Q, up, dn, xp, XS;

    S := Source( X0 ); 
    R := Range( X0 ); 
    Q := ImagesSource( Boundary( X0 ) ); 
    up := SubXMod( X0, S, Q ); 
    dn := XModByNormalSubgroup( R, Q ); 
    xp := CrossedPairingByPreImages( up, up ); 
    XS := PreCrossedSquareObj( up, up, dn, dn, X0, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by xmod splitting" ); 
    fi; 
    return XS;
end ); 

#############################################################################
##
#M  CrossedSquareByPullback . . . . . . . . for two xmods with a common range 
##
InstallMethod( CrossedSquareByPullback, "for 2 xmods with a common range", 
    true, [ IsXMod, IsXMod ], 0,
function( dn, rt )

    local M, N, P, actM, actN, mu, nu, L, genL, lenL, Linfo, dp, dpinfo, 
          embM, embN, kappa, lambda, map, xp, autL, genLN, genLM, genLNP, 
          genLMP, genP, lenP, imactPL, i, g, ima, a, actPL, genN, lenN, 
          imactNL, actNL, lt, genM, lenM, imactML, actML, up, 
          imdelta, delta, dg, XS;

    M := Source( rt ); 
    N := Source( dn );
    P := Range( rt ); 
    if not ( Range( dn ) = P ) then 
        Error( "the two xmods should have a common range" ); 
    fi;
    actM := XModAction( rt );
    actN := XModAction( dn );
    mu := Boundary( rt );
    nu := Boundary( dn );
    L := Pullback( nu, mu ); 
    genL := GeneratorsOfGroup( L ); 
    lenL := Length( genL );
    Linfo := PullbackInfo( L ); 
    if HasName(M) and HasName(N) and HasName(P) then 
        SetName( L, Concatenation( "(", Name(N), " x_", Name(P), 
                                   " ", Name(M), ")" ) ); 
    fi;
    dp := Linfo!.directProduct; 
    dpinfo := DirectProductInfo( dp ); 
    embN := Embedding( dp, 1 ); 
    embM := Embedding( dp, 2 ); 
    lambda := Linfo!.projections[1]; 
    genLN := List( genL, l -> ImageElm( lambda, l ) );
    kappa := Linfo!.projections[2]; 
    genLM := List( genL, l -> ImageElm( kappa, l ) );
    ## construct the crossed pairing 
    xp := function( n, m ) 
              local nun, mum, n2, m2, l;
              nun := ImageElm( nu, n );
              mum := ImageElm( mu, m );
              ## h(n,m) = (n^{-1}.n^mum, (m^{-1})^nun.m)
              n2 := n^(-1) * ImageElm( ImageElm( actN, mum ), n );
              m2 := ImageElm( ImageElm( actM, nun ), m^(-1) ) * m; 
              l := ImageElm( embN, n2 ) * ImageElm( embM, m2 ); 
              if not ( l in L ) then 
                  Error( "element l appears not to be in L" ); 
              fi;
              return l;
          end;
    autL := AutomorphismGroup( L );
    genP := GeneratorsOfGroup( P );
    lenP := Length( genP );
    imactPL := ListWithIdenticalEntries( lenP, 0 ); 
    for i in [1..lenP] do 
        g := genP[i]; 
        genLNP := List( genLN, n -> ImageElm( ImageElm( actN, g ), n ) ); 
        genLMP := List( genLM, m -> ImageElm( ImageElm( actM, g ), m ) ); 
        ima := List( [1..lenL], j -> ImageElm( embN, genLNP[j] ) 
                                     * ImageElm( embM, genLMP[j] ) ); 
        a := GroupHomomorphismByImages( L, L, genL, ima ); 
        imactPL[i] := a; 
    od; 
    actPL := GroupHomomorphismByImages( P, autL, genP, imactPL );
    imdelta := List( genL, l -> ImageElm( nu, ImageElm( lambda, l ) ) ); 
    delta := GroupHomomorphismByImages( L, P, genL, imdelta ); 
    dg := XModByBoundaryAndAction( delta, actPL ); 
    genN := GeneratorsOfGroup( N );
    lenN := Length( genN ); 
    imactNL := ListWithIdenticalEntries( lenN, 0 ); 
    for i in [1..lenN] do 
        g := ImageElm( nu, genN[i] ); 
        a := ImageElm( actPL, g );
        imactNL[i] := a;
    od;
    actNL := GroupHomomorphismByImages( N, autL, genN, imactNL ); 
    lt := XMod( lambda, actNL ); 
    genM := GeneratorsOfGroup( M );
    lenM := Length( genM ); 
    imactML := ListWithIdenticalEntries( lenM, 0 ); 
    for i in [1..lenM] do 
        g := ImageElm( mu, genM[i] ); 
        a := ImageElm( actPL, g );
        imactML[i] := a;
    od;
    actML := GroupHomomorphismByImages( M, autL, genM, imactML ); 
    up := XMod( kappa, actML ); 
    XS := PreCrossedSquareObj( up, lt, rt, dn, dg, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by pullback" ); 
    fi; 
    return XS;
end );

#############################################################################
##
#M  ActorCrossedSquare . . create a crossed square from an xmod and its actor
##
InstallMethod( ActorCrossedSquare, "actor crossed square", true, 
    [ IsXMod ], 0,
function( X0 )

    local XS, WX, LX, NX, AX, xp;

    AX := ActorXMod( X0 );
    WX := WhiteheadXMod( X0 );
    NX := NorrieXMod( X0 );
    LX := LueXMod( X0 );
    ##  define the pairing as evaluation of a derivation
    xp := CrossedPairingByDerivations( X0 );
    ## XS := PreCrossedSquareObj( X0, WX, NX, AX, da, xp );
    XS := PreCrossedSquareObj( WX, X0, AX, NX, LX, xp );
##    SetIsCrossedSquare( XS, true );
## Error("here");
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be an actor crossed square" ); 
    fi; 
    return XS;
end );

#############################################################################
##
#M  CrossedSquareByAutomorphismGroup . . crossed square G -> Inn(G) -> Aut(G)
##
InstallMethod( CrossedSquareByAutomorphismGroup, "G -> Inn(G) -> Aut(G)", 
    true, [ IsGroup ], 0,
function( G )

    local genG, innG, up, autG, dn, dg, xp, XS;

    genG := GeneratorsOfGroup( G ); 
    innG := InnerAutomorphismsByNormalSubgroup( G, G );
    if ( not HasName( innG ) and HasName( G ) ) then
        SetName( innG, Concatenation( "Inn(", Name( G ), ")" ) );
    fi;
    SetIsGroupOfAutomorphisms( innG, true );
    up := XModByGroupOfAutomorphisms( G, innG );
    autG := AutomorphismGroup( G );
    if ( not HasName( autG ) and HasName( G ) ) then
        SetName( autG, Concatenation( "Aut(", Name( G ), ")" ) );
    fi;
    if not IsSubgroup( autG, innG ) then 
        Error( "innG is not a subgroup of autG" ); 
    fi;
    dg := XModByAutomorphismGroup( G ); 
    dn := XModByNormalSubgroup( autG, innG );
    ##  define the pairing 
    xp := CrossedPairingByConjugators( innG );
    XS := PreCrossedSquareObj( up, up, dn, dn, dg, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by automorphism group" ); 
    fi; 
    return XS;
end );

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . for a 3-dimensional group 
##
InstallOtherMethod( Name, "method for a pre-crossed square", true, 
    [ IsHigherDimensionalGroup ], 0,
function( PS )

    local nul, nur, ndl, ndr, name, mor;

    if not ( HigherDimension( PS ) = 3 ) then 
        TryNextMethod(); 
    else 
        if HasName( Source( Up2DimensionalGroup( PS ) ) ) then
            nul := Name( Source( Up2DimensionalGroup( PS ) ) );
        else
            nul := "..";
        fi;
        if HasName( Range( Up2DimensionalGroup( PS ) ) ) then
            nur := Name( Range( Up2DimensionalGroup( PS ) ) );
        else
            nur := "..";
        fi;
        if HasName( Source( Down2DimensionalGroup( PS ) ) ) then
            ndl := Name( Source( Down2DimensionalGroup( PS ) ) );
        else
            ndl := "..";
        fi;
        if HasName( Range( Down2DimensionalGroup( PS ) ) ) then
            ndr := Name( Range( Down2DimensionalGroup( PS ) ) );
        else
            ndr := "..";
        fi;
        name := Concatenation( "[", nul, "->", nur, ",", ndl, "->", ndr, "]" );
        SetName( PS, name );
        return name;
    fi;
end );

#############################################################################
##
#M  Size3d . . . . . . . . . . . . . . . . . . . .  for a 3-dimensional group
##
InstallMethod( Size3d, "method for a 3d-object", true, 
    [ Is3DimensionalDomain ], 0,
function( PS ) 
    return [ Size( Source( Up2DimensionalGroup( PS ) ) ), 
             Size( Range( Up2DimensionalGroup( PS ) ) ),
             Size( Source( Down2DimensionalGroup( PS ) ) ),
             Size( Range( Down2DimensionalGroup( PS ) ) ) ]; 
end ); 

InstallOtherMethod( Size, "generic method for a 3d-object", 
    [ Is3DimensionalDomain ], 0, 
function ( obj )
    Error( "use operation Size3d for 3d-objects" );
    return fail; 
end );

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . . . . for a 3-dimensional group
##
InstallOtherMethod( IdGroup, "method for a 3-dimensional group", true, 
    [ IsHigherDimensionalGroup ], 0,
function( PS ) 
    return [ IdGroup( Source( Up2DimensionalGroup( PS ) ) ), 
             IdGroup( Range( Up2DimensionalGroup( PS ) ) ),
             IdGroup( Source( Down2DimensionalGroup( PS ) ) ),
             IdGroup( Range( Down2DimensionalGroup( PS ) ) ) ]; 
end ); 

#############################################################################
##
#M  StructureDescription  . . . . . . . . . . . . . for a 3-dimensional group
##
InstallOtherMethod( StructureDescription, "method for a 3-dimensional group", 
    true, [ IsHigherDimensionalGroup ], 0,
function( PS ) 
    return [ StructureDescription( Source( Up2DimensionalGroup( PS ) ) ), 
             StructureDescription( Range( Up2DimensionalGroup( PS ) ) ),
             StructureDescription( Source( Down2DimensionalGroup( PS ) ) ),
             StructureDescription( Range( Down2DimensionalGroup( PS ) ) ) ]; 
end ); 

#############################################################################
##
#M  \=( <dom1>, <dom2> ) . . . . . . . . . . test if two 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-domains", IsIdenticalObj, 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ], 0,
function ( dom1, dom2 ) 
    if not ( ( HigherDimension( dom1 ) = 3 ) 
           and ( HigherDimension( dom2 ) = 3 ) ) then 
        TryNextMethod(); 
    else 
        return( 
            ( Up2DimensionalGroup(dom1) = Up2DimensionalGroup(dom2) )
        and ( Left2DimensionalGroup(dom1) = Left2DimensionalGroup(dom2) ) 
        and ( Right2DimensionalGroup(dom1) = Right2DimensionalGroup(dom2) ) 
        and ( Down2DimensionalGroup(dom1) = Down2DimensionalGroup(dom2) ) 
        and ( Diagonal2DimensionalGroup(dom1) 
              = Diagonal2DimensionalGroup(dom2) ) ); 
    fi;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj . . . . for a 3d-group 
##
InstallMethod( String, "for a 3d-group", true, [ IsPreCrossedSquare ], 0, 
function( g3d ) 
    return( STRINGIFY( "pre-crossed square" ) ); 
end );

InstallMethod( ViewString, "for a 3d-group", true, [ IsPreCrossedSquare ], 
    0, String ); 

InstallMethod( PrintString, "for a 3d-group", true, [ IsPreCrossedSquare ], 
    0, String ); 

InstallMethod( PrintObj, "method for a 3d-group", true, 
    [ IsPreCrossedSquare ], 0,
function( g3d )

    local L, M, N, P, lenL, lenM, lenN, lenP, len1, len2, j, q1, q2, 
           ispsq, arrow, ok, n, i;

    if HasName( g3d ) then
        Print( Name( g3d ), "\n" );
    else
        if ( HasIsPreCrossedSquare( g3d ) 
             and IsPreCrossedSquare( g3d ) ) then 
            ispsq := true; 
            arrow := " -> "; 
        else 
            ispsq := false; 
            arrow := " => "; 
        fi; 
        L := Source( Up2DimensionalGroup( g3d ) );
        M := Range( Up2DimensionalGroup( g3d ) );
        N := Source( Down2DimensionalGroup( g3d ) );
        P := Range( Down2DimensionalGroup( g3d ) );
        ok := HasName(L) and HasName(M) and HasName(N) and HasName(P);
        if ok then
            lenL := Length( Name( L ) );
            lenM := Length( Name( M ) );
            lenN := Length( Name( N ) );
            lenP := Length( Name( P ) );
            len1 := Maximum( lenL, lenN );
            len2 := Maximum( lenM, lenP );
            q1 := QuoInt( len1, 2 );
            q2 := QuoInt( len2, 2 );
            Print( "[ " );
            for j in [1..lenN-lenL] do Print(" "); od;
            Print( Name(L), arrow, Name(M) );
            for j in [1..lenP-lenM] do Print(" "); od;
            Print( " ]\n[ " );
            for j in [1..q1] do Print(" "); od;
            Print( "|" );
            for j in [1..q1+RemInt(len1,2)+2+q2+RemInt(len2,2)] do 
                Print(" "); 
            od;
            Print( "|" );
            for j in [1..q2] do Print(" "); od;
            Print( " ]\n" );
            Print( "[ " );
            for j in [1..lenL-lenN] do Print(" "); od;
            Print( Name(N), " -> ", Name(P) );
            for j in [1..lenM-lenP] do Print(" "); od;
            Print( " ]\n" );
        else 
            if ispsq then 
                if ( HasIsCrossedSquare(g3d) and IsCrossedSquare(g3d) ) then 
                    Print( "crossed square with crossed modules:\n" ); 
                else 
                    Print( "pre-crossed square with pre-crossed modules:\n" ); 
                fi; 
            fi;
            Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
            Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
            Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
            Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
        fi;
    fi;
end );

InstallMethod( ViewObj, "method for a 3d-group", true, 
    [ IsPreCrossedSquare ], 0, PrintObj ); 

#############################################################################
##
#M Display( <g3d> . . . . . . . . . . . . . . . . . . . . display a 3d-group 
##
InstallMethod( DisplayLeadMaps, "method for a pre-cat2-group", true, 
    [ IsPreCat2Group ], 0,
function( g3d )

    local up, tup, hup, lt, tlt, hlt; 

    up := Up2DimensionalGroup( g3d ); 
    tup := TailMap( up );
    hup := HeadMap( up ); 
    lt := Left2DimensionalGroup( g3d ); 
    tlt := TailMap( lt );
    hlt := HeadMap( lt ); 
    Print( "(pre-)cat2-group with up-left group: ", 
           MappingGeneratorsImages( tup )[1], "\n" ); 
    if ( tup = hup ) then 
        Print( "   up tail=head images: ", 
           MappingGeneratorsImages( tup )[2], "\n" );  
    else 
        Print( "   up tail/head images: ", 
           MappingGeneratorsImages( tup )[2], ", ", 
           MappingGeneratorsImages( hup )[2], "\n" );  
    fi; 
    if ( tlt = hlt ) then 
        Print( " left tail=head images: ", 
           MappingGeneratorsImages( tlt )[2], "\n" );  
    else 
        Print( " left tail/head images: ", 
           MappingGeneratorsImages( tlt )[2], ", ", 
           MappingGeneratorsImages( hlt )[2], "\n" );  
    fi;
end );

#############################################################################
##
#M Display( <g3d> . . . . . . . . . . . . . . . . . . . . display a 3d-group 
##
InstallMethod( Display, "method for a pre-cat2-group", true, 
    [ IsPreCat2Group ], 0,
function( g3d )

    local up, tup, hup, lt, tlt, hlt, rt, trt, hrt, dn, tdn, hdn; 

    up := Up2DimensionalGroup( g3d ); 
    tup := TailMap( up );
    hup := HeadMap( up ); 
    lt := Left2DimensionalGroup( g3d ); 
    tlt := TailMap( lt );
    hlt := HeadMap( lt ); 
    rt := Right2DimensionalGroup( g3d ); 
    trt := TailMap( rt );
    hrt := HeadMap( rt ); 
    dn := Down2DimensionalGroup( g3d ); 
    tdn := TailMap( dn );
    hdn := HeadMap( dn ); 
    Print( "(pre-)cat2-group with groups: ", 
           [ Source(up), Range(up), Range(lt), Range(dn) ], "\n" );
    if ( tup = hup ) then 
        Print( "   up tail=head: ", 
           MappingGeneratorsImages( tup ), "\n" );  
    else 
        Print( "   up tail/head: ", 
           MappingGeneratorsImages( tup ),  
           MappingGeneratorsImages( hup ), "\n" );  
    fi; 
    if ( tlt = hlt ) then 
        Print( " left tail=head: ", 
           MappingGeneratorsImages( tlt ), "\n" );  
    else 
        Print( " left tail/head: ", 
           MappingGeneratorsImages( tlt ),  
           MappingGeneratorsImages( hlt ), "\n" );  
    fi;
    if ( trt = hrt ) then 
        Print( "right tail=head: ", 
           MappingGeneratorsImages( trt ), "\n" );  
    else 
        Print( "right tail/head: ", 
           MappingGeneratorsImages( trt ),  
           MappingGeneratorsImages( hrt ), "\n" ); 
    fi; 
    if ( tdn = hdn ) then 
        Print( " down tail=head: ", 
           MappingGeneratorsImages( tdn ), "\n" );  
    else 
        Print( " down tail/head: ", 
           MappingGeneratorsImages( tdn ),  
           MappingGeneratorsImages( hdn ), "\n" ); 
    fi; 
end );

InstallMethod( Display, "method for a pre-crossed square", true, 
    [ IsPreCrossedSquare ], 0,
function( g3d )

    local L, M, N, P, lenL, lenM, lenN, lenP, len1, len2, j, q1, q2, 
          ispsq, arrow, ok, n, i;

    arrow := " -> "; 
    L := Source( Up2DimensionalGroup( g3d ) );
    M := Range( Up2DimensionalGroup( g3d ) );
    N := Source( Down2DimensionalGroup( g3d ) );
    P := Range( Down2DimensionalGroup( g3d ) );
    ok := HasName(L) and HasName(M) and HasName(N) and HasName(P);
    if ok then
        lenL := Length( Name( L ) );
        lenM := Length( Name( M ) );
        lenN := Length( Name( N ) );
        lenP := Length( Name( P ) );
        len1 := Maximum( lenL, lenN );
        len2 := Maximum( lenM, lenP );
        q1 := QuoInt( len1, 2 );
        q2 := QuoInt( len2, 2 );
        Print( "[ " );
        for j in [1..lenN-lenL] do Print(" "); od;
        Print( Name(L), arrow, Name(M) );
        for j in [1..lenP-lenM] do Print(" "); od;
        Print( " ]\n[ " );
        for j in [1..q1] do Print(" "); od;
        Print( "|" );
        for j in [1..q1+RemInt(len1,2)+2+q2+RemInt(len2,2)] do 
            Print(" "); 
        od;
        Print( "|" );
        for j in [1..q2] do Print(" "); od;
        Print( " ]\n" );
        Print( "[ " );
        for j in [1..lenL-lenN] do Print(" "); od;
        Print( Name(N), " -> ", Name(P) );
        for j in [1..lenM-lenP] do Print(" "); od;
        Print( " ]\n" );
    else 
        Print( "(pre-)crossed square with (pre-)crossed modules:\n" ); 
        Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
        Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
        Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
        Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
    fi; 
end );

#############################################################################
##
#M  IsPreCat2Group . . . . . . . . . . .  check that this is a pre-cat2-group
#M  IsCat2Group . . . . . . . . . . . . check that the object is a cat2-group
##
InstallMethod( IsPreCat2Group, "generic method for a pre-cat2-group",
    true, [ IsHigherDimensionalGroup ], 0,
function( P )
    if not ( IsPreCatnObj( P )  ) then
        return false;
    fi;
    if ( IsPreCatnGroup( P ) and ( HigherDimension( P ) = 3 )  ) then
        return true;
    else 
        return false;
    fi;
end );

InstallMethod( IsCat2Group, "generic method for a cat2-group",
    true, [ IsHigherDimensionalGroup ], 0, 
function( P ) 
    local ok; 
    ok := ( HigherDimension( P ) = 3 ) 
        and IsCat1Group( Up2DimensionalGroup( P ) )  
        and IsCat1Group( Left2DimensionalGroup( P ) )  
        and IsCat1Group( Right2DimensionalGroup( P ) )  
        and IsCat1Group( Down2DimensionalGroup( P ) ) 
        and IsPreCat1Group( Diagonal2DimensionalGroup( P ) ); 
    return ok; 
end ); 

#############################################################################
##
#M  PreCat2GroupObj( [<up>,<left>,<right>,<down>,<diag>] ) 
##
InstallMethod( PreCat2GroupObj, "for a list of pre-cat1-groups", true,
    [ IsList ], 0,
function( L )

    local PC, ok;

    if not ( Length( L ) = 5 ) then 
        Error( "there should be 5 pre-cat1-groups in the list L" ); 
    fi; 
    PC := rec();
    ObjectifyWithAttributes( PC, PreCat2GroupObjType, 
      Up2DimensionalGroup, L[1], 
      Left2DimensionalGroup, L[2],
      Right2DimensionalGroup, L[3],
      Down2DimensionalGroup, L[4],
      Diagonal2DimensionalGroup, L[5], 
      GeneratingCat1Groups, [ L[1], L[2] ],
      HigherDimension, 3, 
      IsHigherDimensionalGroup, true, 
      IsPreCat2Group, true, 
      IsPreCatnGroup, true );
    ok := IsCat2Group( PC );
    return PC;
end );

#############################################################################
##
#F  (Pre)Cat2Group( [ up, lt (,diag) )  cat2-group from 2/3 (pre)cat1-groups
#F  (Pre)Cat2Group( XS )                cat2-group from (pre)crossed square
##
InstallGlobalFunction( PreCat2Group, function( arg )

    local nargs, up, left, diag, C2G, G1, G2, G3, isoG, genG, 
          imt, t12, imh, h12, e12, idQ, isoleft, drd, ok; 

    nargs := Length( arg ); 
    if not ( nargs in [1,2] ) then
        Print( "standard usage: (Pre)Cat2Group( up, left );\n" );
        Print( "            or: (Pre)Cat2Group( XS );\n" );
        return fail;
    fi; 
    if ( nargs = 1 ) then 
        C2G := PreCat2GroupOfPreCrossedSquare( arg[1] );
    else 
        up := arg[1]; 
        left := arg[2];
        G1 := Source( up ); 
        G2 := Source( left );         
        ## if the two sources are unequal but isomorphic then make 
        ## an isomorphic copy of left with the same source as up
        if not ( G1 = G2 ) then 
            isoG := IsomorphismGroups( G2, G1 ); 
            if ( isoG = fail ) then 
                Error( "the two arguments do now have the same source" ); 
            else 
                idQ := IdentityMapping( Range( left ) ); 
                isoleft := IsomorphismByIsomorphisms( left, [ isoG, idQ ] );
                left := Range( isoleft ); 
            fi; 
        fi; 
        drd := DetermineRemainingCat1Groups( up, left ); 
        if ( drd = fail ) then 
            Info( InfoXMod, 2, "failure determining remaining cat1-groups" ); 
            return fail; 
        fi;
        C2G := PreCat2GroupByPreCat1Groups( up,left,drd[1],drd[2],drd[3] ); 
        if ( C2G = fail ) then 
            return fail;   ## Error( "C2G fails to be a PreCat2Group" ); 
        fi; 
    fi;
    ok := IsPreCat2Group( C2G );
    if ok then 
        ok := IsCat2Group( C2G ); 
        return C2G;
    else
        return fail;
    fi;
end );

InstallGlobalFunction( Cat2Group, function( arg )

    local C2G, ok; 

    C2G := PreCat2Group( arg[1], arg[2] ); 
    ok := not ( C2G = fail ) and IsCat2Group( C2G ); 
    if ok then 
        return C2G; 
    else 
        return fail; 
    fi; 
end ); 

#############################################################################
##
#M  DetermineRemainingCat1Groups . . . . . . . . . . . for 2 pre-cat1-groups
## 
InstallMethod( DetermineRemainingCat1Groups, "for up, left pre-cat1-groups", 
    true, [ IsPreCat1Group, IsPreCat1Group ], 0,
function( up, left )

    local G, genG, Q, genQ, R, genR, tu, hu, eu, tl, hl, el, tea, hea, 
          P, genP, diag, imtd, td, imhd, hd, imed, ed, down, 
          imtr, tr, imhr, hr, imer, er, right; 

    G := Source( up ); 
    if not ( G = Source( left ) ) then 
        Print( "the two pre-cat1-groups should have the same source\n" ); 
    fi; 
    genG := GeneratorsOfGroup( G ); 
    R := Range( up ); 
    genR := GeneratorsOfGroup( R ); 
    Q := Range( left );
    genQ := GeneratorsOfGroup( Q );
    tu := TailMap( up ); 
    hu := HeadMap( up ); 
    eu := RangeEmbedding( up ); 
    tl := TailMap( left ); 
    hl := HeadMap( left ); 
    el := RangeEmbedding( left ); 
    ## check that the up-maps commute with the lt-maps 
    tea := tu*eu*tl*el; 
    hea := hu*eu*hl*el; 
    if not ( ( tea = tl*el*tu*eu ) 
         and ( hea = hl*el*hu*eu ) 
         and ( tu*eu*hl*el = hl*el*tu*eu ) 
         and ( hu*eu*tl*el = tl*el*hu*eu ) )  then 
        Info( InfoXMod, 2, "up-maps do not commute with lt-maps" ); 
        return fail; 
    fi; 
    Info( InfoXMod, 2, "yes : up-maps do commute with the lt-maps" ); 
    ## more checks? 
    ## determine the group P 
    P := Image( tea ); 
    if not ( Image( hea ) = P ) then 
        Error( "t*e*a and h*e*a do not have the same range" ); 
    fi; 
    genP := GeneratorsOfGroup( P ); 
    diag := PreCat1GroupWithIdentityEmbedding( tea, hea ); 
    if ( diag = fail ) then 
        Print( "diag fails to be a cat1-group\n" ); 
        return fail; 
    fi; 
    ## now construct down 
    imtd := List( genQ, q -> ImageElm( el * tea, q ) ); 
    td := GroupHomomorphismByImages( Q, P, genQ, imtd ); 
    imhd := List( genQ, q -> ImageElm( el * hea, q ) ); 
    hd := GroupHomomorphismByImages( Q, P, genQ, imhd ); 
    imed := List( genP, p -> ImageElm( tl, p ) ); 
    ed := GroupHomomorphismByImages( P, Q, genP, imed ); 
    down := PreCat1GroupByTailHeadEmbedding( td, hd, ed ); 
    ## now construct right 
    imtr := List( genR, r -> ImageElm( eu * tea, r ) ); 
    tr := GroupHomomorphismByImages( R, P, genR, imtr ); 
    imhr := List( genR, r -> ImageElm( eu * hea, r ) ); 
    hr := GroupHomomorphismByImages( R, P, genR, imhr ); 
    imer := List( genP, p -> ImageElm( tu, p ) ); 
    er := GroupHomomorphismByImages( P, R, genP, imer ); 
    right := PreCat1GroupByTailHeadEmbedding( tr, hr, er ); 
    if ( ( right = fail ) or ( down = fail) ) then 
        Info( InfoXMod, 2, "right or down fail to be cat1-groups" ); 
        return fail; 
    fi; 
    return [ right, down, diag ]; 
end ); 

#############################################################################
##
#M  PreCat2GroupByPreCat1Groups . . . . . . . . . . for five pre-cat1-groups
## 
InstallMethod( PreCat2GroupByPreCat1Groups, "for five pre-cat1-groups", 
    true, [ IsPreCat1Group, IsPreCat1Group, IsPreCat1Group, 
            IsPreCat1Group, IsPreCat1Group ], 0,
function( up, left, right, down, diag )

    local G, R, Q, P, genG, tu, hu, tl, hl, tr, hr, td, hd, ta, ha,   
          imtld, imtur, imhld, imhur, dtld, dtur, dhld, dhur, PC2, ok;

    G := Source( up ); 
    genG := GeneratorsOfGroup( G ); 
    R := Range( up ); 
    Q := Range( left );
    P := Range( diag ); 
    if not ( ( G = Source( left ) ) and ( G = Source( diag ) ) 
             and ( R = Source( right ) ) and ( P = Range( right ) ) 
             and ( Q = Source( down ) ) and ( P = Range( down ) ) ) then 
        Info( InfoXMod, 2, "sources and/or ranges do not agree" ); 
        return fail; 
    fi; 
    tu := TailMap( up ); 
    hu := HeadMap( up ); 
    tl := TailMap( left ); 
    hl := HeadMap( left ); 
    tr := TailMap( right ); 
    hr := HeadMap( right ); 
    td := TailMap( down ); 
    hd := HeadMap( down ); 
    ta := TailMap( diag ); 
    ha := HeadMap( diag ); 

    imtld := List( genG, g -> ImageElm( td, ImageElm( tl, g ) ) ); 
    dtld := GroupHomomorphismByImages( G, P, genG, imtld ); 
    imtur := List( genG, g -> ImageElm( tr, ImageElm( tu, g ) ) ); 
    dtur := GroupHomomorphismByImages( G, P, genG, imtur ); 
    imhld := List( genG, g -> ImageElm( hd, ImageElm( hl, g ) ) ); 
    dhld := GroupHomomorphismByImages( G, P, genG, imhld ); 
    imhur := List( genG, g -> ImageElm( hr, ImageElm( hu, g ) ) ); 
    dhur := GroupHomomorphismByImages( G, P, genG, imhur ); 
    if not ( ( dtld = ta ) and ( dtur= ta ) 
             and ( dhld = ha ) and ( dhur = ha ) ) then 
        Info( InfoXMod, 2, "tail and head maps are inconsistent" ); 
        return fail; 
    fi; 
    PC2 := PreCat2GroupObj( [ up, left, right, down, diag ] );
    SetIsPreCat2Group( PC2, true );
    SetIsPreCatnGroup( PC2, true ); 
    SetHigherDimension( PC2, 3 ); 
    ok := IsCat2Group( PC2 ); 
    ok := IsPreCatnGroupWithIdentityEmbeddings( PC2 ); 
    return PC2;
end ); 

#############################################################################
##
#M  Subdiagonal2DimensionalGroup . . . . . . . . . . . . for a pre-cat2-group
## 
InstallMethod( Subdiagonal2DimensionalGroup, "for a pre-cat2-group", 
    true, [ IsPreCat2Group ], 0,
function( cat2 )

    local  up, ktup, lt, ktlt, L, genL, dg, edg, mgiedg, P, genPL, PL, sdg; 

    up := Up2DimensionalGroup( cat2 );
    lt := Left2DimensionalGroup( cat2 );
    L := Intersection( Kernel( TailMap( up ) ), Kernel( TailMap( lt ) ) ); 
    genL := GeneratorsOfGroup( L );
    dg := Diagonal2DimensionalGroup( cat2 ); 
    edg := RangeEmbedding( dg ); 
    mgiedg := MappingGeneratorsImages( edg );
    P := Range( dg ); 
    genPL := Concatenation( genL, GeneratorsOfGroup( Image( edg ) ) ); 
    PL := Subgroup( Source( up ), genPL );
    sdg := PreCat1GroupByTailHeadEmbedding( 
               RestrictedMapping( TailMap( dg ), PL ), 
               RestrictedMapping( HeadMap( dg ), PL ), 
               GroupHomomorphismByImages( P, PL, mgiedg[1], mgiedg[2] ) ); 
    if not IsCat1Group( sdg ) then 
        Error( "expecting sdg to be a cat1-group" ); 
    fi; 
    return sdg;
end ); 

#############################################################################
## 
#M  DirectProductOp . . . . . . . . . . . . . . . . . for two pre-cat2-groups 
## 
InstallOtherMethod( DirectProductOp,
    "method for pre-cat2-groups", true, [ IsList, IsPreCat2Group ], 0,
function( list, A )

    local C, i, B, upA, ltA, rtA, dnA, dgA, upB, ltB, rtB, dnB, dgB, 
          G, R, Q, P, eG1, eG2, eR1, eR2, eQ1, eQ2, eP1, eP2, 
          mtuA, mhuA, meuA, mtuB, mhuB, meuB, mtlA, mhlA, melA, 
          mtlB, mhlB, melB, mtrA, mhrA, merA, mtrB, mhrB, merB, 
          mtdA, mhdA, medA, mtdB, mhdB, medB, mtgA, mhgA, megA, 
          mtgB, mhgB, megB, mtuC, mhuC, meuC, tuC, huC, euC, upC, 
          mtlC, mhlC, melC, tlC, hlC, elC, ltC, mtrC, mhrC, merC, 
          trC, hrC, erC, rtC, mtdC, mhdC, medC, tdC, hdC, edC, dnC, 
          mtgC, mhgC, megC, tgC, hgC, egC, dgC, 
          eupA, eupB, eltA, eltB, embA, embB, pG1, pG2, 
          pR1, pR2, pQ1, pQ2, pupA, pupB, pltA, pltB, proA, proB, info;

    if not ( list[1] = A ) then
        Error( "second argument should be first entry in first argument list" );
    fi;
    if ( Length( list ) > 2 ) then 
        C := DirectProductOp( [ A, list[2] ], A ); 
        for i in [3..Length(list)] do
            C := DirectProductOp( [ C, list[i] ], C ); 
        od; 
        return C; 
    fi;
    B := list[2]; 
    upA := Up2DimensionalGroup( A ); 
    ltA := Left2DimensionalGroup( A ); 
    rtA := Right2DimensionalGroup( A ); 
    dnA := Down2DimensionalGroup( A ); 
    dgA := Diagonal2DimensionalGroup( A ); 
    upB := Up2DimensionalGroup( B ); 
    ltB := Left2DimensionalGroup( B ); 
    rtB := Right2DimensionalGroup( B ); 
    dnB := Down2DimensionalGroup( B ); 
    dgB := Diagonal2DimensionalGroup( B ); 
    G := DirectProductOp( [ Source(upA), Source(upB) ], Source(upA) ); 
    R := DirectProductOp( [ Range(upA), Range(upB) ], Range(upA) ); 
    Q := DirectProductOp( [ Source(dnA), Source(dnB) ], Source(dnA) ); 
    P := DirectProductOp( [ Range(dnA), Range(dnB) ], Range(dnA) ); 
    eG1 := Embedding( G, 1 ); 
    eG2 := Embedding( G, 2 ); 
    eR1 := Embedding( R, 1 ); 
    eR2 := Embedding( R, 2 ); 
    eQ1 := Embedding( Q, 1 ); 
    eQ2 := Embedding( Q, 2 ); 
    eP1 := Embedding( P, 1 ); 
    eP2 := Embedding( P, 2 ); 
    ## construct the up cat2-group for C
    mtuA := MappingGeneratorsImages( TailMap( upA ) ); 
    mtuA := [ List( mtuA[1], x -> ImageElm( eG1, x ) ), 
              List( mtuA[2], x -> ImageElm( eR1, x ) ) ];
    mtuB := MappingGeneratorsImages( TailMap( upB ) ); 
    mtuB := [ List( mtuB[1], x -> ImageElm( eG2, x ) ), 
              List( mtuB[2], x -> ImageElm( eR2, x ) ) ];
    mtuC := [ Concatenation(mtuA[1],mtuB[1]), Concatenation(mtuA[2],mtuB[2]) ];
    tuC := GroupHomomorphismByImages( G, R, mtuC[1], mtuC[2] ); 
    mhuA := MappingGeneratorsImages( HeadMap( upA ) ); 
    mhuA := [ List( mhuA[1], x -> ImageElm( eG1, x ) ), 
              List( mhuA[2], x -> ImageElm( eR1, x ) ) ];
    mhuB := MappingGeneratorsImages( HeadMap( upB ) ); 
    mhuB := [ List( mhuB[1], x -> ImageElm( eG2, x ) ), 
              List( mhuB[2], x -> ImageElm( eR2, x ) ) ];
    mhuC := [ Concatenation(mhuA[1],mhuB[1]), 
              Concatenation(mhuA[2],mhuB[2]) ];
    huC := GroupHomomorphismByImages( G, R, mhuC[1], mhuC[2] ); 
    meuA := MappingGeneratorsImages( RangeEmbedding( upA ) ); 
    meuA := [ List( meuA[1], x -> ImageElm( eR1, x ) ), 
              List( meuA[2], x -> ImageElm( eG1, x ) ) ];
    meuB := MappingGeneratorsImages( RangeEmbedding( upB ) ); 
    meuB := [ List( meuB[1], x -> ImageElm( eR2, x ) ), 
              List( meuB[2], x -> ImageElm( eG2, x ) ) ];
    meuC := [ Concatenation(meuA[1],meuB[1]), 
              Concatenation(meuA[2],meuB[2]) ];
    euC := GroupHomomorphismByImages( R, G, meuC[1], meuC[2] ); 
    upC := PreCat1GroupByTailHeadEmbedding( tuC, huC, euC );
    ## construct the left cat2-group for C
    mtlA := MappingGeneratorsImages( TailMap( ltA ) ); 
    mtlA := [ List( mtlA[1], x -> ImageElm( eG1, x ) ), 
              List( mtlA[2], x -> ImageElm( eQ1, x ) ) ];
    mtlB := MappingGeneratorsImages( TailMap( ltB ) ); 
    mtlB := [ List( mtlB[1], x -> ImageElm( eG2, x ) ), 
              List( mtlB[2], x -> ImageElm( eQ2, x ) ) ];
    mtlC := [ Concatenation(mtlA[1],mtlB[1]), 
              Concatenation(mtlA[2],mtlB[2]) ];
    tlC := GroupHomomorphismByImages( G, Q, mtlC[1], mtlC[2] ); 
    mhlA := MappingGeneratorsImages( HeadMap( ltA ) ); 
    mhlA := [ List( mhlA[1], x -> ImageElm( eG1, x ) ), 
              List( mhlA[2], x -> ImageElm( eQ1, x ) ) ];
    mhlB := MappingGeneratorsImages( HeadMap( ltB ) ); 
    mhlB := [ List( mhlB[1], x -> ImageElm( eG2, x ) ), 
              List( mhlB[2], x -> ImageElm( eQ2, x ) ) ];
    mhlC := [ Concatenation(mhlA[1],mhlB[1]), 
              Concatenation(mhlA[2],mhlB[2]) ];
    hlC := GroupHomomorphismByImages( G, Q, mhlC[1], mhlC[2] ); 
    melA := MappingGeneratorsImages( RangeEmbedding( ltA ) ); 
    melA := [ List( melA[1], x -> ImageElm( eQ1, x ) ), 
              List( melA[2], x -> ImageElm( eG1, x ) ) ];
    melB := MappingGeneratorsImages( RangeEmbedding( ltB ) ); 
    melB := [ List( melB[1], x -> ImageElm( eQ2, x ) ), 
              List( melB[2], x -> ImageElm( eG2, x ) ) ];
    melC := [ Concatenation(melA[1],melB[1]), 
              Concatenation(melA[2],melB[2]) ];
    elC := GroupHomomorphismByImages( Q, G, melC[1], melC[2] ); 
    ltC := PreCat1GroupByTailHeadEmbedding( tlC, hlC, elC );
    ## construct the right cat2-group for C
    mtrA := MappingGeneratorsImages( TailMap( rtA ) ); 
    mtrA := [ List( mtrA[1], x -> ImageElm( eR1, x ) ), 
              List( mtrA[2], x -> ImageElm( eP1, x ) ) ];
    mtrB := MappingGeneratorsImages( TailMap( rtB ) ); 
    mtrB := [ List( mtrB[1], x -> ImageElm( eR2, x ) ), 
              List( mtrB[2], x -> ImageElm( eP2, x ) ) ];
    mtrC := [ Concatenation(mtrA[1],mtrB[1]), 
              Concatenation(mtrA[2],mtrB[2]) ];
    trC := GroupHomomorphismByImages( R, P, mtrC[1], mtrC[2] ); 
    mhrA := MappingGeneratorsImages( HeadMap( rtA ) ); 
    mhrA := [ List( mhrA[1], x -> ImageElm( eR1, x ) ), 
              List( mhrA[2], x -> ImageElm( eP1, x ) ) ];
    mhrB := MappingGeneratorsImages( HeadMap( rtB ) ); 
    mhrB := [ List( mhrB[1], x -> ImageElm( eR2, x ) ), 
              List( mhrB[2], x -> ImageElm( eP2, x ) ) ];
    mhrC := [ Concatenation(mhrA[1],mhrB[1]), 
              Concatenation(mhrA[2],mhrB[2]) ];
    hrC := GroupHomomorphismByImages( R, P, mhrC[1], mhrC[2] ); 
    merA := MappingGeneratorsImages( RangeEmbedding( rtA ) ); 
    merA := [ List( merA[1], x -> ImageElm( eP1, x ) ), 
              List( merA[2], x -> ImageElm( eR1, x ) ) ];
    merB := MappingGeneratorsImages( RangeEmbedding( rtB ) ); 
    merB := [ List( merB[1], x -> ImageElm( eP2, x ) ), 
              List( merB[2], x -> ImageElm( eR2, x ) ) ];
    merC := [ Concatenation(merA[1],merB[1]), 
              Concatenation(merA[2],merB[2]) ];
    erC := GroupHomomorphismByImages( P, R, merC[1], merC[2] ); 
    rtC := PreCat1GroupByTailHeadEmbedding( trC, hrC, erC );
    ## construct the down cat2-group for C
    mtdA := MappingGeneratorsImages( TailMap( dnA ) ); 
    mtdA := [ List( mtdA[1], x -> ImageElm( eQ1, x ) ), 
              List( mtdA[2], x -> ImageElm( eP1, x ) ) ];
    mtdB := MappingGeneratorsImages( TailMap( dnB ) ); 
    mtdB := [ List( mtdB[1], x -> ImageElm( eQ2, x ) ), 
              List( mtdB[2], x -> ImageElm( eP2, x ) ) ];
    mtdC := [ Concatenation(mtdA[1],mtdB[1]), 
              Concatenation(mtdA[2],mtdB[2]) ];
    tdC := GroupHomomorphismByImages( Q, P, mtdC[1], mtdC[2] ); 
    mhdA := MappingGeneratorsImages( HeadMap( dnA ) ); 
    mhdA := [ List( mhdA[1], x -> ImageElm( eQ1, x ) ), 
              List( mhdA[2], x -> ImageElm( eP1, x ) ) ];
    mhdB := MappingGeneratorsImages( HeadMap( dnB ) ); 
    mhdB := [ List( mhdB[1], x -> ImageElm( eQ2, x ) ), 
              List( mhdB[2], x -> ImageElm( eP2, x ) ) ];
    mhdC := [ Concatenation(mhdA[1],mhdB[1]), 
              Concatenation(mhdA[2],mhdB[2]) ];
    hdC := GroupHomomorphismByImages( Q, P, mhdC[1], mhdC[2] ); 
    medA := MappingGeneratorsImages( RangeEmbedding( dnA ) ); 
    medA := [ List( medA[1], x -> ImageElm( eP1, x ) ), 
              List( medA[2], x -> ImageElm( eQ1, x ) ) ];
    medB := MappingGeneratorsImages( RangeEmbedding( dnB ) ); 
    medB := [ List( medB[1], x -> ImageElm( eP2, x ) ), 
              List( medB[2], x -> ImageElm( eQ2, x ) ) ];
    medC := [ Concatenation(medA[1],medB[1]), 
              Concatenation(medA[2],medB[2]) ];
    edC := GroupHomomorphismByImages( P, Q, medC[1], medC[2] ); 
    dnC := PreCat1GroupByTailHeadEmbedding( tdC, hdC, edC );
    ## construct the diagonal cat2-group for C
    mtgA := MappingGeneratorsImages( TailMap( dgA ) ); 
    mtgA := [ List( mtgA[1], x -> ImageElm( eG1, x ) ), 
              List( mtgA[2], x -> ImageElm( eP1, x ) ) ];
    mtgB := MappingGeneratorsImages( TailMap( dgB ) ); 
    mtgB := [ List( mtgB[1], x -> ImageElm( eG2, x ) ), 
              List( mtgB[2], x -> ImageElm( eP2, x ) ) ];
    mtgC := [ Concatenation(mtgA[1],mtgB[1]), 
              Concatenation(mtgA[2],mtgB[2]) ];
    tgC := GroupHomomorphismByImages( G, P, mtgC[1], mtgC[2] ); 
    mhgA := MappingGeneratorsImages( HeadMap( dgA ) ); 
    mhgA := [ List( mhgA[1], x -> ImageElm( eG1, x ) ), 
              List( mhgA[2], x -> ImageElm( eP1, x ) ) ];
    mhgB := MappingGeneratorsImages( HeadMap( dgB ) ); 
    mhgB := [ List( mhgB[1], x -> ImageElm( eG2, x ) ), 
              List( mhgB[2], x -> ImageElm( eP2, x ) ) ];
    mhgC := [ Concatenation(mhgA[1],mhgB[1]), 
              Concatenation(mhgA[2],mhgB[2]) ];
    hgC := GroupHomomorphismByImages( G, P, mhgC[1], mhgC[2] ); 
    megA := MappingGeneratorsImages( RangeEmbedding( dgA ) ); 
    megA := [ List( megA[1], x -> ImageElm( eP1, x ) ), 
              List( megA[2], x -> ImageElm( eG1, x ) ) ];
    megB := MappingGeneratorsImages( RangeEmbedding( dgB ) ); 
    megB := [ List( megB[1], x -> ImageElm( eP2, x ) ), 
              List( megB[2], x -> ImageElm( eG2, x ) ) ];
    megC := [ Concatenation(megA[1],megB[1]), 
              Concatenation(megA[2],megB[2]) ];
    egC := GroupHomomorphismByImages( P, G, megC[1], megC[2] ); 
    dgC := PreCat1GroupByTailHeadEmbedding( tgC, hgC, egC );
    C := PreCat2GroupByPreCat1Groups( upC, ltC, rtC, dnC, dgC ); 
    ## now for the embeddings and projections 
    eupA := PreCat1GroupMorphismByGroupHomomorphisms( upA, upC, eG1, eR1 );
    eupB := PreCat1GroupMorphismByGroupHomomorphisms( upB, upC, eG2, eR2 );
    eltA := PreCat1GroupMorphismByGroupHomomorphisms( ltA, ltC, eG1, eQ1 );
    eltB := PreCat1GroupMorphismByGroupHomomorphisms( ltB, ltC, eG2, eQ2 );
    embA := PreCat2GroupMorphismByPreCat1GroupMorphisms( A, C, eupA, eltA ); 
    embB := PreCat2GroupMorphismByPreCat1GroupMorphisms( B, C, eupB, eltB ); 
    pG1 := Projection( G, 1 ); 
    pG2 := Projection( G, 2 ); 
    pR1 := Projection( R, 1 ); 
    pR2 := Projection( R, 2 ); 
    pQ1 := Projection( Q, 1 ); 
    pQ2 := Projection( Q, 2 ); 
    pupA := PreCat1GroupMorphismByGroupHomomorphisms( upC, upA, pG1, pR1 );
    pupB := PreCat1GroupMorphismByGroupHomomorphisms( upC, upB, pG2, pR2 );
    pltA := PreCat1GroupMorphismByGroupHomomorphisms( ltC, ltA, pG1, pQ1 );
    pltB := PreCat1GroupMorphismByGroupHomomorphisms( ltC, ltB, pG2, pQ2 );
    proA := PreCat2GroupMorphismByPreCat1GroupMorphisms( C, A, pupA, pltA ); 
    proB := PreCat2GroupMorphismByPreCat1GroupMorphisms( C, B, pupB, pltB ); 
    info := rec( embeddings := [ embA, embB ], 
                 objects := list, 
                 projections := [ proA, proB ] ); 
    SetDirectProductInfo( C, info ); 
    return C;
end );

#############################################################################
##
#M  AllCat2GroupsWithImagesIterator . . .at2-groups with given up,left range
#M  DoAllCat2GroupsWithImagesIterator 
#M  AllCat2GroupsWithImages . . cat2-groups with specified range for up,left
#M  AllCat2GroupsWithImagesNumber . # cat2-groups with specified up,left gps
#M  AllCat2GroupsWithImagesUpToIsomorphism . . iso class reps of cat2-groups
##
BindGlobal( "NextIterator_AllCat2GroupsWithImages", function ( iter ) 

    local ok, pair, C; 

    ok := false; 
    while ( not ok ) and ( not IsDoneIterator( iter ) ) do 
        pair := NextIterator( iter!.pairsIterator ); 
        Info( InfoXMod, 1, pair ); 
        if ( fail in pair ) then 
            return fail; 
        fi; 
        C := Cat2Group( pair[1], pair[2] ); 
        if ( not ( C = fail ) and IsCat2Group( C ) ) then 
            return C; 
        fi; 
    od; 
    return fail;
end ); 

BindGlobal( "IsDoneIterator_AllCat2GroupsWithImages", 
    iter -> IsDoneIterator( iter!.pairsIterator ) 
); 

BindGlobal( "ShallowCopy_AllCat2GroupsWithImages", 
    iter -> rec(      group := iter!.group, 
              pairsIterator := ShallowCopy( iter!.pairsIterator ) 
    )  
); 

InstallGlobalFunction( "DoAllCat2GroupsWithImagesIterator", 
function( G, R, Q )

    local upIterator, pairsIterator, ltIterator, iter;

    upIterator := AllCat1GroupsWithImageIterator( G, R ); 
    if ( R = Q ) then 
        pairsIterator := UnorderedPairsIterator( upIterator ); 
    else 
           ltIterator := AllCat1GroupsWithImageIterator( G, Q ); 
        pairsIterator := CartesianIterator( upIterator, ltIterator ); 
    fi;
    iter := IteratorByFunctions( 
        rec(      group := G, 
          pairsIterator := ShallowCopy( pairsIterator ),
           NextIterator := NextIterator_AllCat2GroupsWithImages, 
         IsDoneIterator := IsDoneIterator_AllCat2GroupsWithImages, 
            ShallowCopy := ShallowCopy_AllCat2GroupsWithImages ) ); 
    return iter;
end );

InstallMethod( AllCat2GroupsWithImagesIterator, 
    "for a group and two subgroups", [ IsGroup, IsGroup, IsGroup ], 0, 
function ( G, R, Q ) 
    if not ( IsSubgroup( G, R ) and IsSubgroup( G, Q ) ) then 
        Error( "R,Q are not subgroups of G" ); 
    fi; 
    return DoAllCat2GroupsWithImagesIterator( G, R, Q ); 
end ); 

InstallMethod( AllCat2GroupsWithImages, 
    "for a group and two subgroups", [ IsGroup, IsGroup, IsGroup ], 0, 
function ( G, R, Q ) 

    local L0, C; 

    if not ( IsSubgroup( G, R ) and IsSubgroup( G, Q ) ) then 
        Error( "R,Q are not subgroups of G" ); 
    fi; 
    L0 := [ ]; 
    for C in AllCat2GroupsWithImagesIterator( G, R, Q ) do 
        if not ( C = fail ) then 
            Add( L0, C ); 
        fi; 
    od;
    return L0; 
end ); 

InstallMethod( AllCat2GroupsWithImagesNumber, 
    "for a group and two subgroups", [ IsGroup, IsGroup, IsGroup ], 0, 
function ( G, R, Q ) 

    local num, C; 

    if not ( IsSubgroup( G, R ) and IsSubgroup( G, Q ) ) then 
        Error( "R,Q are not subgroups of G" ); 
    fi; 
    num := 0;
    for C in AllCat2GroupsWithImagesIterator( G, R, Q ) do 
        if not ( C = fail ) then 
            num := num+1;
        fi; 
    od;
    return num; 
end ); 

InstallMethod( AllCat2GroupsWithImagesUpToIsomorphism, 
    "for a group and two subgroups", [ IsGroup, IsGroup, IsGroup ], 0, 
function ( G, R, Q ) 

    local L0, len0, num, GRiter, upiter, up, GQiter, leftiter, left, 
          C, j, found, iso; 

    if not ( IsSubgroup( G, R ) and IsSubgroup( G, Q ) ) then 
        Error( "R,Q are not subgroups of G" ); 
    fi; 
    L0 := [ ]; 
    len0 := 0; 
    num := 0; 
    GRiter := AllCat1GroupsWithImageIterator( G, R ); 
    if ( Q = R ) then 
        GQiter := ShallowCopy( GRiter ); 
    else 
        GQiter := AllCat1GroupsWithImageIterator( G, Q ); 
    fi;
    upiter := ShallowCopy( GRiter ); 
    up := 0; 
    while not IsDoneIterator( upiter ) do 
        up := NextIterator( upiter ); 
        if not ( up = fail ) then 
            leftiter := ShallowCopy( GQiter ); 
            left := 0;
            while not IsDoneIterator( leftiter ) do 
                left := NextIterator( leftiter ); 
                if not ( left = fail ) then 
                    C := PreCat2Group( up, left ); 
                    if ( not ( C = fail ) and IsCat2Group( C ) ) then 
                        num := num+1; 
                        j := 0; 
                        found := false; 
                        while ( not found ) and ( j < len0 ) do 
                            j := j+1; 
                            iso := IsomorphismPreCat2Groups( C, L0[j] );
                            if not ( iso = fail ) then 
                                found := true; 
                            fi; 
                        od; 
                        if not found then 
                            Add( L0, C ); 
                            len0 := len0+1; 
                            if ( InfoLevel( InfoXMod ) > 0 ) then 
                                Display( C ); 
                                Print( "---------------------------------\n" ); 
                            fi; 
                        fi; 
                    fi; 
                fi;
            od; 
        fi; 
    od; 
    Info( InfoXMod, 1, "cat2-groups: ", num, " found, ", len0, " classes" ); 
    return L0; 
end ); 

#############################################################################
##
#M  AllCat2GroupsWithFixedUpAndLeftRange . .  cat2-groups with given up and R 
#M  AllCat2GroupsWithFixedUp . . . . . . . cat2-groups with specified up cat1 
##
InstallMethod( AllCat2GroupsWithFixedUpAndLeftRange, 
    "for a group and and a cat1-group", [ IsPreCat1Group, IsGroup ], 0, 
function ( up, R ) 

    local G, L0, C1, C2, iter; 

    G := Source( up );  
    L0 := [ ]; 
    iter := AllCat1GroupsWithImageIterator( G, R );
    while not IsDoneIterator( iter ) do 
        C1 := NextIterator( iter ); 
        C2 := Cat2Group( up, C1 ); 
        if ( not ( C2 = fail ) and IsCat2Group( C2 ) ) then 
            Add( L0, C2 ); 
        fi; 
    od;
    return L0; 
end ); 

InstallMethod( AllCat2GroupsWithFixedUp, 
    "for a cat1-group", [ IsPreCat1Group ], 0, 
function ( up ) 

    local G, L0, C1, C2, iter; 

    G := Source( up );  
    L0 := [ ]; 
    iter := AllCat1GroupsIterator( G );
    while not IsDoneIterator( iter ) do 
        C1 := NextIterator( iter ); 
        C2 := Cat2Group( up, C1 ); 
        if ( not ( C2 = fail ) and IsCat2Group( C2 ) ) then 
            Add( L0, C2 ); 
        fi; 
    od;
    return L0; 
end ); 

#############################################################################
##
#M  AllCat2Groups . . . . . . list of cat2-group structures on a given group
#O  AllCat2GroupsIterator( <gp> ) . . . . . . iterator for the previous list
#F  NextIterator_AllCat2Groups( <iter> ) 
#F  IsDoneIterator_AllCat2Groups( <iter> ) 
#F  ShallowCopy_AllCat2Groups( <iter> ) 
#M  AllCat2GroupsMatrix  . . . . . . . . 0-1 matrix indexed by AllCat1Groups
#A  AllCat2GroupsNumber( <gp> ) . . . . . . . .  number of these cat2-groups
#M  AllCat2GroupsUpToIsomorphism . . iso class reps of cat2-group structures
##
BindGlobal( "NextIterator_AllCat2Groups", function ( iter ) 
    local pair, next; 
    if IsDoneIterator( iter!.imagesIterator ) then 
        pair := NextIterator( iter!.pairsIterator ); 
        iter!.imagesIterator := 
            AllCat2GroupsWithImagesIterator( iter!.group, pair[1], pair[2] ); 
    fi; 
    next := NextIterator( iter!.imagesIterator ); 
    if ( next <> fail ) then 
        iter!.count := iter!.count + 1;
    fi;
    return next;   
end ); 

BindGlobal( "IsDoneIterator_AllCat2Groups", 
    iter -> ( IsDoneIterator( iter!.pairsIterator ) 
              and IsDoneIterator( iter!.imagesIterator ) ) 
); 

BindGlobal( "ShallowCopy_AllCat2Groups", 
    iter -> rec( group := iter!.group, 
                 count := iter!.count, 
         pairsIterator := ShallowCopy( iter!.pairsIterator ), 
        imagesIterator := ShallowCopy( iter!.imagesIterator ) 
    )  
); 

BindGlobal( "DoAllCat2GroupsIterator", 
function( G )

    local subsIterator, pairsIterator, imagesIterator, iter;

    subsIterator := AllSubgroupsIterator( G ); 
    pairsIterator := UnorderedPairsIterator( subsIterator ); 
    imagesIterator := IteratorList( [ ] );
    iter := IteratorByFunctions( 
        rec(     group := G, 
                 count := 0, 
          subsIterator := ShallowCopy( subsIterator ), 
         pairsIterator := ShallowCopy( pairsIterator ),  
        imagesIterator := ShallowCopy( imagesIterator ), 
          NextIterator := NextIterator_AllCat2Groups, 
        IsDoneIterator := IsDoneIterator_AllCat2Groups, 
           ShallowCopy := ShallowCopy_AllCat2Groups ) ); 
    return iter;
end );

InstallMethod( AllCat2GroupsIterator, "for a group", [ IsGroup ], 0, 
    G -> DoAllCat2GroupsIterator( G ) ); 

InstallMethod( AllCat2Groups, "for a group", [ IsGroup ], 0, 
function( G ) 

    local L, omit, pairs, all1, C, genC, predg; 

    InitCatnGroupRecords( G ); 
    predg := 0; 
    L := [ ]; 
    omit := CatnGroupLists( G ).omit; 
    if not omit then 
        all1 := AllCat1Groups( G ); 
        pairs := [ ];
    fi; 
    for C in AllCat2GroupsIterator( G ) do 
        if not ( C = fail ) then 
            Add( L, C ); 
            if not omit then 
                genC := GeneratingCat1Groups( C ); 
                if not IsCat1Group( Diagonal2DimensionalGroup( C ) ) then 
                    predg := predg + 1; 
                fi; 
                Add( pairs, 
                    [ Position( all1, genC[1] ), Position( all1, genC[2] ) ] );
            fi; 
        fi; 
    od;
    if not IsBound( CatnGroupNumbers( G ).cat2 ) then 
        CatnGroupNumbers( G ).cat2 := Length( L ); 
        CatnGroupNumbers( G ).predg := predg; 
    fi; 
    if not omit then 
        Sort( pairs ); 
        CatnGroupLists( G ).cat2pairs := pairs; 
    fi; 
    return L; 
end ); 

InstallMethod( AllCat2GroupsMatrix, "for a group", [ IsGroup ], 0, 
function( G ) 

    local all1, gamma1, L, M, tot, i, j, C; 

    all1 := AllCat1Groups( G ); 
    gamma1 := Length( all1 ); 
    M := List( [1..gamma1], x -> List( [1..gamma1], y -> 0 ) );
    tot := 0; 
    for i in [1..gamma1] do 
        for j in [i..gamma1] do 
            C := Cat2Group( all1[i], all1[j] ); 
            if not ( C = fail ) then 
                tot := tot+1; 
                M[i][j] := 1; 
            fi;
        od; 
    od;
    for i in [2..gamma1] do 
        for j in [1..i-1] do 
            M[i][j] := M[j][i]; 
        od; 
    od;
    Print( "number of cat2-groups found = ", tot, "\n" ); 
    for i in [1..gamma1] do 
        for j in [1..gamma1] do 
            if M[i][j]=1 then Print( "1" ); else Print("."); fi;
        od;
        Print( "\n" );
    od; 
    return M; 
end ); 

InstallMethod( AllCat2GroupsNumber, "for a group", [ IsGroup ], 0, 
function( G ) 

    local n, C, all; 

    InitCatnGroupRecords( G ); 
    if IsBound( CatnGroupNumbers( G ).cat2 ) then 
        return CatnGroupNumbers( G ).cat2; 
    fi; 
    ## not already known, so perform the calculation 
    all := AllCat2Groups( G ); 
    return CatnGroupNumbers( G ).cat2;  
end ); 

InstallMethod( AllCat2GroupsUpToIsomorphism, "iso class reps of cat2-groups", 
    true, [ IsGroup ], 0,
function( G )

    local all1, iso1, omit, classes, L, numL, posL, sisopos,   
          predg, isopredg, pisopos, i, C, k, found, iso, genC, perm; 

    InitCatnGroupRecords( G ); 
    if not IsBound( CatnGroupNumbers( G ).iso1 ) then 
        iso1 := AllCat1GroupsUpToIsomorphism( G ); 
    fi; 
    all1 := AllCat1Groups( G ); 
    omit := CatnGroupLists( G ).omit; 
    if not omit then 
        classes := [ ]; 
    fi; 
    L := [ ];
    numL := 0; 
    posL := [ ]; 
    sisopos := [ ]; 
    predg := 0; 
    isopredg := 0; 
    pisopos := [ ];
    i := 0;
    for C in AllCat2GroupsIterator( G ) do 
        if not ( C = fail ) then 
            genC := GeneratingCat1Groups( C ); 
            i := i+1; 
            if not IsCat1Group( Diagonal2DimensionalGroup( C ) ) then 
                predg := predg + 1; 
            fi; 
            k := 0; 
            found := false; 
            while ( not found ) and ( k < numL ) do 
                k := k+1; 
                iso := IsomorphismCat2Groups( C, L[k] );
                if ( iso <> fail ) then 
                     found := true; 
                     if not omit then 
                         Add( classes[k], [ Position( all1, genC[1] ), 
                                            Position( all1, genC[2] ) ] ); 
                     fi; 
                fi; 
            od; 
            if not found then 
                Add( L, C ); 
                Add( posL, i );
                numL := numL + 1; 
                if ( genC[1] = genC[2] ) then 
                    Add( sisopos, numL ); 
                fi; 
                if not IsCat1Group( Diagonal2DimensionalGroup( C ) ) then 
                    isopredg := isopredg + 1; 
                    Add( pisopos, numL ); 
                fi; 
                if not omit then 
                    Add( classes, 
                      [ [ Position( all1, genC[1] ), 
                          Position( all1, genC[2] ) ] ] );
                fi; 
            fi;
        fi;
    od; 
    if not IsBound( CatnGroupNumbers( G ).cat2 ) then 
        CatnGroupNumbers( G ).cat2 := i; 
    fi; 
    if not IsBound( CatnGroupLists( G ).allcat2pos ) then 
        CatnGroupLists( G ).allcat2pos := posL; 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).iso2 ) then 
        CatnGroupNumbers( G ).iso2 := numL; 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).predg ) then 
        CatnGroupNumbers( G ).predg := predg; 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).isopredg ) then 
        CatnGroupNumbers( G ).isopredg := isopredg; 
        CatnGroupLists( G ).pisopos := pisopos; 
    fi; 
    Info( InfoXMod, 1, "reps found at positions ", posL ); 
    if not omit then 
        perm := Sortex( classes ); 
        L := Permuted( L, perm ); 
        CatnGroupLists( G ).cat2classes := classes; 
        sisopos := List( sisopos, i -> i^perm ); 
        CatnGroupLists( G ).sisopos := sisopos; 
    fi; 
    return L; 
end ); 

InstallMethod( AllCat2GroupFamilies, "gives lists of isomorphic cat2-groups", 
    true, [ IsGroup ], 0,
function( G )

    local reps, cat2, iso2, classes, i, C, k, found, iso; 

    reps := AllCat2GroupsUpToIsomorphism( G ); 
    cat2 := CatnGroupNumbers( G ).cat2; 
    iso2 := CatnGroupNumbers( G ).iso2; 
    classes := ListWithIdenticalEntries( iso2, 0 ); 
    for k in [1..iso2] do 
        classes[k] := [ ]; 
    od; 
    i := 0;
    for C in AllCat2GroupsIterator( G ) do 
        if not ( C = fail ) then 
            i := i+1; 
            k := 0; 
            found := false; 
            while ( not found ) do 
                k := k+1; 
                iso := IsomorphismCat2Groups( C, reps[k] ); 
                if ( iso <> fail ) then 
                    found := true; 
                    Add( classes[k], i ); 
                fi;
            od;
        fi;
    od;
    return classes; 
end ); 

#############################################################################
##
#M  TableRowForCat1Groups  . . . . . . . . cat1-structure data for a group G
#M  TableRowForCat2Groups  . . . . . . . . cat2-structure data for a group G
##
InstallMethod( TableRowForCat1Groups, "for a group G", true, [ IsGroup ], 0,
function( G )

    local Eler, Iler, i, j, allprecat1, allcat1, B;

    allprecat1 := [];
    Eler := AllHomomorphisms( G, G );
    Iler := Filtered( Eler, h -> CompositionMapping( h, h ) = h );
    for i in [1..Length(Iler)] do
        for j in [1..Length(Iler)] do
            if PreCat1GroupWithIdentityEmbedding(Iler[i],Iler[j]) <> fail then 
                Add( allprecat1, 
                     PreCat1GroupWithIdentityEmbedding(Iler[i],Iler[j]));                    
            else 
                continue; 
            fi;                
        od;    
    od;    
    allcat1 := Filtered( allprecat1,IsCat1Group );     
    B := AllCat1GroupsUpToIsomorphism( G ); 
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "GAP id", "\t\t", "Group Name", "\t", "|End(G)|", "\t", 
           "|IE(G)|", "\t\t", "C(G)", "\t\t", "|C/~| \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( IdGroup(G), "\t", StructureDescription(G), "\t\t", 
           Length(Eler), "\t\t", Length(Iler), "\t\t", Length(allcat1), 
           "\t\t", Length(B), "\n" );    
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    return B;
end );

InstallMethod( TableRowForCat2Groups, "for a group G", true, [ IsGroup ], 0,
function( G )

    local Eler, Iler, i, j, allcat1, Cat2_ler, B, a, n, C1, c1, c2, 
          PreCat2_ler, Cat2_ler2;

    i := IdGroup(G)[1];
    j := IdGroup(G)[2];
    n := Cat1Select(i,j,0);;
    Cat2_ler := AllCat2Groups(G);    
    B := AllCat2GroupsUpToIsomorphism(G);;
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "GAP id", "\t\t", "Group Name", "\t\t", "C2(G)", "\t\t", 
           "|C2/~| \n");
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( IdGroup(G), "\t", StructureDescription(G), "\t\t\t", 
           Length(Cat2_ler), "\t\t", Length(B), "\n" );    
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    Print( "-----------------------------------------------------------", 
           "-------------------------------- \n" );
    return B;
end );

#############################################################################
##
#M  ConjugationActionForCrossedSquare 
##  . . . . conjugation action for crossed square from cat2-group
##
InstallMethod( ConjugationActionForCrossedSquare, 
    "conjugation action for crossed square with G acting on N", 
    true, [ IsGroup, IsGroup ], 0,
function( G, N )

    local genG, genN, autgen, g, imautgen, a, idN, aut, act;

    genG := GeneratorsOfGroup( G );
    genN := GeneratorsOfGroup( N );
    autgen := [ ];
    for g in genG do
        imautgen := List( genN, n -> n^g );
        a := GroupHomomorphismByImages( N, N, genN, imautgen );
        Add( autgen, a );
    od;
    if ( Length( genG ) = 0 ) then
        idN := IdentityMapping( N );
        aut := Group( idN );
    else
        aut := Group( autgen );
    fi;
    SetIsGroupOfAutomorphisms( aut, true );
    act := GroupHomomorphismByImages( G, aut, genG, autgen );
    return act;
end ); 

#############################################################################
##
#M  PreCrossedSquareOfPreCat2Group
#M  PreCat2GroupOfPreCrossedSquare
#M  CrossedSquareOfCat2Group
#M  Cat2GroupOfCrossedSquare
##
InstallMethod( PreCrossedSquareOfPreCat2Group, true, 
    [ IsPreCat2Group ], 0,
function( C2G )
 
    local C1, C2, h1, t1, h2, t2, L, M, N, P, kappa, actML, up, 
          lambda, actNL, left, mu, actPM, right, nu, actPN, down,
          actPL, genL, imdelta, delta, diag, xp, XS;

    C1 := GeneratingCat1Groups( C2G )[1];
    C2 := GeneratingCat1Groups( C2G )[2];
    if not ( IsPerm2DimensionalGroup( C1 ) ) then
        C1 := Image(IsomorphismPermObject( C1 ) );
    fi;
    if not ( IsPerm2DimensionalGroup( C2 ) ) then
        C2 := Image(IsomorphismPermObject( C2 ) );
    fi; 
    h1 := HeadMap( C1 );
    t1 := TailMap( C1 );
    h2 := HeadMap( C2 );
    t2 := TailMap( C2 );  
    L := Intersection( Kernel( t1 ), Kernel( t2 ) );
    M := Intersection( Image( t1 ), Kernel( t2 ) );
    N := Intersection( Kernel ( t1 ), Image( t2 ) );
    P := Intersection( Image( t1 ), Image( t2 ) );
    Info( InfoXMod, 4, "L = ", L );
    Info( InfoXMod, 4, "M = ", M );
    Info( InfoXMod, 4, "N = ", N );
    Info( InfoXMod, 4, "P = ", P );
    kappa := GroupHomomorphismByFunction( L, M, x -> ImageElm(h1,x) );
    actML := ConjugationActionForCrossedSquare( M, L );
    up := XModByBoundaryAndAction( kappa, actML );    
    lambda := GroupHomomorphismByFunction( L, N, x -> ImageElm(h2,x) );
    actNL := ConjugationActionForCrossedSquare( N, L );
    left := XModByBoundaryAndAction( lambda, actNL );
    mu := GroupHomomorphismByFunction( M, P, x -> ImageElm(h2,x) );
    actPM := ConjugationActionForCrossedSquare( P, M );
    right := XModByBoundaryAndAction( mu, actPM );    
    nu := GroupHomomorphismByFunction( N, P, x -> ImageElm(h1,x) );
    actPN := ConjugationActionForCrossedSquare( P, N );
    down := XModByBoundaryAndAction( nu, actPN );
    Info( InfoXMod, 3, "   up = ", up );
    Info( InfoXMod, 3, " left = ", left );
    Info( InfoXMod, 3, "right = ", right );
    Info( InfoXMod, 3, " down = ", down );
    actPL := ConjugationActionForCrossedSquare( P, L ); 
    genL := GeneratorsOfGroup( L ); 
    imdelta := List( genL, l -> ImageElm( nu, ImageElm( lambda, l ) ) ); 
    delta := GroupHomomorphismByImages( L, P, genL, imdelta ); 
    diag := XModByBoundaryAndAction( delta, actPL ); 
    xp := CrossedPairingByCommutators( N, M, L );
    XS := PreCrossedSquareObj( up, left, right, down, diag, xp );
##     SetIsCrossedSquare( XS, true ); 
    if ( HasIsCat2Group( C2G ) and IsCat2Group( C2G ) ) then 
        if not IsCrossedSquare( XS ) then 
            Error( "XS fails to be the crossed square of a cat2-group" ); 
        fi;
    fi;
    if HasName( C2G ) then 
        SetName( XS, Concatenation( "xsq(", Name( C2G ), ")" ) ); 
    fi; 
    return XS;
end );

InstallMethod( PreCat2GroupOfPreCrossedSquare, true, 
    [ IsPreCrossedSquare ], 0,
function( XS )
 
    local up, left, right, down, diag, L, M, N, P, genL, genM, genN, genP, 
          kappa, lambda, nu, mu, 
          act_up, act_lt, act_dn, act_rt, act_dg, xpair, Cup, Cdown, 
          NxL, genNxL, e1NxL, e2NxL, Cleft, MxL, genMxL, e1MxL, e2MxL, 
          PxM, genPxM, e1PxM, e2PxM, Cright, PxN, genPxN, e1PxN, e2PxN, 
          MxLbyP, MxLbyN, autgenMxL, autMxL, actPNML, NxLbyP, NxLbyM, 
          autgenNxL, autNxL, actPMNL, imPNML, bdyPNML, XPNML, CPNML, PNML, 
          e1PNML, e2PNML, genPNML, imPMNL, bdyPMNL, XPMNL, CPMNL, PMNL, 
          e1PMNL, e2PMNL, genPMNL, imiso, iso, inv, iminv, tup, hup, eup, 
          C2PNML, tlt, hlt, elt, tdn, hdn, edn, trt, hrt, ert, tdi, hdi, edi, 
          PC, Cdiag, imnukappa, nukappa, morCleftCright, 
          immulambda, mulambda, morCupCdown; 

    Info( InfoXMod, 1, 
          "these conversion functions are under development\n" ); 
    up := Up2DimensionalGroup( XS );
    left := Left2DimensionalGroup( XS );
    right := Right2DimensionalGroup( XS );
    down := Down2DimensionalGroup( XS );
    diag := Diagonal2DimensionalGroup( XS );
    L := Source( up );
    M := Range( up );
    N := Source( down );
    P := Range( down );
    genL := GeneratorsOfGroup( L ); 
    genM := GeneratorsOfGroup( M ); 
    genN := GeneratorsOfGroup( N ); 
    genP := GeneratorsOfGroup( P ); 
    Info( InfoXMod, 4, "L = ", L );    
    Info( InfoXMod, 4, "M = ", M );
    Info( InfoXMod, 4, "N = ", N );
    Info( InfoXMod, 4, "P = ", P );
    kappa := Boundary( up );
    lambda := Boundary( left );
    mu := Boundary( right );
    nu := Boundary( down );
    act_up := XModAction( up );
    act_lt := XModAction( left );
    act_rt := XModAction( right );
    act_dn := XModAction( down );
    act_dg := XModAction( diag );
    xpair := CrossedPairing( XS );

    Cup := Cat1GroupOfXMod( up );
    MxL := Source( Cup );
    e1MxL := Embedding( MxL, 1 );
    e2MxL := Embedding( MxL, 2 );
    genMxL := Concatenation( List( genM, m -> ImageElm( e1MxL, m ) ), 
                             List( genL, l -> ImageElm( e2MxL, l ) ) ); 
    Cleft := Cat1GroupOfXMod( left );
    NxL := Source( Cleft );
    e1NxL := Embedding( NxL, 1 );
    e2NxL := Embedding( NxL, 2 );
    genNxL := Concatenation( List( genN, n -> ImageElm( e1NxL, n ) ), 
                             List( genL, l -> ImageElm( e2NxL, l ) ) ); 
    Cright := Cat1GroupOfXMod( right ); 
    PxM := Source( Cright ); 
    e1PxM := Embedding( PxM, 1 );
    e2PxM := Embedding( PxM, 2 );
    genPxM := Concatenation( List( genP, p -> ImageElm( e1PxM, p ) ), 
                             List( genM, m -> ImageElm( e2PxM, m ) ) ); 

    ## construct the cat1-morphism Cleft => Cright 
    imnukappa := Concatenation( 
        List( genN, n -> ImageElm( e1PxM, ImageElm( nu, n ) ) ), 
        List( genL, l -> ImageElm( e2PxM, ImageElm( kappa, l ) ) ) ); 
    nukappa := GroupHomomorphismByImages( NxL, PxM, genNxL, imnukappa ); 
    morCleftCright := Cat1GroupMorphism( Cleft, Cright, nukappa, nu ); 
    if ( InfoLevel( InfoXMod ) > 2 ) then 
        Display( morCleftCright ); 
    fi;
    Cdown := Cat1GroupOfXMod( down ); 
    PxN := Source( Cdown ); 
    e1PxN := Embedding( PxN, 1 );
    e2PxN := Embedding( PxN, 2 );
    genPxN := Concatenation( List( genP, p -> ImageElm( e1PxN, p ) ), 
                             List( genN, n -> ImageElm( e2PxN, n ) ) ); 

    ## construct the cat1-morphism Cup => Cdown 
    immulambda := Concatenation( 
        List( genM, m -> ImageElm( e1PxN, ImageElm( mu, m ) ) ), 
        List( genL, l -> ImageElm( e2PxN, ImageElm( lambda, l ) ) ) ); 
    mulambda := GroupHomomorphismByImages( MxL, PxN, genMxL, immulambda ); 
    morCupCdown := Cat1GroupMorphism( Cup, Cdown, mulambda, mu ); 
    if ( InfoLevel( InfoXMod ) > 2 ) then 
        Display( morCupCdown ); 
    fi;
    ## construct the action of PxM on NxL using: 
    ## (n,l)^(p,m) = ( n^p, (n^p \box m).l^{pm} ) 
    NxLbyP := List( genP, p -> GroupHomomorphismByImages( NxL, NxL, genNxL, 
            Concatenation( 
                List( genN, n -> ImageElm( e1NxL, 
                                 ImageElm( ImageElm( act_dn, p ), n ) )),
                List( genL, l -> ImageElm( e2NxL, 
                                 ImageElm( ImageElm( act_dg, p ), l ))) ))); 
    NxLbyM := List( genM, m -> GroupHomomorphismByImages( NxL, NxL, genNxL, 
            Concatenation( 
                List( genN, n -> ImageElm( e1NxL, n ) * 
                                 ImageElm( e2NxL, xpair( n, m ) ) ), 
                List( genL, l -> ImageElm( e2NxL, 
                                 ImageElm( ImageElm( act_up, m ), l ))) ))); 
    autgenNxL := Concatenation( NxLbyP, NxLbyM ); 
    Info( InfoXMod, 2, "autgenNxL = ", autgenNxL ); 
    autNxL := Group( autgenNxL ); 
    Info( InfoXMod, 2, "autNxL has size ", Size( autNxL ) );
    SetIsGroupOfAutomorphisms( autNxL, true ); 
    actPMNL := GroupHomomorphismByImages( PxM, autNxL, genPxM, autgenNxL );
    imPMNL := Concatenation( 
                List( genN, n -> ImageElm( e1PxM, ImageElm( nu, n ) ) ), 
                List( genL, l -> ImageElm( e2PxM, ImageElm( kappa, l ) ) ) ); 
    Info( InfoXMod, 2, "imPMNL = ", imPMNL ); 
    bdyPMNL := GroupHomomorphismByImages( NxL, PxM, genNxL, imPMNL );
    XPMNL := XModByBoundaryAndAction( bdyPMNL, actPMNL ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "crossed module XPMNL:\n" );
        Display( XPMNL );
    fi; 
    CPMNL := Cat1GroupOfXMod( XPMNL ); 
    Info( InfoXMod, 2, "cat1-group CPMNL: ", StructureDescription(CPMNL) );
    PMNL := Source( CPMNL );
    e1PMNL := Embedding( PMNL, 1 );
    e2PMNL := Embedding( PMNL, 2 );
    genPMNL := Concatenation( List( genPxM, g -> ImageElm( e1PMNL, g ) ), 
                              List( genNxL, g -> ImageElm( e2PMNL, g ) ) ); 
    ## construct the action of PxN on MxL using the transpose action: 
    ## (m,l)^(p,n) = (m^p, (n \box m^p)^{-1}.l^{pn} ) 
    MxLbyP := List( genP, p -> GroupHomomorphismByImages( MxL, MxL, genMxL, 
            Concatenation( 
                List( genM, m -> ImageElm( e1MxL, 
                                 ImageElm( ImageElm( act_rt, p ), m ) )),
                List( genL, l -> ImageElm( e2MxL, 
                                 ImageElm( ImageElm( act_dg, p ), l ))) ))); 
    MxLbyN := List( genN, n -> GroupHomomorphismByImages( MxL, MxL, genMxL, 
            Concatenation( 
                List( genM, m -> ImageElm( e1MxL, m ) * 
                                 ImageElm( e2MxL, xpair( n, m )^(-1) ) ), 
                List( genL, l -> ImageElm( e2MxL, 
                                 ImageElm( ImageElm( act_lt, n ), l ))) )));
    autgenMxL := Concatenation( MxLbyP, MxLbyN ); 
    Info( InfoXMod, 2, "autgenMxL = ", autgenMxL ); 
    autMxL := Group( autgenMxL ); 
    SetIsGroupOfAutomorphisms( autMxL, true ); 
    actPNML := GroupHomomorphismByImages( PxN, autMxL, genPxN, autgenMxL );
    imPNML := Concatenation( 
                List( genM, m -> ImageElm( e1PxN, ImageElm( mu, m ) ) ), 
                List( genL, l -> ImageElm( e2PxN, ImageElm( lambda, l ) ) ) ); 
    bdyPNML := GroupHomomorphismByImages( MxL, PxN, genMxL, imPNML );
    XPNML := XModByBoundaryAndAction( bdyPNML, actPNML ); 
    if ( InfoLevel( InfoXMod ) > 2 ) then 
        Print( "crossed module XPNML:\n" );
        Display( XPNML );
    fi;
    CPNML := Cat1GroupOfXMod( XPNML ); 
    Info( InfoXMod, 2, "cat1-group CPNML: ", StructureDescription(CPNML) );
    PNML := Source( CPNML );
    e1PNML := Embedding( PNML, 1 );
    e2PNML := Embedding( PNML, 2 );
    genPNML := Concatenation( List( genPxN, g -> ImageElm( e1PNML, g ) ), 
                              List( genMxL, g -> ImageElm( e2PNML, g ) ) ); 
    ## now find the isomorphism between the sources PMNL and PNML 
    imiso := Concatenation( 
           List( genP, p -> ImageElm( e1PNML, ImageElm( e1PxN, p ) ) ), 
           List( genM, m -> ImageElm( e2PNML, ImageElm( e1MxL, m ) ) ), 
           List( genN, n -> ImageElm( e1PNML, ImageElm( e2PxN, n ) ) ), 
           List( genL, l -> ImageElm( e2PNML, ImageElm( e2MxL, l ) ) ) ); 
    iso := GroupHomomorphismByImages( PMNL, PNML, genPMNL, imiso ); 
    inv := InverseGeneralMapping( iso ); 
    iminv := List( genPNML, g -> ImageElm( inv, g ) ); 
    Info( InfoXMod, 2, "iminv = ", iminv );

    ##  construct an isomorphic up cat1-group 
    tup := iso * TailMap( CPNML );
    hup := iso * HeadMap( CPNML ); 
    eup := e1PNML * inv; 
    C2PNML := PreCat1GroupByTailHeadEmbedding( tup, hup, eup ); 

    ##  now see if a cat2-group has been constructed 
    tlt := TailMap( CPMNL );
    hlt := HeadMap( CPMNL ); 
    elt := e1PMNL; 
    tdn := TailMap( Cright ); 
    hdn := HeadMap( Cright );
    edn := e1PxM; 
    trt := TailMap( Cdown );
    hrt := HeadMap( Cdown ); 
    ert := e1PxN; 
    tdi := tlt * tdn; 
    if not ( tdi = tup * trt ) then 
        Error( "tlt * tdn <> tup * trt" );  
    fi; 
    hdi := hlt * hdn; 
    if not ( hdi = hup * hrt ) then 
        Error( "hlt * hdn <> hup * hrt" );  
    fi; 
    edi := edn * elt; 
    if not ( edi = ert * eup ) then 
        Error( "edn * elt <> ert * eup" );  
    fi; 
    Cdiag := PreCat1GroupByTailHeadEmbedding( tdi, hdi, edi ); 
    PC := PreCat2GroupByPreCat1Groups( CPMNL, C2PNML, Cright, Cdown, Cdiag );
    return PC; 
end ); 

InstallMethod( CrossedSquareOfCat2Group, "generic method for cat2-groups",
    true, [ IsCat2Group ], 0,
function( C2 )

    local XS;
    XS := PreCrossedSquareOfPreCat2Group( C2 );
##    SetIsCrossedSquare( XS, true ); 
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be the crossed square of a cat2-group" ); 
    fi; 
    SetCrossedSquareOfCat2Group( C2, XS );
##    SetCat2GroupOfCrossedSquare( XS, C2 );
    return XS;
end );

InstallMethod( Cat2GroupOfCrossedSquare, "generic method for crossed squares",
    true, [ IsCrossedSquare ], 0,
function( XS )

    local C2;
    C2 := PreCat2GroupOfPreCrossedSquare( XS );
    SetCrossedSquareOfCat2Group( C2, XS );
    SetCat2GroupOfCrossedSquare( XS, C2 );
    return C2;
end );
    
#############################################################################
##
#M  Transpose3DimensionalGroup . transpose of a crossed square or cat2-group 
##
InstallMethod( Transpose3DimensionalGroup, "transposed crossed square", true, 
    [ IsCrossedSquare ], 0,
function( XS )

    local xpS, xpT, XT;

    xpS := CrossedPairing( XS );
    xpT := function( m, n ) return xpS( n, m )^(-1); end;
    XT := PreCrossedSquareObj( Left2DimensionalGroup(XS), 
              Up2DimensionalGroup(XS), Down2DimensionalGroup(XS), 
           Right2DimensionalGroup(XS), Diagonal2DimensionalGroup(XS), xpT );
##    SetIsCrossedSquare( XT, true );
    return XT;
end );    

InstallMethod( Transpose3DimensionalGroup, "transposed cat2-group", true, 
    [ IsCat2Group ], 0,
function( C2G )
    return PreCat2GroupByPreCat1Groups( 
               Left2DimensionalGroup( C2G ), 
               Up2DimensionalGroup( C2G ), 
               Down2DimensionalGroup( C2G ), 
               Right2DimensionalGroup( C2G ), 
               Diagonal2DimensionalGroup( C2G ) );
end );

#############################################################################
##
#M  LeftRightMorphism  . . . . . . . . . . . . . . . for a precrossed square
#M  UpDownMorphism . . . . . . . . . . . . . . . . . for a precrossed square
## 
InstallMethod( LeftRightMorphism, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )
    return XModMorphismByGroupHomomorphisms( 
        Left2DimensionalGroup(s), Right2DimensionalGroup(s), 
        Boundary( Up2DimensionalGroup(s) ), 
        Boundary( Down2DimensionalGroup(s) ) ); 
end );

InstallMethod( UpDownMorphism, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )
    return XModMorphismByGroupHomomorphisms( 
           Up2DimensionalGroup(s), Down2DimensionalGroup(s), 
           Boundary( Left2DimensionalGroup(s) ), 
           Boundary( Right2DimensionalGroup(s) ) ); 
end );

#############################################################################
##
#M  IsSymmetric3DimensionalGroup . . . check whether a 3d-group is symmetric
##
InstallMethod( IsSymmetric3DimensionalGroup, 
    "generic method for 3d-groups", true, [ IsHigherDimensionalGroup ], 0,
function( XS )
    return IsHigherDimensionalGroup( XS ) 
           and HigherDimension( XS ) = 3 
           and ( Up2DimensionalGroup( XS ) 
                 = Left2DimensionalGroup( XS ) ) 
           and ( Right2DimensionalGroup( XS ) 
                 = Down2DimensionalGroup( XS ) );
end );

##############################################################################
##
#M  SubPreCrossedSquare                  creates SubPreXSq from four subgroups
#M  SubCrossedSquare                     creates SubXSq from four subgroups
##
InstallMethod( SubPreCrossedSquare, "generic method for pre-crossed squares",
     true, [ IsPreCrossedSquare, IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( PXS, L1, M1, N1, P1 )

    local  L, M, N, P, up, lt, rt, dn, dg, up1, lt1, rt1, dn1, dg1,
           xp, sub;

    up := Up2DimensionalGroup( PXS );
    lt := Left2DimensionalGroup( PXS );
    rt := Right2DimensionalGroup( PXS );
    dn := Down2DimensionalGroup( PXS );
    dg := Diagonal2DimensionalGroup( PXS );
    L := Source( up );
    M := Range( up );
    N := Source( dn );
    P := Range( dn );
    up1 := SubPreXMod( up, L1, M1 );
    lt1 := SubPreXMod( lt, L1, N1 );
    rt1 := SubPreXMod( rt, M1, P1 );
    dn1 := SubPreXMod( dn, N1, P1 );
    dg1 := SubPreXMod( dg, L1, P1 );
    if fail in [ up1, lt1, rt1, dn1, dg1 ] then
        return fail;
    fi;
    xp := CrossedPairing( PXS );
    sub := PreCrossedSquareByPreXMods( up1, lt1, rt1, dn1, dg1, xp );
    return sub;
end );

InstallMethod( SubCrossedSquare, "generic method for pre-crossed squares",
     true, [ IsCrossedSquare, IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( XS, L1, M1, N1, P1 )

    local sub, ok;

    sub := SubPreCrossedSquare( XS, L1, M1, N1, P1 );
    ok := IsCrossedSquare( sub );
    if ok then
        return sub;
    else
        return fail;
    fi;
end );

##############################################################################
##
#M  SubPreCat2Group                    creates SubPreCat2 from three subgroups
#M  SubCat2Group                       creates SubCat2 from three subgroups
##
InstallMethod( SubPreCat2Group, "generic method for pre-cat2-groups",
     true, [ IsPreCat2Group, IsGroup, IsGroup, IsGroup ], 0,
function( C2G, L1, M1, N1 )

    local  L, M, N, up, lt, up1, lt1, rt1, dn1, dg1,
           xp, sub;

    up := Up2DimensionalGroup( C2G );
    lt := Left2DimensionalGroup( C2G );
    L := Source( up );
    M := Range( up );
    N := Range( lt );
    up1 := SubPreCat1Group( up, L1, M1 );
    lt1 := SubPreCat1Group( lt, L1, N1 );
    if fail in [ up1, lt1 ] then
        return fail;
    fi;
    sub := PreCat2Group( up1, lt1 );
    return sub;
end );

InstallMethod( SubCat2Group, "generic method for cat2-groups",
     true, [ IsCat2Group, IsGroup, IsGroup, IsGroup ], 0,
function( C2G, L1, M1, N1 )

    local sub, ok;

    sub := SubPreCat2Group( C2G, L1, M1, N1 );
    ok := IsCat2Group( sub );
    if ok then
        return sub;
    else
        return fail;
    fi;
end );

#############################################################################
##
#M  TrivialSub3DimensionalGroup . . . . . . . . . .  of a 3d-object
#M  TrivialSubPreCrossedSquare . . . . . . . . . . . of a pre-crossed square
#M  TrivialSubCrossedSquare  . . . . . . . . . . . . of a crossed square
#M  TrivialSubPreCat2Group . . . . . . . . . . . . . of a pre-cat2-group
#M  TrivialSubCat2Group  . . . . . . . . . . . . . . of a cat2-group
##
InstallMethod( TrivialSub3DimensionalGroup, "of a 3d-object", true, 
    [ Is3DimensionalDomain ], 0,
function( obj )

    local upid, dnid;

    upid := TrivialSub2DimensionalGroup( Up2DimensionalGroup( obj ) );
    dnid := TrivialSub2DimensionalGroup( Down2DimensionalGroup( obj ) );
    if IsPreCrossedSquare( obj ) then
        return SubPreCrossedSquare( obj, Source( upid ), Range( upid ),
                                         Source( dnid ), Range( dnid ) );
    elif IsPreCat2Group( obj ) then
        return SubPreCat2Group( obj, Source( upid ), Range( upid ),
                                     Source( dnid ) );
    else
        Error( "<obj> must be a pre-crossed square or a pre-cat2-group" );
    fi;
end );

InstallMethod( TrivialSubPreCrossedSquare, "of a pre-crossed square", true,
    [ IsPreCrossedSquare ], 0,
function( obj )
    return TrivialSub3DimensionalGroup( obj );
end );

InstallMethod( TrivialSubCrossedSquare, "of a crossed square", true, 
    [ IsCrossedSquare ], 0,
function( obj )
    local triv, ok;
    triv := TrivialSub3DimensionalGroup( obj );
    ok := IsCrossedSquare( triv );
    return triv;
end );

InstallMethod( TrivialSubPreCat2Group, "of a pre-cat2-group", true, 
    [ IsPreCat2Group ], 0,
function( obj )
    return TrivialSub3DimensionalGroup( obj );
end );

InstallMethod( TrivialSubCat2Group, "of a cat2-group", true, [ IsCat2Group ], 0,
function( obj )
    return TrivialSub3DimensionalGroup( obj );
end );
