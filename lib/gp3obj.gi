##############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)crossed squares 
##  and (pre-)cat2-groups.
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
    
#############################################################################
##
#M  IsPerm3DimensionalGroup . . . . check whether the 4 sides are perm groups
#M  IsFp3DimensionalGroup . . . . . check whether the 4 sides are fp groups
#M  IsPc3DimensionalGroup . . . . . check whether the 4 sides are pc groups
##
InstallMethod( IsPerm3DimensionalGroup, "generic method for 3d-group objects",
    true, [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsPermGroup( Up2DimensionalGroup(obj) ) 
             and IsPermGroup( Left2DimensionalGroup(obj) ) 
             and IsPermGroup( Down2DimensionalGroup(obj) ) 
             and IsPermGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsFp3DimensionalGroup, "generic method for 3d-group objects",
    true, [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsFpGroup( Up2DimensionalGroup(obj) ) 
             and IsFpGroup( Left2DimensionalGroup(obj) ) 
             and IsFpGroup( Down2DimensionalGroup(obj) ) 
             and IsFpGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsPc3DimensionalGroup, "generic method for 3d-group obj ects",
    true, [ IsHigherDimensionalGroup ], 0,
function( obj )
    return ( IsPcGroup( Up2DimensionalGroup(obj) ) 
             and IsPcGroup( Left2DimensionalGroup(obj) ) 
             and IsPcGroup( Down2DimensionalGroup(obj) ) 
             and IsPcGroup( Right2DimensionalGroup(obj) ) );
end );

##############################################################################
##
#M  IsCrossedPairing
#M  CrossedPairingObj( [<src1>,<src2>],<rng>,<map> ) .. make a crossed pairing
##
InstallMethod( IsCrossedPairing, "generic method for mappings", true, 
    [ IsGeneralMapping ], 0,
function( map )
    return ( HasSource( map ) and HasRange( map ) 
             and HasCrossedPairingMap( map ) );
end );

InstallMethod( CrossedPairingObj, "for a general mapping", true,
    [ IsList, IsGroup, IsGeneralMapping ], 0,
function( src, rng, map )

    local obj;

    obj := rec();
    if not ( Length(src)=2 ) and IsGroup(src[1]) and IsGroup(src[2]) then 
        Error( "the first parameter should be a list of two groups" ); 
    fi;
    ObjectifyWithAttributes( obj, CrossedPairingType,
        Source, src,
        Range, rng, 
        CrossedPairingMap, map,
        IsCrossedPairing, true );
    return obj;
end );

InstallMethod( PrintObj, "method for a crossed pairing", true, 
    [ IsCrossedPairing ], 0,
function( h )
    local map; 
    map := CrossedPairingMap( h );
    Print( "crossed pairing: ", Source(map), " -> ", Range(map), "\n" ); 
end ); 

#############################################################################
##
#M  ImageElmCrossedPairing( <map>, <elm> )  . . . . . . . for crossed pairing
##
InstallMethod( ImageElmCrossedPairing, "for crossed pairing", true, 
    [ IsCrossedPairing, IsList ], 0,
function ( xp, elm ) 
    return ImageElmMapping2ArgumentsByFunction( CrossedPairingMap(xp), elm );
end );

##############################################################################
##
#M  CrossedPairingByCommutators( <grp>, <grp>, <grp> ) . . . . . make an xpair
##
InstallMethod( CrossedPairingByCommutators, "for three groups", true,
    [ IsGroup, IsGroup, IsGroup ], 0,
function( N, M, L )

    local map, xp;

    if not IsSubgroup( L, CommutatorSubgroup(N,M) ) then 
        Error( "require CommutatorSubgroup(N,M) <= L" );
    fi;
    map := Mapping2ArgumentsByFunction( [N,M], L, 
               function(c) return Comm( c[1], c[2] ); end );
    xp := CrossedPairingObj( [N,M], L, map );
    return xp;
end );

##############################################################################
##
#M  CrossedPairingByConjugators( <grp> ) . . . make an xpair : Inn(M)^2 -> M 
##
InstallMethod( CrossedPairingByConjugators, "for an inner automorphism group", 
    true, [ IsGroup ], 0,
function( innM )

    local gens, M, map, xp;

    gens := GeneratorsOfGroup( innM ); 
    if not ForAll( gens, g -> HasConjugatorOfConjugatorIsomorphism(g) ) then 
        Error( "innM is not a group of inner automorphisms" ); 
    fi;
    M := Source( gens[1] );
    map := Mapping2ArgumentsByFunction( [ innM, innM ], M, 
               function(p) 
               local c1, c2;
               c1 := ConjugatorOfConjugatorIsomorphism( p[1] );
               c2 := ConjugatorOfConjugatorIsomorphism( p[2] );
               return Comm( c1, c2 ); end );
    xp := CrossedPairingObj( [ innM, innM ], M, map );
    return xp;
end );

##############################################################################
##
#M  CrossedPairingByDerivations( <xmod> ) . . .  make an actor crossed pairing
##
InstallMethod( CrossedPairingByDerivations, "for a crossed module", true,
    [ IsXMod ], 0,
function( X0 )

    local SX, RX, WX, reg, imlist, map;

    SX := Source( X0 );
    RX := Range( X0 );
    WX := WhiteheadPermGroup( X0 );
    reg := RegularDerivations( X0 );
    imlist := ImagesList( reg );
    map := Mapping2ArgumentsByFunction( [RX,WX], SX, 
               function(t) 
                   local pos, chi;
                   pos := Position( Elements( WX ), t[2] );
                   chi := DerivationByImages( X0, imlist[pos] ); 
                   return DerivationImage( chi, t[1] ); 
               end );
    return CrossedPairingObj( [RX,WX], SX, map );
end );

##############################################################################
##
#M  CrossedPairingByPreImages( <xmod>, <xmod> ) . . . inner autos -> x-pairing 
##
InstallMethod( CrossedPairingByPreImages, "for two crossed modules", 
    true, [ IsXMod, IsXMod ], 0,
function( up, lt )

    local L, M, N, kappa, lambda, map, xp;

    L := Source( up );
    if not ( Source( lt ) = L ) then 
        Error( "up and lt should have the same source" ); 
    fi;
    M := Range( up ); 
    N := Range( lt );
    kappa := Boundary( up ); 
    lambda := Boundary( lt );
    map := Mapping2ArgumentsByFunction( [N,M], L, 
               function( c ) 
                   local lm, ln;
                   lm := PreImagesRepresentative( kappa, c[2] ); 
                   ln := PreImagesRepresentative( lambda, c[1] ); 
                   return Comm( ln, lm ); 
               end );
    xp := CrossedPairingObj( [N,M], L, map );
    return xp;
end );

##############################################################################
##
#M  CrossedPairingBySingleXModAction( <xmod>, <subxmod> ) . action -> x-pairing 
#M  PrincipalCrossedPairing( <xmod > )   
##
InstallMethod( CrossedPairingBySingleXModAction, "for xmod and normal subxmod", 
    true, [ IsXMod, IsXMod ], 0,
function( rt, lt )

    local M, L, N, act, map, xp;

    if not IsNormalSub2DimensionalDomain( rt, lt ) then 
        Error( "lt not a normal subxmod of rt" ); 
    fi;
    M := Source( rt ); 
    L := Source( lt );
    N := Range( lt ); 
    act := XModAction( rt );
    map := Mapping2ArgumentsByFunction( [N,M], L, 
               function( c ) 
                   return ImageElm( ImageElm( act, c[1] ), c[2]^(-1) ) * c[2];
               end );
    xp := CrossedPairingObj( [N,M], L, map );
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
function( P )

    local u, d, l, r, ul, dl, ur, dr, bu, bd, bl, br, blbd, bubr,
          autu, autl, act, diag, ok, morud, morlr;

    if not ( IsPreCrossedSquareObj ( P ) and HasDiagonalAction( P ) 
             and HasCrossedPairing( P ) ) then
        return false;
    fi;
    u := Up2DimensionalGroup( P );
    l := Left2DimensionalGroup( P );
    r := Right2DimensionalGroup( P );
    d := Down2DimensionalGroup( P );
    act := DiagonalAction( P );
    ul := Source( u );
    ur := Range( u );
    dl := Source( d );
    dr := Range( d );
    if not ( ( ul = Source(l) ) and ( dl = Range(l) ) and
             ( ur = Source(r) ) and ( dr = Range(r) ) ) then
        Info( InfoXMod, 2, "Incompatible source/range" );
        return false;
    fi;
    ## construct the boundary of the diagonal
    bl := Boundary( l );
    bd := Boundary( d );
    blbd := bl * bd;
    bu := Boundary( u );
    br := Boundary( r );
    bubr := bu * br;
    if not ( blbd = bubr ) then
        Info( InfoXMod, 2, "Boundaries in square do not commute" );
        return false;
    fi;
    ## check the action of the diagonal? 
    diag := PreXModByBoundaryAndAction( blbd, act );
    ok := IsPreXMod( diag );
    if not ok then
        Info( InfoXMod, 2, "diagonal not a pre-crossed module" );
        return false;
    fi;
    #? compatible actions to be checked?
    morud := PreXModMorphism( u, d, bl, br );
    morlr := PreXModMorphism( l, r, bu, bd );
    if not ( IsPreXModMorphism( morud ) and IsPreXModMorphism( morlr ) ) then
        Info( InfoXMod, 2, "morud and/or modlr not prexmod morphisms" );
        return false;
    fi;
    #? we have checked that P has a crossed pairing but not any properties 
    return true;
end );

#############################################################################
##
#M  IsCrossedSquare . . . . . . . . check all the axioms for a crossed square
##
InstallMethod( IsCrossedSquare, "generic method for a crossed square",
    true, [ IsHigherDimensionalGroup ], 0,
function( XS )

    local up, lt, rt, dn, L, M, N, P, kappa, lambda, mu, nu, 
          lambdanu, kappamu, autu, autl, actdg, dg, ok, morud, morlr, 
          genL, genM, genN, genP, actup, actlt, actrt, actdn, l, p, 
          xp, x, y, m, n, m2, n2, am, an, apdg, aprt, apdn, nboxm;

    if not ( IsPreCrossedSquareObj ( XS ) and HasDiagonalAction( XS ) 
             and HasCrossedPairing( XS ) ) then
        return false;
    fi;
    up := Up2DimensionalGroup( XS );
    lt := Left2DimensionalGroup( XS );
    rt := Right2DimensionalGroup( XS );
    dn := Down2DimensionalGroup( XS );
    actdg := DiagonalAction( XS );
    L := Source( up );
    M := Range( up );
    N := Source( dn );
    P := Range( dn );
    if not ( ( L = Source(lt) ) and ( N = Range(lt) ) and
             ( M = Source(rt) ) and ( P = Range(rt) ) ) then
        Info( InfoXMod, 2, "Incompatible source/range" );
        return false;
    fi;
    ## construct the boundary of the diagonal
    lambda := Boundary( lt );
    nu := Boundary( dn );
    lambdanu := lambda * nu;
    kappa := Boundary( up );
    mu := Boundary( rt );
    kappamu := kappa * mu;
    if not ( lambdanu = kappamu ) then
        Info( InfoXMod, 2, "Boundaries in square do not commute" );
        return false;
    fi;
    ## check the action of the diagonal? 
    dg := PreXModByBoundaryAndAction( lambdanu, actdg );
    ok := IsPreXMod( dg );
    if not ok then
        Info( InfoXMod, 2, "diagonal not a pre-crossed module" );
        return false;
    fi;
    #? compatible actions to be checked?
    morud := PreXModMorphism( up, dn, lambda, mu );
    morlr := PreXModMorphism( lt, rt, kappa, nu );
    if not ( IsPreXModMorphism( morud ) and IsPreXModMorphism( morlr ) ) then
        Info( InfoXMod, 2, "morud and/or morlr not prexmod morphisms" );
        return false;
    fi;
    genL := GeneratorsOfGroup( L ); 
    genM := GeneratorsOfGroup( M ); 
    genN := GeneratorsOfGroup( N ); 
    genP := GeneratorsOfGroup( P ); 
    actup := XModAction( up ); 
    actlt := XModAction( lt );
    actrt := XModAction( rt );
    actdn := XModAction( dn ); 
    ## check that kappa,lambda preserve the action of P 
    for p in genP do 
        apdg := ImageElm( actdg, p );
        aprt := ImageElm( actrt, p );
        apdn := ImageElm( actdn, p );
        for l in genL do 
            if not ( ImageElm( kappa, ImageElm( apdg, l ) ) 
                     = ImageElm( aprt, ImageElm( kappa, l ) ) ) then 
                Error( "action of P on up is not preserved" );
            fi;
            if not ( ImageElm( lambda, ImageElm( apdg, l ) ) 
                     = ImageElm( apdn, ImageElm( lambda, l ) ) ) then 
                Error( "action of P on lt is not preserved" );
            fi;
        od;
    od;
    ## check the axioms for a crossed pairing 
    xp := CrossedPairing( XS ); 
    for n in genN do 
        for n2 in genN do 
            for m in genM do 
                x := ImageElmCrossedPairing( xp, [ n*n2, m ] ); 
                an := ImageElm( actlt, n2 ); 
                y := ImageElm( an, ImageElmCrossedPairing( xp, [n,m] ) ); 
                if not x = y * ImageElmCrossedPairing( xp, [n2,m] ) then 
                    Error( "n1,n2,m crossed pairing axiom fails" ); 
                fi; 
            od;
       od;
    od;
    for n in genN do 
        for m in genM do 
            for m2 in genM do 
                x := ImageElmCrossedPairing( xp, [ n, m*m2 ] ); 
                am := ImageElm( actup, m2 ); 
                y := ImageElm( am, ImageElmCrossedPairing( xp, [n,m] ) ); 
                if not x = ImageElmCrossedPairing( xp, [n,m2] ) * y then 
                    Error( "n,m1,m2 crossed pairing axiom fails" ); 
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
                if not ImageElm( apdg, ImageElmCrossedPairing( xp, [n,m] ) ) 
                     = ImageElmCrossedPairing( xp, 
                           [ ImageElm( apdn, n ), ImageElm( aprt, m ) ] ) then
                    Error( "n,m,p crossed pairing axiom fails" ); 
                fi; 
            od;
       od;
    od;
    ## check that kappa,lambda correctly map (n box m) 
    for n in genN do 
        an := ImageElm( actrt, ImageElm( nu, n ) ); 
        for m in genM do 
            am := ImageElm( actdn, ImageElm( mu, m ) ); 
            nboxm := ImageElmCrossedPairing( xp, [n,m] ); 
            if not ImageElm( lambda, nboxm ) = n^(-1) * ImageElm( am, n ) 
               and ImageElm( kappa, nboxm ) = ImageElm( an, m^(-1) ) * m then 
                Error( "kappa,lambda do not map nboxm correctly" ); 
            fi; 
        od;
    od; 
    ## check crossed pairing on images of kappa,lambda 
    for m in genM do 
        am := ImageElm( actdg, ImageElm( mu, m ) ); 
        for l in genL do 
            if not ( ImageElmCrossedPairing( xp, [ImageElm(lambda,l),m] ) 
                   = l^(-1) * ImageElm( am, l ) ) then 
            fi; 
        od;
    od;
    for n in genN do 
        an := ImageElm( actdg, ImageElm( nu, n ) ); 
        for l in genL do 
            if not ( ImageElmCrossedPairing( xp, [n,ImageElm(kappa,l)] ) 
                   = ImageElm( an, l^(-1) ) * l ) then 
                Error( "incorrect image for (n box kappa(l))" ); 
            fi; 
        od;
    od;
##    Print( "SUCCESS\n" );
    return true;
end );

##############################################################################
##
#M  PreCrossedSquareObj ( <up>, <down>, <right>, <left>, <act>, <pair> ) 
##                                               . . . make a PreCrossedSquare
##
InstallMethod( PreCrossedSquareObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( up, lt, rt, dn, act, xp )

    local PS;

    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, PreCrossedSquareObjType, 
      Up2DimensionalGroup, up, 
      Left2DimensionalGroup, lt,
      Right2DimensionalGroup, rt,
      Down2DimensionalGroup, dn,
      DiagonalAction, act,
      CrossedPairing, xp,
      IsHigherDimensionalGroup, true );
    if not IsPreCrossedSquare( PS ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed square." );
    fi;
    return PS;
end );

#############################################################################
##
#F  PreCrossedSquare( <arg> ) 
## 
InstallGlobalFunction( PreCrossedSquare, function( arg )

    Print( "no specific methods for precrossed squares yet implemented\n" ); 
    return fail; 
end );

#############################################################################
##
#F  CrossedSquare( <L>, <M>, <N>, <P> ) . . . . . . by normal subgroups
#F  CrossedSquare( <X0> ) . . . . . . . . . . . . . actor crossed square
#F  CrossedSquare( <X0>, <X1> ) . . . . . . . . . . by pullback 
#F  CrossedSquare( <X0>, <X1> ) . . . . . . . . . . by normal subxmod
#F  CrossedSquare( <C0> ) . . . . . . . . . . . . . for a cat2-group
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
    elif ( nargs = 4 ) and ForAll( arg, a -> IsGroup(a) ) then
        XS := CrossedSquareByNormalSubgroups(arg[1],arg[2],arg[3],arg[4]);
    elif ( nargs = 6  ) then
        XS := CrossedSquareByXMods(arg[1],arg[2],arg[3],arg[4],arg[5],arg[6]);
    else  
        ok := false; 
    fi; 
    if not ok then 
        Print( "standard usage for the function CrossedSquare:\n" );  
        Print( "    CrossedSquare( <L>, <M>, <N>, <P> );  normal subgroups\n" );
        Print( "or: CrossedSquare( <X0>, <X1> );  for a pullback\n" );
        Print( "or: CrossedSquare( <X0>, <X1> );  for normal subxmod\n" );
        Print( "or: CrossedSquare( <X0> );  for splitting an xmod\n" );
        Print( "or: CrossedSquare( <C0> );  for a cat2-group> );\n" );
        return fail;
    fi;
    return XS;
end );

##############################################################################
##
#M  CrossedSquareByXMods . . . . crossed square from 4 xmods + action & xpair
##
InstallMethod( CrossedSquareByXMods, "default crossed square", true, 
    [ IsXMod, IsXMod, IsXMod, IsXMod, IsGroupHomomorphism, IsCrossedPairing ], 
    0,
function( up, left, right, down, action, xpair )

    Error( "this operation is not yet implemented" ); 
    return fail;
end );

##############################################################################
##
#M  CrossedSquareByNormalSubgroups . . . crossed square from normal L,M,N in P
##
InstallMethod( CrossedSquareByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( L, M, N, P )

    local XS, up, lt, rt,dn, a, xp, diag;

    if IsSubgroup( L, P ) and ( L <> P ) then 
        Error( "require ordering L <= M,N <= P" ); 
    fi;
    if not ( IsNormal( P, M ) and IsNormal( P, N ) and IsNormal( L, P ) ) then
        Error( "M,N,L fail to be normal subgroups of P" ); 
    fi;
    if not ( IsNormal( M, L ) and IsNormal( N, L ) ) then
        Error( "L fails to be a normal subgroup of M and of N" ); 
    fi;
    if not ( IsSubgroup( Intersection(M,N), L ) 
         and IsSubgroup( L, CommutatorSubgroup(M,N) ) ) then 
        Error( "require CommutatorSubgroup(M,N) <= L <= Intersection(M,N)" );
    fi;
    up := XModByNormalSubgroup( M, L );
    lt := XModByNormalSubgroup( N, L );
    rt := XModByNormalSubgroup( P, M );
    dn := XModByNormalSubgroup( P, N );
    diag := XModByNormalSubgroup( P, L );
    a := XModAction( diag );
    ##  define the pairing as a commutator
    xp := CrossedPairingByCommutators( N, M, L ); 
    XS := PreCrossedSquareObj( up, lt, rt, dn, a, xp );
##    SetIsCrossedSquare( XS, true ); 
    SetDiagonal2DimensionalGroup( XS, diag ); 
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by normal subgroups" ); 
    fi;
    return XS;
end );

InstallOtherMethod( CrossedSquareByNormalSubgroups, 
    "conjugation crossed square", true, [ IsGroup, IsGroup, IsGroup ], 0,
function( M, N, P )

    local XS, genP, genM, genN, L, genL, u, d, l, r, a, p, diag;

    if not ( IsNormal( P, M ) and IsNormal( P, N ) ) then
        return fail;
    fi;
    L := Intersection( M, N );
    return CrossedSquareByNormalSubgroups( L, M, N, P ); 
end );

###############################################################################
##
#M  CrossedSquareByNormalSubXMod . crossed square from xmod and normal subxmod 
##
InstallMethod( CrossedSquareByNormalSubXMod, "for an xmod and normal subxmod", 
    true, [ IsXMod, IsXMod ], 0,
function( rt, lt )

    local M, L, up, P, N, dn, xp, act1, diag, act, XS;

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
    diag := SubXMod( rt, L, P );
    act := XModAction( diag );
    XS := PreCrossedSquareObj( up, lt, rt, dn, act, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by normal subxmod" ); 
    fi; 
    return XS;
end );

###############################################################################
##
#M  CrossedSquareByXModSplitting . . . xsq by surjection followed by injection
##
InstallMethod( CrossedSquareByXModSplitting, "for an xmod", true, [ IsXMod ], 0,
function( X0 )

    local S, R, bdy, Q, up, dn, act, xp, XS;

    S := Source( X0 ); 
    R := Range( X0 ); 
    Q := ImagesSource( Boundary( X0 ) ); 
    up := SubXMod( X0, S, Q ); 
    dn := XModByNormalSubgroup( R, Q ); 
    act := XModAction( X0 ); 
    xp := CrossedPairingByPreImages( up, up ); 
    XS := PreCrossedSquareObj( up, up, dn, dn, act, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by xmod splitting" ); 
    fi; 
    return XS;
end );

##############################################################################
##
#M  CrossedSquareByPullback . . . . . . . . for two xmods with a common range 
##
InstallMethod( CrossedSquareByPullback, "for 2 xmods with a common range", 
    true, [ IsXMod, IsXMod ], 0,
function( XN, XM )

    local M, N, P, actM, actN, mu, nu, L, genL, lenL, Linfo, dp, dpinfo, 
          embM, embN, bdyM, bdyN, map, xp, autL, genLN, genLM, genLNP, 
          genLMP, genP, lenP, imactPL, i, g, ima, a, actPL, genN, lenN, 
          imactNL, actNL, left, genM, lenM, imactML, actML, up, XS;

    M := Source( XM ); 
    N := Source( XN );
    P := Range( XM ); 
    if not ( Range( XN ) = P ) then 
        Error( "the two xmods should have a common range" ); 
    fi;
    actM := XModAction( XM );
    actN := XModAction( XN );
    mu := Boundary( XM );
    nu := Boundary( XN );
    L := Pullback( nu, mu ); 
    genL := GeneratorsOfGroup( L ); 
    lenL := Length( genL );
    Linfo := PullbackInfo( L ); 
    if HasName(M) and HasName(N) and HasName(P) then 
        SetName( L, Concatenation("(",Name(N)," x_",Name(P)," ",Name(M),")") ); 
    fi;
    dp := Linfo!.directProduct; 
    dpinfo := DirectProductInfo( dp ); 
    embN := Embedding( dp, 1 ); 
    embM := Embedding( dp, 2 ); 
    bdyN := Linfo!.projections[1]; 
    genLN := List( genL, l -> ImageElm( bdyN, l ) );
    bdyM := Linfo!.projections[2]; 
    genLM := List( genL, l -> ImageElm( bdyM, l ) );
    ## construct the crossed pairing 
    map := Mapping2ArgumentsByFunction( [N,M], L, 
               function( c ) 
                   local n, m, nun, mum, n2, m2, l; 
                   n := c[1];
                   m := c[2]; 
                   nun := ImageElm( nu, n );
                   mum := ImageElm( mu, m );
                   ## h(n,m) = (n^{-1}.n^mum, (m^{-1})^nun.m)
                   n2 := n^(-1) * ImageElm(ImageElm(actN,mum),n);
                   m2 := ImageElm(ImageElm(actM,nun),m^(-1)) * m; 
                   l := ImageElm( embN, n2 ) * ImageElm( embM, m2 ); 
                   if not ( l in L ) then 
                       Error( "element l appears not to be in L" ); 
                   fi;
                   return l;
               end );
    xp := CrossedPairingObj( [N,M], L, map ); 
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
    genN := GeneratorsOfGroup( N );
    lenN := Length( genN ); 
    imactNL := ListWithIdenticalEntries( lenN, 0 ); 
    for i in [1..lenN] do 
        g := ImageElm( nu, genN[i] ); 
        a := ImageElm( actPL, g );
        imactNL[i] := a;
    od;
    actNL := GroupHomomorphismByImages( N, autL, genN, imactNL ); 
    left := XMod( bdyN, actNL ); 
    genM := GeneratorsOfGroup( M );
    lenM := Length( genM ); 
    imactML := ListWithIdenticalEntries( lenM, 0 ); 
    for i in [1..lenM] do 
        g := ImageElm( mu, genM[i] ); 
        a := ImageElm( actPL, g );
        imactML[i] := a;
    od;
    actML := GroupHomomorphismByImages( M, autL, genM, imactML ); 
    up := XMod( bdyM, actML ); 
    XS := PreCrossedSquareObj( up, left, XM, XN, actPL, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by pullback" ); 
    fi; 
    return XS;
end );

###############################################################################
##
#M  ActorCrossedSquare . . . create a crossed square from an xmod and its actor
##
InstallMethod( ActorCrossedSquare, "actor crossed square", true, [ IsXMod ], 0,
function( X0 )

    local XS, WX, LX, NX, AX, xp, da;

    AX := ActorXMod( X0 );
    WX := WhiteheadXMod( X0 );
    NX := NorrieXMod( X0 );
    LX := LueXMod( X0 );
    da := XModAction( LX );
    ##  define the pairing as evaluation of a derivation
    xp := CrossedPairingByDerivations( X0 );
    ## XS := PreCrossedSquareObj( X0, WX, NX, AX, da, xp );
    XS := PreCrossedSquareObj( WX, X0, AX, NX, da, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be an actor crossed square" ); 
    fi; 
    return XS;
end );

###############################################################################
##
#M  CrossedSquareByAutomorphismGroup . . . crossed square G -> Inn(G) -> Aut(G)
##
InstallMethod( CrossedSquareByAutomorphismGroup, "G -> Inn(G) -> Aut(G)", 
    true, [ IsGroup ], 0,
function( G )

    local genG, innG, up, autG, lt, act, xp, XS;

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
    lt := XModByNormalSubgroup( autG, innG );
    act := IdentityMapping( autG );
    ##  define the pairing 
    xp := CrossedPairingByConjugators( innG );
    XS := PreCrossedSquareObj( up, up, lt, lt, act, xp );
##    SetIsCrossedSquare( XS, true );
    if not IsCrossedSquare( XS ) then 
        Error( "XS fails to be a crossed square by automorphism group" ); 
    fi; 
    return XS;
end );

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . .  for a 3-dimensional group 
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
#M  Size . . . . . . . . . . . . . . . . . . . . .  for a 3-dimensional group
##
InstallOtherMethod( Size, "method for a 3-dimensional group", true, 
    [ IsHigherDimensionalGroup ], 0,
function( PS ) 
    return [ Size( Source( Up2DimensionalGroup( PS ) ) ), 
             Size( Range( Up2DimensionalGroup( PS ) ) ),
             Size( Source( Down2DimensionalGroup( PS ) ) ),
             Size( Range( Down2DimensionalGroup( PS ) ) ) ]; 
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

##############################################################################
##
#M  \=( <dom1>, <dom2> ) . . . . . . . . . . test if two 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-domains",
    IsIdenticalObj, [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ], 0,
function ( dom1, dom2 ) 
    if not ( ( HigherDimension( dom1 ) = 3 ) 
           and ( HigherDimension( dom2 ) = 3 ) ) then 
        TryNextMethod(); 
    else 
        return( 
            ( Up2DimensionalGroup(dom1) = Up2DimensionalGroup(dom2) )
        and ( Left2DimensionalGroup(dom1) = Left2DimensionalGroup(dom2) ) 
        and ( Down2DimensionalGroup(dom1) = Down2DimensionalGroup(dom2) ) 
        and ( Right2DimensionalGroup(dom1) = Right2DimensionalGroup(dom2) ) ); 
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
        if ( HasIsPreCrossedSquare( g3d ) and IsPreCrossedSquare( g3d ) ) then 
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

InstallMethod( ViewObj, "method for a 3d-group", true, [ IsPreCrossedSquare ], 
    0, PrintObj ); 

#############################################################################
##
#M Display( <g3d> . . . . . . . . . . . . . . . . . . . . display a 3d-group 
##
InstallMethod( Display, "method for a 3d-group", true, 
    [ IsPreCrossedSquare ], 0,
function( g3d )

    local L, M, N, P, lenL, lenM, lenN, lenP, len1, len2, j, q1, q2, 
          ispsq, arrow, ok, n, i;

    if ( HasIsPreCrossedSquare( g3d ) and IsPreCrossedSquare( g3d ) ) then 
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
        Print( "(pre-)crossed square with (pre-)crossed modules:\n" ); 
        Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
        Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
        Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
        Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
        fi; 
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

    local u, d, h1, t1, h2, t2, h1h2, h2h1, t1t2, t2t1, h1t2, 
           t2h1, h2t1, t1h2, G, gensrc, x, y, z;

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
    return ( HigherDimension( P ) = 3 ) 
        and IsCat1Group( Up2DimensionalGroup( P ) )  
        and IsCat1Group( Left2DimensionalGroup( P ) )  
        and IsCat1Group( Down2DimensionalGroup( P ) )  
        and IsCat1Group( Right2DimensionalGroup( P ) );  
end ); 

##############################################################################
##
#M  PreCat2GroupObj ( [<up>,<down>,<left>,<right>(,<diag>)] ) 
##
InstallMethod( PreCat2GroupObj, "for a list of pre-cat1-groups", true,
    [ IsList ], 0,
function( L )

    local len, d, PC, ok;

    len := Length( L ); 
    if not ( len in [4,5] ) then 
        Error( "there should be 4 or 5 pre-cat1-groups in the list L" ); 
    fi; 
    if ( len = 4 ) then 
        Error( "add code here to construct the diagonal d" ); 
    fi;
    d := L[5]; 
    PC := rec();
    ObjectifyWithAttributes( PC, PreCat2GroupObjType, 
      Up2DimensionalGroup, L[1], 
      Left2DimensionalGroup, L[2],
      Down2DimensionalGroup, L[3],
      Right2DimensionalGroup, L[4],
      Diagonal2DimensionalGroup, d, 
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
#F  (Pre)Cat2Group( C1G1, C1G2 )         cat2-group from two (pre)cat1-groups
#F  (Pre)Cat2Group( XS )                 cat2-group from (pre)crossed square
##
InstallGlobalFunction( PreCat2Group, function( arg )

    local nargs, C1G1, C1G2, C2G, S1, S2, iso, idR2, isoC1, dr, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 2 ) ) then
        Print( "standard usage: (Pre)Cat2Group( cat1, cat1 );\n" );
        Print( "            or: (Pre)Cat2Group( XS );\n" );
        return fail;
    fi; 
    if ( nargs = 1 ) then 
        if not IsPreCrossedSquare( arg[1] ) then 
            Error( "argument is not a pre-crossed square" ); 
        fi; 
        C2G := PreCat2GroupOfPreCrossedSquare( arg[1] );
    else 
        C1G1 := arg[1]; 
        C1G2 := arg[2];
        if not IsPreCat1Group( C1G1 ) and IsPreCat1Group( C1G2 ) then 
            Error( "the two arguments are not pre-cat1-groups" ); 
        fi; 
        S1 := Source( C1G1 ); 
        S2 := Source( C1G2 ); 
        ## if the two sources are unequal but isomorphic then make 
        ## an isomorphic copy of C1G2 with the same source as C1G1
        if not ( S1 = S2 ) then 
            iso := IsomorphismGroups( S2, S1 ); 
            if ( iso = fail ) then 
                Error( "the two arguments are not pre-cat1-groups" ); 
            else 
                idR2 := IdentityMapping( Range( C1G2 ) ); 
                isoC1 := IsomorphismByIsomorphisms( C1G2, [ iso, idR2 ] );
                C1G2 := Range( isoC1 ); 
            fi; 
        fi; 
        dr := DetermineRightDownCat1Groups( C1G1, C1G2 );
        C2G := PreCat2GroupByPreCat1Groups( C1G1, C1G2, dr[1], dr[2] ); 
        if ( C2G = fail ) then 
            Error( "C2G fails to be a PreCat2Group" ); 
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

    local nargs, arg2, C2G, ok; 

    nargs := Length( arg );
    if ( nargs = 1 ) then 
        C2G := PreCat2Group( arg[1] ); 
    elif ( nargs = 2 ) then 
        C2G := PreCat2Group( arg[1], arg[2] ); 
    else 
        Print( "standard usage: (Pre)Cat2Group( cat1, cat1 );\n" );
        Print( "            or: (Pre)Cat2Group( XS );\n" );
        return fail; 
    fi;
    ok := IsCat2Group( C2G ); 
    if ok then 
        return C2G; 
    else 
        return fail; 
    fi; 
end ); 

##############################################################################
##
#M  DetermineRightDownCat1Groups . . . . . . . . . . . for two pre-cat1-groups
## 
InstallMethod( DetermineRightDownCat1Groups, "for two pre-cat1-groups", true,
    [ IsPreCat1Group, IsPreCat1Group ], 0,
function( up, left )

    local G, genG, R1, R2, genR1, genR2, ddt1, ddh1, dde1, ddt2, ddh2, dde2, 
          tau1, tau2, im, dt1, dh1, de1, dt2, dh2, de2,  
          P, genP, isoP, invP, down, right, dt0, dh0, de0, diag, PC2, ok; 

    G := Source( up ); 
    genG := GeneratorsOfGroup( G ); 
    R1 := Range( up ); 
    genR1 := GeneratorsOfGroup( R1 ); 
    if not ( G = Source( left ) ) then 
        Error( "the two cat1-groups should have the same source" ); 
    fi; 
    R2 := Range( left );
    genR2 := GeneratorsOfGroup( R2 );
    ddt1 := TailMap( up ); 
    ddh1 := HeadMap( up ); 
    dde1 := RangeEmbedding( up ); 
    ddt2 := TailMap( left ); 
    ddh2 := HeadMap( left ); 
    dde2 := RangeEmbedding( left ); 
    ## check that the 1-maps commute with the 2-maps 
    if not ( ( ddt1*dde1*ddt2*dde2 = ddt2*dde2*ddt1*dde1 ) 
         and ( ddh1*dde1*ddh2*dde2 = ddh2*dde2*ddh1*dde1 ) 
         and ( ddt1*dde1*ddh2*dde2 = ddh2*dde2*ddt1*dde1 ) 
         and ( ddh1*dde1*ddt2*dde2 = ddt2*dde2*ddh1*dde1 ) )  then 
        Error( "1-maps do not commute with the 2-maps" ); 
    fi; 
    Info( InfoXMod, 1, "yes : 1-maps do commute with the 2-maps" ); 
    ## more checks? 
    im := List( genG, g -> ImageElm( ddt1 * dde1, g ) ); 
    tau1 := GroupHomomorphismByImages( G, G, genG, im ); 
    im := List( genG, g -> ImageElm( ddt2 * dde2, g ) ); 
    tau2 := GroupHomomorphismByImages( G, G, genG, im ); 
    P := Intersection( ImagesSource( tau1 ), ImagesSource( tau2 ) ); 
    genP := GeneratorsOfGroup( P ); 
    if ( genP = [ ] ) then 
        dt1 := MappingToOne( R2, P ); 
        dh1 := MappingToOne( R2, P ); 
        de1 := MappingToOne( P, R2 ); 
        dt2 := MappingToOne( R1, P ); 
        dh2 := MappingToOne( R1, P ); 
        de2 := MappingToOne( P, R1 ); 
    else 
        im := List( genR2, g -> ImageElm( dde2 * tau1, g ) ); 
        dt1 := GroupHomomorphismByImages( R2, P, genR2, im ); 
        im := List( genR2, g -> ImageElm( dde2 * ddh1 * dde1, g ) ); 
        dh1 := GroupHomomorphismByImages( R2, P, genR2, im ); 
        im := List( genP, g -> ImageElm( ddt2, g ) ); 
        de1 := GroupHomomorphismByImages( P, R2, genP, im ); 
        im := List( genR1, g -> ImageElm( dde1 * tau2, g ) ); 
        dt2 := GroupHomomorphismByImages( R1, P, genR1, im ); 
        im := List( genR1, g -> ImageElm( dde1 * ddh2 * dde2, g ) ); 
        dh2 := GroupHomomorphismByImages( R1, P, genR1, im ); 
        im := List( genP, g -> ImageElm( ddt1, g ) ); 
        de2 := GroupHomomorphismByImages( P, R1, genP, im ); 
    fi; 
    down := PreCat1GroupByTailHeadEmbedding( dt1, dh1, de1 ); 
    Info( InfoXMod, 1, "cat1-group down constructed" ); 
    right := PreCat1GroupByTailHeadEmbedding( dt2, dh2, de2 );
    Info( InfoXMod, 1, "cat1-group right constructed" ); 
    if ( ( right = fail ) or ( down = fail) ) then 
        Error( "right or down fail to be cat1-groups" ); 
        return fail; 
    fi; 
    return [ right, down ]; 
end ); 

##############################################################################
##
#M  PreCat2GroupByPreCat1Groups . . . . . . . . . . . for four pre-cat1-groups
## 
InstallMethod( PreCat2GroupByPreCat1Groups, "for four pre-cat1-groups", true,
    [ IsPreCat1Group, IsPreCat1Group, IsPreCat1Group, IsPreCat1Group ], 0,
function( up, left, right, down )

    local G, genG, R1, R2, P, genR1, genR2, genP, 
          ddt1, ddh1, dde1, ddt2, ddh2, dde2, dt1, dh1, de1, dt2, dh2, de2, 
          imt, imt2, imh, imh2, ime, ime2, dt0, dh0, de0, diag, PC2, ok;

    G := Source( up ); 
    genG := GeneratorsOfGroup( G ); 
    R1 := Range( up ); 
    R2 := Range( left );
    P := Range( down ); 
    if not ( ( G = Source( left ) ) and ( R1 = Source( right ) ) 
             and ( R2 = Source( down ) ) and ( P = Range( right ) ) ) then 
        Error( "sources and/or ranges do not agree" ); 
    fi; 
    genR1 := GeneratorsOfGroup( R1 ); 
    genR2 := GeneratorsOfGroup( R2 );
    genP := GeneratorsOfGroup( P ); 
    ddt1 := TailMap( up ); 
    ddh1 := HeadMap( up ); 
    dde1 := RangeEmbedding( up ); 
    ddt2 := TailMap( left ); 
    ddh2 := HeadMap( left ); 
    dde2 := RangeEmbedding( left ); 
    dt1 := TailMap( down ); 
    dh1 := HeadMap( down ); 
    de1 := RangeEmbedding( down ); 
    dt2 := TailMap( right ); 
    dh2 := HeadMap( right ); 
    de2 := RangeEmbedding( right ); 

    imt := List( genG, g -> ImageElm( dt1, ImageElm( ddt2, g ) ) ); 
    dt0 := GroupHomomorphismByImages( G, P, genG, imt ); 
    imt2 := List( genG, g -> ImageElm( dt2, ImageElm( ddt1, g ) ) ); 
    imh := List( genG, g -> ImageElm( dh1, ImageElm( ddh2, g ) ) ); 
    dh0 := GroupHomomorphismByImages( G, P, genG, imh ); 
    imh2 := List( genG, g -> ImageElm( dh2, ImageElm( ddh1, g ) ) ); 
    ime := List( genP, g -> ImageElm( dde2, ImageElm( de1, g ) ) ); 
    de0 := GroupHomomorphismByImages( P, G, genP, ime ); 
    ime2 := List( genP, g -> ImageElm( dde1, ImageElm( de2, g ) ) ); 
    if not ( ( imt = imt2 ) and ( imh = imh2 ) and ( ime = ime2 ) ) then 
        Error( "tail/head/embedding maps do not all commute" ); 
    fi; 
    diag := PreCat1GroupByTailHeadEmbedding( dt0, dh0, de0 ); 
    PC2 := PreCat2GroupObj( [ up, left, down, right, diag ] );
    SetIsPreCat2Group( PC2, true );
    ok := IsCat2Group( PC2 );
    return PC2;
end ); 

##############################################################################
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

##############################################################################
## 
#?  this function should be got rid of a.s.a.p. 
##
#M  ElementsRelationsForSemidirectProduct 
##
InstallMethod( ElementsRelationsForSemidirectProduct, "elements relation",
    true, [ IsGroup ], 0,
function( GxH  )

    local info, G, H, elG, embG, embH, elH, g, h, im, list1, list2, genGxH;

    if not HasSemidirectProductInfo( GxH ) then 
        Error( "group is not a semidirect product" ); 
    fi; 
    info := SemidirectProductInfo( GxH ); 
    G := info!.groups[1]; 
    H := info!.groups[2];
    elG := Elements( G );
    elH := Elements( H );
    list1 := [ ];
    list2 := [ ];
    embG := info!.embeddings[1];
    embH := info!.embeddings[2];
    for g in elG do
        for h in elH do
            im := ImageElm( embG, g) * ImageElm( embH, h );
            Add( list1, [g,h] );
            Add( list2, im );
        od;
    od;
    return [ list1, list2 ];
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
 
    local n, l, i, j, k, up, down, left, right, isolar, liste1, liste2, G, 
          gensrc, x, u, d, h1, t1, h2, t2, L, M, N, P, XS, diag, liste, 
          partial, action, aut, actML, XM, kappa, CM1, CM2, lambda,
          actNL, CM3, mu, actPM, CM4, nu, actPN, xp, actPL;

    u := GeneratingCat1Groups( C2G )[1];
    d := GeneratingCat1Groups( C2G )[2];
    
    if not ( IsPerm2DimensionalGroup( u ) ) then
        u := Image(IsomorphismPermObject( u ) );
    fi;
    if not ( IsPerm2DimensionalGroup( d ) ) then
        d := Image(IsomorphismPermObject( d ) );
    fi;    
    
    h1 := HeadMap( u );
    t1 := TailMap( u );
    h2 := HeadMap( d );
    t2 := TailMap( d );
    
    G := Image( IsomorphismPermObject( Source( t1 ) ) );
    gensrc := GeneratorsOfGroup( G ); 

    t1 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> ImageElm( t1, x ) ) ); 
    h1 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> ImageElm( h1, x ) ) ); 
    t2 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> ImageElm( t2, x ) ) ); 
    h2 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> ImageElm( h2, x ) ) ); 
    
    L := Intersection( Kernel( t1 ), Kernel( t2 ) ) ;
    M := Intersection( Image( t1 ), Kernel( t2 ) );
    N := Intersection( Kernel ( t1 ), Image( t2 ) );
    P := Intersection( Image( t1 ), Image( t2 ) )  ;
    
    Info( InfoXMod, 4, "G = ", G );
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
    xp := CrossedPairingByCommutators( N, M, L );
    XS := PreCrossedSquareObj( up, left, right, down, actPL, xp );
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
 
    local up, left, down, right, L, M, N, P, genL, genM, genN, genP, 
          kappa, lambda, nu, mu, 
          act_up, act_lt, act_dn, act_rt, act_diag, xpair, 
          Cup, NxL, genNxL, e1NxL, e2NxL, Cleft, MxL, genMxL, e1MxL, e2MxL, 
          Cdown, PxM, genPxM, e1PxM, e2PxM, Cright, PxN, genPxN, e1PxN, e2PxN, 
          MxLbyP, MxLbyN, autgenMxL, autMxL, actPNML, NxLbyP, NxLbyM, 
          autgenNxL, autNxL, actPMNL, imPNML, bdyPNML, XPNML, CPNML, PNML, 
          e1PNML, e2PNML, genPNML, imPMNL, bdyPMNL, XPMNL, CPMNL, PMNL, 
          e1PMNL, e2PMNL, genPMNL, imiso, iso, inv, iminv, tup, hup, eup, 
          C2PNML, tlt, hlt, elt, tdn, hdn, edn, trt, hrt, ert, tdi, hdi, edi, 
          PC, Cdiag, imnukappa, nukappa, morCleftCright, immulambda, mulambda, 
          morCupCdown; 

    Info( InfoXMod, 1, "these conversion functions are under development\n" ); 

    up := Up2DimensionalGroup(XS);
    left := Left2DimensionalGroup(XS);
    right := Right2DimensionalGroup(XS);
    down := Down2DimensionalGroup(XS);
    L := Source(up);
    M := Range(up);
    N := Source(down);
    P := Range(down);
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
    act_diag := DiagonalAction( XS );
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
                                 ImageElm( ImageElm( act_diag, p ), l ))) ))); 
    NxLbyM := List( genM, m -> GroupHomomorphismByImages( NxL, NxL, genNxL, 
            Concatenation( 
                List( genN, n -> ImageElm( e1NxL, n ) * 
                                 ImageElm( e2NxL, 
                                 ImageElmCrossedPairing( xpair, [n,m] ))), 
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
                                 ImageElm( ImageElm( act_diag, p ), l ))) ))); 
    MxLbyN := List( genN, n -> GroupHomomorphismByImages( MxL, MxL, genMxL, 
            Concatenation( 
                List( genM, m -> ImageElm( e1MxL, m ) * 
                                 ImageElm( e2MxL,  
                                 ImageElmCrossedPairing( xpair, [n,m] )^(-1) )), 
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
    PC := PreCat2GroupByPreCat1Groups( CPMNL, C2PNML, Cright, Cdown );
    Cdiag := PreCat1GroupByTailHeadEmbedding( tdi, hdi, edi ); 
    SetDiagonal2DimensionalGroup( PC, Cdiag ); 
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
    SetCat2GroupOfCrossedSquare( XS, C2 );
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
    
##############################################################################
##
#M  Diagonal2DimensionalGroup . . . . . . . . . . . . . . for a crossed square
## 
InstallMethod( Diagonal2DimensionalGroup, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )

    local ur, dl, act, diag; 

    ur := Boundary( Up2DimensionalGroup( s ) ) 
          * Boundary( Right2DimensionalGroup( s ) ); 
    dl := Boundary( Left2DimensionalGroup( s ) ) 
          * Boundary( Down2DimensionalGroup( s ) ); 
    if not ( ur = dl ) then 
        Error( "diagonal boundary is not well-defined" );
    fi;
    act := DiagonalAction( s );
    return XModByBoundaryAndAction( ur, act );  
end );

##############################################################################
##
#M  Transpose3DimensionalGroup . . . . . . . the transpose of a crossed square
##
InstallMethod( Transpose3DimensionalGroup, "transposed crossed square", true, 
    [ IsCrossedSquare ], 0,
function( XS )

    local xpS, NxM, L, map, xpT, XT;

    xpS := CrossedPairing( XS );
    NxM := Reversed( Source( xpS ) );
    L := Range( xpS );
    map := Mapping2ArgumentsByFunction( NxM, L, 
        function(c) 
            return ImageElmCrossedPairing( xpS, Reversed(c) )^(-1); 
        end );
    xpT := CrossedPairingObj( NxM, L, map );
    XT := PreCrossedSquareObj( Left2DimensionalGroup(XS), 
              Up2DimensionalGroup(XS), Down2DimensionalGroup(XS), 
              Right2DimensionalGroup(XS), DiagonalAction(XS), xpT );
##    SetIsCrossedSquare( XT, true );
    return XT;
end );    

##############################################################################
##
#M  LeftRightMorphism . . . . . . . . . . . . . . . . for a precrossed square
#M  UpDownMorphism  . . . . . . . . . . . . . . . . . for a precrossed square
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
#E gp3obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
