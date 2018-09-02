##############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)crossed squares 
##  and (pre-)cat2-groups.
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
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
    return ImageElm( CrossedPairingMap( xp ), elm );
end );

##############################################################################
##
#M  CrossedPairingByNormalSubgroups( <grp>, <grp>, <grp> ) . . . make an xpair
##
InstallMethod( CrossedPairingByNormalSubgroups, 
    "for the intersection of two normal subgroups", true,
    [ IsGroup, IsGroup, IsGroup ], 0,
function( M, N, L )

    local map, xp;

    if not ( IsNormal( M, L ) and IsNormal( N, L ) ) then 
        Error( "L not normal in M and N" );
    fi;
    map := Mapping2ArgumentsByFunction( [M,N], L, 
               function(c) return Comm( c[1], c[2] ); end );
    xp := CrossedPairingObj( [M,N], L, map );
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
                   local r, p, pos, chi;
                   r := t[1];  p := t[2];
                   pos := Position( Elements( WX ), p );
                   chi := DerivationByImages( X0, imlist[pos] ); 
                   return DerivationImage( chi, r ); 
               end );
    return CrossedPairingObj( [RX,WX], SX, map );
end );

##############################################################################
##
#M  CrossedPairingByXModAction( <xmod> ) . . convert action to crossed pairing
##
InstallMethod( CrossedPairingByXModAction, "for a crossed module", true,
    [ IsXMod ], 0,
function( X0 )

    local S, R, act, map, xp;

    S := Source( X0 ); 
    R := Range( X0 ); 
    act := XModAction( X0 );
    map := Mapping2ArgumentsByFunction( [S,R], S, 
               function(c) 
                   return c[1]^(-1) * ImageElm( ImageElm(act,c[2]), c[1] ); 
               end );
    xp := CrossedPairingObj( [S,R], S, map );
    return xp;
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
    d := Down2DimensionalGroup( P );
    r := Right2DimensionalGroup( P );
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
    return true;
end );

##############################################################################
##
#M  PreCrossedSquareObj ( <up>, <down>, <left>, <right>, <act>, <pair> ) 
##                                               . . . make a PreCrossedSquare
##
InstallMethod( PreCrossedSquareObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( u, l, d, r, a, p )

    local PS, ok, src, rng, aut, narne;

    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, PreCrossedSquareObjType, 
      Up2DimensionalGroup, u, 
      Left2DimensionalGroup, l,
      Down2DimensionalGroup, d,
      Right2DimensionalGroup, r,
      CrossedPairing, p,
      DiagonalAction, a,
      IsHigherDimensionalGroup, true );
    if not IsPreCrossedSquare( PS ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed square." );
    fi;
    return PS;
end );

#############################################################################
##
#F  (Pre)CrossedSquare( <up>, <left>, <down>, <right>, <action>, <pairing> ) 
##      . . . . . . crossed square from xmods
#F  (Pre)CrossedSquare( <P>, <N>, <M>, <L> )  
##      . . . . . . crossed square normal subgroups
#F  (Pre)CrossedSquare( <xmod> )                                                 
##      . . . . . . actor crossed square
#F  (Pre)CrossedSquare( <(pre)cat2-group> )                                            
##      . . . . . . crossed square from (pre)cat2-group
##
InstallGlobalFunction( PreCrossedSquare, function( arg )

    local nargs, XS, ok;

    nargs := Length( arg );
    if ( nargs = 1 ) then
        if ( IsHigherDimensionalGroup( arg[1] ) ) then
            XS := PreCrossedSquareOfPreCat2Group( arg[1] );
        elif  IsXMod( arg[1] )   then
            XS := ActorCrossedSquare( arg[1] );
        fi;
    elif ( nargs = 4 ) then
        XS := CrossedSquareByNormalSubgroups(arg[1],arg[2],arg[3],arg[4]);
    elif ( nargs = 6  ) then
        XS := CrossedSquareByXMods(arg[1],arg[2],arg[3],arg[4],arg[5],arg[6]);
    else   
        Print( "standard usage: CrossedSquare( <up>, <left>, <down>, <right>, <action>, <pairing> );\n" );
        Print( "            or: CrossedSquare( <P>, <N>, <M>, <L> );\n" );
        Print( "            or: CrossedSquare( <xmod> );\n" );
        Print( "            or: PreCrossedSquare( <pre-cat2-group> );\n" );
        return fail;
    fi;
    ok := IsPreCrossedSquare( XS );
    if ok then 
        ok := IsCrossedSquare( XS ); 
        return XS;
    else
        return fail;
    fi;
end );

InstallGlobalFunction( CrossedSquare, function( arg )

    local nargs, XS, ok;

    if ( nargs = 1 ) then 
        XS := PreCrossedSquare( arg[1] ); 
    elif ( nargs = 4 ) then 
        XS := PreCrossedSquare( arg[1], arg[2], arg[3], arg[4] ); 
    elif ( nargs = 6 ) then 
        XS := PreCrossedSquare(arg[1], arg[2], arg[3], arg[4], arg[5], arg[6]); 
    else 
        Print( "standard usage: CrossedSquare( <up>, <left>, <down>, <right>, <action>, <pairing> );\n" );
        Print( "            or: CrossedSquare( <P>, <N>, <M>, <L> );\n" );
        Print( "            or: CrossedSquare( <xmod> );\n" );
        Print( "            or: PreCrossedSquare( <pre-cat2-group> );\n" );
        return fail;
    fi;
    if ( XS = fail ) then 
        return fail; 
    else 
        ok := IsCrossedSquare( XS ); 
        if ( ok = fail ) then 
            return fail; 
        else 
            return XS; 
        fi; 
    fi;
end );

##############################################################################
##
#M  CrossedSquareByXMods . . . . crossed square from 4 xmods + action & xpair
##
InstallMethod( CrossedSquareByXMods, "default crossed square", true, 
    [ IsXMod, IsXMod, IsXMod, IsXMod, IsGroupHomomorphism, IsCrossedPairing ], 
    0,
function( top, left, down, right, action, xpair )

    Error( "this operation is not yet implemented" ); 
    return fail;
end );

##############################################################################
##
#M  CrossedSquareByNormalSubgroups . . . . crossed square from normal M,N in P
##
InstallMethod( CrossedSquareByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( P, N, M, L )

    local XS, u, d, l, r, a, xp, diag;

    if not ( IsNormal( P, M ) and IsNormal( P, N ) 
             and IsNormal( M, L ) and IsNormal( N, L ) ) then
        Error( "M,N,L fail to be normal subgroups of P" ); 
    fi;
    if not ( IsSubgroup( Intersection(M,N), L ) 
         and IsSubgroup( L, CommutatorSubgroup(M,N) ) ) then 
        Error( "require CommutatorSubgroup(M,N) <= L <= Intersection(M,N)" );
    fi;
    u := XModByNormalSubgroup( N, L );
    l := XModByNormalSubgroup( M, L );
    d := XModByNormalSubgroup( P, M );
    r := XModByNormalSubgroup( P, N );
    diag := XModByNormalSubgroup( P, L );
    a := XModAction( diag );
    ##  define the pairing as a commutator
    xp := CrossedPairingByNormalSubgroups( M, N, L ); 
    XS := PreCrossedSquareObj( u, l, d, r, a, xp );
    SetIsCrossedSquare( XS, true );
    SetDiagonal2DimensionalGroup( XS, diag ); 
    return XS;
end );

InstallOtherMethod( CrossedSquareByNormalSubgroups, 
    "conjugation crossed square", true, [ IsGroup, IsGroup, IsGroup ], 0,
function( P, M, N )

    local XS, genP, genM, genN, L, genL, u, d, l, r, a, p, diag;

    if not ( IsNormal( P, M ) and IsNormal( P, N ) ) then
        return fail;
    fi;
    L := Intersection( M, N );
    return CrossedSquareByNormalSubgroups( P, M, N, L );;
end );

###############################################################################
##
#M  CrossedSquareByXModUpDown . . . . . . create a crossed square from an xmod 
##
InstallMethod( CrossedSquareByXModUpDown, "use a repeated xmod", true, 
    [ IsXMod ], 0,
function( X0 )

    local S, R, X1S, X1R, xp, XS;

    S := Source( X0 ); 
    X1S := XModByNormalSubgroup( S, S ); 
    R := Range( X0 ); 
    X1R := XModByNormalSubgroup( R, R ); 
    xp := CrossedPairingByXModAction( X0 ); 
    XS := PreCrossedSquareObj( X0, X1S, X0, X1R, XModAction( X0 ), xp );
    SetIsCrossedSquare( XS, true );
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
    XS := PreCrossedSquareObj( WX, X0, NX, AX, da, xp );
    SetIsCrossedSquare( XS, true );
    return XS;
end );

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . . for a pre-crossed square
##
InstallMethod( Name, "method for a pre-crossed square", true, 
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
        M := Source( Down2DimensionalGroup( g3d ) );
        N := Range( Up2DimensionalGroup( g3d ) );
        P := Range( Down2DimensionalGroup( g3d ) );
        ok := HasName(L) and HasName(M) and HasName(N) and HasName(P);
        if ok then
            lenL := Length( Name( L ) );
            lenM := Length( Name( M ) );
            lenN := Length( Name( N ) );
            lenP := Length( Name( P ) );
            len1 := Maximum( lenL, lenM );
            len2 := Maximum( lenN, lenP );
            q1 := QuoInt( len1, 2 );
            q2 := QuoInt( len2, 2 );
            Print( "[ " );
            for j in [1..lenM-lenL] do Print(" "); od;
            Print( Name(L), arrow, Name(N) );
            for j in [1..lenP-lenN] do Print(" "); od;
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
            for j in [1..lenL-lenM] do Print(" "); od;
            Print( Name(M), " -> ", Name(P) );
            for j in [1..lenN-lenP] do Print(" "); od;
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
            Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
            Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
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
    M := Source( Down2DimensionalGroup( g3d ) );
    N := Range( Up2DimensionalGroup( g3d ) );
    P := Range( Down2DimensionalGroup( g3d ) );
    ok := HasName(L) and HasName(M) and HasName(N) and HasName(P);
    if ok then
        lenL := Length( Name( L ) );
        lenM := Length( Name( M ) );
        lenN := Length( Name( N ) );
        lenP := Length( Name( P ) );
        len1 := Maximum( lenL, lenM );
        len2 := Maximum( lenN, lenP );
        q1 := QuoInt( len1, 2 );
        q2 := QuoInt( len2, 2 );
        Print( "[ " );
        for j in [1..lenM-lenL] do Print(" "); od;
        Print( Name(L), arrow, Name(N) );
        for j in [1..lenP-lenN] do Print(" "); od;
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
        for j in [1..lenL-lenM] do Print(" "); od;
        Print( Name(M), " -> ", Name(P) );
        for j in [1..lenN-lenP] do Print(" "); od;
        Print( " ]\n" );
    else 
        if ispsq then 
        Print( "(pre-)crossed square with (pre-)crossed modules:\n" ); 
        Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
        Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
        Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
        Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
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
        dr := DetermineDownRightCat1Groups( C1G1, C1G2 );
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
#M  DetermineDownRightCat1Groups . . . . . . . . . . . for two pre-cat1-groups
## 
InstallMethod( DetermineDownRightCat1Groups, "for two pre-cat1-groups", true,
    [ IsPreCat1Group, IsPreCat1Group ], 0,
function( top, left )

    local G, genG, R1, R2, genR1, genR2, ddt1, ddh1, dde1, ddt2, ddh2, dde2, 
          tau1, tau2, im, dt1, dh1, de1, dt2, dh2, de2,  
          P, genP, isoP, invP, down, right, dt0, dh0, de0, diag, PC2, ok; 

    G := Source( top ); 
    genG := GeneratorsOfGroup( G ); 
    R1 := Range( top ); 
    genR1 := GeneratorsOfGroup( R1 ); 
    if not ( G = Source( left ) ) then 
        Error( "the two cat1-groups should have the same source" ); 
    fi; 
    R2 := Range( left );
    genR2 := GeneratorsOfGroup( R2 );
    ddt1 := TailMap( top ); 
    ddh1 := HeadMap( top ); 
    dde1 := RangeEmbedding( top ); 
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
    return [ down, right ]; 
end ); 

##############################################################################
##
#M  PreCat2GroupByPreCat1Groups . . . . . . . . . . . for four pre-cat1-groups
## 
InstallMethod( PreCat2GroupByPreCat1Groups, "for four pre-cat1-groups", true,
    [ IsPreCat1Group, IsPreCat1Group, IsPreCat1Group, IsPreCat1Group ], 0,
function( top, left, down, right )

    local G, genG, R1, R2, P, genR1, genR2, genP, 
          ddt1, ddh1, dde1, ddt2, ddh2, dde2, dt1, dh1, de1, dt2, dh2, de2, 
          imt, imt2, imh, imh2, ime, ime2, dt0, dh0, de0, diag, PC2, ok;

    G := Source( top ); 
    genG := GeneratorsOfGroup( G ); 
    R1 := Range( top ); 
    R2 := Range( left );
    P := Range( down ); 
    if not ( ( G = Source( left ) ) and ( R1 = Source( right ) ) 
             and ( R2 = Source( down ) ) and ( P = Range( right ) ) ) then 
        Error( "sources and/or ranges do not agree" ); 
    fi; 
    genR1 := GeneratorsOfGroup( R1 ); 
    genR2 := GeneratorsOfGroup( R2 );
    genP := GeneratorsOfGroup( P ); 
    ddt1 := TailMap( top ); 
    ddh1 := HeadMap( top ); 
    dde1 := RangeEmbedding( top ); 
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
    PC2 := PreCat2GroupObj( [ top, left, down, right, diag ] );
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
    "conjugation action for crossed square", true, [ IsGroup, IsGroup ], 0,
function( G, N )

    local genrng, gensrc, autgen, g, imautgen, a, idsrc, aut, act;

    genrng := GeneratorsOfGroup( G );
    gensrc := GeneratorsOfGroup( N );
    autgen := [ ];
    for g in genrng do
        imautgen := List( gensrc, n -> n^g );
        a := GroupHomomorphismByImages( N, N, gensrc, imautgen );
        Add( autgen, a );
    od;
    if ( Length( genrng ) = 0 ) then
        idsrc := IdentityMapping( N );
        aut := Group( idsrc );
    else
        aut := Group( autgen );
    fi;
    SetIsGroupOfAutomorphisms( aut, true );
    act := GroupHomomorphismByImages( G, aut, genrng, autgen );
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
          partial, action, aut, act, XM, bdy1, CM1, CM2, bdy2,
          act2, CM3, bdy3, act3, CM4, bdy4, act4, xp, a;

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
    
    bdy1 := GroupHomomorphismByFunction( L, M, x -> ImageElm(h1,x) );
    act := ConjugationActionForCrossedSquare(M,L);
    up := XModByBoundaryAndAction(bdy1,act);
    
    bdy2 := GroupHomomorphismByFunction( L, N, x -> ImageElm(h2,x) );
    act2 := ConjugationActionForCrossedSquare(N,L);
    left := XModByBoundaryAndAction(bdy2,act2);
    
    bdy3 := GroupHomomorphismByFunction( N, P, x -> ImageElm(h1,x) );
    act3 := ConjugationActionForCrossedSquare(P,N);
    down := XModByBoundaryAndAction(bdy3,act3);
    
    bdy4 := GroupHomomorphismByFunction( M, P, x -> ImageElm(h2,x) );
    act4 := ConjugationActionForCrossedSquare(P,M);
    right := XModByBoundaryAndAction(bdy4,act4);
    
    Info( InfoXMod, 3, "   up = ", up );
    Info( InfoXMod, 3, " left = ", left );
    Info( InfoXMod, 3, " down = ", down );
    Info( InfoXMod, 3, "right = ", right );

    a := ConjugationActionForCrossedSquare( P, L );
    xp := CrossedPairingByNormalSubgroups( M, N, L );
    XS := PreCrossedSquareObj( up, left, down, right, a, xp );
    ## SetIsCrossedSquare( XS, true );
    if HasName( C2G ) then 
        SetName( XS, Concatenation( "xsq(", Name( C2G ), ")" ) ); 
    fi; 
    return XS;
end );

InstallMethod( PreCat2GroupOfPreCrossedSquare, true, 
    [ IsPreCrossedSquare ], 0,
function( XS )
 
    local up, left, down, right, L, M, N, P, genL, genM, genN, genP, 
          bdy_up, bdy_lt, bdy_dn, bdy_rt, act_up, act_lt, act_dn, act_rt, 
          act_diag, xpair, 
          Cup, NxL, genNxL, e1NxL, e2NxL, Cleft, MxL, genMxL, e1MxL, e2MxL, 
          Cdown, PxM, genPxM, e1PxM, e2PxM, Cright, PxN, genPxN, e1PxN, e2PxN, 
          autgenMxL, autMxL, actPNML, autgenNxL, autNxL, actPMNL, 
          imPNML, bdyPNML, XPNML, CPNML, PNML, e1PNML, e2PNML, genPNML, 
          imPMNL, bdyPMNL, XPMNL, CPMNL, PMNL, e1PMNL, e2PMNL, genPMNL, 
          imiso, iso, inv, iminv, guess, ok, tup, hup, eup, C2PMNL, 
          tlt, hlt, elt, tdn, hdn, edn, trt, hrt, ert, tdi, hdi, edi, PC, Cdiag; 

    Info( InfoXMod, 1, "these conversion functions are under development\n" ); 

    up := Up2DimensionalGroup(XS);
    left := Left2DimensionalGroup(XS);
    down := Down2DimensionalGroup(XS);
    right := Right2DimensionalGroup(XS);
     
    L := Source(up);
    N := Range(up);
    M := Source(down);
    P := Range(down);
    genL := GeneratorsOfGroup( L ); 
    genM := GeneratorsOfGroup( M ); 
    genN := GeneratorsOfGroup( N ); 
    genP := GeneratorsOfGroup( P ); 

    Info( InfoXMod, 4, "L = ", L );    
    Info( InfoXMod, 4, "M = ", M );
    Info( InfoXMod, 4, "N = ", N );
    Info( InfoXMod, 4, "P = ", P );

    bdy_up := Boundary(up);
    bdy_lt := Boundary(left);
    bdy_dn := Boundary(down);
    bdy_rt := Boundary(right);
     
    act_up := XModAction(up);
    act_lt := XModAction(left);
    act_dn := XModAction(down);
    act_rt := XModAction(right);
    act_diag := DiagonalAction(XS);
    xpair := CrossedPairing( XS );

    Cup := Cat1GroupOfXMod( up );
    NxL := Source( Cup );
    e1NxL := Embedding( NxL, 1 );
    e2NxL := Embedding( NxL, 2 );
    genNxL := Concatenation( List( genN, n -> ImageElm( e1NxL, n ) ), 
                             List( genL, l -> ImageElm( e2NxL, l ) ) ); 
    Cleft := Cat1GroupOfXMod( left );
    MxL := Source( Cleft );
    e1MxL := Embedding( MxL, 1 );
    e2MxL := Embedding( MxL, 2 );
    genMxL := Concatenation( List( genM, m -> ImageElm( e1MxL, m ) ), 
                             List( genL, l -> ImageElm( e2MxL, l ) ) ); 
    Cdown := Cat1GroupOfXMod( down ); 
    PxM := Source( Cdown ); 
    e1PxM := Embedding( PxM, 1 );
    e2PxM := Embedding( PxM, 2 );
    genPxM := Concatenation( List( genP, p -> ImageElm( e1PxM, p ) ), 
                             List( genM, m -> ImageElm( e2PxM, m ) ) ); 
    Cright := Cat1GroupOfXMod( right ); 
    PxN := Source( Cright ); 
    e1PxN := Embedding( PxN, 1 );
    e2PxN := Embedding( PxN, 2 );
    genPxN := Concatenation( List( genP, p -> ImageElm( e1PxN, p ) ), 
                             List( genN, n -> ImageElm( e2PxN, n ) ) ); 

    ## construct the action of PxN on MxL using: 
    ## (m,l)^(p,n) = ( m^p, (m^p \box n).l^{pn} ) 
    autgenMxL := Concatenation( 
        List( genP, p -> GroupHomomorphismByImages( MxL, MxL, genMxL, 
            Concatenation( 
                List( genM, m -> ImageElm( e1MxL, 
                                 ImageElm( ImageElm( act_dn, p ), m ) )),
                List( genL, l -> ImageElm( e2MxL, 
                                 ImageElm( ImageElm( act_diag, p ), l ))) ))),  
        List( genN, n -> GroupHomomorphismByImages( MxL, MxL, genMxL, 
            Concatenation( 
                List( genM, m -> ImageElm( e1MxL, m ) * 
                                 ImageElm( e2MxL,  
                                 ImageElmCrossedPairing( xpair, [m,n] ))), 
                List( genL, l -> ImageElm( e2MxL, 
                                 ImageElm( ImageElm( act_up, n ), l ))) ))) ); 
    Info( InfoXMod, 2, "autgenMxL = ", autgenMxL ); 
    autMxL := Group( autgenMxL ); 
    Info( InfoXMod, 2, "autMxL has size ", Size( autMxL ) );
    SetIsGroupOfAutomorphisms( autMxL, true ); 
    actPNML := GroupHomomorphismByImages( PxN, autMxL, genPxN, autgenMxL );
    imPNML := Concatenation( 
                List( genM, m -> ImageElm( e1PxN, ImageElm( bdy_dn, m ) ) ), 
                List( genL, l -> ImageElm( e2PxN, ImageElm( bdy_up, l ) ) ) ); 
    Info( InfoXMod, 2, "imPNML = ", imPNML ); 
    bdyPNML := GroupHomomorphismByImages( MxL, PxN, genMxL, imPNML );
    XPNML := XModByBoundaryAndAction( bdyPNML, actPNML ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
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

    ## construct the action of PxM on NxL using the transpose action: 
    ## (n,l)^(p,m) = (n^p, (m \box n^p)^{-1}.l^{pm} ) 
    autgenNxL := Concatenation( 
        List( genP, p -> GroupHomomorphismByImages( NxL, NxL, genNxL, 
            Concatenation( 
                List( genN, n -> ImageElm( e1NxL, 
                                 ImageElm( ImageElm( act_rt, p ), n ) )),
                List( genL, l -> ImageElm( e2NxL, 
                                 ImageElm( ImageElm( act_diag, p ), l ))) ))),  
        List( genM, m -> GroupHomomorphismByImages( NxL, NxL, genNxL, 
            Concatenation( 
                List( genN, n -> ImageElm( e1NxL, n ) * 
                                 ImageElm( e2NxL,  
                                 ImageElmCrossedPairing( xpair, [n,m] ) )), 
                List( genL, l -> ImageElm( e2NxL, 
                                 ImageElm( ImageElm( act_lt, m ), l ))) ))) ); 
    Info( InfoXMod, 2, "autgenNxL = ", autgenNxL ); 
    autNxL := Group( autgenNxL ); 
    SetIsGroupOfAutomorphisms( autNxL, true ); 
    actPMNL := GroupHomomorphismByImages( PxM, autNxL, genPxM, autgenNxL );
    imPMNL := Concatenation( 
                List( genN, n -> ImageElm( e1PxM, ImageElm( bdy_rt, n ) ) ), 
                List( genL, l -> ImageElm( e2PxM, ImageElm( bdy_lt, l ) ) ) ); 
    bdyPMNL := GroupHomomorphismByImages( NxL, PxM, genNxL, imPMNL );
    XPMNL := XModByBoundaryAndAction( bdyPMNL, actPMNL ); 
    if ( InfoLevel( InfoXMod ) > 2 ) then 
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

    ##  now find the isomorphism between the sources PNML and PMNL 
    imiso := Concatenation( 
           List( genP, p -> ImageElm( e1PMNL, ImageElm( e1PxM, p ) ) ), 
           List( genN, n -> ImageElm( e2PMNL, ImageElm( e1NxL, n ) ) ), 
           List( genM, m -> ImageElm( e1PMNL, ImageElm( e2PxM, m ) ) ), 
           List( genL, l -> ImageElm( e2PMNL, ImageElm( e2NxL, l ) ) ) ); 
    iso := GroupHomomorphismByImages( PNML, PMNL, genPNML, imiso ); 
    inv := InverseGeneralMapping(iso); 
    iminv := List( genPMNL, g -> ImageElm( inv, g ) ); 
    guess := Concatenation( 
           List( genP, p -> ImageElm( e1PNML, ImageElm( e1PxN, p ) ) ), 
           List( genM, m -> ImageElm( e2PNML, ImageElm( e1MxL, m ) ) ), 
           List( genN, n -> ImageElm( e1PNML, ImageElm( e2PxN, n ) ) ), 
           List( genL, l -> ImageElm( e2PNML, ImageElm( e2MxL, l ) ) ) ); 
    ok := (iminv = guess); 
    Info( InfoXMod, 2, "iminv = guess? ", ok ); 
    ##  construct an isomorphic up cat1-group 
    tup := iso * TailMap( CPMNL );
    hup := iso * HeadMap( CPMNL ); 
    eup := e1PMNL * inv; 
    C2PMNL := PreCat1GroupByTailHeadEmbedding( tup, hup, eup ); 

    ##  now see if a cat2-group has been constructed 
    tlt := TailMap( CPNML );
    hlt := HeadMap( CPNML ); 
    elt := e1PNML; 
    tdn := TailMap( Cright ); 
    hdn := HeadMap( Cright );
    edn := e1PxN; 
    trt := TailMap( Cdown );
    hrt := HeadMap( Cdown ); 
    ert := e1PxM; 
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
    PC := PreCat2GroupByPreCat1Groups( CPNML, C2PMNL, Cdown, Cright );
    Cdiag := PreCat1GroupByTailHeadEmbedding( tdi, hdi, edi ); 
    SetDiagonal2DimensionalGroup( PC, Cdiag ); 
    return PC; 
end ); 

InstallMethod( CrossedSquareOfCat2Group, "generic method for cat2-groups",
    true, [ IsCat2Group ], 0,
function( C2 )

    local XS;
    XS := PreCrossedSquareOfPreCat2Group( C2 );
    SetIsCrossedSquare( XS, true ); 
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

    local xpS, NM, L, map, xpT, XT;

    xpS := CrossedPairing( XS );
    NM := Reversed( Source( xpS ) );
    L := Range( xpS );
    map := Mapping2ArgumentsByFunction( NM, L, 
        function(c) 
            return ImageElmCrossedPairing( xpS, Reversed(c) )^(-1); 
        end );
    xpT := CrossedPairingObj( NM, L, map );
    XT := PreCrossedSquareObj( Left2DimensionalGroup(XS), 
              Up2DimensionalGroup(XS), Right2DimensionalGroup(XS), 
              Down2DimensionalGroup(XS), DiagonalAction(XS), xpT );
    SetIsCrossedSquare( XT, true );
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
    return XModMorphismByHoms( 
        Left2DimensionalGroup(s), Right2DimensionalGroup(s), 
        Boundary( Up2DimensionalGroup(s) ), 
        Boundary( Down2DimensionalGroup(s) ) ); 
end );

InstallMethod( UpDownMorphism, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )
    return XModMorphismByHoms( Up2DimensionalGroup(s), Down2DimensionalGroup(s), 
           Boundary( Left2DimensionalGroup(s) ), 
           Boundary( Right2DimensionalGroup(s) ) ); 
end );

#############################################################################
##
#E gp3obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
