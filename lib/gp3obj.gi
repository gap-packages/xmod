##############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)crossed squares 
##  and (pre-)cat2-groups.
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
    
#############################################################################
##
#M  IsPerm3DimensionalGroup . . . . check whether the 4 sides are perm groups
#M  IsFp3DimensionalGroup . . . . . check whether the 4 sides are fp groups
#M  IsPc3DimensionalGroup . . . . . check whether the 4 sides are pc groups
##
InstallMethod( IsPerm3DimensionalGroup, "generic method for 3d-group objects",
    true, [ IsHigherDimensionalGroup ], 0,
function (obj)
    return ( IsPermGroup( Up2DimensionalGroup(obj) ) 
             and IsPermGroup( Range(obj) )
             and IsPermGroup( Left2DimensionalGroup(obj) ) 
             and IsPermGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsFp3DimensionalGroup, "generic method for 3d-group objects",
    true, [ IsHigherDimensionalGroup ], 0,
function (obj)
    return ( IsFpGroup( Up2DimensionalGroup(obj) ) and IsFpGroup( Range(obj) )
             and IsFpGroup( Left2DimensionalGroup(obj) ) 
             and IsFpGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsPc3DimensionalGroup, "generic method for 3d-group obj ects",
    true, [ IsHigherDimensionalGroup ], 0,
function (obj)
    return ( IsPcGroup( Up2DimensionalGroup(obj) ) and IsPcGroup( Range(obj) )
             and IsPcGroup( Left2DimensionalGroup(obj) ) 
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

##############################################################################
##
#M  CrossedPairingByNormalSubgroups( <grp>, <grp>, <grp> ) 
##                                                 . . . make a CrossedPairing
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

#############################################################################
##
#M  ImageElmCrossedPairing( <map>, <elm> )  . . . . . . . for crossed pairing
##
InstallMethod( ImageElmCrossedPairing, "for crossed pairing", true, 
    [ IsCrossedPairing, IsList ], 0,
function ( xp, elm ) 
    return ImageElm( CrossedPairingMap( xp ), elm );
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
    ## check the action of the diagonal
    autu := Range( XModAction( u ) );
    autl := Range( XModAction( l ) );
    if not ( autu = autl ) then
        Info( InfoXMod, 1, "Allow the case autu <> autl ?" );
        return false;
    fi;
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
    # ok := IsCrossedSquare( PS );
    # name:= Name( PS );
    return PS;
end );

#############################################################################
##
#M  IsPreCat2Group . . . . . . . . . . .  check that this is a pre-cat2-group
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

#############################################################################
##
#M  IsCat2Group . . . . . . . . . . . . check that the object is a cat2-group
##
InstallMethod( IsCat2Group, "generic method for a cat2-group",
    true, [ IsHigherDimensionalGroup ], 0,
function( P )

    local u, d;

    if not ( HigherDimension( P ) = 3 ) then 
        return false; 
    fi;
    if (  IsCatnGroup( P ) ) then
        return true;
    fi;        
    
    if not ( HasIsPreCat2Group(P) and IsPreCat2Group(P) ) then 
        return false; 
    elif ( HasIsPreCrossedSquare(P) and IsPreCrossedSquare(P) ) then 
        return false; 
    else 
        Print( "no method for checking IsCat2Group is available so far\n" );  
    fi;
end );

#############################################################################
##
#F  CrossedSquare( <up>, <left>, <down>, <right>, <action>, <pairing> ) 
##      . . . . . . crossed square from xmods
#F  CrossedSquare( <P>, <N>, <M>, <L> )  
##      . . . . . . crossed square normal subgroups
#F  CrossedSquare( <xmod> )                                                 
##      . . . . . . actor crossed square
#F  CrossedSquare( <cat2-group> )                                            
##      . . . . . . crossed square from cat2-group
##
InstallGlobalFunction( CrossedSquare, function( arg )

    local nargs, XS, ok;

    nargs := Length( arg );
    if ( nargs = 1 ) then
        if ( IsHigherDimensionalGroup( arg[1] ) ) then
            XS := CrossedSquareOfCat2Group( arg[1] );
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
        Print( "            or: CrossedSquare( <cat2-group> );\n" );
        return fail;
    fi;
    ok := IsCrossedSquare( XS );
    if ok then
        return XS;
    else
        return fail;
    fi;
end );

#############################################################################
##
#F  Cat2Group( <size>, <gpnum>, <num> )     cat2-group from data in CAT2_LIST
#F  Cat2Group( C1G1, C1G2 )                 cat2-group from two cat1-groups
#F  Cat2Group( XS )                         cat2-group from crossed square
##
InstallGlobalFunction( Cat2Group, function( arg )

    local nargs, C1G1, C1G2, C2G, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 3 ) ) then
        Print( "standard usage: Cat2Group( cat1, cat1 );\n" );
        Print( "            or: Cat2Group( size, gpnum, num );\n" );
        Print( "            or: Cat2Group( XS );\n" );
        return fail;
    elif not IsInt( arg[1] ) then
        if ( nargs = 1 ) then 
            if not IsCrossedSquare( arg[1] ) then 
                Error( "argument is not a crossed square" ); 
            fi; 
            C2G := Cat2GroupOfCrossedSquare( arg[1] );
        elif ( nargs = 2 ) then 
            if not IsCat1Group( arg[1] ) and IsCat1Group( arg[2] ) then 
                Error( "the two argumewnts are not cat1-groups" ); 
            fi; 
            C2G := PreCatnObj( [ arg[1], arg[2] ] ); 
            if ( C2G = fail ) then 
                Error( "C2G fails to be a PreCatnObj" ); 
            fi;
        fi;
        ok := IsCatnGroup( C2G );
        if ok then
            return C2G;
        else
            return fail;
        fi;
    else   
        Print( "Cat2Select is not yet implemented\n" );
    fi;
end );

##############################################################################
##
#M  LeftRightMorphism . . . . . . . . . . . . . . . . for a precrossed square
#M  UpDownMorphism . . . . . . . . . . . . . . . . . for a precrossed square
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
        return fail;
    fi;
    if not ( L = Intersection( M, N ) ) then 
        Print( "Warning: expecting L = Intersection( M, N )\n" );
    fi;
    if ( not HasName(L) and HasName(M) and HasName(N) ) then
        SetName( L, Concatenation ( "(", Name(M), "^", Name(N), ")" ) );
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

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . . for a pre-crossed square
##
InstallMethod( Name, "method for a pre-crossed square", true, 
    [ IsPreCrossedSquare ], 0,
function( PS )

    local nul, nur, ndl, ndr, name, mor;

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
end );

##############################################################################
##
#M  \=( <dom1>, <dom2> ) . . . . . . . . . . test if two 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-domain",
    IsIdenticalObj, [ IsPreCrossedSquare, IsPreCrossedSquare ], 0,
function ( dom1, dom2 )
        return( 
            ( Up2DimensionalGroup( dom1 ) = Up2DimensionalGroup( dom2 ) )
        and ( Down2DimensionalGroup( dom1 ) = Down2DimensionalGroup( dom2 ) ) 
        and ( Right2DimensionalGroup( dom1 ) = Right2DimensionalGroup( dom2 ) ) 
        and ( Left2DimensionalGroup( dom1 ) = Left2DimensionalGroup( dom2 ) ) );
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
                    Print( "crossed square with:\n" ); 
                else 
                    Print( "pre-crossed square with:\n" ); 
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
        Print( "(pre-)crossed square with:\n" ); 
        Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
        Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
        Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
        Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
        fi; 
    fi; 
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
#M  CrossedSquareOfCat2Group
#M  Cat2GroupOfCrossedSquare
##
InstallMethod( CrossedSquareOfCat2Group, "generic method for cat2-groups",
    true, [ IsCat2Group ], 0,
function( C2 )

    local XS;
    XS := PreCrossedSquareOfPreCat2Group( C2 );
    SetCrossedSquareOfCat2Group( C2, XS );
    SetCat2GroupOfCrossedSquare( XS, C2 );
    return XS;
end );

InstallMethod( Cat2GroupOfCrossedSquare, "generic method for crossed squares",
    true, [ IsCrossedSquare ], 0,
function( XS )

    local C2;
    C2 := PreCat2GroupOfPreCrossedSquare(XS);
    SetCrossedSquareOfCat2Group( C2, XS );
    SetCat2GroupOfCrossedSquare( XS, C2 );
    return C2;
end );
    
#############################################################################
##
#M  PreCrossedSquareOfPreCat2Group
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
              List(gensrc, x -> Image( t1, x ) ) ); 
    h1 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> Image( h1, x ) ) ); 
    t2 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> Image( t2, x ) ) ); 
    h2 := GroupHomomorphismByImagesNC( G, G, gensrc, 
              List(gensrc, x -> Image( h2, x ) ) ); 
    
    L := Intersection( Kernel( t1 ), Kernel( t2 ) ) ;
    M := Intersection( Image ( t1 ), Kernel( t2 ) );
    N := Intersection( Kernel ( t1 ), Image( t2 ) );
    P := Intersection( Image( t1 ), Image( t2 ) )  ;
    
    Info( InfoXMod, 4, "G = ", G );
    Info( InfoXMod, 4, "L = ", L );
    Info( InfoXMod, 4, "M = ", M );
    Info( InfoXMod, 4, "N = ", N );
    Info( InfoXMod, 4, "P = ", P );
    
    bdy1 := GroupHomomorphismByFunction( L, M, x -> Image(h1,x) );
    act := ConjugationActionForCrossedSquare(M,L);
    up := XModByBoundaryAndAction(bdy1,act);
    
    bdy2 := GroupHomomorphismByFunction( L, N, x -> Image(h2,x) );
    act2 := ConjugationActionForCrossedSquare(N,L);
    left := XModByBoundaryAndAction(bdy2,act2);
    
    bdy3 := GroupHomomorphismByFunction( N, P, x -> Image(h1,x) );
    act3 := ConjugationActionForCrossedSquare(P,N);
    down := XModByBoundaryAndAction(bdy3,act3);
    
    bdy4 := GroupHomomorphismByFunction( M, P, x -> Image(h2,x) );
    act4 := ConjugationActionForCrossedSquare(P,M);
    right := XModByBoundaryAndAction(bdy4,act4);
    
    Info( InfoXMod, 3, "   up = ", up );
    Info( InfoXMod, 3, " left = ", left );
    Info( InfoXMod, 3, " down = ", down );
    Info( InfoXMod, 3, "right = ", right );

    a := ConjugationActionForCrossedSquare( P, L );
    xp := CrossedPairingByNormalSubgroups( M, N, L );
    XS := PreCrossedSquareObj( up, left, down, right, a, xp );
    SetIsCrossedSquare( XS, true );
    if HasName( C2G ) then 
        SetName( XS, Concatenation( "xsq(", Name( C2G ), ")" ) ); 
    fi; 
    return XS;
end );

#?  this function should be got rid of a.s.a.p. 
##############################################################################
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
            im := Image( embG, g) * Image( embH, h );
            Add( list1, [g,h] );
            Add( list2, im );
        od;
    od;
    return [ list1, list2 ];
end ); 

#############################################################################
##
#M  PreCat2GroupOfPreCrossedSquare
##
InstallMethod( PreCat2GroupOfPreCrossedSquare, true, 
    [ IsPreCrossedSquare ], 0,
function( XS )
 
    local L, M, N, P, up, left, down, right, bdy_up, bdy_left, bdy_right, 
          bdy_down, act_up, act_left, act_down, act_right, act_diag, 
          h, g, n, l, nl, m, p, pm, a, aut, act, aut2, act2, i, 
          G2, relsG2, 
          pmnl, n2p, l2p, l2pm, hmn2p, 
          PxM, genPxM, e1PxM, e2PxM, p1PxM, p2PxM, relsPxM, 
          NxL, genNxL, e1NxL, e2NxL, p1NxL, p2NxL, 
          PxN, genPxN, e1PxN, e2PxN, p1PxN, p2PxN, relsPxN, genPxNform2, 
          NxM, genNxM, e1NxM, e2NxM, p1NxM, p2NxM, 
          G, e1G, e2G, p1G, p2G, 
          autgen, imautgen, imautgenform2, genGform2, 
          genG, genGform3, genGform4, imt1, imt1form2, t1, h1, C1, 
          genG2form2, genG2, genG2form3, genG2form4, imt2, 
          imt2form2, t2, h2, C2, Cat2, 
          MxL, genMxL, relsMxL, genMxLform2, 
          autgen2, imautgen2, imautgen2form2, emb;

    Print( "Warning: these conversion functions are still under development\n" ); 
    return fail; 

    up := Up2DimensionalGroup(XS);
    left := Left2DimensionalGroup(XS);
    down := Down2DimensionalGroup(XS);
    right := Right2DimensionalGroup(XS);
     
    L := Source(up);
    N := Range(up);
    M := Source(down);
    P := Range(down);
    
    Info( InfoXMod, 4, "L = ", L );    
    Info( InfoXMod, 4, "M = ", M );
    Info( InfoXMod, 4, "N = ", N );
    Info( InfoXMod, 4, "P = ", P );

    bdy_up := Boundary(up);
    bdy_left := Boundary(left);
    bdy_down := Boundary(down);
    bdy_right := Boundary(right);
     
    act_up := XModAction(up);
    act_left := XModAction(left);
    act_down := XModAction(down);
    act_right := XModAction(right);
    act_diag := DiagonalAction(XS);
    h := CrossedPairing( XS );

    NxL := SemidirectProduct(N,act_up,L); 
    e1NxL := Embedding( NxL, 1 ); 
    e2NxL := Embedding( NxL, 2 ); 
    p1NxL := Projection( NxL, 1 );
    p2NxL := Projection( NxL, 2 );
    PxM := SemidirectProduct(P,act_down,M);
    e1PxM := Embedding( PxM, 1 ); 
    e2PxM := Embedding( PxM, 2 ); 
    p1PxM := Projection( PxM, 1 ); 
    p2PxM := Projection( PxM, 2 );
    genNxL := GeneratorsOfGroup( NxL );    
    genPxM := GeneratorsOfGroup( PxM );    
    if ( Length( genNxL ) = 0 ) then
        genMxL := [ Identity( NxL ) ];
    fi;
    if ( Length( genPxM ) = 0 ) then
        genPxN := [ Identity(PxM) ];
    fi;
    
    Info( InfoXMod, 3, "genPxM = ", genPxM );
    Info( InfoXMod, 3, "NxL = ", NxL );
    Info( InfoXMod, 3, "genNxL = ", genNxL );

    ## action: (n,l)^(p,m) = ( n^p, (h(m,n^p)^-1)*l^(pn) ) 
    autgen := [ ];
    for pm in genPxM do
        p := p1PxM( pm );
        m := p2PxM( pm );
        for nl in genNxL do 
            imautgen := [ ]; 
            n := p1NxL( nl );
            l := p2NxL( nl );
            n2p := Image( Image( act_right, p ), n ); 
            l2p := Image( Image( act_diag, p ), l ); 
            l2pm := Image( Image( act_left, m ), l2p ); 
            hmn2p := ImageElmCrossedPairing( h, [m,n2p] ); 
            Add( imautgen, e1NxL( n2p ) * e2NxL( (hmn2p^-1)*l2pm ) ); 
        od; 
        Add( autgen, GroupHomomorphismByImages( NxL, NxL, genNxL, imautgen ) );
    od;
    aut := Group( autgen );
    SetIsGroupOfAutomorphisms( aut, true );
    act := GroupHomomorphismByImages( PxM, aut, genPxM, autgen ); 
    G := SemidirectProduct( PxM, act, NxL ); 
    e1G := Embedding( G, 1 ); 
    e2G := Embedding( G, 2 ); 
    p1G := Projection( G, 1 ); 
    p2G := Projection( G, 2 ); 
    genG := GeneratorsOfGroup( G ); 
    Info( InfoXMod, 3, "G = ", G );
    Info( InfoXMod, 3, "genG = ", genG ); 
    imt1 := [ ];
    for pmnl in genG do 
        pm := p1G( pmnl ); 
        p := p1PxM( pm ); 
        m := p2PxM( pm );
        nl := p2G( pmnl );
        n := p1NxL( nl ); 
        l := p2NxL( nl ); 
        a := [ Image(bdy_right,m)*p, 
               Image(bdy_left,l) * 
                   Image(Image(act_right,Image(bdy_down,n)),m) ];
        Add( imt1, a );
    od;
    imt1form2 := List( imt1, x -> relsPxM[2][ Position( relsPxM[1], x )] );

    t1 := GroupHomomorphismByImages( G, PxM, genG, imt1form2 );
## returns fail on calling C2conj := PreCat2GroupOfPreCrossedSquare( XSconj );

    h1 := Projection( G );
        t1 := GroupHomomorphismByImagesNC( G, G, genG, 
              List( genG, x -> Image( t1, x ) ) );
    h1 := GroupHomomorphismByImagesNC( G, G, genG, 
              List( genG, x -> Image( h1, x ) )  );
    C1 := PreCat1GroupByEndomorphisms( t1, h1 );
    
    MxL := SemidirectProduct( M, act_up, L );     
    emb := Embedding( MxL, 1 ); 
    emb := Embedding( MxL, 2 ); 
    PxN := SemidirectProduct( P, act_down, N);    
    emb := Embedding( PxN, 1 ); 
    emb := Embedding( PxN, 2 ); 
    genMxL := GeneratorsOfGroup( MxL );    
    genPxN := GeneratorsOfGroup( PxN );
    if ( Length( genMxL ) = 0 ) then
        genMxL := [ Identity( MxL ) ];
    fi;
    if ( Length( genPxN ) = 0 ) then
        genPxN := [ Identity(PxN) ];
    fi;

    relsMxL := ElementsRelationsForSemidirectProduct( MxL );
    relsPxN := ElementsRelationsForSemidirectProduct( PxN );
    genMxLform2 := List( genMxL, x -> relsMxL[1][ Position( relsMxL[2], x )] );
    genPxNform2 := List( genPxN, x -> relsPxN[1][ Position( relsPxN[2], x )] );

    Info( InfoXMod, 3, "PxN = ", PxN );    
    Info( InfoXMod, 3, "genPxN = ", genPxN );
    Info( InfoXMod, 3, "genPxN2 = ", genPxNform2 );
    Info( InfoXMod, 3, "relsPxN = ", relsPxN );
    Info( InfoXMod, 3, "MxL = ", MxL );
    Info( InfoXMod, 3, "genMxL = ", genMxL );
    Info( InfoXMod, 3, "genMxL2 = ", genMxLform2 );
    Info( InfoXMod, 3, "relsMxL = ", relsMxL );
    
    autgen2 := [ ];
    for g in genPxNform2 do
        p := g[1];
        n := g[2];
        ##  l := ml[2] and m := ml[1];
        imautgen2form2 := List( genMxLform2, 
                                ml -> [ Image( Image( act_right, p ), ml[1]),
        Image ( Image(act_left, n ), Image( Image(act_diag,p), ml[2]) ) * 
         ImageElmCrossedPairing(h,[Image(Image(act_right,p),ml[1]), n] )] );
        imautgen2 := List( imautgen2form2, 
                           x -> relsMxL[2][ Position( relsMxL[1], x )] );
        a := GroupHomomorphismByImages( MxL, MxL, genMxL, imautgen2 );
        Add( autgen2, a );
    od;
    
    aut2 := Group( autgen2 );
    SetIsGroupOfAutomorphisms( aut2, true );
    act2 := GroupHomomorphismByImages( PxN, aut2, genPxN, autgen2 );
    G2 := SemidirectProduct( PxN, act2, MxL );    
    emb := Embedding( G2, 1 ); 
    emb := Embedding( G2, 2 ); 
    relsG2 := ElementsRelationsForSemidirectProduct( G2 );
    genG2 := GeneratorsOfGroup( G2 );
    genG2form2 := List( genG2, x -> relsG2[1][ Position( relsG2[2], x )] );
    genG2form3 := List( genG2form2, 
                        x -> [ relsPxN[1][ Position( relsPxN[2], x[1] )], 
                               relsMxL[1][ Position( relsMxL[2], x[2] )] ] );
    genG2form4 := []; 
    for i in [1..Length(genG2form3)] do
        Add( genG2form4, Flat(genG2form3[i]) );
    od;
 
    Info( InfoXMod, 3, "G2 = ", G2 );
    Info( InfoXMod, 3, "genG2 = ", genG2 );
    Info( InfoXMod, 3, "relsG2 = ", relsG2 );
    Info( InfoXMod, 3, "genG2form2 = ", genG2form2 );
    Info( InfoXMod, 3, "genG2form3 = ", genG2form3 );
    Info( InfoXMod, 3, "genG2form4 = ", genG2form4 );
    
    imt2 := [];    
    for i in [1..Length(genG2form4)] do
        p := genG2form4[i][1];
        n := genG2form4[i][2];
        m := genG2form4[i][3];
        l := genG2form4[i][4];
        a := [ Image(bdy_right,m)*p, Image(bdy_left,l) * 
               Image( Image( act_down, Image(bdy_right,m) ), n) ];
        Add( imt2, a );        
    od;
   
    imt2form2 := List( imt2, x -> relsPxN[2][ Position( relsPxN[1], x )] );
    t2 := GroupHomomorphismByImages(G2, PxN, genG2,  imt2form2 );
    h2 := Projection( G2 ); 
    t2 := GroupHomomorphismByImagesNC( G2, G2, genG2, 
              List(genG2, x -> Image( t2, x ) )  );
    h2 := GroupHomomorphismByImagesNC( G2, G2, genG2, 
              List(genG2, x -> Image( h2, x ) )  );
    C2 := PreCat1GroupByEndomorphisms( t2, h2 );
    Cat2 := CatnGroup( [ C1, C2 ] );
    if HasName( XS ) then 
        SetName( Cat2, Concatenation( "cat2(", Name(XS), ")" ) ); 
    fi;
    return Cat2;
end );

#############################################################################
##
#E gp3obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
