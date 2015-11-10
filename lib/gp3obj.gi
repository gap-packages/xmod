##############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'              Chris Wensley
##
##  This file implements generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.
##
##  version 2.43, 11/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  IsPerm3dGroup . . . . . . . . check whether the 4 sides are perm groups
#M  IsFp3dGroup . . . . . . . . . check whether the 4 sides are fp groups
#M  IsPc3dGroup . . . . . . . . . check whether the 4 sides are pc groups
##
InstallMethod( IsPerm3dGroup, "generic method for 3d-group objects",
    true, [ Is3dGroup ], 0,
function (obj)
    return ( IsPermGroup( Up2dGroup(obj) ) and IsPermGroup( Range(obj) )
             and IsPermGroup( Left2dGroup(obj) ) 
             and IsPermGroup( Right2dGroup(obj) ) );
end );

InstallMethod( IsFp3dGroup, "generic method for 3d-group objects",
    true, [ Is3dGroup ], 0,
function (obj)
    return ( IsFpGroup( Up2dGroup(obj) ) and IsFpGroup( Range(obj) )
             and IsFpGroup( Left2dGroup(obj) ) 
             and IsFpGroup( Right2dGroup(obj) ) );
end );

InstallMethod( IsPc3dGroup, "generic method for 3d-group obj ects" ,
    true, [ Is3dGroup ], 0,
function (obj)
    return ( IsPcGroup( Up2dGroup(obj) ) and IsPcGroup( Range(obj) )
             and IsPcGroup( Left2dGroup(obj) ) 
             and IsPcGroup( Right2dGroup(obj) ) );
end );

##############################################################################
##
#M  IsXPairing
##
InstallMethod( IsXPairing, "generic method for mappings", true, 
    [ IsGeneralMapping ], 0,
function( map )
    return ( HasSource( map ) and HasRange( map ) 
             and HasXPairingMap( map ) );
end );

##############################################################################
##
#M  XPairingObj( [<src1>,<src2>],<rng>,<map> ) .. make a crossed pairing
##
InstallMethod( XPairingObj, "for a general mapping", true,
    [ IsList, IsGroup, IsGeneralMapping ], 0,
function( src, rng, map )

    local  filter, fam, obj;

    fam := FamilyObj( [ src, rng, map ] );
    filter := IsXPairingObj;
    obj := rec();
    ObjectifyWithAttributes( obj, NewType( fam, filter ),
        Source, src,
        Range, rng, 
        XPairingMap, map,
        IsXPairing, true );
    return obj;
end );

##############################################################################
##
#M  XPairingByNormalSubgroups( <grp>, <grp>, <grp> ) . . . make an XPairing
##
InstallMethod( XPairingByNormalSubgroups, 
    "for the intersection of two normal subgroups", true,
    [ IsGroup, IsGroup, IsGroup ], 0,
function( M, N, L )

    local  map, xp;

    if not ( IsNormal( M, L ) and IsNormal( N, L ) ) then 
        Error( "L not normal in M and N" );
    fi;
    map := Mapping2ArgumentsByFunction( [M,N], L, 
               function(c) return Comm( c[1], c[2] ); end );
    xp := XPairingObj( [M,N], L, map );
    return xp;
end );

##############################################################################
##
#M  XPairingByDerivations( <xmod> ) . . .  make an actor crossed pairing
##
InstallMethod( XPairingByDerivations, "for a crossed module", true,
    [ IsXMod ], 0,
function( X0 )

    local  SX, RX, WX, reg, imlist, map;

    SX := Source( X0 );
    RX := Range( X0 );
    WX := WhiteheadPermGroup( X0 );
    reg := RegularDerivations( X0 );
    imlist := ImagesList( reg );
    map := Mapping2ArgumentsByFunction( [RX,WX], SX, 
               function(t) 
               local  r, p, pos, chi;
               r := t[1];  p := t[2];
               pos := Position( Elements( WX ), p );
               chi := DerivationByImages( X0, imlist[pos] ); 
               return DerivationImage( chi, r ); 
               end );
    return XPairingObj( [RX,WX], SX, map );
end );

#############################################################################
##
#M  ImageElmXPairing( <map>, <elm> )  . . . . . . . for crossed pairing
##
InstallMethod( ImageElmXPairing, "for crossed pairing", true, 
    [ IsXPairing, IsList ], 0,
    function ( xp, elm ) 
        return ImageElm( XPairingMap( xp ), elm );
    end );

#############################################################################
##
#M  IsPreCrossedSquare . . . . . . . . . . . . . . . . .  check that the square commutes
##
InstallMethod( IsPreCrossedSquare, "generic method for a pre-crossed square",
    true, [ Is3dGroup ], 0,
function( P )

    local  u, d, l, r, ul, dl, ur, dr, bu, bd, bl, br, blbd, bubr,
           autu, autl, act, diag, ok, morud, morlr;

    if not ( IsPreCrossedSquareObj ( P ) and HasDiagonalAction( P ) 
             and HasXPairing( P ) ) then
        return false;
    fi;
    u := Up2dGroup( P );
    l := Left2dGroup( P );
    d := Down2dGroup( P );
    r := Right2dGroup( P );
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
    autu := AutoGroup( u );
    autl := AutoGroup( l );
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
    ## compatible actions to be checked?
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
#M  PreCrossedSquareObj ( <up>, <down>, <left>, <right>, <act>, <pair> ) . .make a PreCrossedSquare
##
InstallMethod( PreCrossedSquareObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( u, l, d, r, a, p )

    local  filter, fam, PS, ok, src, rng, aut, narne;

    fam := Family3dGroup;
    filter := IsPreCrossedSquareObj;
    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, NewType( fam, filter), 
      Up2dGroup, u, 
      Left2dGroup, l,
      Down2dGroup, d,
      Right2dGroup, r,
      XPairing, p,
      DiagonalAction, a,
      Is3dGroup, true );
    if not IsPreCrossedSquare( PS ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed square." );
    fi;
    # ok := IsCrossedSquare( PS );
    # name:= Name( PS );
    return PS;
end );

##############################################################################
##
#M  LeftRightMorphism . . . . . . . . . . . . . . . . for a precrossed square
#M  UpDownMorphism . . . . . . . . . . . . . . . . . for a precrossed square
##
InstallMethod( LeftRightMorphism, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )
    return XModMorphismByHoms( Left2dGroup(s), Right2dGroup(s), 
               Boundary( Up2dGroup(s) ), Boundary( Down2dGroup(s) ) ); 
end );

InstallMethod( UpDownMorphism, "for a precrossed square", true,
    [ IsPreCrossedSquare ], 0,
function( s )
    return XModMorphismByHoms( Up2dGroup(s), Down2dGroup(s), 
               Boundary( Left2dGroup(s) ), Boundary( Right2dGroup(s) ) ); 
end );

##############################################################################
##
#M  CrossedSquareByNormalSubgroups          create a crossed square from normal M,N in P
##
InstallMethod( CrossedSquareByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup, IsGroup ], 0,
function( P, N, M, L )

    local  XS, u, d, l, r, a, xp, diag;

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
    xp := XPairingByNormalSubgroups( M, N, L );
    XS := PreCrossedSquareObj( u, l, d, r, a, xp );
    SetIsCrossedSquare( XS, true );
    return XS;
end );

InstallOtherMethod( CrossedSquareByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup ], 0,
function( P, M, N )

    local  XS, genP, genM, genN, L, genL, u, d, l, r, a, p, diag;

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

    local  XS, WX, LX, NX, AX, xp, da;

    AX := ActorXMod( X0 );
    WX := WhiteheadXMod( X0 );
    NX := NorrieXMod( X0 );
    LX := LueXMod( X0 );
    da := XModAction( LX );
    ##  define the pairing as evaluation of a derivation
    xp := XPairingByDerivations( X0 );
    XS := PreCrossedSquareObj( WX, X0, NX, AX, da, xp );
    SetIsCrossedSquare( XS, true );
    return XS;
end );

##############################################################################
##
#M  Transpose3dGroup                        the transpose of a crossed square
##
InstallMethod( Transpose3dGroup, "transposed crossed square", true, 
    [ IsCrossedSquare ], 0,
function( XS )

    local  xpS, NM, L, map, xpT, XT;

    xpS := XPairing( XS );
    NM := Reversed( Source( xpS ) );
    L := Range( xpS );
    map := Mapping2ArgumentsByFunction( NM, L, 
             function(c) return ImageElmXPairing( xpS, Reversed(c) )^(-1); end );
    xpT := XPairingObj( NM, L, map );
    XT := PreCrossedSquareObj( Left2dGroup(XS), Up2dGroup(XS), Right2dGroup(XS), 
                     Down2dGroup(XS), DiagonalAction(XS), xpT );
    SetIsCrossedSquare( XT, true );
    return XT;
end );    

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . . for a pre-crossed square
##
InstallMethod( Name, "method for a pre-crossed square", true, [ IsPreCrossedSquare ], 0,
function( PS )

    local  nul, nur, ndl, ndr, name, mor;

    if HasName( Source( Up2dGroup( PS ) ) ) then
        nul := Name( Source( Up2dGroup( PS ) ) );
    else
        nul := "..";
    fi;
    if HasName( Range( Up2dGroup( PS ) ) ) then
        nur := Name( Range( Up2dGroup( PS ) ) );
    else
        nur := "..";
    fi;
    if HasName( Source( Down2dGroup( PS ) ) ) then
        ndl := Name( Source( Down2dGroup( PS ) ) );
    else
        ndl := "..";
    fi;
    if HasName( Range( Down2dGroup( PS ) ) ) then
        ndr := Name( Range( Down2dGroup( PS ) ) );
    else
        ndr := "..";
    fi;
    name := Concatenation( "[", nul, "->", nur, ",", ndl, "->", ndr, "]" );
    SetName( PS, name );
    return name;
end );

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . . . . . . . . . for a 3d-domain
##
InstallOtherMethod( IdGroup, "method for a 3d-domain", true, [ Is3dDomain ], 0,
function( dom )
    local  u, d;
    u := Up2dGroup( dom ); 
    d := Down2dGroup( dom ); 
    return [ [ IdGroup( Source(u) ), IdGroup( Range(u) ) ], 
             [ IdGroup( Source(d) ), IdGroup( Range(d) ) ] ]; 
end ); 

#############################################################################
##
#M  PrintObj( <g3d> . . . . . . . . . . . . . . . . . . . . print a 3d-group 
#M  ViewObj( <g2d> ) . . . . . . . . . . . . . . . . . . . . view a 3d-group 
##
InstallMethod( PrintObj, "method for a 3d-group", true, [ Is3dGroup ], 0,
function( g3d )

    local  L, M, N, P, lenL, lenM, lenN, lenP, len1, len2, j, q1, q2, 
           ispsq, arrow, ok;

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
        L := Source( Up2dGroup( g3d ) );
        M := Source( Down2dGroup( g3d ) );
        N := Range( Up2dGroup( g3d ) );
        P := Range( Down2dGroup( g3d ) );
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
                if ( HasIsCrossedSquare( g3d ) and IsCrossedSquare( g3d ) ) then 
                    Print( "crossed square with:\n" ); 
                else 
                    Print( "pre-crossed square with:\n" ); 
                fi; 
            else 
                if ( HasIsCat2( g3d ) and IsCat2( g3d ) ) then  
                    Print( "cat2-group with:\n" ); 
                else 
                    Print( "(pre-)cat2-group with:\n" ); 
                fi; 
            fi; 
            Print( "      up = ",    Up2dGroup( g3d ), "\n" );
            Print( "    left = ",  Left2dGroup( g3d ), "\n" );
            Print( "    down = ",  Down2dGroup( g3d ), "\n" );
            Print( "   right = ", Right2dGroup( g3d ), "\n" );
        fi;
    fi;
end );

InstallMethod( ViewObj, "method for a 3d-group", true, [ Is3dGroup ], 0,
    PrintObj ); 

#############################################################################
##
#M Display( <g3d> . . . . . . . . . . . . . . . . . . . . display a 3d-group 
##
InstallMethod( Display, "method for a 3d-group", true, [ Is3dGroup ], 0,
function( g3d )

    local  L, M, N, P, lenL, lenM, lenN, lenP, len1, len2, j, q1, q2, 
           ispsq, arrow, ok;

    if ( HasIsPreCrossedSquare( g3d ) and IsPreCrossedSquare( g3d ) ) then 
        ispsq := true; 
        arrow := " -> "; 
    else 
        ispsq := false; 
        arrow := " => "; 
    fi; 
    L := Source( Up2dGroup( g3d ) );
    M := Source( Down2dGroup( g3d ) );
    N := Range( Up2dGroup( g3d ) );
    P := Range( Down2dGroup( g3d ) );
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
        else 
            Print( "(pre-)cat2-group with:\n" ); 
        fi; 
        Print( "      up = ",    Up2dGroup( g3d ), "\n" );
        Print( "    left = ",  Left2dGroup( g3d ), "\n" );
        Print( "    down = ",  Down2dGroup( g3d ), "\n" );
        Print( "   right = ", Right2dGroup( g3d ), "\n" );
    fi; 
end ); 

#############################################################################
##
#E gp3obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
