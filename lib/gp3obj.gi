##############################################################################
##
#W  gp3obj.gi                   GAP4 package `XMod'              Chris Wensley
##
##  This file implements generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
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
#M  IsXPair
##
InstallMethod( IsXPair, "generic method for mappings", true, 
    [ IsGeneralMapping ], 0,
function( map )
    return ( HasSource( map ) and HasRange( map ) 
             and HasXPairMap( map ) );
end );

##############################################################################
##
#M  XPairObj( [<src1>,<src2>],<rng>,<map> ) .. make a crossed pairing
##
InstallMethod( XPairObj, "for a general mapping", true,
    [ IsList, IsGroup, IsGeneralMapping ], 0,
function( src, rng, map )

    local  filter, fam, obj;

    fam := FamilyObj( [ src, rng, map ] );
    filter := IsXPairObj;
    obj := rec();
    ObjectifyWithAttributes( obj, NewType( fam, filter ),
        Source, src,
        Range, rng, 
        XPairMap, map,
        IsXPair, true );
    return obj;
end );

##############################################################################
##
#M  XPairByNormalSubgroups( <grp>, <grp>, <grp> ) . . . make an xpair
##
InstallMethod( XPairByNormalSubgroups, 
    "for the intersection of two normal subgroups", true,
    [ IsGroup, IsGroup, IsGroup ], 0,
function( M, N, L )

    local  map, xp;

    if not ( IsNormal( M, L ) and IsNormal( N, L ) ) then 
        Error( "L not normal in M and N" );
    fi;
    map := Mapping2ArgumentsByFunction( [M,N], L, 
               function(c) return Comm( c[1], c[2] ); end );
    xp := XPairObj( [M,N], L, map );
    return xp;
end );

##############################################################################
##
#M  XPairByDerivations( <xmod> ) . . .  make an actor crossed pairing
##
InstallMethod( XPairByDerivations, "for a crossed module", true,
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
    return XPairObj( [RX,WX], SX, map );
end );

#############################################################################
##
#M  ImageElmXPair( <map>, <elm> )  . . . . . . . for crossed pairing
##
InstallMethod( ImageElmXPair, "for crossed pairing", true, 
    [ IsXPair, IsList ], 0,
    function ( xp, elm ) 
        return ImageElm( XPairMap( xp ), elm );
    end );

#############################################################################
##
#M  IsPreXSq . . . . . . . . . . . . . . . . .  check that the square commutes
##
InstallMethod( IsPreXSq, "generic method for a pre-crossed square",
    true, [ Is3dDomain ], 0,
function( P )

    local  u, d, l, r, ul, dl, ur, dr, bu, bd, bl, br, blbd, bubr,
           autu, autl, act, diag, ok, morud, morlr;

    if not ( IsPreXSqObj ( P ) and HasDiagonalAction( P ) 
             and HasXPair( P ) ) then
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
#M  PreXSqObj ( <up>, <down>, <left>, <right>, <act>, <pair> ) . .make a PreXSq
##
InstallMethod( PreXSqObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( u, l, d, r, a, p )

    local  filter, fam, PS, ok, src, rng, aut, narne;

    fam := Family3dGroup;
    filter := IsPreXSqObj;
    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, NewType( fam, filter), 
      Up2dGroup, u, 
      Left2dGroup, l,
      Down2dGroup, d,
      Right2dGroup, r,
      XPair, p,
      DiagonalAction, a,
      Is3dGroup, true );
    if not IsPreXSq( PS ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed square." );
    fi;
    # ok := IsXSq( PS );
    # name:= Name( PS );
    return PS;
end );

##############################################################################
##
#M  XSqByNormalSubgroups          create a crossed square from normal M,N in P
##
InstallMethod( XSqByNormalSubgroups, "conjugation crossed square",
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
    xp := XPairByNormalSubgroups( M, N, L );
    XS := PreXSqObj( u, l, d, r, a, xp );
    SetIsXSq( XS, true );
    return XS;
end );

InstallOtherMethod( XSqByNormalSubgroups, "conjugation crossed square",
    true, [ IsGroup, IsGroup, IsGroup ], 0,
function( P, M, N )

    local  XS, genP, genM, genN, L, genL, u, d, l, r, a, p, diag;

    if not ( IsNormal( P, M ) and IsNormal( P, N ) ) then
        return fail;
    fi;
    L := Intersection( M, N );
    return XSqByNormalSubgroups( P, M, N, L );;
end );

##############################################################################
##
#M  ActorXSq                create a crossed square from an xmod and its actor
##
InstallMethod( ActorXSq, "actor crossed square", true, [ IsXMod ], 0,
function( X0 )

    local  XS, WX, LX, NX, AX, xp, da;

    AX := ActorXMod( X0 );
    WX := WhiteheadXMod( X0 );
    NX := NorrieXMod( X0 );
    LX := LueXMod( X0 );
    da := XModAction( LX );
    ##  define the pairing as evaluation of a derivation
    xp := XPairByDerivations( X0 );
    XS := PreXSqObj( WX, X0, NX, AX, da, xp );
    SetIsXSq( XS, true );
    return XS;
end );

##############################################################################
##
#M  Transpose3dGroup                        the transpose of a crossed square
##
InstallMethod( Transpose3dGroup, "transposed crossed square", true, 
    [ IsXSq ], 0,
function( XS )

    local  xpS, NM, L, map, xpT, XT;

    xpS := XPair( XS );
    NM := Reversed( Source( xpS ) );
    L := Range( xpS );
    map := Mapping2ArgumentsByFunction( NM, L, 
             function(c) return ImageElmXPair( xpS, Reversed(c) )^(-1); end );
    xpT := XPairObj( NM, L, map );
    XT := PreXSqObj( Left2dGroup(XS), Up2dGroup(XS), Right2dGroup(XS), 
                     Down2dGroup(XS), DiagonalAction(XS), xpT );
    SetIsXSq( XT, true );
    return XT;
end );    

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . . for a pre-crossed square
##
InstallMethod( Name, "method for a pre-crossed square", true, [ IsPreXSq ], 0,
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
        if ( HasIsPreXSq( g3d ) and IsPreXSq( g3d ) ) then 
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
                if ( HasIsXSq( g3d ) and IsXSq( g3d ) ) then 
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

    if ( HasIsPreXSq( g3d ) and IsPreXSq( g3d ) ) then 
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
