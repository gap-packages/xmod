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
    true, [ Is3DimensionalGroup ], 0,
function (obj)
    return ( IsPermGroup( Up2DimensionalGroup(obj) ) 
             and IsPermGroup( Range(obj) )
             and IsPermGroup( Left2DimensionalGroup(obj) ) 
             and IsPermGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsFp3DimensionalGroup, "generic method for 3d-group objects",
    true, [ Is3DimensionalGroup ], 0,
function (obj)
    return ( IsFpGroup( Up2DimensionalGroup(obj) ) and IsFpGroup( Range(obj) )
             and IsFpGroup( Left2DimensionalGroup(obj) ) 
             and IsFpGroup( Right2DimensionalGroup(obj) ) );
end );

InstallMethod( IsPc3DimensionalGroup, "generic method for 3d-group obj ects" ,
    true, [ Is3DimensionalGroup ], 0,
function (obj)
    return ( IsPcGroup( Up2DimensionalGroup(obj) ) and IsPcGroup( Range(obj) )
             and IsPcGroup( Left2DimensionalGroup(obj) ) 
             and IsPcGroup( Right2DimensionalGroup(obj) ) );
end );

##############################################################################
##
#M  IsCrossedPairing
##
InstallMethod( IsCrossedPairing, "generic method for mappings", true, 
    [ IsGeneralMapping ], 0,
function( map )
    return ( HasSource( map ) and HasRange( map ) 
             and HasCrossedPairingMap( map ) );
end );

##############################################################################
##
#M  CrossedPairingObj( [<src1>,<src2>],<rng>,<map> ) .. make a crossed pairing
##
InstallMethod( CrossedPairingObj, "for a general mapping", true,
    [ IsList, IsGroup, IsGeneralMapping ], 0,
function( src, rng, map )

    local  filter, fam, obj;

    fam := FamilyObj( [ src, rng, map ] );
    filter := IsCrossedPairingObj;
    obj := rec();
    ObjectifyWithAttributes( obj, NewType( fam, filter ),
        Source, src,
        Range, rng, 
        CrossedPairingMap, map,
        IsCrossedPairing, true );
    return obj;
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

    local  map, xp;

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
    true, [ Is3DimensionalGroup ], 0,
function( P )

    local  u, d, l, r, ul, dl, ur, dr, bu, bd, bl, br, blbd, bubr,
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
#M  PreCrossedSquareObj ( <up>, <down>, <left>, <right>, <act>, <pair> ) 
##                                               . . . make a PreCrossedSquare
##
InstallMethod( PreCrossedSquareObj, "for prexmods, action and pairing", true,
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject ], 0,
function( u, l, d, r, a, p )

    local  filter, fam, PS, ok, src, rng, aut, narne;

    fam := Family3DimensionalGroup;
    filter := IsPreCrossedSquareObj;
    ## test commutativity here?
    PS := rec();
    ObjectifyWithAttributes( PS, NewType( fam, filter), 
      Up2DimensionalGroup, u, 
      Left2DimensionalGroup, l,
      Down2DimensionalGroup, d,
      Right2DimensionalGroup, r,
      CrossedPairing, p,
      DiagonalAction, a,
      Is3DimensionalGroup, true );
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
    true, [ Is3DimensionalGroup ], 0,
function( P )

    local  u, d, h1, t1, h2, t2, h1h2, h2h1, t1t2, t2t1, h1t2, 
           t2h1, h2t1, t1h2, G, gensrc, x, y, z;

    if not ( IsPreCat2Obj( P )  ) then
        return false;
    fi;
    
    u := Up2DimensionalGroup( P );
    d := Down2DimensionalGroup( P );
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

    if not ( ( Source(h1) = Source(t1) ) and ( Range(h1) = Range(t1) ) and
             ( Source(h2) = Source(t2) ) and ( Range(h2) = Range(t2) ) ) then
        Info( InfoXMod, 2, "Incompatible source/range" );
        return false;
    fi;
    
    if not ( ( Source(h1) = Source(h2) ) and ( Source(t1) = Source(t2) ) ) then
        Info( InfoXMod, 2, "Incompatible source" );
        return false;
    fi;
    
    G := Source(h1);
    gensrc := GeneratorsOfGroup(G);
    
    h1 := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( h1, x ) )  );
    t1 := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( t1, x ) )  );
    h2 := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( h2, x ) )  );
    t2 := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( t2, x ) )  );
    
     h1h2 := h1 * h2;
     h2h1 := h2 * h1;
     t1t2 := t1 * t2;
     t2t1 := t2 * t1;
     h1t2 := h1 * t2;
     t2h1 := t2 * h1;
     h2t1 := h2 * t1;
     t1h2 := t1 * h2;
        
    # check the condition 1 
    if not ( h1h2 = h2h1 ) then
        Info( InfoXMod, 2, "Condition 1 is not provided" );
    #    Print("Condition 1 is not provided \n");
        return false;
    fi;
    
    
    # check the condition 2
    if not ( t1t2 = t2t1 ) then
        Info( InfoXMod, 2, "Condition 2 is not provided" );
    #    Print("Condition 2 is not provided \n");
        return false;
    fi;
    
    # check the condition 3
    if not ( h1t2 = t2h1 ) then
        Info( InfoXMod, 2, "Condition 3 is not provided" );
    #    Print("Condition 3 is not provided \n");
        return false;
    fi;
    
    # check the condition 4
    if not ( h2t1 = t1h2 ) then
        Info( InfoXMod, 2, "Condition 4 is not provided" );
    #    Print("Condition 4 is not provided \n");
        return false;
    fi;     
    
        return true;
end );

#############################################################################
##
#M  IsCat2Group . . . . . . . . . . . . check that the object is a cat2-group
##
InstallMethod( IsCat2Group, "generic method for a cat2-group",
    true, [ Is3DimensionalGroup ], 0,
function( P )

    local  u, d;

    if not ( IsPreCat2Group(P) ) then
        Info( InfoXMod, 2, "pre-cat2-group is not provided" );
        return false;
    fi;
    u := Up2DimensionalGroup( P );
    d := Down2DimensionalGroup( P );
    
    if not ( ( IsCat1Group( u ) ) and ( IsCat1Group( d ) ) )  then
        Info( InfoXMod, 2, 
            "Up2DimensionalGroup, Down2DimensionalGroup must be Cat1Groups" );
        return false;
    fi;
    return true;
end );

##############################################################################
##
#M  PreCat2Obj( <up>, <down> ) . . . . . . . . . . . . . make a pre-cat2-group
##
InstallMethod( PreCat2Obj, "for precat2", true,
    [ IsPreCat1Group, IsPreCat1Group ], 0,
function( u, d )

    local  filter, fam, PC, ok, name;

    fam := Family3DimensionalGroup;
    filter := IsPreCat2Obj;
    PC := rec();
    ObjectifyWithAttributes( PC, NewType( fam, filter), 
        Up2DimensionalGroup, u, 
        Down2DimensionalGroup, d,
        Is3DimensionalGroup, true );
    if not IsPreCat2Group( PC ) then
        Info( InfoXMod, 1, "Warning: not a pre-cat2-group" );
    fi;
    # ok := IsCat2Group( PC );
    # name:= Name( PC );
    return PC;
end );

#############################################################################
##
#F  Cat2Group( <size>, <gpnum>, <num> )     cat2-group from data in CAT2_LIST
#F  Cat2Group( C1G1, C1G2 )                 cat2-group from two cat1-groups
##
InstallGlobalFunction( Cat2Group, function( arg )

    local  nargs, C1G1, C1G2, C2G, ok;

    nargs := Length( arg );
    if ( ( nargs < 2 ) or ( nargs > 3 ) ) then
        Print( "standard usage: Cat2Group( cat1, cat1 );\n" );
        Print( "            or: Cat2Group( size, gpnum, num );\n" );
        return fail;
    elif not IsInt( arg[1] ) then
        if ( nargs = 2 ) then
            C2G := PreCat2Obj( arg[1], arg[2] );
        fi;
        ok := IsCat2Group( C2G );
        if ok then
            return C2G;
        else
            return fail;
        fi;
    else   
        Print( "Cat2Select is not yet implemented\n" );
        # return Cat2Select( arg[1], arg[2], arg[3] );
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
#M  CrossedSquareByNormalSubgroups 
##                                create a crossed square from normal M,N in P
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
    xp := CrossedPairingByNormalSubgroups( M, N, L );
    XS := PreCrossedSquareObj( u, l, d, r, a, xp );
    SetIsCrossedSquare( XS, true );
    return XS;
end );

InstallOtherMethod( CrossedSquareByNormalSubgroups, 
    "conjugation crossed square", true, [ IsGroup, IsGroup, IsGroup ], 0,
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

    local  xpS, NM, L, map, xpT, XT;

    xpS := CrossedPairing( XS );
    NM := Reversed( Source( xpS ) );
    L := Range( xpS );
    map := Mapping2ArgumentsByFunction( NM, L, 
      function(c) return ImageElmCrossedPairing( xpS, Reversed(c) )^(-1); end );
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

    local  nul, nur, ndl, ndr, name, mor;

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

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . . . . . . . . . for a 3d-domain
##
InstallOtherMethod( IdGroup, "method for a 3d-domain", true, 
    [ Is3DimensionalDomain ], 0,    
function( dom )
    local  u, d;
    u := Up2DimensionalGroup( dom ); 
    d := Down2DimensionalGroup( dom ); 
    return [ [ IdGroup( Source(u) ), IdGroup( Range(u) ) ], 
             [ IdGroup( Source(d) ), IdGroup( Range(d) ) ] ]; 
end ); 

##############################################################################
##
#M  \=( <dom1>, <dom2> ) . . . . . . . . . . test if two 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-domain",
    IsIdenticalObj, [ Is3DimensionalGroup, Is3DimensionalGroup ], 0,
    function ( dom1, dom2 )
    
    if ( IsPreCat2Group( dom1 ) and IsPreCat2Group( dom2 ) ) then
        return( 
            ( Up2DimensionalGroup( dom1 ) = Up2DimensionalGroup( dom2 ) )
        and ( Down2DimensionalGroup( dom1 ) = Down2DimensionalGroup( dom2 ) ) 
        );
    else
        return( 
            ( Up2DimensionalGroup( dom1 ) = Up2DimensionalGroup( dom2 ) )
        and ( Down2DimensionalGroup( dom1 ) = Down2DimensionalGroup( dom2 ) ) 
        and ( Right2DimensionalGroup( dom1 ) = Right2DimensionalGroup( dom2 ) ) 
        and ( Left2DimensionalGroup( dom1 ) = Left2DimensionalGroup( dom2 ) ) );
    fi;
end );

#############################################################################
##
#M  PrintObj( <g3d> . . . . . . . . . . . . . . . print a 3Dimensional-group 
#M  ViewObj( <g2d> ) . . . . . . . . . . . . . . . view a 3Dimensional-group 
##
InstallMethod( PrintObj, "method for a 3d-group", true, 
    [ Is3DimensionalGroup ], 0,
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
            Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
            Print( "    left = ",  Left2DimensionalGroup( g3d ), "\n" );
            Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
            Print( "   right = ", Right2DimensionalGroup( g3d ), "\n" );
            else 
               if ( HasIsCat2Group( g3d ) and IsCat2Group( g3d ) ) then  
                   Print( "cat2-group with:\n" ); 
               else 
                   Print( "(pre-)cat2-group with:\n" ); 
               fi; 
            Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
            Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
            fi; 
        fi;
    fi;
end );

InstallMethod( ViewObj, "method for a 3d-group", true, 
    [ Is3DimensionalGroup ], 0,
    PrintObj ); 

#############################################################################
##
#M Display( <g3d> . . . . . . . . . . . . . . . . . . . . display a 3d-group 
##
InstallMethod( Display, "method for a 3d-group", true, 
    [ Is3DimensionalGroup ], 0,
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
        else 
        Print( "(pre-)cat2-group with:\n" ); 
        Print( "      up = ",    Up2DimensionalGroup( g3d ), "\n" );
        Print( "    down = ",  Down2DimensionalGroup( g3d ), "\n" );
        fi; 

    fi; 
end ); 

#############################################################################
##
#E gp3obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
