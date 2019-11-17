#############################################################################
##
#W  gp2obj.gi                 GAP4 package `XMod'               Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2019, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##
##  This file contains generic methods for (pre-)crossed modules and
##  (pre-)cat1-groups.
##

##############################################################################
##
##  Global constants which determine whether to read cat1data.g
##  CAT1_LIST_CLASS_SIZES  for groups of size <= CAT1_LIST_MAX_SIZE
##

CAT1_LIST_MAX_SIZE := 70; 
CAT1_LIST_CLASS_SIZES := 
    List( [1..CAT1_LIST_MAX_SIZE], n -> NumberSmallGroups(n) ); 
CAT1_LIST_LOADED := false;
CAT1_LIST := [ ];

##############################################################################
##
#M  IsPerm2DimensionalGroup . . check whether source and range are perm groups
#M  IsFp2DimensionalGroup . . . check whether source and range are fp groups
#M  IsPc2DimensionalGroup . . . check whether source and range are pc groups
##
InstallMethod( IsPerm2DimensionalGroup, "generic method for 2d-group objects",
    true, [ Is2DimensionalGroup ], 0,
function( obj )
    return ( IsPermGroup( Source( obj ) ) and IsPermGroup( Range( obj ) ) );
end );

InstallMethod( IsFp2DimensionalGroup, "generic method for 2d-group objects",
    true, [ Is2DimensionalGroup ], 0,
function( obj )
    return ( IsFpGroup( Source( obj ) ) and IsFpGroup( Range( obj ) ) );
end );

InstallMethod( IsPc2DimensionalGroup, "generic method for 2d-group objects",
    true, [ Is2DimensionalGroup ], 0,
function( obj )
    return ( IsPcGroup( Source( obj ) ) and IsPcGroup( Range( obj ) ) );
end );

#############################################################################
##
#M  IsPreXMod . . . . . . . . check that the first crossed module axiom holds 
##
InstallMethod( IsPreXMod, "generic method for 2d-group",
    true, [ Is2DimensionalGroup ], 0,
function( P )

    local Xsrc, Xrng, bdy, a, aut, act, gensrc, ngsrc, genrng, ngrng, 
          ssrc, x1, y1, z1, x2, y2, z2, w2;

    if not IsPreXModObj( P ) then
        return false;
    fi;
    Xrng := Range( P );
    genrng := GeneratorsOfGroup( Xrng );
    ngrng := Length( genrng );
    Xsrc := Source( P );
    gensrc := GeneratorsOfGroup( Xsrc );
    ngsrc := Length( gensrc );
    ssrc := Size( Xsrc );
    bdy := Boundary( P );
    # Check  P.boundary: P.source -> P.range
    if not ( ( Source( bdy ) = Xsrc ) and ( Range( bdy ) = Xrng ) ) then
        Info( InfoXMod, 2,
              "Error: require  X.boundary : X.source -> X.range" );
        return false;
    fi;
    # checking  IsHomomorphism(bdy) gives cokernel error when Xsrc = [ ]
    if ( ssrc > 1 ) then
        if not IsGroupHomomorphism( bdy ) then
            Info( InfoXMod, 2,
                  "Error:  the boundary map is NOT a homomorphism!" );
            return false;
        fi;
    fi;
    act := XModAction( P );
    aut := Range( act );
    # Check  X.aut  is a group of automorphisms  X.source -> X.source
    if not ( IsGroup( aut ) ) then
        Info( InfoXMod, 2,
              "Error: group of actions on X.source does not exist!" );
        return false;
    fi;
    if ( aut = Group( IdentityMapping( Xsrc ) ) ) then
       SetIsTrivialAction2DimensionalGroup( P, true );
    else
        a := GeneratorsOfGroup( aut )[1];
        if not ( ( Source( a ) = Xsrc ) and ( Range( a ) = Xsrc ) 
                                        and IsBijective( a )  ) then
            Info( InfoXMod, 2,
                  "Require automorphism  X.aut.1  on  X.source" );
            return false;
        fi;
    fi;
    # Check  X.action: X.range -> X.aut
    if not ( ( Source( act ) = Xrng ) and ( Range( act ) = aut ) ) then
        Info( InfoXMod, 2,
              "Error: require  X.action : X.range -> X.aut" );
        return false;
    fi;
    if ( Size( aut ) = 1 ) then
        Info( InfoXMod, 2,
              "X.action trivial => not checking a homomorphism!" );
    else
        if not IsGroupHomomorphism( act ) then
            Info( InfoXMod, 2, " X.action is not a homomorphism|" );
            return false;
        fi;
    fi;
    Info( InfoXMod, 3,
          "Checking  CM1) bdy(x2^x1) = (bdy(x2))^x1 " );
    for x1 in genrng do
        for x2 in gensrc do
            # Print( "x1,x2 = ", x1, ",  ", x2, "\n" );
            y1 := ( x2 ^ ( x1^act ) ) ^ bdy;
            z1 := ( x2 ^ bdy ) ^ x1;
            if ( y1 <> z1 ) then
                Info( InfoXMod, 3,
                    "CM1) fails at  x1 = ", x1, ",  x2 = ", x2, "\n",
                    "  bdy(x2^x1) = ", y1, "\n", "(bdy(x2))^x1 = ", z1 );
                return false;
            fi;
        od;
    od;
    return true;
end );

##############################################################################
##
#M  \=( <P>, <Q> )  . . . . . . . .  test if two pre-crossed modules are equal
##
InstallMethod( \=, "generic method for two pre-crossed modules",
    IsIdenticalObj, [ IsPreXMod, IsPreXMod ], 0,
function ( P, Q )
    return ( ( Boundary(P) = Boundary(Q) )
         and ( XModAction(P) = XModAction(Q) ) );
end );

##############################################################################
##
#M  Size( <P> )  . . . . . . . . . . . . . . . . size for a pre-crossed module
##
InstallOtherMethod( Size, "generic method for a 2d-object", 
    [ Is2DimensionalDomain ], 20,
function ( obj )
    return [ Size( Source( obj ) ), Size( Range( obj ) ) ];
end );

#############################################################################
##
#M  IsTrivialAction2DimensionalGroup  . . . . check whether action is trivial
##
InstallMethod( IsTrivialAction2DimensionalGroup,
    "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local act, genrng, onesrc;

    act := XModAction( PM );
    genrng := GeneratorsOfGroup( Range( PM ) );
    onesrc := IdentityMapping( Source( PM ) );
    return ForAll( genrng, r -> ( ImageElm( act, r ) = onesrc ) );
end );

###############################################################################
##
#M  IsCentralExtension2DimensionalGroup( <xmod> ) . . . . . . check the axioms
##
InstallMethod( IsCentralExtension2DimensionalGroup, "for an xmod",
    true, [ IsXMod ], 0,
function( X0 )

    local S, R, bdy, act, genS, genR, preR, len, i, r, pr, actr, s; 

    S := Source( X0 ); 
    R := Range( X0 ); 
    bdy := Boundary( X0 ); 
    if not IsSurjective( bdy ) then 
        return false; 
    fi;
    if not IsSubgroup( Centre( S ), Kernel( bdy ) ) then 
        return false; 
    fi; 
    act := XModAction( X0 ); 
    genS := GeneratorsOfGroup( S );
    genR := GeneratorsOfGroup( R );
    preR := List( genR, r -> PreImagesRepresentative( bdy, r ) ); 
    len := Length( genR ); 
    for i in [1..len] do 
        r := genR[i];
        pr := preR[i];
        actr := ImageElm( act, r ); 
        for s in genS do 
            if not ( ImageElm( actr, s ) = s^pr ) then 
                return false; 
            fi;
        od;
    od; 
    return true; 
end );

##############################################################################
##
#M  PreXModObj( <bdy>, <act> ) . . . . . . . . . . . make a pre-crossed module
##
InstallMethod( PreXModObj, "for homomorphism and action", true,
    [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( bdy, act )

    local type, PM, ok, src, rng, aut, name;

    src := Source( bdy );
    rng := Range( bdy );
    if not ( rng = Source( act ) ) then
        Error( "require Range( bdy ) = Source( act )" );
    fi;
    aut := Range( act );
    if not IsGroupOfAutomorphisms( aut ) then
        Error( "Range( act ) must be a group of automorphisms" );
    fi;
    if ( IsPermGroup( src ) and IsPermGroup( rng ) ) then
        type := PermPreXModObjType;  
    elif ( IsPcGroup( src ) and IsPcGroup( rng ) ) then 
        type := PcPreXModObjType; 
    else 
        type := PreXModObjType; 
    fi;
    PM := rec();
    ObjectifyWithAttributes( PM, type,
      Source, src,
      Range, rng,
      Boundary, bdy,
      XModAction, act,
      IsPreXModDomain, true, 
      Is2DimensionalGroup, true );
    if not IsPreXMod( PM ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed module." );
    else 
        ok := IsXMod( PM ); # for running properly the function AllXMods 
    fi;
    # name := Name( PM );
    ## check the types 
    if ( IsPermGroup(src) and IsPermGroup(rng) ) then 
        SetIsPerm2DimensionalGroup( PM, true ); 
    elif ( IsPcGroup(src) and IsPcGroup(rng) ) then 
        SetIsPc2DimensionalGroup( PM, true ); 
    fi;
    return PM;
end );

#############################################################################
##
#M  ImageElmXModAction( <pxmod>, <s>, <r> )  pre-xmod module action of s on r
##
InstallMethod( ImageElmXModAction, "method for a precrossed module", true, 
    [ Is2DimensionalDomain, IsObject, IsObject ], 0,
function( PM, s, r ) 

    local actr; 

    if ( HasIsPreXModWithObjects(PM) and IsPreXModWithObjects(PM) ) then 
        ## this is the crossed module of groupoids case 
        actr := ImageElm( XModAction( PM ), r )![1]; 
        return ImageElm( actr, s ); 
    else 
        return ImageElm( ImageElm( XModAction(PM), r ), s ); 
    fi; 
end ); 

#############################################################################
##
#M  ExternalSetXMod( <pxm> ) . . . . . . . source group as a range group set
##
InstallMethod( ExternalSetXMod, "method for a precrossed module", true, 
    [ IsPreXMod ], 0,
function( PM ) 

    local rng, genR, act;

    rng := Range( PM  ); 
    genR := GeneratorsOfGroup( rng ); 
    act := XModAction( PM );
    return ExternalSet( rng, Source(PM), genR, 
               List( genR, g -> ImageElm( act, g ) ) ); 
end ); 

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . for two-dimensional domains 
##
InstallMethod( String, "for a 2d-domain", true, [ Is2DimensionalDomain ], 0, 
function( g2d ) 
    return( STRINGIFY( "[", String( Source(g2d) ), " -> ", 
                            String( Range(g2d) ), "]" ) ); 
end );

InstallMethod( ViewString, "for a 2d-domain", true, [ Is2DimensionalDomain ], 
    0, String ); 

InstallMethod( PrintString, "for a 2d-domain", true, [ Is2DimensionalDomain ], 
    0, String ); 

InstallMethod( ViewObj, "for a 2d-domain", true, [ Is2DimensionalDomain ], 
    0,
function( g2d )
    if HasName( g2d ) then
        Print( Name( g2d ), "\n" );
    elif ( HasIsPreXModDomain( g2d ) and IsPreXModDomain( g2d ) ) then 
        Print( "[", Source( g2d ), "->", Range( g2d ), "]" ); 
    elif ( HasIsPreCat1Domain( g2d ) and IsPreCat1Domain( g2d ) ) then 
        Print( "[", Source( g2d ), "=>", Range( g2d ), "]" );
    else 
        TryNextMethod(); 
    fi;
end );

InstallMethod( PrintObj, "for a 2d-domain", true, [ Is2DimensionalDomain ], 0,
function( g2d )
    if HasName( g2d ) then
        Print( Name( g2d ), "\n" );
    elif ( HasIsPreXModDomain( g2d ) and IsPreXModDomain( g2d ) ) then 
        Print( "[", Source( g2d ) ); 
        if IsGroupoid( Source( g2d ) ) then 
            Print( "\n->  " ); 
        else 
            Print( " -> " ); 
        fi; 
        Print( Range( g2d ), "]" ); 
    elif ( HasIsPreCat1Domain( g2d ) and IsPreCat1Domain( g2d ) ) then 
        Print( "[", Source( g2d ) ); 
        if IsGroupoid( Source( g2d ) ) then 
            Print( "\n=>  " ); 
        else 
            Print( " => " ); 
        fi; 
        Print( Range( g2d ), "]" ); 
    else 
        TryNextMethod(); 
    fi;
end );

#############################################################################
##
#F  Display( <g2d> ) . . . . . . . . . . . . . . print details of a 2d-group 
##
InstallMethod( Display, "method for prexmods and precat2groups", true, 
    [ Is2DimensionalGroup ], 20,
function( g2d )

    local name, bdy, act, aut, len, i, ispar, src, rng, 
           gensrc, genrng, ker, genker, mor, triv, imact, a, 
           t, h, e, b, k, imt, imh, ime, imb, imk;

    src := Source( g2d );
    rng := Range( g2d );
    if ( HasName(src) and HasName(rng) ) then 
        name := Name( g2d ); 
    else 
        name := "[??->??]"; 
    fi;
    gensrc := GeneratorsOfGroup( src );
    genrng := GeneratorsOfGroup( rng ); 
    if ( HasIsPreXMod( g2d ) and IsPreXMod( g2d ) ) then 
        if ( HasIsXMod( g2d ) and IsXMod( g2d ) ) then
            Print( "\nCrossed module " );
        else
            Print( "\nPre-crossed module " );
        fi; 
    elif ( HasIsPreCat1Group( g2d ) and IsPreCat1Group( g2d ) ) then 
        if ( HasIsCat1Group( g2d ) and IsCat1Group( g2d ) ) then 
            Print( "\nCat1-group " );
        else
            Print( "\nPre-cat1-group " ); 
        fi; 
    else 
        Print( "WARNING: neither a PreXMod nor a PreCat1Group" ); 
    fi; 
    if HasName( g2d ) then 
        Print( Name(g2d), " :- \n" ); 
    else 
        Print( ":- \n" ); 
    fi;
    ispar := not HasParent( src );
    if ( ispar and HasName( src ) ) then
        Print( ": Source group ", src );
    elif ( ispar and HasName( Parent( src ) ) ) then
        Print( ": Source group has parent ( ", Parent( src), " ) and" );
    else
        Print( ": Source group" );
    fi;
    Print( " has generators:\n" );
    Print( "  ", gensrc, "\n" );
    ispar := not HasParent( rng );
    if ( ispar and HasName( rng ) ) then
        Print( ": Range group ", rng );
    elif ( ispar and HasName( Parent( rng ) ) ) then
        Print( ": Range group has parent ( ", Parent( rng ), " ) and" );
    else
        Print( ": Range group" );
    fi;
    Print( " has generators:\n" );
    Print( "  ", genrng, "\n" );
    if ( HasIsPreXMod( g2d ) and IsPreXMod( g2d ) ) then 
        Print( ": Boundary homomorphism maps source generators to:\n" );
        bdy := Boundary( g2d );
        Print( "  ",  List( gensrc, s -> ImageElm( bdy, s ) ), "\n" );
        act := XModAction( g2d );
        imact := List( genrng, r -> ImageElm( act, r ) );
        aut := Range( act );
        triv := ( aut = Group( InclusionMappingGroups( src, src ) ) );
        len := Length( genrng );
        if ( len = 0 ) then
            triv := true;
        else
            for i in [1..len] do
                a := imact[i];
            od;
        fi;
        if not triv then
            Print( ": Action homomorphism maps" );
            Print( " range generators to automorphisms:\n" );
            for i in [1..len] do
                Print( "  ", genrng[i], " --> { source gens --> " );
                Print( List( gensrc, s -> ImageElm( imact[i], s ) ), " }\n" );
            od;
        fi;
        if triv then
            Print( "  The automorphism group is trivial\n" );
        else
            if ( len = 1 ) then
                Print( "  This automorphism generates" );
            else
                Print( "  These ", len, " automorphisms generate" );
            fi;
            Print( " the group of automorphisms.\n" );
        fi; 
    else  ## g2d is at least a PreCat1Group 
        ker := Kernel( g2d ); 
        genker := GeneratorsOfGroup( ker ); 
        t := TailMap( g2d );
        h := HeadMap( g2d );
        e := RangeEmbedding( g2d );
        b := Boundary( g2d );
        k := KernelEmbedding( g2d );
        imt := List( gensrc, x -> ImageElm( t, x ) );
        imh := List( gensrc, x -> ImageElm( h, x ) );
        ime := List( genrng, x -> ImageElm( e, x ) );
        imb := List( genker, x -> ImageElm( b, x ) );
        imk := List( genker, x -> ImageElm( k, x ) );
        Print( ": tail homomorphism maps source generators to:\n" );
        Print( "  ", imt, "\n" );
        Print( ": head homomorphism maps source generators to:\n" );
        Print( "  ", imh, "\n" );
        Print( ": range embedding maps range generators to:\n" );
        Print( "  ", ime, "\n" ); 
        if ( Size( ker ) = 1 ) then
            Print( ": the kernel is trivial.\n" );
        else
            Print( ": kernel has generators:\n" );
            Print( "  ", genker, "\n" );
            Print( ": boundary homomorphism maps generators of kernel to:\n" );
            Print( "  ", imb, "\n" );
            Print( ": kernel embedding maps generators of kernel to:\n" );
            Print( "  ", imk, "\n" );
        fi;
    fi; 
    if ( HasIsXMod( g2d ) and IsXMod( g2d ) and HasCat1GroupOfXMod( g2d ) ) then
        Print( ": associated cat1-group is ", 
               Cat1GroupOfXMod( g2d ), "\n" );
    elif ( HasIsCat1Group( g2d ) and IsCat1Group( g2d ) 
           and HasXModOfCat1Group( g2d ) ) then
        Print( ": associated crossed module is ", 
               XModOfCat1Group( g2d ), "\n" );
    fi;
    Print( "\n" );
end ); 

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . . . . for a 2Dimensional-domain
#M  StructureDescription  . . . . . . . . . . . . . for a 2Dimensional-domain
##
InstallOtherMethod( IdGroup, "method for a 2d-domain", true, 
    [ Is2DimensionalDomain ], 0,
function( dom )
    return [ IdGroup( Source(dom) ), IdGroup( Range(dom) ) ]; 
end ); 

InstallOtherMethod( StructureDescription, "method for a 2d-domain", true, 
    [ Is2DimensionalDomain ], 0,
function( dom )
    return [ StructureDescription( Source(dom) ), 
             StructureDescription( Range(dom) ) ]; 
end ); 

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . .  for a 2Dimensional-domain
##
InstallMethod( Name, "method for a 2d-domain", true, 
    [ Is2DimensionalDomain ], 0,
function( dom )

    local nsrc, nrng, name, arrow;

    if HasName( Source( dom ) ) then
        nsrc := Name( Source( dom ) );
    else
        nsrc := "..";
    fi;
    if HasName( Range( dom ) ) then
        nrng := Name( Range( dom ) );
    else
        nrng := "..";
    fi; 
    if ( HasIsPreXModDomain( dom ) and IsPreXModDomain( dom ) ) then 
        arrow := "->"; 
    elif ( HasIsPreCat1Domain( dom ) and IsPreCat1Domain( dom ) ) then 
        arrow := "=>"; 
    else 
        arrow := "->-"; 
    fi; 
    name := Concatenation( "[", nsrc, arrow, nrng, "]" );
    SetName( dom, name );
    return name;
end );

#############################################################################
##
#M  PreXModByBoundaryAndAction
##
InstallMethod( PreXModByBoundaryAndAction,
    "pre-crossed module from boundary and action maps",
    true, [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( bdy, act )

    local rng, src, genrng, gensrc, aut, genaut, imact, i, a0, ima, a, PX;

    src := Source( bdy );
    gensrc := GeneratorsOfGroup( src );
    rng := Range( bdy );
    genrng := GeneratorsOfGroup( rng );
    if not ( Source( act ) = rng ) then
        Info( InfoXMod, 2,
              "The range group is not the source of the action." );
        return fail;
    fi;
    aut := Range( act );
    genaut := GeneratorsOfGroup( aut );
    if not IsGroupOfAutomorphisms( aut ) then
        Info( InfoXMod, 2, "<aut> is not a group of automorphisms" );
        return fail;
    fi;
    for a in genaut do
        if not ( ( Source( a ) = src ) and ( Range( a ) = src ) ) then
            Info( InfoXMod, 2, "error in source and range of automorphism" );
            return fail;
        fi;
    od;
    if not ( One( aut ) = IdentityMapping( src ) ) then
        Info( InfoXMod, 2,
              "aut.identity <> IdentityMapping( src )" );
        return fail;
    fi;
    imact := List( genrng, r -> ImageElm( act, r ) );
    for i in [ 1..Length( imact ) ] do
        a0 := imact[i];
        ima := List( gensrc, s -> ImageElm( a0, s ) );
        a := GroupHomomorphismByImages( src, src, gensrc, ima );
        imact[i] := a;
    od;
    PX := PreXModObj( bdy, act ); 
    if not IsPreXMod( PX ) then 
        Error( "PX fails to be a pre-crossed module" ); 
    fi;
    return PX; 
end );

#############################################################################
##
#M  IsPreCat1Group            check that the first pre-cat1-group axiom holds
##
InstallMethod( IsPreCat1Group, "generic method for 2dim-group", true, 
    [ Is2DimensionalGroup ], 0,
function( C1G )

    local Csrc, Crng, x, e, t, h, idrng, he, te, kert, kerh, kerth;

    if not IsPreCat1Obj( C1G ) then
        return false;
    fi;
    Crng := Range( C1G );
    h := HeadMap( C1G );
    t := TailMap( C1G );
    e := RangeEmbedding( C1G );
    # checking the first condition of cat-1 group
    idrng := IdentityMapping( Crng );
    he := CompositionMapping( h, e );
    te := CompositionMapping( t, e );
    if not ( te = idrng ) then
        Print( "te <> range identity \n" );
        return false;
    fi;
    if not ( he = idrng ) then
        Print( "he <> range identity \n" );
        return false;
    fi;
    return true;
end );

##############################################################################
##
#M  \=( <C1>, <C2> ) . . . . . . . . . . test if two pre-cat1-groups are equal
##
InstallMethod( \=, "generic method for pre-cat1-groups",
    IsIdenticalObj, [ IsPreCat1Group, IsPreCat1Group ], 0,
function( C1, C2 ) 
    return ( ( TailMap(C1) = TailMap(C2) ) and ( HeadMap(C1) = HeadMap(C2) )
             and ( RangeEmbedding(C1) = RangeEmbedding(C2) ) );
end );

##############################################################################
##
#M  PreCat1Obj . . . . . . . . . . . . . . . . . . . . . make a pre-cat1-group
##
InstallMethod( PreCat1Obj, "for tail, head, embedding", true,
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( t, h, e )

    local src, rng, type, C1G, ok, name;

    src := Source( t ); 
    rng := Range( t );
    if not ( ( src = Source( h ) ) and ( rng = Range( h ) ) ) then
        Error( "tail & head must have same source and range" );
    fi;
    if not ( ( Source( e ) = rng ) and ( Range( e ) = src ) ) then
        Error( "tail, embedding must have opposite source and range" );
    fi;
    if ( IsPermGroup( src ) and IsPermGroup( rng ) ) then
        type := PermPreCat1ObjType;  
    elif ( IsPcGroup( src ) and IsPcGroup( rng ) ) then 
        type := PcPreCat1ObjType; 
    else 
        type := PreCat1ObjType; 
    fi;
    C1G := rec();
    ObjectifyWithAttributes( C1G, type,
      Source, src,
      Range, rng,
      TailMap, t,
      HeadMap, h,
      RangeEmbedding, e, 
      IsPreCat1Domain, true, 
      Is2DimensionalGroup, true );
    ok := IsPreCat1Group( C1G ); 
    if not ok then
        Error( "not a pre-cat1-group" );
    fi;
    ok := IsPreCat1GroupByEndomorphisms( C1G ); 
    ok := IsCat1Group( C1G );
    ## check the types 
    if ( IsPermGroup( src ) and IsPermGroup( rng ) ) then 
        SetIsPerm2DimensionalGroup( C1G, true ); 
    elif ( IsPcGroup( src ) and IsPcGroup( rng ) ) then 
        SetIsPc2DimensionalGroup( C1G, true ); 
    fi;
    if ( HasName( src ) and HasName( rng ) ) then 
        name := Name( C1G );
    fi; 
    return C1G;
end );

##############################################################################
##
#M  Elements( <P> )  . . . . . . . . . . . . elements for a pre-crossed module
##
##  replaced by Enumerator ???

#############################################################################
##
#M  ReverseCat1Group                                     for a pre-cat1-group
##
InstallMethod( ReverseCat1Group, "method for a cat1-group", true, 
    [ IsPreCat1Group ], 0,
function( C1G )
    local rev;
    rev := PreCat1Group( HeadMap(C1G), TailMap(C1G), RangeEmbedding(C1G ) );
    SetReverseCat1Group( rev, C1G );
    return rev;
end );

#############################################################################
##
#F  PreCat1Group( <t>, <h>, <e> ) pre-cat1-group from given tail, head, embed
#F  PreCat1Group( <t>, <h> )     pre-cat1-group from tail, head endomorphisms
##
InstallGlobalFunction( PreCat1Group, function( arg )

    local nargs, usage, C1G;

    nargs := Length( arg ); 
    usage := "standard usage: PreCat1Group( tail, head [,embed] );"; 
    if not ForAll( arg, a -> IsGroupHomomorphism(a) ) then 
        Error( usage ); 
    fi; 
    # one endomorphism 
    if ( ( nargs=1 ) and IsEndoMapping( arg[1] ) ) then 
        return PreCat1GroupByEndomorphisms( arg[1], arg[1] );
    # two endomorphisms
    elif ( nargs=2 ) then
        if ( IsEndoMapping( arg[1] ) and IsEndoMapping( arg[2] ) ) then
            return PreCat1GroupByEndomorphisms( arg[1], arg[2] ); 
        elif ( Image( arg[1] ) = Source( arg[2] ) ) then 
            return PreCat1GroupByTailHeadEmbedding( arg[1], arg[1], arg[2] );
        fi;
    # two homomorphisms and an embedding
    elif ( nargs=3 ) then
        return PreCat1GroupByTailHeadEmbedding( arg[1], arg[2], arg[3] );
    fi;
    # other alternatives not allowed
    Error( usage );
end );

##############################################################################
##
#M  PreCat1GroupOfPreXMod . . convert a pre-crossed module to a pre-cat1-group
##
InstallMethod( PreCat1GroupOfPreXMod,
    "convert a pre-crossed module to a pre-cat1-group", true, [ IsPreXMod ], 0,
function( X0 )

    local S0, genS0, R0, genR0, iso, Xact, Xbdy, one, imbdy, info, G, genG, 
          t, h, f, eR, eS, imeR, imeS, projS, imt, imh, ime, imf, C, pcrec;

    S0 := Source( X0 ); 
    genS0 := GeneratorsOfGroup( S0 ); 
    R0 := Range( X0 ); 
    genR0 := GeneratorsOfGroup( R0 ); 
    genS0 := GeneratorsOfGroup( S0 );
    genR0 := GeneratorsOfGroup( R0 );
    one := One( R0 );
    Xact := XModAction( X0 );
    Xbdy := Boundary( X0 );
    if IsTrivialAction2DimensionalGroup( X0 ) then
        Info( InfoXMod, 2, "Using direct product: ", R0, " x ", S0 );
        G := DirectProduct( R0, S0 );
        info := DirectProductInfo( G );
        if ( HasName( S0 ) and HasName( R0 ) ) then
            SetName( G, Concatenation( Name( R0 ), "x", Name( S0 ) ) );
        fi;
        genG := GeneratorsOfGroup( G );
        imbdy := List( genS0, s -> ImageElm( Xbdy, s ) );
        imt := Concatenation( genR0, List( genS0, s -> one ) );
        imh := Concatenation( genR0, imbdy );
        t := GroupHomomorphismByImages( G, R0, genG, imt );
        h := GroupHomomorphismByImages( G, R0, genG, imh );
        eR := Embedding( G, 1 );
        eR := AsGroupGeneralMappingByImages( eR );
        eS := Embedding( G, 2 );
        eS := AsGroupGeneralMappingByImages( eS );
    else
        Info( InfoXMod, 2, "Using semidirect product: ", R0, " |X ", S0 );
        G := SemidirectProduct( R0, Xact, S0 );
        info := SemidirectProductInfo( G );
        if ( HasName( S0 ) and HasName( R0 ) ) then
             SetName( G, 
                 Concatenation( "(", Name(R0), " |X ", Name(S0), ")" ) );
        else
             SetName( G, "(..|X..)" );
        fi;
        genG := GeneratorsOfGroup( G );
        eR := Embedding( G, 1 );
        imeR := List( genR0, r -> ImageElm( eR, r ) );
        eS := Embedding( G, 2 );
        imeS := List( genS0, s -> ImageElm( eS, s ) );
        t := Projection( G );
        imt := List( genG, g -> ImageElm( t, g ) );
        t := GroupHomomorphismByImages( G, R0, genG, imt );
        projS := List( imt, r -> ImageElm( eR, r^-1 ) );
        projS := List( [ 1..Length( genG ) ], i -> projS[i] * genG[i] );
        projS := List( projS, x -> PreImagesRepresentative( eS, x ) );
        imh := List( [ 1..Length( genG ) ],
            i -> imt[i] * ImageElm( Xbdy, projS[i] ) ); 
        h := GroupHomomorphismByImages( G, R0, genG, imh );
    fi;
    C := PreCat1GroupByTailHeadEmbedding( t, h, eR );
    if HasName( X0 ) then 
        SetName( C, Concatenation( "cat1(", Name( X0 ), ")" ) ); 
    fi; 
    pcrec := rec( precat1 := C, 
                  xmodRangeEmbedding := Image( eR ), 
                  xmodRangeEmbeddingIsomorphism := eR, 
                  xmodSourceEmbedding := Image( eS ),
                  xmodSourceEmbeddingIsomorphism := eS );
    if HasIsXMod( X0 ) and IsXMod( X0 ) then 
        pcrec.iscat1 := true; 
    fi;
    return pcrec; 
end ); 

#############################################################################
##
#M  IsXMod . . . . . . . . . check that the second crossed module axiom holds
##
InstallMethod( IsXMod, "generic method for pre-crossed modules",
    true, [ IsPreXMod ], 0,
function( XM )

    local gensrc, genrng, x2, y2, w2, z2, hom, act;

    hom := Boundary( XM );
    act := XModAction( XM );
    gensrc := GeneratorsOfGroup( Source( XM ) );
    genrng := GeneratorsOfGroup( Range( XM ) );
    for x2 in gensrc do
        for y2 in gensrc do
            # Print( "x2,y2 = ", x2, ",  ", y2, "\n" );
            z2 := x2 ^ ((y2 ^ hom) ^ act);
            w2 := x2 ^ y2;
            if ( z2 <> w2 ) then
                Info( InfoXMod, 2,
                      "CM2) fails at  x2 = ", x2, ",  y2 = ", y2, "\n",
                      "x2^(hom(y2)) = ", z2, "\n","      x2^y2 = ", w2, "\n" );
                return false;
            fi;
        od;
    od;
    return true;
end );

#############################################################################
##
#M  XModByBoundaryAndAction
##
InstallMethod( XModByBoundaryAndAction,
    "crossed module from boundary and action maps", true,
    [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( bdy, act )

    local PM;

    PM := PreXModByBoundaryAndAction( bdy, act );
    if not IsXMod( PM ) then
        Error( "this boundary and action only defines a pre-crossed module" );
    fi;
    return PM;
end );

#############################################################################
##
#M  XModByTrivialAction
##
InstallMethod( XModByTrivialAction, "crossed module with trivial action", 
    true, [ IsGroupHomomorphism ], 0,
function( f )
    local R, ZR, S, XM, aut, act, name;
    S := Source( f );
    if not IsAbelian( S ) then
        Error( "the source of  f  must be abelian" );
    fi;
    R := Range( f );
    ZR := Centre( R );
    if not IsSubgroup( ZR, Image( f, S ) ) then
        Error( "image of source must lie in the centre of range" );
    fi;
    aut := Group( IdentityMapping( S ) );
    act := MappingToOne( R, aut );
    XM := XModByBoundaryAndAction( f, act );
    SetIsTrivialAction2DimensionalGroup( XM, true );
    return XM;
end );

##############################################################################
##
#F  XModByNormalSubgroup            create a crossed module from normal N in G
##
InstallMethod( XModByNormalSubgroup, "conjugation crossed module",
    true, [ IsGroup, IsGroup ], 0,
function( G, N )

    local XM, bdy, act, aut, genrng, gensrc, name, a, triv, idsrc,
           autgen, imautgen, phi, j, g, n, genN, f2pN, imgenN;

    if not IsNormal( G, N ) then
        return fail;
    fi;
    genrng := GeneratorsOfGroup( G );
    gensrc := GeneratorsOfGroup( N );
    bdy := GroupHomomorphismByImages( N, G, gensrc, gensrc );
    autgen := [ ];
    for g in genrng do
        imautgen := List( gensrc, n -> n^g );
        a := GroupHomomorphismByImages( N, N, gensrc, imautgen );
        Add( autgen, a );
    od;
    if ( Length( genrng ) = 0 ) then
        idsrc := IdentityMapping( N );
        aut := Group( idsrc );
        Info( InfoXMod, 2,
              "Group of conjugations has size ", Size( aut ) );
    else
        aut := Group( autgen );
    fi;
    SetIsGroupOfAutomorphisms( aut, true );
    act := GroupHomomorphismByImages( G, aut, genrng, autgen );
    XM := PreXModObj( bdy, act );
    SetIsNormalSubgroup2DimensionalGroup( XM, true );
    if ( Length( autgen ) = 0 ) then
        SetIsTrivialAction2DimensionalGroup( XM, true );
    fi;
    return XM;
end );

#############################################################################
##
#F  XModByCentralExtension           xmod from surjection with central kernel
##
InstallMethod( XModByCentralExtension, "central extension crossed module",
    true, [ IsGroupHomomorphism ], 0,
function( hom )

    local rng, src, Zsrc, ker, gensrc, ngsrc, imhom, genrng, autgen, 
           j, imsrc, aut, act, XM, ok, idsrc; 

    if not IsSurjective( hom ) then
        Error( "homomorphism must be surjective" );
    fi;
    src := Source( hom );
    rng := Range( hom );
    Zsrc := Centre( src );
    ker := Kernel( hom );
    if not IsSubgroup( Zsrc, ker ) then
        Error( "Kernel of surjection is not central" );
    fi;
    gensrc := GeneratorsOfGroup( src );
    ngsrc := Length( gensrc );
    imhom := List( gensrc, s -> ImageElm( hom, s ) );
    genrng := GeneratorsOfGroup( rng );
    autgen := ListWithIdenticalEntries( ngsrc, 0 );
    for j in [1..ngsrc] do 
        imsrc := List( gensrc, s -> s^gensrc[j] );
        autgen[j] := GroupHomomorphismByImages( src, src, gensrc, imsrc );
    od;
    aut := Group( autgen );
    SetIsGroupOfAutomorphisms( aut, true );
    Info( InfoXMod, 2, "Group of conjugations has size ", Size(aut) );
    act := GroupHomomorphismByImages( rng, aut, imhom, autgen );
    if ( not IsGroupHomomorphism( act ) ) then
        Error( "action is not a homomorphism" );
    fi;
    XM := PreXModObj( hom, act );
    SetIsCentralExtension2DimensionalGroup( XM, true );
    idsrc := IdentityMapping( src );
    if ForAll( autgen, a -> ( a = idsrc ) ) then
        SetIsTrivialAction2DimensionalGroup( XM, true );
    fi;
    ok := IsXMod( XM );
    return XM;
end );

#############################################################################
##
#M  XModByAbelianModule( <abmod> )      crossed module  [zero : abmod -> grp]
##
InstallMethod( XModByAbelianModule, "abelian module crossed module", true, 
    [ IsAbelianModule ], 0,
function( abmod )

    local aut, act, z;
    act := AbelianModuleAction( abmod );
    z := MappingToOne( AbelianModuleGroup( abmod ), Source( act ) );
    return XModByBoundaryAndAction( z, act );
end );

#############################################################################
##
#M  XModByGroupOfAutomorphisms                       crossed module  [G -> A]
##
InstallMethod( XModByGroupOfAutomorphisms, "automorphism crossed module",
    true, [ IsGroup, IsGroup ], 0,
function( G, A )

    local genA, autG, innG, abelian, genG, oneA, imbdy, g, ima, a, bdy, act, 
          iso1, A1, bdy1, act1, iso2, iso12, bdy2, act2, XM;

    if not IsGroupOfAutomorphisms( A ) then 
        Error( "A is not a group of automorphisms" ); 
    fi;
    genA := GeneratorsOfGroup( A ); 
    autG := AutomorphismGroup( G );
    if not IsSubgroup( autG, A ) then 
        Error( "A is not a group of automorphisms of G" ); 
    fi;
    innG := InnerAutomorphismsAutomorphismGroup( autG ); 
    if not IsSubgroup( A, innG ) then 
        Error( "the inner automorphism group of G is not a subgroup of A" ); 
    fi;
    abelian := IsAbelian( G );
    genG := GeneratorsOfGroup( G );
    oneA := One( A );
    if abelian then
        imbdy := List( genG, g -> oneA );
    else
        imbdy := [ ];
        for g in genG do
            ima := List( genG, h -> h^g );
            a := GroupHomomorphismByImages( G, G, genG, ima );
            Add( imbdy, a );
        od;
    fi;  
    bdy := GroupHomomorphismByImages( G, A, genG, imbdy ); 
    XM := PreXModObj( bdy, IdentityMapping( A ) ); 
    SetIsAutomorphismGroup2DimensionalGroup( XM, true );
    if not IsXMod( XM ) then
        Error( "this boundary and action only defines a pre-crossed module" );
    fi; 
    return XM;
end );

#############################################################################
##
#M  XModByAutomorphismGroup( <G> )               crossed module [G -> Aut(G)]
#M  XModByInnerAutomorphismGroup( <G> )          crossed module [G -> Inn(G)]
##
InstallMethod( XModByAutomorphismGroup, "automorphism xmod of a group",
    true, [ IsGroup ], 0,
function( G )

    local  autG, innG, a;

    autG := AutomorphismGroup( G );
    if ( not HasName( autG ) and HasName( G ) ) then
        SetName( autG, Concatenation( "Aut(", Name( G ), ")" ) );
    fi;
    SetIsGroupOfAutomorphisms( autG, true );
    return XModByGroupOfAutomorphisms( G, autG );
end );

InstallMethod( XModByInnerAutomorphismGroup, "inner automorphism xmod",
    true, [ IsGroup ], 0,
function( G )
    local A, innG, a;

    A := InnerAutomorphismsByNormalSubgroup( G, G );
    if ( not HasName( A ) and HasName( G ) ) then
        SetName( A, Concatenation( "Inn(", Name( G ), ")" ) );
    fi;
    SetIsGroupOfAutomorphisms( A, true );
    return XModByGroupOfAutomorphisms( G, A );
end );

##############################################################################
##
#M  XModOfCat1Group
##
InstallMethod( XModOfCat1Group, "generic method for cat1-groups",
    true, [ IsCat1Group ], 0,
function( C1 )

    local X1;
    X1 := PreXModOfPreCat1Group( C1 );
    SetIsXMod( X1, true );
    SetXModOfCat1Group( C1, X1 );
    SetCat1GroupOfXMod( X1, C1 );
    return X1;
end );

##############################################################################
##
#M  Cat1GroupOfXMod
##
InstallMethod( Cat1GroupOfXMod, "generic method for crossed modules", 
    true, [ IsXMod ], 0, 
function( X1 )

    local PC1, C1;

    PC1 := PreCat1GroupOfPreXMod( X1 ); 
    C1 := PC1.precat1;
    SetXModOfCat1Group( C1, X1 );
    return C1;
end );

##############################################################################
##
#M  PeifferSubgroupPreXMod . . . . . normally generated by Peiffer commutators
##
InstallMethod( PeifferSubgroupPreXMod, "generic method for pre-crossed xmods",
    true, [ IsPreXMod ], 0,
function( PM )

    local Pf, s1, s2, a1, src, gensrc, comm, bdy, act, ok, XPf;

    # this code mimics that of DerivedSubgroup
    src := Source( PM );
    bdy := Boundary( PM );
    act := XModAction( PM );
    gensrc := GeneratorsOfGroup( src );
    Pf := TrivialSubgroup( src );
    for s1 in gensrc do
        a1 := ImageElm( act, ImageElm( bdy, s1 ) );
        for s2 in gensrc do
            comm := (s2^-1)^s1 * ImageElm( a1, s2 );
            if not ( comm in Pf ) then
                Pf := ClosureSubgroup( Pf, comm );
            fi;
        od;
    od;
    Pf := NormalClosure( src, Pf );
    if ( Pf = src ) then
        Pf := src;
    fi; 
    XPf := Sub2DimensionalGroup( PM, Pf, TrivialSubgroup( Range(PM) ) ); 
    ok := IsNormal( PM, XPf );
    SetPeifferSub2DimensionalGroup( PM, XPf ); 
    return Pf;
end );

##############################################################################
##
#M  PeifferSubgroupPreCat1Group . . . . commutator of kernels of tail and head
##
InstallMethod( PeifferSubgroupPreCat1Group, 
    "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local src, kerh, kert, Pf;

    src := Source( PCG );
    kert := Kernel( TailMap( PCG ) );
    kerh := Kernel( HeadMap( PCG ) );
    Pf := CommutatorSubgroup( kert, kerh );
    if ( Pf = src ) then
        Pf := src;
    fi;
    return Pf;
end );

##############################################################################
##
#M  PeifferSubgroup . . . . . . . . 
##
InstallMethod( PeifferSubgroup, "generic method for 2d-groups",
               true, [ Is2DimensionalGroup ], 0,
function( obj ) 
    local P, ok, NP;
    if IsPreXModObj( obj ) then
        if IsXMod( obj ) then
            return Subgroup( Source( obj ), [ One( Source( obj ) ) ] );
        else
            P := PeifferSubgroupPreXMod( obj );
            NP := SubPreXMod( obj, P, TrivialSubgroup( Range(obj) ) );
            ok := IsNormal( obj, NP );
            return P; 
        fi;
    elif IsPreCat1Obj( obj ) then
        if IsCat1Group( obj ) then
            return Subgroup( Source( obj ), [ One( Source( obj ) ) ] );
        else
            P := PeifferSubgroupPreCat1Group( obj );
            NP := SubPreCat1Group( obj, P, TrivialSubgroup( Range(obj) ) );
            ok := IsNormal( obj, NP );            
            return P; 
        fi;
    else
        return fail;
    fi;
end );

##############################################################################
##
#A  XModByPeifferQuotient . . . . . . . xmod from prexmod and Peiffer subgroup
##
InstallMethod( XModByPeifferQuotient, 
    "crossed module from a pre-crossed module and Peiffer subgroup", true,
    [ IsPreXMod ], 0,
function( PM )

    local pfsub, pfxmod, name, nat, ok, FM;
    
    if IsXMod( PM ) then
        Info( InfoXMod, 1, "this object is already a crossed module!" );
        return PM;
    fi;
    pfsub := PeifferSubgroup( PM );
    if not IsNormal( Source( PM ), pfsub ) then
        Error( "Peiffer subgroup not normal in source group" );
    fi;
    pfxmod := SubPreXMod( PM, pfsub, TrivialSubgroup( Range(PM) ) ); 
    ok := IsNormal( PM, pfxmod ); 
    if not ok then 
        Error( "Peiffer precrossed module not normal!" ); 
    fi; 
    FM := FactorPreXMod( PM, pfxmod ); 
    nat := ProjectionOfFactorPreXMod( FM );
    if HasName( PM ) then
        name := Name( PM ); 
        SetName( FM, Concatenation( "Peiffer(", name, ")" ) ); 
    fi;
    return FM; 
end );
    
##############################################################################
##
#O  XModByPullback . . . . . . . . xmod from an xmod and a group homomorphism
##
InstallMethod( XModByPullback, 
    "crossed module from a crossed module and a group homomorphism", true,
    [ IsXMod, IsGroupHomomorphism ], 0,
function( X0, nu )

    local M, P, act0, N, L, genL, lenL, info, dp, emb1, emb2, lambda, 
          kappa, genLN, genLM, autL, genN, lenN, imact1, i, n, an, 
          actLN, actLM, prod, act1, X1, mor;
    
    M := Source( X0 ); 
    P := Range( X0 ); 
    if not ( Range( nu ) = P ) then
        Error( "Range(hom) <> Range(X0)" );
    fi;
    act0 := XModAction( X0 ); 
    N := Source( nu ); 
    L := Pullback( Boundary( X0 ), nu ); 
    genL := GeneratorsOfGroup( L ); 
    lenL := Length( genL );
    info := PullbackInfo( L ); 
    dp := info!.directProduct;
    emb1 := Embedding( dp, 1 ); 
    emb2 := Embedding( dp, 2 );
    kappa := info!.projections[1]; 
    lambda := info!.projections[2]; 
    genLN := List( genL, g -> ImageElm( lambda, g ) ); 
    genLM := List( genL, g -> ImageElm( kappa, g ) ); 
    autL := AutomorphismGroup( L );
    genN := GeneratorsOfGroup( N );
    lenN := Length( genN ); 
    imact1 := ListWithIdenticalEntries( lenN, 0 ); 
    for i in [1..lenN] do 
        n := genN[i]; 
        an := ImageElm( act0, ImageElm( nu, n ) ); 
        actLN := List( genLN, g -> g^n ); 
        actLM := List( genLM, g -> ImageElm( an, g ) ); 
        prod := List( [1..lenL], j -> ImageElm( emb1, actLM[j] ) 
                                    * ImageElm( emb2, actLN[j] ) ); 
        imact1[i] := GroupHomomorphismByImages( L, L, genL, prod ); 
    od;
    act1 := GroupHomomorphismByImages( N, autL, genN, imact1 ); 
    X1 := XMod( lambda, act1 ); 
    mor := XModMorphismByGroupHomomorphisms( X1, X0, kappa, nu ); 
    SetMorphismOfPullback( X1, mor ); 
    return X1; 
end );
    
##############################################################################
##
#A  KernelCokernelXMod . . . . . ( ker(bdy) -> range/image(bdy) ) for an xmod 
##
InstallMethod( KernelCokernelXMod, "kernel -> cokernel for an xmod", true,
    [ IsXMod ], 0,
function( X0 )

    local S, R, bdy, act, K, J, nat, F, iso, mgi, inv, C, genK, imres, res, 
          genC, preC, imact, i, g, p, ap, im, autK, actC;

    S := Source( X0 );
    R := Range( X0 ); 
    bdy := Boundary( X0 );
    act := XModAction( X0 );
    K := Kernel( bdy );
    J := Image( bdy );
    if ( J = R ) then ## trivial cokernel 
        C := Group( () ); 
        res := MappingToOne( K, C );
        return XModByTrivialAction( res );
    fi; 
    nat := NaturalHomomorphismByNormalSubgroup( R, J );
    F := FactorGroup( R, J ); 
    iso := IsomorphismPermGroup( F ); 
    C := Image( iso );
    mgi := MappingGeneratorsImages( iso );
    inv := GroupHomomorphismByImages( C, F, mgi[2], mgi[1] ); 
    genK := GeneratorsOfGroup( K ); 
    imres := List( genK, g -> Image( iso, Image( nat, Image( bdy, g ) ) ) );
    res := GroupHomomorphismByImages( K, C, genK, imres ); 
    genC := GeneratorsOfGroup( C ); 
    preC := List( genC, 
                  g -> PreImagesRepresentative( nat, ImageElm( inv, g ) ) ); 
    imact := ShallowCopy( genC ); 
    for i in [1..Length( genC )] do 
        g := genC[i]; 
        p := preC[i];
        ap := ImageElm( act, p ); 
        im := List( genK, k -> ImageElm( ap, k ) );
        imact[i] := GroupHomomorphismByImages( K, K, genK, im ); 
    od;
    autK := Group( imact );
    SetIsGroupOfAutomorphisms( autK, true ); 
    actC := GroupHomomorphismByImages( C, autK, genC, imact );  
    return XModByBoundaryAndAction( res, actC );  
end );

#############################################################################
##
#F  XMod( <bdy>, <act> )          crossed module from given boundary & action
#F  XMod( <G>, <N> )              crossed module from a normal inclusion
#F  XMod( <surj> )                crossed module from a surjective hom
#F  XMod( <cat1> )                crossed module associated to a cat1-group
#F  XMod( <aut> )                 crossed module from automorphism group
#F  XMod( <pxm> )                 crossed module by Peiffer quotient
##
InstallGlobalFunction( XMod, function( arg )

    local nargs;
    nargs := Length( arg );

    # two homomorphisms
    if ( ( nargs = 2 ) and IsGroupHomomorphism( arg[1] )
                       and IsGroupHomomorphism( arg[2] ) ) then
        return XModByBoundaryAndAction( arg[1], arg[2] );

    # group and normal subgroup
    elif ( ( nargs = 2 ) and IsGroup( arg[1] ) and IsGroup( arg[2] )
      and IsSubgroup( arg[1], arg[2] ) and IsNormal( arg[1], arg[2] ) ) then
        return XModByNormalSubgroup( arg[1], arg[2] );

    # xmod plus list of objects plus boolean
    elif ( ( nargs = 3 ) and IsXMod( arg[1] ) 
           and IsList( arg[2] ) and IsBool( arg[3] ) ) then
        return SinglePiecePreXModWithObjects( arg[1], arg[2], arg[3] );

    # surjective homomorphism
    elif ( ( nargs = 1 ) and IsGroupHomomorphism( arg[1] )
                         and IsSurjective( arg[1] ) ) then
        return XModByCentralExtension( arg[1] );

    # convert a cat1-group
    elif ( ( nargs = 1 ) and HasIsCat1Group( arg[1] ) 
           and IsCat1Group( arg[1] ) ) then
        return PreXModOfPreCat1Group( arg[1] );

    # group of automorphisms
    elif ( ( nargs = 1 ) and IsGroupOfAutomorphisms( arg[1] ) ) then
        return XModByAutomorphismGroup( arg[1] );

    # just a group
    elif ( ( nargs = 1 ) and IsGroup( arg[1] ) ) then
        return XModByNormalSubgroup( arg[1], arg[1] );

    # pre-crossed module
    elif ( ( nargs = 1 ) and IsPreXMod( arg[1] ) ) then
        return XModByPeifferQuotient( arg[1] );

    fi;
    # alternatives not allowed
    Error( "usage: XMod( bdy, act );  or  XMod( G, N );" );
end );

##############################################################################
##
#M  IsSubPreXMod
##
InstallMethod( IsSubPreXMod, "generic method for pre-crossed modules", true,
    [ Is2DimensionalGroup, Is2DimensionalGroup ], 0,
function( PM, SM )

    local ok, Ssrc, Srng, gensrc, genrng, s, r, r1, r2, im1, im2;

    if not ( IsPreXMod( PM ) and IsPreXMod( SM ) ) then
        return false;
    fi;
    if ( HasParent( SM ) and ( Parent( SM ) = PM ) ) then
        return true;
    fi;
    Ssrc := Source( SM );
    Srng := Range( SM );
    if not (     IsSubgroup( Source( PM ), Ssrc )
             and IsSubgroup( Range( PM ), Srng ) ) then
        Info( InfoXMod, 3, "IsSubgroup failure in IsSubPreXMod" );
        return false;
    fi;
    ok := true;
    gensrc := GeneratorsOfGroup( Ssrc );
    genrng := GeneratorsOfGroup( Srng );
    for s in gensrc do
        if ( ImageElm( Boundary(PM), s ) <> ImageElm( Boundary(SM), s ) ) then
            ok := false;
        fi;
    od;
    if not ok then
        Info( InfoXMod, 3, "boundary maps have different images" );
        return false;
    fi;
    for r in genrng do
        r1 := ImageElm( XModAction( PM ), r );
        r2 := ImageElm( XModAction( SM ), r );
        for s in gensrc do
            im1 := ImageElm( r1, s );
            im2 := ImageElm( r2, s );
            if ( im1 <> im2 ) then
                ok := false;
                Info( InfoXMod, 3, "s,im1,im2 = ", [s,im1,im2] );
            fi;
        od;
    od;
    if not ok then
        Info( InfoXMod, 3, "actions have different images" );
        return false;
    fi;
    if ( PM <> SM ) then
        SetParent( SM, PM );
    fi;
    return true;
end );

##############################################################################
##
#M  IsSubXMod( <XM>, <SM> )
##
InstallMethod( IsSubXMod, "generic method for crossed modules", true,
    [ Is2DimensionalGroup, Is2DimensionalGroup ], 0,
function( XM, SM )

    if not ( IsXMod( XM ) and IsXMod( SM ) ) then
        return false;
    fi;
    return IsSubPreXMod( XM, SM );
end );

##############################################################################
##
#M  IsSubPreCat1Group
##
InstallMethod( IsSubPreCat1Group, "generic method for pre-cat1-groups", true,
    [ Is2DimensionalGroup, Is2DimensionalGroup ], 0,
function( C0, S0 )

    local ok, Ssrc, Srng, gensrc, genrng, tc, hc, ec, ts, hs, es, s, r;

    if not ( IsPreCat1Group( C0 ) and IsPreCat1Group( S0 ) ) then
        return false;
    fi;
    if ( HasParent( S0 ) and ( Parent( S0 ) = C0 ) ) then
        return true;
    fi;
    Ssrc := Source( S0 );
    Srng := Range( S0 );
    if not (     IsSubgroup( Source( C0 ), Ssrc )
             and IsSubgroup( Range( C0 ), Srng ) ) then
        Info( InfoXMod, 3, "IsSubgroup failure in IsSubPreCat1Group" );
        return false;
    fi;
    ok := true;
    gensrc := GeneratorsOfGroup( Ssrc );
    genrng := GeneratorsOfGroup( Srng );
    tc := TailMap(C0);  hc := HeadMap(C0);  ec := RangeEmbedding(C0);
    ts := TailMap(S0);  hs := HeadMap(S0);  es := RangeEmbedding(S0);
    for s in gensrc do
        if ( ImageElm( tc, s ) <> ImageElm( ts, s ) ) then
            ok := false;
        fi;
        if ( ImageElm( hc, s ) <> ImageElm( hs, s ) ) then
            ok := false;
        fi;
    od;
    if not ok then
        Info( InfoXMod, 3, "tail/head maps have different images" );
        return false;
    fi;
    for r in genrng do
        if ( ImageElm( ec, r ) <> ImageElm( es, r ) ) then
            ok := false;
        fi;
    od;
    if not ok then
        Info( InfoXMod, 3, "embeddingss have different images" );
        return false;
    fi;
    if ( C0 <> S0 ) then
        SetParent( S0, C0 );
    fi;
    return true;
end );

##############################################################################
##
#M  IsSubCat1Group( <C1>, <S1> )
##
InstallMethod( IsSubCat1Group, "generic method for cat1-groups", true,
    [ Is2DimensionalGroup, Is2DimensionalGroup ], 0,
function( C1, S1 )

    if not ( IsCat1Group( C1 ) and IsCat1Group( S1 ) ) then
        return false;
    fi;
    return IsSubPreCat1Group( C1, S1 );
end );

##############################################################################
##
#M  IsNormalSub2DimensionalDomain( <XM>, <SM> )
##
InstallMethod( IsNormalSub2DimensionalDomain, "for xmod and subxmod etc.", 
    true, [ Is2DimensionalGroup, Is2DimensionalGroup ], 0,
function( X0, X1 )

    local ispx, ok, S0, R0, S1, R1, r0, s0, r1, s1; 

    ispx := IsPreXMod( X0 );
    if ispx then 
        ok := IsSubPreXMod( X0, X1 ); 
    else 
        ok := IsSubPreCat1Group( X0, X1 ); 
    fi; 
    if not ok then 
        return false; 
    fi;
    if not ispx then 
        Error( "not yet installed for cat1-groups" ); 
    fi; 
    S0 := Source( X0 );
    R0 := Range( X0 );
    S1 := Source( X1 );
    R1 := Range( X1 );
    if not IsNormal( R0, R1 ) then 
        return false; 
    fi;
    ## apparently no requirement for S1 to be normal in S0
    for r0 in GeneratorsOfGroup( R0 ) do 
        for s1 in GeneratorsOfGroup( S1 ) do
            if not ImageElmXModAction( X0, s1, r0 ) in S1 then 
                return false; 
            fi; 
        od; 
    od; 
    for r1 in GeneratorsOfGroup( R1 ) do 
        for s0 in GeneratorsOfGroup( S0 ) do
            if not s0^-1 * ImageElmXModAction( X0, s0, r1 ) in S1 then 
                return false; 
            fi; 
        od; 
    od; 
    return true;
end );

##############################################################################
##
#M  Sub2DimensionalGroup . .  creates Sub2bObject from Ssrc<=Osrc & Srng<=Orng
##
InstallMethod( Sub2DimensionalGroup, "generic method for 2d-objects", true,
    [ Is2DimensionalGroup, IsGroup, IsGroup ], 0,
function( obj, src, rng )
    if ( HasIsXMod(obj) and IsXMod(obj) ) then
        return SubXMod( obj, src, rng );
    elif ( HasIsPreXMod(obj) and IsPreXMod(obj) ) then
        return SubPreXMod( obj, src, rng );
    elif ( HasIsCat1Group(obj) and IsCat1Group(obj) ) then
        return SubCat1Group( obj, src, rng );
    elif ( HasIsPreCat1Group(obj) and IsPreCat1Group(obj) ) then
        return SubPreCat1Group( obj, src, rng );
    else
        Error( "unknown type of 2d-object" );
    fi;
end );

##############################################################################
##
#M  SubPreXMod                 creates SubPreXMod from Ssrc<=Psrc & Srng<=Prng
##
InstallMethod( SubPreXMod, "generic method for pre-crossed modules", true,
    [ IsPreXMod, IsGroup, IsGroup ], 0,
function( PM, Ssrc, Srng )

    local Psrc, Prng, Pbdy, Pact, Paut, genSsrc, genSrng, Pname, Sname,
           SM, Sbdy, Saut, Sact, r, innaut, genPrng, genPsrc, ssrc,
           trivsrc, trivrng, incSsrc, idSsrc, imact, imgen, imbdy, imSsrc,
           imalpha, alpha;

    Psrc := Source( PM );
    Prng := Range( PM );
    Pbdy := Boundary( PM );
    Pact := XModAction( PM );
    Paut := Range( Pact );
    if not IsSubgroup( Psrc, Ssrc ) then
        Print( "Ssrc is not a subgroup of Psrc\n" );
        return fail;
    fi;
    if not ( IsSubgroup( Prng, Srng ) ) then
        Print( "Srng is not a subgroup of Prng\n" );
        return fail;
    fi; 
    ssrc := Size( Ssrc );
    genPsrc := GeneratorsOfGroup( Psrc );
    genPrng := GeneratorsOfGroup( Prng );
    genSsrc := GeneratorsOfGroup( Ssrc );
    genSrng := GeneratorsOfGroup( Srng );
    incSsrc := InclusionMappingGroups( Psrc, Ssrc ); 
    imgen := List( genSsrc, x -> ImageElm( Pbdy, x ) );
    imSsrc := Subgroup( Prng, imgen );
    if not IsSubgroup( Srng, imSsrc ) then
        Info( InfoXMod, 2, "Pbdy(Ssrc) is not a subgroup of Srng" );
        return fail;
    fi; 
    trivsrc := ( Size( Ssrc ) = 1 );
    trivrng := ( Size( Srng ) = 1 );
    if ( trivrng or trivsrc ) then
        Sbdy := MappingToOne( Ssrc, Srng );
    else
        Sbdy:= GroupHomomorphismByImages( Ssrc, Srng, genSsrc, imgen );
    fi;
    innaut := [ ];
    for r in genSrng do
        alpha := ImageElm( Pact, r );
        imgen := List( genSsrc, x -> ImageElm( alpha, x ) );
        if not ForAll( imgen, x -> ( x in Ssrc ) ) then
            return fail;
        fi;
        imalpha := Subgroup( Ssrc, imgen );
        if not ( IsSubgroup( Ssrc, imalpha ) ) then
            Info( InfoXMod, 2, "Srng does not act correctly on Ssrc" );
            return fail;
        fi;
        alpha:=GroupHomomorphismByImages( Ssrc, Ssrc, genSsrc, imgen );
        Add( innaut, alpha );
    od;
    idSsrc := IdentityMapping( Ssrc );
    if ( ssrc = 1 ) then
        Saut := Group( idSsrc );
        innaut := List( genSrng, s -> idSsrc );
    else
        Saut := Group( innaut, idSsrc );
    fi;
    Sact := GroupHomomorphismByImages( Srng, Saut, genSrng, innaut );
    if ( not IsGroupHomomorphism( Sact ) ) then
        Print( "Sact is not a homomorphism\n" );
        return fail;
    fi;
    SM := PreXModByBoundaryAndAction( Sbdy, Sact );
    if HasParent( PM ) then
        SetParent( SM, Parent( PM ) );
    else
        SetParent( SM, PM );
    fi;
    return SM;
end );

##############################################################################
##
#M  SubXMod . . . . . . . . . . . creates SubXMod from Ssrc<=Psrc & Srng<=Prng
##
InstallMethod( SubXMod, "generic method for crossed modules", true,
    [ IsXMod, IsGroup, IsGroup ], 0,
function( XM, Ssrc, Srng )

    local SM;
    SM := SubPreXMod( XM, Ssrc, Srng );
    if ( SM = fail ) then
        return fail;
    fi;
    if not IsXMod( SM ) then
        Error( "the result is only a pre-crossed module" );
    fi;
    return SM;
end );

###############################################################################
##
#M  SubPreCat1Group . .  created from PreCat1Group and a subgroup of the source
##
InstallMethod( SubPreCat1Group, "generic method for (pre-)cat1-groups", true,
    [ IsPreCat1Group, IsGroup, IsGroup ], 0,
function( C, G, R )

    local Csrc, Crng, Ct, Ch, Ce, t, h, e, SC, ok;

    Csrc := Source( C );
    Crng := Range( C );
    Ct := TailMap( C );
    Ch := HeadMap( C );
    Ce := RangeEmbedding( C );
    ok := true;
    if not ( IsSubgroup( Csrc, G ) ) then
        Print( "G is not a subgroup of Csrc\n" );
        ok := false;
    fi;
    if not ( ( R = Image( Ct, G ) ) and
             ( R = Image( Ch, G ) ) ) then
        Print( "restrictions of Ct, Ch to G must have common image R\n" );
        ok := false;
    fi;
    t := GeneralRestrictedMapping( Ct, G, R );
    h := GeneralRestrictedMapping( Ch, G, R );
    e := GeneralRestrictedMapping( Ce, R, G );
    SC := PreCat1GroupByTailHeadEmbedding( t, h, e );
    if not ( C = SC ) then
        SetParent( SC, C );
    fi;
    return SC;
end );

##############################################################################
##
#M  SubCat1Group . . creates SubCat1Group from Cat1Group and subgroup of source
##
InstallMethod( SubCat1Group, "generic method for cat1-groups", true,
    [ IsCat1Group, IsGroup, IsGroup ], 0,
function( C, G, R )

    local S;
    S := SubPreCat1Group( C, G, R );
    if not IsCat1Group( S ) then
        Error( "result is only a pre-cat1-group" );
    fi;
    return S;
end );

#############################################################################
##
#M  IsCat1Group                  check that the second cat1-group axiom holds
##
InstallMethod( IsCat1Group, "generic method for crossed modules", true, 
    [ IsPreCat1Group ], 0,
function( C1G )

    local Csrc, Crng, h, t, e, f, kerC, kert, kerh, kerth;

    Csrc := Source( C1G );
    Crng := Range( C1G );
    h := HeadMap( C1G );
    t := TailMap( C1G );
    e := RangeEmbedding( C1G );
    kerC := Kernel( C1G );
    f := KernelEmbedding( C1G );
    kert := Kernel( t );
    kerh := Kernel( h );
    kerth := CommutatorSubgroup( kert, kerh );
    if not ( Size( kerth ) = 1 ) then
        Info( InfoXMod, 1, "condition  [kert,kerh] = 1  is not satisfied"); 
        return false;
    fi;
    if not ( ( Source( f ) = kerC ) and ( Range( f ) = Csrc ) ) then
        Print( "Warning: KernelEmbedding( C1G ) incorrectly defined?\n" );
    fi;
    return true;
end );

#############################################################################
##
#M  IsIdentityPreCat1Group
#M  IsPreCat1GroupByEndomorphisms 
##
InstallMethod( IsIdentityPreCat1Group, "test a pre-cat1-group", true, 
    [ IsPreCat1Group ], 0,
function( C1G )
    return ( ( TailMap( C1G ) = IdentityMapping( Source( C1G ) ) ) and
             ( HeadMap( C1G ) = IdentityMapping( Source( C1G ) ) ) );
end );

InstallMethod( IsPreCat1GroupByEndomorphisms, "test a pre-cat1-group", true, 
    [ IsPreCat1Group ], 0,
function( obj )
    return IsSubgroup( Source(obj), Range(obj) ); 
end );

#############################################################################
##
#F  Cat1Group( <size>, <gpnum>, <num> )     cat1-group from data in CAT1_LIST
#F  Cat1Group( <t>, <h>, <e> )              cat1-group from given t,h,e
#F  Cat1Group( <t>, <h> )                   cat1-group from t,h endomorphisms 
##
InstallGlobalFunction( Cat1Group, function( arg )

    local nargs, C1G, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 3 ) ) then
        Print( "standard usage: Cat1Group( tail, head [,embed] );\n" );
        Print( "            or: Cat1Group( size, gpnum, num );\n" );
        return fail;
    elif not IsInt( arg[1] ) then 
        if ( nargs = 1 ) then 
            C1G := PreCat1Group( arg[1] ); 
        elif ( nargs = 2 ) then 
            C1G := PreCat1Group( arg[1], arg[2] ); 
        elif ( nargs = 3 ) then 
            C1G := PreCat1Group( arg[1], arg[2], arg[3] ); 
        fi;
        ok := IsCat1Group( C1G );
        if ok then
            return C1G;
        else
            Error( "quotient by Peiffer group is not yet implemented" );
            return fail;
        fi;
    else   ## arg[1] is an integer, so use the data in cat1data.g
        return Cat1Select( arg[1], arg[2], arg[3] );
    fi;
end );

#############################################################################
##
#F  Cat1Select( <size>, <gpnum>, <num> )     cat1-group from data in CAT1_LIST
##
InstallMethod( Cat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt ], 0,
function( size )
    return Cat1Select( size, 0, 0 );
end );

InstallMethod( Cat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt, IsInt ], 0,
function( size, gpnum )
    return Cat1Select( size, gpnum, 0 );
end );

InstallMethod( Cat1Select, "construct a cat1-group using data in file", true, 
    [ IsInt, IsInt, IsInt ], 0,
function( size, gpnum, num )

    local ok, type, norm, usage, usage2, maxsize, start, iso, count, comm, 
           pos, pos2, names, i, j, k, ncat1, G, genG, fam, M, L, genR, R, 
           imt, t, kert, imh, h, C1G, XC, i0;

    maxsize := CAT1_LIST_MAX_SIZE;
    usage := "Usage:  Cat1Select( size, gpnum, num );";
    usage2 := "   where size <= CAT1_LIST_MAX_SIZE = ";
    if not ( ( size > 0 ) and ( size <= maxsize ) ) then
        Print( usage, usage2, CAT1_LIST_MAX_SIZE, "\n" );
        return fail;
    fi;
    if ( size = 1 ) then 
        if ( num = 0 ) then 
            Print( usage, "\nThere is only " ); 
            Print( "the trivial cat1-structure on the trivial group.\n" ); 
            Print( "(1)  [ [ ],  tail = head = zero mapping ]\n" ); 
            return 1; 
        elif ( num = 1 ) then 
            G := SmallGroup( 1, 1 ); 
            t := IdentityMapping( G ); 
            return PreCat1GroupByEndomorphisms( t, t ); 
        else 
            return fail;
        fi; 
    fi; 
    if ( CAT1_LIST_LOADED = false ) then
        ReadPackage( "xmod", "lib/cat1data.g" );
    fi;
    # find starting positions of iso classes of groups of size <= maxsize
    iso := CAT1_LIST_CLASS_SIZES;
    count := 1;
    start := [ 1 ];
    for j in iso do
        count := count + j;
        Add( start, count );
    od;
    Info( InfoXMod, 2, "  iso = ", iso, "\n  start = ", start );
    if ( ( size < 1 ) or ( size > maxsize ) ) then
        Error( "only groups of order up to ", maxsize, " in CAT1_LIST");
        return false;
    fi;
    pos := start[ size ];
    if ( size < maxsize ) then
        pos2 := start[ size + 1 ] - 1;
    else
        pos2 := Length( CAT1_LIST );
    fi;
    names := List( [ pos..pos2], n -> CAT1_LIST[n][3] );
    if not ( gpnum > 0 ) then
        Print( usage, "\n" );
        return names;
    fi;
    if ( gpnum > iso[size] ) then
        Print( "# isomorphism classes of groups of size ", size, 
               " is ", iso[size], ", less than ", gpnum, "\n" ); 
        Print( usage, "\n" );
        return names;
    fi;
    j := pos + gpnum - 1;
    M := CAT1_LIST[j]; 
    if not ( ( M[1] = size ) and ( M[2] = gpnum ) ) then 
        Error( "M[1..2] <> [ size, gpnum ]" ); 
    fi; 
    G  := SmallGroup( size, gpnum ); 
    SetName( G, M[3] ); 
    comm := IsCommutative( G ); 
    if comm then 
        ncat1 := Length( M[5] ) + 2; 
        k := 2; 
    else 
        ncat1 := Length( M[5] ) + 1; 
        k := 1; 
    fi; 
    if IsPermGroup( G ) then 
        return PermCat1Select( size, gpnum, num ); 
    fi; 
    fam := FamilyObj( GeneratorsOfGroup( G )[1] ); 
    genG := List( M[4], e -> ObjByExtRep( fam, e ) ); 
    if not ( ( num >= 1 ) and ( num <= ncat1 ) ) then
        Print( usage, "\n" ); 
        Print( "There are ", ncat1, " cat1-structures for the group ");
        Print( G, ".\n" ); 
        Print( "Using small generating set ", genG, " for source of homs.\n" ); 
        Print( "[ [range gens]," ); 
        Print( " [tail genimages], [head genimages] ]" );
        Print( " :-\n" );
        if comm then 
            Print("(1)  [ ","[ ]",",  tail = head = zero mapping ]\n"); 
        fi; 
        for i in [1..ncat1-k] do 
            if comm then i0 := i+1; else i0 := i; fi; 
            L := M[5][i]; 
            genR := List( L[1], e -> ObjByExtRep( fam, e ) ); 
            imt := List( L[2], e -> ObjByExtRep( fam, e ) ); 
            imh := List( L[3], e -> ObjByExtRep( fam, e ) ); 
            Print( "(", i0, ")  ", [ genR, imt, imh ], "\n" );
        od;
        Print("(",ncat1,")  [ ",genG,",  tail = head = identity mapping ]\n"); 
        return ncat1;
    fi;
    if ( num = ncat1 ) then
        t := IdentityMapping( G ); 
        h := ShallowCopy( t ); 
    else 
        if ( ( num = 1 ) and IsCommutative( G ) ) then 
            R := TrivialSubgroup( G ); 
            SetName( R, "triv" ); 
            t := MappingToOne( G, R ); 
            h := ShallowCopy( t ); 
        else
            L := M[5][num-k+1]; 
            genR := List( L[1], e -> ObjByExtRep( fam, e ) );
            R := Subgroup( G, genR );
            imt := List( L[2], e -> ObjByExtRep( fam, e ) ); 
            t := GroupHomomorphismByImages( G, R, genG, imt );
            imh := List( L[3], e -> ObjByExtRep( fam, e ) ); 
            h := GroupHomomorphismByImages( G, R, genG, imh );
        fi; 
        SetIsEndoMapping( t, true );
        SetIsEndoMapping( h, true );
        kert := Kernel( t );
    fi; 
    C1G := PreCat1GroupByEndomorphisms( t, h ); 
    ok := IsCat1Group( C1G ); 
    if ok then 
        XC := XModOfCat1Group( C1G ); 
    fi; 
    return C1G; 
end );

InstallMethod( PermCat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt, IsInt, IsInt ], 0,
function( size, gpnum, num )

    local ok, type, norm, maxsize, start, iso, count, pos, pos2, names,
           i, j, ncat1, G, genG, fam, M, L, genR, R, t, kert, h, C1G, XC;

    # find starting positions of iso classes of groups of size <= maxsize
    maxsize := CAT1_LIST_MAX_SIZE;
    iso := CAT1_LIST_CLASS_SIZES;
    count := 1;
    start := [ 1 ];
    for j in iso do
        count := count + j;
        Add( start, count );
    od;
    Info( InfoXMod, 2, "  iso = ", iso, "\n  start = ", start );
    if ( ( size < 1 ) or ( size > maxsize ) ) then
        Error( "only groups of order up to ", maxsize, " in CAT1_LIST");
        return false;
    fi;
    pos := start[ size ];
    if ( size < maxsize ) then
        pos2 := start[ size + 1 ] - 1;
    else
        pos2 := Length( CAT1_LIST );
    fi;
    names := List( [ pos..pos2], n -> CAT1_LIST[n][4] );
    j := pos + gpnum - 1;
    M := CAT1_LIST[j];
    G  := Group(M[4], ( ));
    SetName( G, M[3] );
    ncat1 := Length( M[5] ) + 1;
    genG := GeneratorsOfGroup( G );
    if not ( ( num >= 1 ) and ( num <= ncat1 ) ) then
        Print( "\nThere are ", ncat1, " cat1-structures for the group ");
        Print( G, ".\n" ); 
        Print( "[ [range gens], source & range names," ); 
        Print( " [tail genimages], [head genimages] ]" );
        Print( " :-\n" );
        Print( "[ ", genG, ",  tail = head = identity mapping ]\n" ); 
        for i in [2..ncat1] do
            Print( M[5][i-1], "\n" );
        od;
        Print( "Group has generators ", genG, "\n" );
        return ncat1;
    fi;
    if ( num = ncat1 ) then
        L := [ genG, genG, genG ];
    else
        L := M[5][num-1];
    fi;
    genR := L[1];
    R := Subgroup( G, genR ); 
    if ( G = R ) then 
        SetName( R, Name(G) ); 
    fi; 
    t := GroupHomomorphismByImages( G, R, genG, L[2] );
    h := GroupHomomorphismByImages( G, R, genG, L[3] );
    SetIsEndoMapping( t, true );
    SetIsEndoMapping( h, true );
    kert := Kernel( t ); 
    C1G := PreCat1GroupByEndomorphisms( t, h ); 
    ok := IsCat1Group( C1G ); 
    if ok then 
        XC := XModOfCat1Group( C1G ); 
    fi; 
    return C1G; 
end );

#############################################################################
##
#M  PreCat1GroupByTailHeadEmbedding
##
InstallMethod( PreCat1GroupByTailHeadEmbedding,
    "cat1-group from tail, head and embedding", true, 
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( t, h, e )

    local genG, R, genR, imh, imt, ime, eR, kert, kergen, bdy, imbdy, 
          f, C1G, ok, G, PC;

    G := Source( t );
    genG := GeneratorsOfGroup( G );
    R := Range( t );
    genR := SmallGeneratingSet( R ); 
    eR := Image( e ); 
    if not ( ( Source( h ) = G )
             and ( Image( h ) = R ) and ( Source( e ) = R )
             and IsInjective( e ) and IsSubgroup( G, eR ) )  then
        return fail;
    fi;
    imh := List( genG, x -> ImageElm( h, x ) );
    imt := List( genG, x -> ImageElm( t, x ) );
    ime := List( genR, x -> ImageElm( e, x ) );
    kert := Kernel( t );
    f := InclusionMappingGroups( G, kert );
    ## hres := GroupHomomorphismByImages( G, R, genG, imh );
    ## tres := GroupHomomorphismByImages( G, R, genG, imt );
    ## eres := GroupHomomorphismByImages( R, G, genR, ime );
    kergen := GeneratorsOfGroup( kert );
    imbdy := List( kergen, x -> ImageElm( h, x) );
    bdy := GroupHomomorphismByImages( kert, R, kergen, imbdy );
    PC := PreCat1Obj( t, h, e );
    SetBoundary( PC, bdy );
    SetKernelEmbedding( PC, f );
    return PC;
end );

#############################################################################
##
#M  IsPreCat1GroupByEndomorphisms( <pcg> ) 
#M  PreCat1GroupByEndomorphisms( <et>, <eh> ) 
#M  EndomorphismPreCat1Group( <pcg> )
##
InstallMethod( IsPreCat1GroupByEndomorphisms, "tail & head are endomorphisms", 
    true, [ IsPreCat1Group ], 0,
function( C1G )
    return IsSubgroup( Source( C1G ), Range( C1G ) ); 
end );

InstallMethod( PreCat1GroupByEndomorphisms,
    "cat1-group from tail and head endomorphisms", true, 
    [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( et, eh )

    local G, gG, R, t, h, e;

    if not ( IsEndoMapping( et ) and IsEndoMapping( eh ) ) then
        Print( "et, eh must both be group endomorphisms \n" );
		return fail;
    fi;
    if not ( Source( et ) = Source( eh ) ) then
        Info( InfoXMod, 2, "et and eh must have same source" );
	return fail;
    fi;
    G := Source( et );
    if not ( Image( et ) = Image( eh ) ) then
        Info( InfoXMod, 2, "et and eh must have same image" );
	return fail;
    fi;
    R := Image( et );
    gG := GeneratorsOfGroup( G );
    t := GroupHomomorphismByImages( G, R, gG, List( gG, g->ImageElm(et,g) ) );
    h := GroupHomomorphismByImages( G, R, gG, List( gG, g->ImageElm(eh,g) ) );
    e := InclusionMappingGroups( G, R );
    return PreCat1GroupByTailHeadEmbedding( t, h, e ); 
end );

InstallMethod( EndomorphismPreCat1Group,
    "convert cat1-group to one with endomorphisms", true, [ IsPreCat1Group ], 0,
function( C1G )

    local e, t, h;

    if IsPreCat1GroupByEndomorphisms( C1G ) then 
        return C1G; 
    fi; 
    e := RangeEmbedding( C1G ); 
    t := TailMap( C1G ) * e; 
    h := HeadMap( C1G ) * e; 
    return PreCat1GroupByEndomorphisms( t, h );
end ); 

#############################################################################
##
#M  PreXModOfPreCat1Group
##
InstallMethod( PreXModOfPreCat1Group, true, [ IsPreCat1Group ], 0,
function( C1G )
 
    local Csrc, Crng, gensrc, genrng, genker, bdy, kert, innaut, autgen,
           imautgen, idkert, a, aut, act, phi, j, r, PM, Cek, Cer, name; 

    Csrc := Source( C1G );
    Crng := Range( C1G );
    bdy := Boundary( C1G );
    Cer := RangeEmbedding( C1G );
    Cek := KernelEmbedding( C1G );
    kert := Kernel( C1G ); 
    if ( Size( kert ) = 1 ) then 
        SetName( kert, "triv" ); 
    fi; 
    gensrc := GeneratorsOfGroup( Csrc );
    genrng := GeneratorsOfGroup( Crng );
    genker := GeneratorsOfGroup( kert );
    if IsIdentityPreCat1Group( C1G ) then
        # X has trivial source and action
        aut := Group( IdentityMapping( kert ) );
        SetName( aut, "triv_aut" );
        act := MappingToOne( Crng, aut );
        SetName( act, "mapto1" );
    else
        autgen := [ ];
        for r in genrng do
            imautgen := List( genker, s -> ImageElm( Cek, s ) );
            imautgen := List( imautgen, g -> g^( ImageElm( Cer, r ) ) );
            imautgen := List( imautgen,
                              g -> PreImagesRepresentative( Cek, g ) );
            a := GroupHomomorphismByImages( kert, kert, genker, imautgen );
            Add( autgen, a );
        od;
        idkert := IdentityMapping( kert );
        aut := Group( autgen, idkert );
        act := GroupHomomorphismByImages( Crng, aut, genrng, autgen );
        if HasName( kert ) then
            SetName( aut, Concatenation( "innaut(", Name( kert ), ")" ) );
        else
            SetName( aut, "aut" );
        fi;
        if not IsGroupHomomorphism( act ) then
            Error( "act is not a homomorphism" );
        fi;
    fi;
    PM := PreXModObj( bdy, act ); 
    #?  aded 30/04/08 - but is it really needed ?? 
    if ( IsSubgroup( Crng, kert ) and IsNormal( Crng, kert ) ) then 
        SetIsNormalSubgroup2DimensionalGroup( PM, true ); 
    fi; 
    if ( HasName( Source( bdy ) ) and HasName( Range( bdy ) ) ) then 
        name := Name( PM ); 
    elif HasName( C1G ) then 
        SetName( PM, Concatenation( "xmod(", Name( C1G ), ")" ) ); 
    fi; 
    SetPreCat1GroupOfPreXMod( PM, rec( 
        precat1 := C1G, 
        xmodSourceEmbedding := kert, 
        xmodSourceEmbedddingIsomorphism := Cek, 
        xmodRangeEmbedding := Image( Cer ), 
        xmodRangeEmbeddingIsomorphism := Cer ) ); 
    return PM;
end );

#############################################################################
##
#M  Source( C1G ) . . . . . . . . . . . . . . . . . . . .  for a cat1-group
##
InstallOtherMethod( Source,
    "method for a pre-cat1-group",
    true,
    [ IsPreCat1Group ], 0,
    C1G -> Source( TailMap( C1G ) ) );

##############################################################################
##
#M  Range( C1G ) . . . . . . . . . . . . . . . . . . . . . for a cat1-group
##
InstallOtherMethod( Range,
    "method for a pre-cat1-group",
    true,
    [ IsPreCat1Group ], 0,
    C1G -> Range( TailMap( C1G ) ) );

##############################################################################
##
#M  Kernel( C1G ) . . . . . . . . . . . . . . . . . . . for a pre-cat1-group
##
InstallOtherMethod( Kernel,
    "method for a pre-cat1-group", true, [ IsPreCat1Group ], 0,
    C1G -> Kernel( TailMap( C1G ) ) );

#############################################################################
##
#M  Boundary( C1G ) . . . . . . . . . . . . . . . . . . .  for a cat1-group
##
InstallOtherMethod( Boundary,
    "method for a pre-cat1-group", true, [ IsPreCat1Group ], 0,
    C1G -> GeneralRestrictedMapping( HeadMap(C1G), Kernel(C1G), Range(C1G) ) );

#############################################################################
##
#M  KernelEmbedding( C1G ) . . .  . . . . . . . . . . . . .  for a cat1-group
##
InstallMethod( KernelEmbedding,
    "method for a pre-cat1-group", true, [ IsPreCat1Group ], 0,
    C1G -> InclusionMappingGroups( Source( C1G ), Kernel( C1G ) ) );

##############################################################################
##
#M  Cat1GroupByPeifferQuotient . . . . cat1 from pre-cat1 and Peiffer subgroup
##
InstallMethod( Cat1GroupByPeifferQuotient, 
               "cat1-group from a pre-cat1-group and Peiffer subgroup",
               true, [ IsPreCat1Group ], 0,
function( PC )

    local PCrng, PCsrc, PCt, PCh, PCe, genrng, gensrc, Pf, nat, quot,
           qgen, pqgen, tpqgen, hpqgen, tail, head, ime, embed, C1G;

    PCrng := Range( PC ) ;
    genrng := GeneratorsOfGroup( PCrng );
    PCsrc := Source( PC );
    gensrc := GeneratorsOfGroup( PCsrc );
    PCt := TailMap( PC );
    PCh := HeadMap( PC );
    PCe := RangeEmbedding( PC );
    # construct the quotient
    Pf := PeifferSubgroupPreCat1Group( PC );
    if not IsNormal( PCsrc, Pf ) then
        Error( "Peiffer subgroup not normal in source group" );
    fi;
    nat := NaturalHomomorphismByNormalSubgroup( PCsrc, Pf );
    quot := Image( nat );
    qgen := GeneratorsOfGroup( quot );
    # construct the head, tail and embedding
    pqgen := List( qgen, q -> PreImagesRepresentative( nat, q ) );
    tpqgen := List( pqgen, p -> ImageElm( PCt, p ) );
    tail := GroupHomomorphismByImages( quot, PCrng, qgen, tpqgen );
    hpqgen := List( pqgen, p -> ImageElm( PCh, p ) );
    head := GroupHomomorphismByImages( quot, PCrng, qgen, hpqgen );
    ime := List( genrng, r -> ImageElm( nat, ImageElm( PCe, r ) ) );
    embed := GroupHomomorphismByImages( PCrng, quot, genrng, ime );
    C1G := PreCat1GroupByTailHeadEmbedding( tail, head, embed );
    if not IsCat1Group( C1G ) then
        Error( "fails to be a cat1-group" );
    fi;
    return C1G;
end );

##############################################################################
##
#M  DiagonalCat1Group . . . . . . cat1-group of the form (GxG => G) with t<>h
##
InstallMethod( DiagonalCat1Group, "cat1-group from a list of generators",
    true, [ IsList ], 0,
function( gen1 )

    local m, lgen, gen2, genR, i, p, L1, len1, L2, j, 
           G, R, genG, one, ids, t, h, e, C;

    m := Maximum( List( gen1, g -> LargestMovedPoint(g) ) ); 
    lgen := Length( gen1 ); 
    gen2 := ShallowCopy( gen1 ); 
    genR := ShallowCopy( gen1 ); 
    for i in [1..lgen] do 
        p := gen1[i]; 
        L1 := ListPerm( p ); 
        len1 := Length( L1 ); 
        L2 := [1..2*m]; 
        for j in [1..len1] do 
            L2[m+j] := L1[j]+m; 
        od; 
        gen2[i] := PermList( L2 ); 
        for j in [1..len1] do 
            L2[j] := L1[j]; 
        od; 
        genR[i] := PermList( L2 ); 
    od;
    genG := Concatenation( gen1, gen2 ); 
    G := Group( genG ); 
    R := Group( genR ); 
    one := One( G ); 
    ids := ListWithIdenticalEntries( lgen, one ); 
    t := GroupHomomorphismByImages( G, R, genG, Concatenation( genR, ids ) ); 
    h := GroupHomomorphismByImages( G, R, genG, Concatenation( ids, genR ) );
    e := GroupHomomorphismByImages( R, G, genR, genR ); 
    C := PreCat1GroupByTailHeadEmbedding( t, h, e ); 
    return C;
end );

##############################################################################
##
#M  AllCat1GroupsWithImage  . . . . . . cat1-group structures with given range
#O  AllCat1GroupsWithImageIterator( <gp> )  . . iterator for the previous list
#F  NextIterator_AllCat1GroupsWithImage( <iter> ) 
#F  IsDoneIterator_AllCat1GroupsWithImage( <iter> ) 
#F  ShallowCopy_AllCat1GroupsWithImage( <iter> ) 
#M  AllCat1GroupsWithImageUpToIsomorphism  . . . . . iso class reps for G => R
##
BindGlobal( "NextIterator_AllCat1GroupsWithImage", function ( iter ) 

    local C, post, pair, t, posh, h, ok; 

    ok := false; 
    while ( not ok ) and ( not IsDoneIterator( iter!.pairsIterator ) ) do 
        pair := NextIterator( iter!.pairsIterator ); 
        ## could attempt to be clever and not calculate t every time 
        t := GroupHomomorphismByImages( iter!.group, iter!.group, 
                                        iter!.gens, iter!.images[ pair[1] ] ); 
        h := GroupHomomorphismByImages( iter!.group, iter!.group, 
                                        iter!.gens, iter!.images[ pair[2] ] ); 
        C := PreCat1GroupByEndomorphisms( t, h ); 
        if ( not ( C = fail ) and IsCat1Group( C ) ) then 
            ok := true; 
            return C; 
        fi; 
        if IsDoneIterator( iter!.pairsIterator ) then 
            return fail; 
        fi; 
    od; 
end ); 

BindGlobal( "IsDoneIterator_AllCat1GroupsWithImage", 
    iter -> IsDoneIterator( iter!.pairsIterator )  
); 

BindGlobal( "ShallowCopy_AllCat1GroupsWithImage", 
    iter -> rec( group := iter!.group, 
                  gens := iter!.gens, 
                images := iter!.images, 
         pairsIterator := ShallowCopy( iter!.pairsIterator ) 
    )  
); 

InstallGlobalFunction( "DoAllCat1GroupsWithImageIterator", 
function( G, R )

    local data, genG, images, found, len, i, lenIterator, pairsIterator, iter;

    data := IdempotentEndomorphismsData( G ); 
    genG := data.gens; 
    images := data.images; 
    found := false; 
    len := Length( images ); 
    i := 0;
    while ( not found ) and ( i < len ) do 
        i := i+1; 
        if ( R = Subgroup( G, images[i][1] ) ) then 
            found := true; 
            images := images[i]; 
        fi; 
    od; 
    if not found then 
        ## there are no idempotent endomorphisms with image R 
        return IteratorList( [ ] ); 
    fi; 
    lenIterator := IteratorList( [1..Length(images)] );
    pairsIterator := CartesianIterator( lenIterator, lenIterator ); 
    iter := IteratorByFunctions( 
        rec(     group := G, 
                  gens := genG, 
                images := images,
         pairsIterator := ShallowCopy( pairsIterator ),
          NextIterator := NextIterator_AllCat1GroupsWithImage, 
        IsDoneIterator := IsDoneIterator_AllCat1GroupsWithImage, 
           ShallowCopy := ShallowCopy_AllCat1GroupsWithImage ) ); 
    return iter;
end );

InstallMethod( AllCat1GroupsWithImageIterator, "for a group and a subgroup", 
    [ IsGroup, IsGroup ], 0, 
function( G, R )
    if not IsSubgroup( G, R ) then 
        Error( "R is not a subgroup of G" ); 
    fi; 
    return DoAllCat1GroupsWithImageIterator( G, R ); 
end ); 

InstallMethod( AllCat1GroupsWithImage, "for a group and a subgroup", 
    [ IsGroup, IsGroup ], 0, 
function( G, R ) 

    local L, C; 

    L := [ ];
    for C in AllCat1GroupsWithImageIterator( G, R ) do 
        if not ( C = fail ) then 
           Add( L, C ); 
        fi;
    od;
    return L; 
end ); 

InstallMethod( AllCat1GroupsWithImageNumber, "for a group and a subgroup", 
    [ IsGroup, IsGroup ], 0, 
function( G, R ) 

    local n, C; 

    n := 0;
    for C in AllCat1GroupsWithImageIterator( G, R ) do 
        if not ( C = fail ) then 
            n := n+1; 
        fi; 
    od;
    return n; 
end ); 

##############################################################################
##
#M  AllCat1Groups . . . . . . . list of cat1-group structures on a given group
#O  AllCat1GroupsIterator( <gp> ) . . . . . . . iterator for the previous list
#F  NextIterator_AllCat1Groups( <iter> ) 
#F  IsDoneIterator_AllCat1Groups( <iter> ) 
#F  ShallowCopy_AllCat1Groups( <iter> ) 
#A  AllCat1GroupsNumber( <gp> ) . . . . . . . . .  number of these cat1-groups
#M  AllCat1GroupsUpToIsomorphism . . . iso class reps of cat1-group structures
##
BindGlobal( "NextIterator_AllCat1Groups", function ( iter ) 
    local R, C; 
    if IsDoneIterator( iter!.imagesIterator ) then 
        R := NextIterator( iter!.subsIterator ); 
        iter!.imagesIterator := 
            AllCat1GroupsWithImageIterator( iter!.group, R ); 
        ## but this iterator might be empty, so: 
        if IsDoneIterator( iter!.imagesIterator ) then 
            return fail; 
        fi; 
    fi; 
    return NextIterator( iter!.imagesIterator ); 
end ); 

BindGlobal( "IsDoneIterator_AllCat1Groups", 
    iter -> ( IsDoneIterator( iter!.subsIterator ) 
              and IsDoneIterator( iter!.imagesIterator ) ) 
); 

BindGlobal( "ShallowCopy_AllCat1Groups", 
    iter -> rec( group := iter!.group, 
          subsIterator := ShallowCopy( iter!.subsIterator ),
        imagesIterator := ShallowCopy( iter!.imagesIterator )
    )  
); 

InstallGlobalFunction( "DoAllCat1GroupsIterator", 
function( G )

    local subsIterator, imagesIterator, iter;

    subsIterator := AllSubgroupsIterator( G ); 
    imagesIterator := IteratorList( [ ] );
    iter := IteratorByFunctions( 
        rec(     group := G, 
          subsIterator := ShallowCopy( subsIterator ), 
        imagesIterator := ShallowCopy( imagesIterator ), 
          NextIterator := NextIterator_AllCat1Groups, 
        IsDoneIterator := IsDoneIterator_AllCat1Groups, 
           ShallowCopy := ShallowCopy_AllCat1Groups ) ); 
    return iter;
end );

InstallMethod( AllCat1GroupsIterator, "for a group", [ IsGroup ], 0, 
    G -> DoAllCat1GroupsIterator( G ) ); 

InstallMethod( AllCat1Groups, "for a group", [ IsGroup ], 0, 
function( G ) 

    local L, C, images, lens; 

    InitCatnGroupRecords( G ); 
    L := [ ];
    for C in AllCat1GroupsIterator( G ) do 
       if not ( C = fail ) then 
           Add( L, C ); 
        fi; 
    od;
    if not IsBound( CatnGroupNumbers( G ).idem ) then 
        images := IdempotentEndomorphismsData( G ).images; 
        lens := List( images, L -> Length( L ) ); 
        CatnGroupNumbers( G ).idem := Sum( lens ); 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).cat1 ) then 
        CatnGroupNumbers( G ).cat1 := Length( L ); 
    fi; 
    return L; 
end ); 

InstallMethod( AllCat1GroupsNumber, "for a group", [ IsGroup ], 0, 
function( G ) 

    local n, C, all; 

    InitCatnGroupRecords( G ); 
    if IsBound( CatnGroupNumbers( G ).cat1 ) then 
        return CatnGroupNumbers( G ).cat1; 
    fi; 
    ## not already known, so perform the calculation 
    all := AllCat1Groups( G );
    return CatnGroupNumbers( G ).cat1; 
end ); 

InstallMethod( AllCat1GroupsUpToIsomorphism, "iso class reps of cat1-groups", 
    true, [ IsGroup ], 0,
function( G )

    local L, numL, i, k, C, ok, found, iso, images, lens;

    InitCatnGroupRecords( G ); 
    L := [ ]; 
    i := 0; 
    numL := 0; 
    for C in AllCat1GroupsIterator( G ) do 
        if not ( C = fail ) then 
            i := i+1; 
            k := 0; 
            found := false; 
            while ( not found ) and ( k < numL ) do 
                k := k+1; 
                iso := IsomorphismCat1Groups( C, L[k] ); 
                if ( iso <> fail ) then 
                     found := true; 
                fi; 
            od; 
            if not found then 
                Add( L, C ); 
                numL := numL + 1;
            fi;
        fi;
    od; 
    if not IsBound( CatnGroupNumbers( G ).idem ) then 
        images := IdempotentEndomorphismsData( G ).images; 
        lens := List( images, L -> Length( L ) ); 
        CatnGroupNumbers( G ).idem := Sum( lens ); 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).cat1 ) then 
        CatnGroupNumbers( G ).cat1 := i; 
    fi; 
    if not IsBound( CatnGroupNumbers( G ).iso1 ) then 
        CatnGroupNumbers( G ).iso1 := numL; 
    fi; 
    return L;
end );

#############################################################################
##
#M  DirectProductInfo( <obj> ) . . . . . . . . . . . . . . . . for 2d-objects
#M  Coproduct2dInfo( <obj> ) . . . . . . . . . . . . . . . . . for 2d-objects
#M  DirectProductOp(  )  . . . . . .  (bdy1 x bdy2) : (S1 x S2) --> (R1 x R2)
##
InstallOtherMethod( DirectProductInfo, "generic method for 2d-objects", true, 
    [ Is2DimensionalDomain ], 0,
function( obj )
    return rec( objects := [ ],
                embeddings := [ ],
                projections := [ ] );
end );

InstallMethod( Coproduct2dInfo, "generic method for 2d-objects", true, 
    [ Is2DimensionalDomain ], 0,
function( obj )
    return rec( objects := [ ],
                embeddings := [ ],
                projections := [ ] );
end );

#?  (19/07/07) : allowed for case when one of Xsrc,Xrng,Ysrc,Yrng trivial ##
#?               using parameter list: spec(=[0,0,0,0] by default)        ##

InstallOtherMethod( DirectProductOp,
    "method for pre-crossed modules", true, [ IsList, IsPreXMod ], 0,
function( list, X1 )

    local Xsrc, Xrng, Y1, Ysrc, Yrng, genXrng, genYrng, genXsrc, genYsrc,
           XSpos, YSpos, XRpos, YRpos, Spos, imaut, autgen, aut, act,
           XY, S, R, genS, lenS, genR, lenR, imbdy, bdy, a, i, j, k,
           Xbdy, Ybdy, Xact, Yact, imXbdy, imYbdy, alpha, info,
           eXS, eYS, pXS, pYS, eXR, eYR, spec;

    if not ( Length( list ) = 2 ) then
        Error( "direct product not yet implemented for more than 2 terms" );
    fi;
    if not ( list[1] = X1 ) then
        Error( "second argument should be first entry in first argument list" );
    fi;
    Y1 := list[2]; 
    ##  first the source group 
    Xsrc := Source( X1 );
    Ysrc := Source( Y1 );
    genXsrc := GeneratorsOfGroup( Xsrc );
    genYsrc := GeneratorsOfGroup( Ysrc );
    spec := [false,false,false,false]; 
    if ( Size( Xsrc ) = 1 ) then 
        spec[1] := true; 
    elif ( Size( Ysrc ) = 1 ) then 
        spec[3] := true;
    fi; 
    if spec[1] then 
        S := Ysrc; 
        eYS := IdentityMapping( S ); 
        pYS := IdentityMapping( S );
    elif spec[3] then 
        S := Xsrc;
        eXS := IdentityMapping( S ); 
        pXS := IdentityMapping( S ); 
    else
        S := DirectProduct( Xsrc, Ysrc );
        if ( not HasName( S ) and HasName( Xsrc ) and HasName( Ysrc ) ) then
            SetName( S, Concatenation( Name( Xsrc ), "x", Name( Ysrc ) ) );
        fi;
        eXS := Embedding( S, 1 );
        eYS := Embedding( S, 2 );
        pXS := Projection( S, 1 );
        pYS := Projection( S, 2 ); 
    fi;
    genS := GeneratorsOfGroup( S );
    lenS := Length( genS );
    Spos := [ 1..lenS ];
    if spec[1] then 
        XSpos := [ ]; 
        YSpos := [ 1..Length( genYsrc ) ]; 
    elif spec[3] then 
        XSpos := [ 1..Length( genXsrc ) ]; 
        YSpos := [ ]; 
    else 
        XSpos := [ 1..Length( genXsrc ) ];
        YSpos := [ 1+Length( genXsrc ) .. lenS ]; 
    fi;
    ##  now for the range group 
    Xrng := Range( X1 );
    Yrng := Range( Y1 );
    genXrng := GeneratorsOfGroup( Xrng );
    genYrng := GeneratorsOfGroup( Yrng );
    if ( Size( Xrng ) = 1 ) then 
        spec[2] := true; 
    elif ( Size( Yrng ) = 1 ) then 
        spec[4] := true;
    fi;
    if spec[2] then 
        R := Yrng; 
        eXR := MappingToOne( Xrng, Yrng ); 
        eYR := IdentityMapping( R );
    elif spec[4] then 
        R := Xrng; 
        eXR := IdentityMapping( R ); 
        eYR := MappingToOne( Yrng, Xrng );
    else
        R := DirectProduct( Xrng, Yrng );
        if ( not HasName( R ) and HasName( Xrng ) and HasName( Yrng ) ) then
            SetName( R, Concatenation( Name( Xrng ), "x", Name( Yrng ) ) );
        fi;
        eXR := Embedding( R, 1 );
        eYR := Embedding( R, 2 );
    fi;
    genR := GeneratorsOfGroup( R );
    lenR := Length( genR );
    if spec[2] then 
        XRpos := [ ]; 
        YRpos := [ 1..Length( genYrng ) ]; 
    elif spec[4] then 
        XRpos := [ 1..Length( genXrng ) ]; 
        YRpos := [ ]; 
    else 
        XRpos := [ 1..Length( genXrng ) ];
        YRpos := [ 1+Length( genXrng ) .. lenR ];
    fi;
    ##  now for the boundary 
    Xbdy := Boundary( X1 );
    Ybdy := Boundary( Y1 );
    Xact := XModAction( X1 );
    Yact := XModAction( Y1 );
    imXbdy := List( genS{ XSpos },
        s -> ImageElm( eXR, ImageElm( Xbdy, ImageElm( pXS, s ) ) ) );
    imYbdy := List( genS{ YSpos },
        s -> ImageElm( eYR, ImageElm( Ybdy, ImageElm( pYS, s ) ) ) );
    imbdy := Concatenation( imXbdy, imYbdy );
    bdy := GroupHomomorphismByImages( S, R, genS, imbdy );
    autgen := 0 * [ 1..lenR ];
    for i in XRpos do
        a := ImageElm( Xact, genXrng[i] );
        imaut := 0 * Spos;
        for j in YSpos do
            imaut[j] := genS[j];
        od;
        for j in XSpos do
            imaut[j] := ImageElm( eXS, ImageElm( a, ImageElm(pXS,genS[j] ) ) );
        od;
        alpha := GroupHomomorphismByImages( S, S, genS, imaut );
        autgen[i] := alpha;
    od; 
    if spec[2] then 
        k := 0; 
    else 
        k := Length( genXrng );
    fi; 
    for i in YRpos do
        a := ImageElm( Yact, genYrng[i-k] );
        imaut := 0 * Spos;
        for j in XSpos do
            imaut[j] := genS[j];
        od;
        for j in YSpos do
            imaut[j] := ImageElm( eYS, ImageElm( a, ImageElm(pYS,genS[j] ) ) );
        od;
        alpha := GroupHomomorphismByImages( S, S, genS, imaut );
        autgen[i] := alpha;
    od;
    aut := Group( autgen );
    act := GroupHomomorphismByImages( R, aut, genR, autgen );
    XY := PreXModByBoundaryAndAction( bdy, act );
    if ( IsXMod( X1 ) and IsXMod( Y1 ) ) then
        SetIsXMod( XY, true );
    fi;
    if ( HasName( X1 ) and HasName( Y1 ) ) then
        SetName( XY, Concatenation( Name( X1 ), "x", Name( Y1 ) ) );
    elif ( HasName( Source(XY) ) and HasName( Range(XY) ) ) then 
        SetName( XY, Concatenation( "[", Name( Source(XY ) ), 
                     "->", Name( Range(XY) ), "]" ) ); 
    fi;
    info := DirectProductInfo( XY );
    info!.objects := [ X1, Y1 ];
    return XY;
end );

##############################################################################
##
#M  Embedding . . . . for direct products of (pre-)xmods and (pre-)cat1-groups
##
InstallOtherMethod( Embedding, "generic method for (pre-)xmods & (pre-)cat1s",
    true, [ Is2DimensionalGroup, IsPosInt ], 0,
function( D, i )
    local info, eS, eR, obj, mor;

    info := DirectProductInfo( D );
    if IsBound( info!.embeddings[i] ) then
        return info!.embeddings[i];
    fi;
    eS := Embedding( Source( D ), i );
    eR := Embedding( Range( D ), i );
    Info( InfoXMod, 3, "SourceEmbedding: ", eS );
    Info( InfoXMod, 3, " RangeEmbedding: ", eR );
    obj := info!.objects[i]; 
    if IsPreXMod( D ) then
        mor := PreXModMorphism( obj, D, eS, eR );
    elif IsPreCat1Group( D ) then
        mor := PreCat1GroupMorphism( obj, D, eS, eR );
    else
        mor := fail;
    fi;
    if not ( mor = fail ) then
        SetIsInjective( mor, true );
        info!.embeddings[i] := mor;
    fi;
    return mor;
end );

##############################################################################
##
#M  Projection . . .  for direct products of (pre-)xmods and (pre-)cat1-groups
##
InstallOtherMethod( Projection, "generic method for (pre-)xmods & (pre-)cat1s",
    true, [ Is2DimensionalGroup, IsPosInt ], 0,
function( D, i )
    local G, info, pS, pR, mor;

    G := Source( D ); 
    if HasDirectProductInfo( G ) then 
        info := DirectProductInfo( D ); 
        if not ( i in [1,2] ) then 
            Error( "only two projections available" ); 
        fi; 
    else 
        info := SemidirectProductInfo( G ); 
        if not ( i = 1 ) then 
            Error( "only the first projection is available" ); 
        fi; 
    fi; 
    if IsBound( info!.projections[i] ) then
        return info!.projections[i];
    fi;
    pS := Projection( G, i );
    pR := Projection( Range( D ), i );
    if IsPreXMod( D ) then
        mor := PreXModMorphism( info!.objects[i], D, pS, pR );
    elif IsPreCat1Group( D ) then
        mor := PreCat1GroupMorphism( info!.objects[i], D, pS, pR );
    else
        mor := fail;
    fi;
    if not ( mor = fail ) then
        SetIsInjective( mor, true );
        info!.projections[i] := mor;
    fi;
    return mor;
end );

##############################################################################
##
#M  TrivialSub2DimensionalGroup . . . . . . . . . .  of a 2d-object
#M  TrivialSubPreXMod  . . . . . . . . . . . . . . . of a pre-crossed module
#M  TrivialSubXMod     . . . . . . . . . . . . . . . of a crossed module
#M  TrivialSubPreCat1Group . . . . . . . . . . . . . of a pre-cat1-group
#M  TrivialSubCat1Group  . . . . . . . . . . . . . . of a cat1-group
##
InstallMethod( TrivialSub2DimensionalGroup, "of a 2d-object", true, 
    [ Is2DimensionalGroup ], 0,
function( obj )

    local idsrc, idrng;

    idsrc := TrivialSubgroup( Source( obj ) );
    idrng := TrivialSubgroup( Range( obj ) );
    if IsPreXMod( obj ) then
        return SubPreXMod( obj, idsrc, idrng );
    elif IsPreCat1Group( obj ) then
        return SubPreCat1Group( obj, idsrc );
    else
        Error( "<obj> must be a pre-crossed module or a pre-cat1-group" );
    fi;
end );

InstallMethod( TrivialSubPreXMod, "of a pre-crossed module", true,
    [ IsPreXMod ], 0,
function( obj )
    return TrivialSub2DimensionalGroup( obj );
end );

InstallMethod( TrivialSubXMod, "of a crossed module", true, [ IsXMod ], 0,
function( obj )
    return TrivialSub2DimensionalGroup( obj );
end );

InstallMethod( TrivialSubPreCat1Group, "of a pre-cat1-group", true, 
    [ IsPreCat1Group ], 0,
function( obj )
    return TrivialSub2DimensionalGroup( obj );
end );

InstallMethod( TrivialSubCat1Group, "of a cat1-group", true, [ IsCat1Group ], 0,
function( obj )
    return TrivialSub2DimensionalGroup( obj );
end );

##############################################################################
##
#M  IsNormalSubgroup2DimensionalGroup . . . . . . . . for 2Dimensional-objects
##
InstallMethod( IsNormalSubgroup2DimensionalGroup, 
    "for crossed modules and cat1-groups", [ Is2DimensionalGroup ], 0,
function( obj )
    local src, rng, gensrc, genrng;
    src := Source( obj );
    rng := Range( obj );
    gensrc := GeneratorsOfGroup( src );
    if IsXMod( obj ) then
        return ( IsNormal(rng,src) and
                 ( gensrc = List( gensrc, s -> ImageElm( Boundary(obj), s ) ) ) );
    elif IsCat1Group( obj ) then
        return IsNormalSubgroup2DimensionalGroup( XModOfCat1Group( obj ) );
    else
        Error( "method not yet implemented" );
    fi;
end );

##############################################################################
##
#M  IsNormal . . . . . . . . . . . . . . . . . . . .  for 2Dimensional-objects
##
InstallOtherMethod( IsNormal, "for precrossed modules", IsIdenticalObj,
    [ IsPreXMod, IsPreXMod ], 0,
function( XM, SM )

    local xr, a, ss, im, xs, sr, Ssrc, Xact, snat, rnat;

    if not IsSubPreXMod( XM, SM ) then
        return false;
    fi;
    Ssrc := Source( SM );
    Xact := XModAction( XM );
    for xr in GeneratorsOfGroup( Range( XM ) ) do
        a := ImageElm( Xact, xr );
        for ss in GeneratorsOfGroup( Ssrc ) do
            im := ImageElm( a, ss );
            if not ( im in Ssrc ) then
                Info( InfoXMod, 2, "ss,xr,ss^xr = ", [ss,xr,im] );
                return false;
            fi;
        od;
    od;
    for sr in GeneratorsOfGroup( Range( SM ) ) do
        a := ImageElm( Xact, sr );
        for xs in GeneratorsOfGroup( Source( XM ) ) do
            im := xs^(-1) * ImageElm( a, xs );
            if not ( im in Ssrc ) then
                Info( InfoXMod, 3, "sr,xs,sr^(-1)*xs^sr = ", [sr,xs,im] );
                return false;
            fi;
        od;
    od; 
    return true;
end );

##############################################################################
##
#M  NormalSubXMods  .  . . . . . . . . . . . . . . . . .  for a crossed module
##
InstallMethod( NormalSubXMods, "for a crossed module", true, [ IsXMod ], 0,
function( XM )

    local Xsrc, Xrng, YM, i, j, slen, rlen, norm, normsrc, normrng, ok;

    Xsrc := Source( XM );
    Xrng := Range( XM );
    norm := [ ];
    normsrc := NormalSubgroups( Xsrc );
    normrng := NormalSubgroups( Xrng );
    slen := Length( normsrc );
    rlen := Length( normrng );
    for i in [ 1..slen ] do
        for j in [ 1..rlen ] do
            if ( ( i = 1 ) and ( j = 1 ) ) then
                YM := TrivialSubXMod( XM );
            elif ( ( i = slen ) and ( j = rlen ) ) then
                YM := XM;
            else
                YM := SubXMod( XM, normsrc[i], normrng[j] );
            fi;
            ok := not ( YM = fail );
            if ( ok and IsXMod( YM ) and IsNormal( XM, YM ) ) then   
                Add( norm, YM );
            fi;
        od;
    od;
    return norm;
end );

##############################################################################
##
#M  InitCatnGroupRecords . . . . . . . . . . . . . . . . . . . . . for a group 
##
InstallMethod( InitCatnGroupRecords, "for a group", true, [ IsGroup ], 0,
function( G )

    if not HasCatnGroupNumbers( G ) then 
        SetCatnGroupNumbers( G, rec() ); 
    fi; 
    if not HasCatnGroupNumbers( G ) then 
        Error( "CatnGroupNumbers not set" ); 
    fi; 
    if not HasCatnGroupLists( G ) then 
        SetCatnGroupLists( G, rec() ); 
        CatnGroupLists( G ).omit := false; 
    fi; 
    if not HasCatnGroupLists( G ) then 
        Error( "CatnGroupLists not set" ); 
    fi; 
end ); 
