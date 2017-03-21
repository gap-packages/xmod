#############################################################################
##
#W  gp2obj.gi                 GAP4 package `XMod'               Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
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

#############################################################################
##
#M  IsPerm2dGroup . . . . . . check whether source and range are perm groups
#M  IsFp2dGroup . . . . . . . check whether source and range are fp groups
#M  IsPc2dGroup . . . . . . . check whether source and range are pc groups
##
InstallMethod( IsPerm2dGroup, "generic method for 2d-group objects",
    true, [ Is2dGroup ], 0,
function( obj )
    return ( IsPermGroup( Source( obj ) ) and IsPermGroup( Range( obj ) ) );
end );

InstallMethod( IsFp2dGroup, "generic method for 2d-group objects",
    true, [ Is2dGroup ], 0,
function( obj )
    return ( IsFpGroup( Source( obj ) ) and IsFpGroup( Range( obj ) ) );
end );

InstallMethod( IsPc2dGroup, "generic method for 2d-group objects",
    true, [ Is2dGroup ], 0,
function( obj )
    return ( IsPcGroup( Source( obj ) ) and IsPcGroup( Range( obj ) ) );
end );

#############################################################################
##
#M  IsPreXMod    check that the first crossed module axiom holds
##
InstallMethod( IsPreXMod, "generic method for 2d-group",
    true, [ Is2dGroup ], 0,
function( P )

    local  Xsrc, Xrng, hom, a, aut, act, gensrc, ngsrc, genrng, ngrng, 
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
    hom := Boundary( P );
    # Check  P.boundary: P.source -> P.range
    if not ( ( Source( hom ) = Xsrc ) and ( Range( hom ) = Xrng ) ) then
        Info( InfoXMod, 2,
              "Error: require  X.boundary : X.source -> X.range" );
        return false;
    fi;
    # checking  IsHomomorphism(hom) gives cokernel error when Xsrc = [ ]
    if ( ssrc > 1 ) then
        if not IsGroupHomomorphism( hom ) then
            Info( InfoXMod, 2,
                  "Error:  the boundary map is NOT a homomorphism!" );
            return false;
        fi;
    fi;
    aut := AutoGroup( P );
    # Check  X.aut  is a group of automorphisms  X.source -> X.source
    if not ( IsGroup( aut ) ) then
        Info( InfoXMod, 2,
              "Error: group of actions on X.source does not exist!" );
        return false;
    fi;
    if ( aut = Group( IdentityMapping( Xsrc ) ) ) then
       SetIsTrivialAction2dGroup( P, true );
    else
        a := GeneratorsOfGroup( aut )[1];
        if not ( ( Source( a ) = Xsrc ) and ( Range( a ) = Xsrc ) 
                                        and IsBijective( a )  ) then
            Info( InfoXMod, 2,
                  "Require automorphism  X.aut.1  on  X.source" );
            return false;
        fi;
    fi;
    act := XModAction( P );
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
          "Checking  CM1) hom(x2^x1) = (hom(x2))^x1 " );
    for x1 in genrng do
        for x2 in gensrc do
            # Print( "x1,x2 = ", x1, ",  ", x2, "\n" );
            y1 := ( x2 ^ ( x1^act ) ) ^ hom;
            z1 := ( x2 ^ hom ) ^ x1;
            if ( y1 <> z1 ) then
                Info( InfoXMod, 3,
                    "CM1) fails at  x1 = ", x1, ",  x2 = ", x2, "\n",
                    "  hom(x2^x1) = ", y1, "\n", "(hom(x2))^x1 = ", z1 );
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
InstallOtherMethod( Size, "generic method for a 2d-object", [ Is2dDomain ], 20,
function ( obj )
    return [ Size( Source( obj ) ), Size( Range( obj ) ) ];
end );

#############################################################################
##
#M  IsTrivialAction2dGroup  . . . . . . . . . check whether action is trivial
##
InstallMethod( IsTrivialAction2dGroup,
    "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local  act, genrng, onesrc;

    act := XModAction( PM );
    genrng := GeneratorsOfGroup( Range( PM ) );
    onesrc := IdentityMapping( Source( PM ) );
    return ForAll( genrng, r -> ( Image( act, r ) = onesrc ) );
end );

##############################################################################
##
#M  PreXModObj( <bdy>, <act> ) . . . . . . . . . . . make a pre-crossed module
##
InstallMethod( PreXModObj, "for homomorphism and action", true,
    [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( bdy, act )

    local  filter, fam, PM, ok, src, rng, aut, name;

    fam := Family2dGroup;
    filter := IsPreXModObj;
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
        filter := filter and IsPerm2dGroup; 
    elif ( IsPcGroup( src ) and IsPcGroup( rng ) ) then 
        filter := filter and IsPc2dGroup; 
    fi;
    PM := rec();
    ObjectifyWithAttributes( PM, 
      NewType( fam, filter ),
      Source, src,
      Range, rng,
      Boundary, bdy,
      AutoGroup, aut,
      XModAction, act,
      IsPreXModDomain, true, 
      Is2dGroup, true );
    if not IsPreXMod( PM ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed module." );
    else 
        ok := IsXMod( PM ); # for running properly the function AllXMods 
    fi;
    # name := Name( PM );
    ## check the types 
    if ( IsPermGroup(src) and IsPermGroup(rng) ) then 
        SetIsPerm2dGroup( PM, true ); 
    elif ( IsPcGroup(src) and IsPcGroup(rng) ) then 
        SetIsPc2dGroup( PM, true ); 
    fi;
    return PM;
end );

#############################################################################
##
#M  ExternalSetXMod( <pxm> ) . . . . . . . source group as a range group set
##
InstallMethod( ExternalSetXMod, "method for a precrossed module", true, 
    [ IsPreXMod ], 0,
function( PM ) 

    local  rng, genR, act;

    rng := Range( PM  ); 
    genR := GeneratorsOfGroup( rng ); 
    act := XModAction( PM );
    return ExternalSet( rng, Source(PM), genR, 
               List( genR, g -> Image(act,g) ) ); 
end ); 

#############################################################################
##
#M  ViewObj( <g2d> ) . . . . . . . . . . . . . . . . . . . . view a 2d-domain 
##
InstallMethod( ViewObj, "method for a 2d domain", true, [ Is2dDomain ], 0,
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

#############################################################################
##
#M  PrintObj( <g2d> )  . . . . . . . . . . . . . . . . . . . view a 2d-domain 
##
InstallMethod( PrintObj, "method for a 2d-domain", true, [ Is2dDomain ], 0,
    function( g2d )
    if HasName( g2d ) then
        Print( Name( g2d ), "\n" );
    elif ( HasIsPreXModDomain( g2d ) and IsPreXModDomain( g2d ) ) then 
        Print( "[", Source( g2d ) ); 
        if IsGroupoid( Source( g2d ) ) then 
            Print( "\n->  " ); 
        else 
            Print( "->" ); 
        fi; 
        Print( Range( g2d ), "]" ); 
    elif ( HasIsPreCat1Domain( g2d ) and IsPreCat1Domain( g2d ) ) then 
        Print( "[", Source( g2d ) ); 
        if IsGroupoid( Source( g2d ) ) then 
            Print( "\n=>  " ); 
        else 
            Print( "=>" ); 
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
InstallMethod( Display, "method for a 2d-group", true, [ Is2dGroup ], 20,
function( g2d )

    local  name, bdy, act, aut, len, i, ispar, src, rng, 
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
    else 
        if ( HasIsCat1( g2d ) and IsCat1( g2d ) ) then 
            Print( "\nCat1-group " );
        else
            Print( "\nPre-cat1-group " ); 
        fi; 
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
        Print( "  ",  List( gensrc, s -> Image( bdy, s ) ), "\n" );
        act := XModAction( g2d );
        imact := List( genrng, r -> Image( act, r ) );
        aut := AutoGroup( g2d );
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
                Print( List( gensrc, s -> Image( imact[i], s ) ), " }\n" );
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
        imt := List( gensrc, x -> Image( t, x ) );
        imh := List( gensrc, x -> Image( h, x ) );
        ime := List( genrng, x -> Image( e, x ) );
        imb := List( genker, x -> Image( b, x ) );
        imk := List( genker, x -> Image( k, x ) );
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
    if ( HasIsXMod( g2d ) and IsXMod( g2d ) and HasCat1OfXMod( g2d ) ) then
        Print( ": associated cat1-group is ", Cat1OfXMod( g2d ), "\n" );
    elif ( HasIsCat1( g2d ) and IsCat1( g2d ) and HasXModOfCat1( g2d ) ) then
        Print( ": associated crossed module is ", XModOfCat1( g2d ), "\n" );
    fi;
    Print( "\n" );
end ); 

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . . . . . . . . . for a 2d-domain
#M  StructureDescription  . . . . . . . . . . . . . . . . . . for a 2d-domain
##
InstallOtherMethod( IdGroup, "method for a 2d-domain", true, [ Is2dDomain ], 0,
function( dom )
    return [ IdGroup( Source(dom) ), IdGroup( Range(dom) ) ]; 
end ); 

InstallOtherMethod( StructureDescription, "method for a 2d-domain", true, 
    [ Is2dDomain ], 0,
function( dom )
    return [ StructureDescription( Source(dom) ), 
             StructureDescription( Range(dom) ) ]; 
end ); 

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . . . . . . .  for a 2d-domain
##
InstallMethod( Name, "method for a 2d-domain", true, [ Is2dDomain ], 0,
function( dom )

    local  nsrc, nrng, name, arrow;

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

    local  rng, src, genrng, gensrc, aut, genaut, imact, i, a0, ima, a;

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
    imact := List( genrng, r -> Image( act, r ) );
    for i in [ 1..Length( imact ) ] do
        a0 := imact[i];
        ima := List( gensrc, s -> Image( a0, s ) );
        a := GroupHomomorphismByImages( src, src, gensrc, ima );
        imact[i] := a;
    od;
    return PreXModObj( bdy, act ); 
end );

#############################################################################
##
#M  IsPreCat1    check that the first pre-cat1-group axiom holds
##
InstallMethod( IsPreCat1, "generic method for 2d-group", true, 
    [ Is2dGroup ], 0,
function( C1G )

    local  Csrc, Crng, x, e, t, h, idrng, he, te, kert, kerh, kerth;

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
    IsIdenticalObj, [ IsPreCat1, IsPreCat1 ], 0,
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

    local  filter, fam, C1G, ok, src, rng, name;

    fam := Family2dGroup;
    filter := IsPreCat1Obj;
    if not ( ( Source(h) = Source(t) ) and ( Range(h) = Range(t) ) ) then
        Error( "tail & head must have same source and range" );
    fi;
    if not ( ( Source(e) = Range(t) ) and ( Range(e) = Source(t) ) ) then
        Error( "tail, embedding must have opposite source and range" );
    fi;
    C1G := rec();
    ObjectifyWithAttributes( C1G, NewType( fam, filter ),
      Source, Source( t ),
      Range, Range( t ),
      TailMap, t,
      HeadMap, h,
      RangeEmbedding, e, 
      IsPreCat1Domain, true, 
      Is2dGroup, true );
    ok := IsPreCat1( C1G );
    # name := Name( C1G );
    if not ok then
        Error( "not a pre-cat1-group" );
    fi;
    ok := IsEndomorphismPreCat1( C1G ); 
    return C1G;
end );

##############################################################################
##
#M  Elements( <P> )  . . . . . . . . . . . . elements for a pre-crossed module
##
##  replaced by Enumerator ???

#############################################################################
##
#M  Reverse                                              for a pre-cat1-group
##
InstallMethod( Reverse, "method for a cat1-group", true, [ IsPreCat1 ], 0,
function( C1G )
    local rev;
    rev := PreCat1( HeadMap(C1G), TailMap(C1G), RangeEmbedding(C1G ) );
    SetReverse( rev, C1G );
    return rev;
end );

#############################################################################
##
#F  PreCat1( <t>, <h>, <e> )      pre-cat1-group from given tail, head, embed
#F  PreCat1( <t>, <h> )           pre-cat1-group from tail, head endomorphisms
##
InstallGlobalFunction( PreCat1, function( arg )

    local  nargs, C1G;

    nargs := Length( arg );
    # two endomorphisms
    if ( ( nargs=2 ) and IsEndoMapping( arg[1] )
                     and IsEndoMapping( arg[2] ) ) then
        return PreCat1ByEndomorphisms( arg[1], arg[2] );

    # two homomorphisms and an embedding
    elif ( ( nargs=3 ) and
           ForAll( arg, a -> IsGroupHomomorphism( a ) ) ) then
        return PreCat1ByTailHeadEmbedding( arg[1], arg[2], arg[3] );
    fi;
    # alternatives not allowed
    Error( "standard usage: PreCat1( tail, head [,embed] );" );
end );

#############################################################################
##
#M  PreCat1ByPreXMod . . . . convert a pre-crossed module to a pre-cat1-group
##
InstallMethod( PreCat1ByPreXMod,
    "convert a pre-crossed module to a pre-cat1-group", true, [ IsPreXMod ], 0,
function( XM )

    local  Xsrc, Xrng, Xact, Xbdy, gensrc, genrng, one, imbdy, info, G, genG,
           t, h, f, eR, eS, imeR, imeS, projS, imt, imh, ime, imf, C;

    if not ( IsPermPreXMod( XM ) or IsPcPreXMod( XM ) ) then
        Print( "#W: should be a perm-xmod or a pc-xmod\n" );
        return fail;
    fi;
    Xsrc := Source( XM );
    gensrc := GeneratorsOfGroup( Xsrc );
    Xrng := Range( XM );
    genrng := GeneratorsOfGroup( Xrng );
    one := One( Xrng );
    Xact := XModAction( XM );
    Xbdy := Boundary( XM );
    if IsTrivialAction2dGroup( XM ) then
        Info( InfoXMod, 2, "Using direct product: ", Xrng, " x ", Xsrc );
        G := DirectProduct( Xrng, Xsrc );
        info := DirectProductInfo( G );
        if ( HasName( Xsrc ) and HasName( Xrng ) ) then
            SetName( G, Concatenation( Name( Xrng ), Name( Xsrc ) ) );
        fi;
        genG := GeneratorsOfGroup( G );
        gensrc := GeneratorsOfGroup( Xsrc );
        genrng := GeneratorsOfGroup( Xrng );
        imbdy := List( gensrc, s -> Image( Xbdy, s ) );
        imt := Concatenation( genrng, List( gensrc, s -> one ) );
        imh := Concatenation( genrng, imbdy );
        t := GroupHomomorphismByImages( G, Xrng, genG, imt );
        h := GroupHomomorphismByImages( G, Xrng, genG, imh );
        eR := Embedding( G, 1 );
        eR := AsGroupGeneralMappingByImages( eR );
    else
        Info( InfoXMod, 2, "Using semidirect product: ", Xrng, " |X ", Xsrc );
        G := SemidirectProduct( Xrng, Xact, Xsrc );
        info := SemidirectProductInfo( G );
        if ( HasName( Xsrc ) and HasName( Xrng ) ) then
             SetName( G, Concatenation ( Name(Xrng), " |X ", Name(Xsrc) ) );
        else
             SetName( G, "..|X.." );
        fi;
        genG := GeneratorsOfGroup( G );
        eR := Embedding( G, 1 );
        imeR := List( genrng, r -> Image( eR, r ) );
        eS := Embedding( G, 2 );
        imeS := List( gensrc, s -> Image( eS, s ) );
        t := Projection( G );
        imt := List( genG, g -> Image( t, g ) );
        projS := List( imt, r -> Image( eR, r^-1 ) );
        projS := List( [ 1..Length( genG ) ], i -> projS[i] * genG[i] );
        projS := List( projS, x -> PreImagesRepresentative( eS, x ) );
        imh := List( [ 1..Length( genG ) ],
            i -> imt[i] * Image( Xbdy, projS[i] ) );
        h := GroupHomomorphismByImages( G, Xrng, genG, imh );
    fi;
    SetSourceEmbedding( XM, eR );
    C := PreCat1ByTailHeadEmbedding( t, h, eR );
    return C;
end ); 

#############################################################################
##
#M  IsXMod                   check that the second crossed module axiom holds
##
InstallMethod( IsXMod, "generic method for pre-crossed modules",
    true, [ IsPreXMod ], 0,
function( XM )

    local  gensrc, genrng, x2, y2, w2, z2, hom, act;

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

    local  PM;

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
InstallMethod( XModByTrivialAction, "crossed module with trivial action", true,
    [ IsGroupHomomorphism ], 0,
function( f )
    local  R, ZR, S, XM, aut, act, name;
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
    SetIsTrivialAction2dGroup( XM, true );
    return XM;
end );

##############################################################################
##
#F  XModByNormalSubgroup            create a crossed module from normal N in G
##
InstallMethod( XModByNormalSubgroup, "conjugation crossed module",
    true, [ IsGroup, IsGroup ], 0,
function( G, N )

    local  XM, bdy, act, aut, genrng, gensrc, name, a, triv, idsrc,
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
    SetIsNormalSubgroup2dGroup( XM, true );
    if ( Length( autgen ) = 0 ) then
        SetIsTrivialAction2dGroup( XM, true );
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

    local  rng, src, Zsrc, ker, gensrc, ngsrc, imhom, genrng, autgen, 
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
    imhom := List( gensrc, s -> Image( hom, s ) );
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
    SetIsCentralExtension2dGroup( XM, true );
    idsrc := IdentityMapping( src );
    if ForAll( autgen, a -> ( a = idsrc ) ) then
        SetIsTrivialAction2dGroup( XM, true );
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

    local  aut, act, z;
    act := AbelianModuleAction( abmod );
    z := MappingToOne( AbelianModuleGroup( abmod ), Source( act ) );
    return XModByBoundaryAndAction( z, act );
end );

#############################################################################
##
#M  XModByGroupOfAutomorphisms                       crossed module  [G -> A]
##
InstallMethod( XModByGroupOfAutomorphisms, "automorphism crossed module",
    true, [ IsGroup, IsGroupOfAutomorphisms ], 0,
function( G, A )

    local  genA, ispc, a2p, p2a, a, ima, genG, oneP, P, genP, bdy, img, imbdy,
           g, idG, abelian, XM;

    ispc := IsPcGroup( G ); 
    if ispc then 
        a2p := IsomorphismPcGroup( A ); 
        ispc := not ( a2p = fail ); 
    else 
        a2p := IsomorphismSmallPermGroup( A );
    fi; 
    P := ImagesSource( a2p );
    if ( not HasName( P ) and HasName( A ) ) then
        SetName( P, Concatenation( "P", Name( A ) ) );
    fi;
    genA := GeneratorsOfGroup( A ); 
    genP := List( genA, a -> Image( a2p, a ) ); 
    p2a := GroupHomomorphismByImages( P, A, genP, genA );
    abelian := IsAbelian( G );
    genG := GeneratorsOfGroup( G );
    oneP := One( P );
    if abelian then
        imbdy := List( genG, g -> oneP );
    else
        imbdy := [ ];
        for g in genG do
            ima := List( genG, h -> h^g );
            a := GroupHomomorphismByImages( G, G, genG, ima );
            img := Image( a2p, a );
            Add( imbdy, img );
        od;
    fi;  
    bdy := GroupHomomorphismByImages( G, P, genG, imbdy );
    XM := PreXModObj( bdy, p2a );
    SetIsAutomorphismGroup2dGroup( XM, true );
    if not IsXMod( XM ) then
        Error( "this boundary and action only defines a pre-crossed module" );
    fi;
    return XM;
end );

#############################################################################
##
#F  XModByAutomorphismGroup( <G> )               crossed module [G -> Aut(G)]
#F  XModByAutomorphismGroup( <G>, <A> )          crossed module [G -> A]
##
InstallGlobalFunction( XModByAutomorphismGroup, function( arg )

    local  nargs, G, A, innG, a;

    nargs := Length( arg );
    # A not specified
    if ( ( nargs = 1 ) and IsGroup( arg[1] ) ) then
        G := arg[1];
        A := AutomorphismGroup( G );
        if ( not HasName( A ) and HasName( G ) ) then
            SetName( A, Concatenation( "Aut(", Name( G ), ")" ) );
        fi;
    elif ( ( nargs = 2 ) and IsGroupOfAutomorphisms( arg[2] ) ) then
        G := arg[1];
        A := arg[2];
        innG := InnerAutomorphismsByNormalSubgroup( G, G );
        for a in GeneratorsOfGroup( innG ) do
            if not ( a in A ) then
                Error( "arg[2] must include all inner automorphisms.\n" );
            fi;
        od;
    else
        # alternatives not allowed
        Print( "usage: XModByAutomorphismGroup( G );\n" ); 
        Print( "   or: XModByAutomorphismGroup( G, A );\n" );
        return fail;
    fi;
    SetIsGroupOfAutomorphisms( A, true );
    return XModByGroupOfAutomorphisms( G, A );
end );

#############################################################################
##
#M  XModByInnerAutomorphismGroup( <G> )          crossed module [G -> Inn(G)]
##
InstallMethod( XModByInnerAutomorphismGroup, "inner automorphism xmod",
    true, [ IsGroup ], 0,
function( G )
    local  A, innG, a;

    A := InnerAutomorphismsByNormalSubgroup( G, G );
    if ( not HasName( A ) and HasName( G ) ) then
        SetName( A, Concatenation( "Inn(", Name( G ), ")" ) );
    fi;
    SetIsGroupOfAutomorphisms( A, true );
    return XModByGroupOfAutomorphisms( G, A );
end );

##############################################################################
##
#M  XModByCat1
#M  XModOfCat1
##
InstallMethod( XModByCat1, "generic method for cat1-groups",
    true, [ IsCat1 ], 0,
function( C1 )

    local  X1;
    X1 := PreXModByPreCat1( C1 );
    SetIsXMod( X1, true );
    SetXModOfCat1( C1, X1 );
    SetCat1OfXMod( X1, C1 );
    return X1;
end );

InstallMethod( XModOfCat1, "generic method for cat1-groups",
    true, [ IsCat1 ], 0, XModByCat1 );

##############################################################################
##
#M  Cat1ByXMod
#M  Cat1OfXMod
##
InstallMethod( Cat1ByXMod, "generic method for crossed modules",
    true, [ IsXMod ], 0,
function( X1 )

    local  C1;
    C1 := PreCat1ByPreXMod( X1 );
    SetIsCat1( C1, true );
    SetXModOfCat1( C1, X1 );
    SetCat1OfXMod( X1, C1 );
    return C1;
end );

InstallMethod( Cat1OfXMod, "generic method for cat1-groups",
    true, [ IsXMod ], 0, Cat1ByXMod );

##############################################################################
##
#M  PeifferSubgroupPreXMod . . . . . normally generated by Peiffer commutators
##
InstallMethod( PeifferSubgroupPreXMod, "generic method for pre-crossed xmods",
               true, [ IsPreXMod ], 0,
function( PM )

    local  Pf, s1, s2, a1, src, gensrc, comm, bdy, act;

    # this code mimics that of DerivedSubgroup
    src := Source( PM );
    bdy := Boundary( PM );
    act := XModAction( PM );
    gensrc := GeneratorsOfGroup( src );
    Pf := TrivialSubgroup( src );
    for s1 in gensrc do
        a1 := Image( act, Image( bdy, s1 ) );
        for s2 in gensrc do
            comm := (s2^-1)^s1 * Image( a1, s2 );
            if not ( comm in Pf ) then
                Pf := ClosureSubgroup( Pf, comm );
            fi;
        od;
    od;
    Pf := NormalClosure( src, Pf );
    if ( Pf = src ) then
        Pf := src;
    fi;
    return Pf;
end );

##############################################################################
##
#M  PeifferSubgroupPreCat1 . . . . . .  commutator of kernels of tail and head
##
InstallMethod( PeifferSubgroupPreCat1, "generic method for pre-cat1-groups",
               true, [ IsPreCat1 ], 0,
function( PCG )

    local  src, kerh, kert, Pf;

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
               true, [ Is2dGroup ], 0,
function( obj )
    if IsPreXModObj( obj ) then
        if IsXMod( obj ) then
            return Subgroup( Source( obj ), [ One( Source( obj ) ) ] );
        else
            return PeifferSubgroupPreXMod( obj );
        fi;
    elif IsPreCat1Obj( obj ) then
        if IsCat1( obj ) then
            return Subgroup( Source( obj ), [ One( Source( obj ) ) ] );
        else
            return PeifferSubgroupPreCat1( obj );
        fi;
    else
        return fail;
    fi;
end );

##############################################################################
##
#F  XModByPeifferQuotient               xmod from prexmod and Peiffer subgroup
##
InstallMethod( XModByPeifferQuotient, 
    "crossed module from a pre-crossed module and Peiffer subgroup", true,
    [ IsPreXMod ], 0,
function( PM )

    local  XM, PMrng, genrng, PMsrc, gensrc, PMbdy, PMact, pf, name, quot, a,
           qgen, pqgen, nat, impqgen, autgen, imautgen, phi, bdy, aut, act, r;
    
    if IsXMod( PM ) then
        Info( InfoXMod, 1, "this object is already a crossed module!" );
        return PM;
    fi;
    PMrng := Range( PM ) ;
    genrng := GeneratorsOfGroup( PMrng );
    PMsrc := Source( PM );
    gensrc := GeneratorsOfGroup( PMsrc );
    PMbdy := Boundary( PM );
    # construct the quotient
    pf := PeifferSubgroup( PM );
    if not IsNormal( PMsrc, pf ) then
        Error( "Peiffer subgroup not normal in source group" );
    fi;
    nat := NaturalHomomorphismByNormalSubgroup( PMsrc, pf );
    quot := Image( nat );
    qgen := GeneratorsOfGroup( quot );
    if HasName( Source( PM ) ) then
        name := Name( Source( PM ) );
        if HasName( pf ) then
            SetName( quot, Concatenation( name, "/", Name( pf ) ) );
        else
            SetName( quot, Concatenation( name, "/P" ) );
        fi;
    fi;
    # construct the boundary
    pqgen := List( qgen, x -> PreImagesRepresentative( nat, x ) );
    impqgen := List( pqgen, x -> Image( PMbdy, x ) );
    bdy := GroupHomomorphismByImages( quot, PMrng, qgen, impqgen );
    # construct the action
    PMact := XModAction( PM );
    imautgen := [ ];
    for r in genrng do
        a := Image( PMact, r );
        autgen := List( pqgen, p -> Image( nat, Image( a, p ) ) ); 
        phi := GroupHomomorphismByImages( quot, quot, qgen, autgen );
        Add( imautgen, phi );
    od;
    aut := Group( imautgen );
    act := GroupHomomorphismByImages( PMrng, aut, genrng, imautgen );
    XM := XModByBoundaryAndAction( bdy, act );
    if not IsXMod( XM ) then
        Error( "fails to be a crossed module" );
    fi;
    return XM;
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

    local  nargs;
    nargs := Length( arg );

    # two homomorphisms
    if ( ( nargs = 2 ) and IsGroupHomomorphism( arg[1] )
                       and IsGroupHomomorphism( arg[2] ) ) then
        return XModByBoundaryAndAction( arg[1], arg[2] );

    # group and normal subgroup
    elif ( ( nargs = 2 ) and IsGroup( arg[1] ) and IsGroup( arg[2] )
      and IsSubgroup( arg[1], arg[2] ) and IsNormal( arg[1], arg[2] ) ) then
        return XModByNormalSubgroup( arg[1], arg[2] );

    # surjective homomorphism
    elif ( ( nargs = 1 ) and IsGroupHomomorphism( arg[1] )
                         and IsSurjective( arg[1] ) ) then
        return XModByCentralExtension( arg[1] );

    # convert a cat1-group
    elif ( ( nargs = 1 ) and HasIsCat1( arg[1] ) and IsCat1( arg[1] ) ) then
        return PreXModByPreCat1( arg[1] );

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
    [ Is2dGroup, Is2dGroup ], 0,
function( PM, SM )

    local  ok, Ssrc, Srng, gensrc, genrng, s, r, r1, r2, im1, im2;

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
        if ( Image( Boundary( PM ), s ) <> Image( Boundary( SM ), s ) ) then
            ok := false;
        fi;
    od;
    if not ok then
        Info( InfoXMod, 3, "boundary maps have different images" );
        return false;
    fi;
    for r in genrng do
        r1 := Image( XModAction( PM ), r );
        r2 := Image( XModAction( SM ), r );
        for s in gensrc do
            im1 := Image( r1, s );
            im2 := Image( r2, s );
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
    [ Is2dGroup, Is2dGroup ], 0,
function( XM, SM )

    if not ( IsXMod( XM ) and IsXMod( SM ) ) then
        return false;
    fi;
    return IsSubPreXMod( XM, SM );
end );

##############################################################################
##
#M  IsSubPreCat1
##
InstallMethod( IsSubPreCat1, "generic method for pre-cat1-groups", true,
    [ Is2dGroup, Is2dGroup ], 0,
function( C0, S0 )

    local  ok, Ssrc, Srng, gensrc, genrng, tc, hc, ec, ts, hs, es, s, r;

    if not ( IsPreCat1( C0 ) and IsPreCat1( S0 ) ) then
        return false;
    fi;
    if ( HasParent( S0 ) and ( Parent( S0 ) = C0 ) ) then
        return true;
    fi;
    Ssrc := Source( S0 );
    Srng := Range( S0 );
    if not (     IsSubgroup( Source( C0 ), Ssrc )
             and IsSubgroup( Range( C0 ), Srng ) ) then
        Info( InfoXMod, 3, "IsSubgroup failure in IsSubPreCat1" );
        return false;
    fi;
    ok := true;
    gensrc := GeneratorsOfGroup( Ssrc );
    genrng := GeneratorsOfGroup( Srng );
    tc := TailMap(C0);  hc := HeadMap(C0);  ec := RangeEmbedding(C0);
    ts := TailMap(S0);  hs := HeadMap(S0);  es := RangeEmbedding(S0);
    for s in gensrc do
        if ( Image( tc, s ) <> Image( ts, s ) ) then
            ok := false;
        fi;
        if ( Image( hc, s ) <> Image( hs, s ) ) then
            ok := false;
        fi;
    od;
    if not ok then
        Info( InfoXMod, 3, "tail/head maps have different images" );
        return false;
    fi;
    for r in genrng do
        if ( Image( ec, r ) <> Image( es, r ) ) then
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
#M  IsSubCat1( <C1>, <S1> )
##
InstallMethod( IsSubCat1, "generic method for cat1-groups", true,
    [ Is2dGroup, Is2dGroup ], 0,
function( C1, S1 )

    if not ( IsCat1( C1 ) and IsCat1( S1 ) ) then
        return false;
    fi;
    return IsSubPreCat1( C1, S1 );
end );

##############################################################################
##
#M  Sub2dGroup               creates Sub2bObject from Ssrc<=Osrc & Srng<=Orng
##
InstallMethod( Sub2dGroup, "generic method for 2d-objects", true,
    [ Is2dGroup, IsGroup, IsGroup ], 0,
function( obj, src, rng )
    if ( HasIsXMod(obj) and IsXMod(obj) ) then
        return SubXMod( obj, src, rng );
    elif ( HasIsPreXMod(obj) and IsPreXMod(obj) ) then
        return SubPreXMod( obj, src, rng );
    elif ( HasIsCat1(obj) and IsCat1(obj) ) then
        return SubCat1( obj, src, rng );
    elif ( HasIsPreCat1(obj) and IsPreCat1(obj) ) then
        return SubPreCat1( obj, src, rng );
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

    local  Psrc, Prng, Pbdy, Pact, Paut, genSsrc, genSrng, Pname, Sname,
           SM, Sbdy, Saut, Sact, r, innaut, genPrng, genPsrc, ssrc,
           trivsrc, trivrng, incSsrc, idSsrc, imact, imgen, imbdy, imSsrc,
           imalpha, alpha;

    Psrc := Source( PM );
    Prng := Range( PM );
    Pbdy := Boundary( PM );
    Paut := AutoGroup( PM );
    Pact := XModAction( PM );
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
    imgen := List( genSsrc, x -> Image( Pbdy, x ) );
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
        alpha := Image( Pact, r );
        imgen := List( genSsrc, x -> Image( alpha, x ) );
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

    local  SM;
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
#M  SubPreCat1 . . creates SubPreCat1 from PreCat1 and a subgroup of the source
##
InstallMethod( SubPreCat1, "generic method for (pre-)cat1-groups", true,
    [ IsPreCat1, IsGroup, IsGroup ], 0,
function( C, G, R )

    local  Csrc, Crng, Ct, Ch, Ce, t, h, e, SC, ok;

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
    SC := PreCat1ByTailHeadEmbedding( t, h, e );
    if not ( C = SC ) then
        SetParent( SC, C );
    fi;
    return SC;
end );

##############################################################################
##
#M  SubCat1 . . . . . . creates SubCat1 from Cat1 and a subgroup of the source
##
InstallMethod( SubCat1, "generic method for cat1-groups", true,
    [ IsCat1, IsGroup, IsGroup ], 0,
function( C, G, R )

    local  S;
    S := SubPreCat1( C, G, R );
    if not IsCat1( S ) then
        Error( "result is only a pre-cat1-group" );
    fi;
    return S;
end );

#############################################################################
##
#M  IsCat1                       check that the second cat1-group axiom holds
##
InstallMethod( IsCat1, "generic method for crossed modules",
    true, [ IsPreCat1 ], 0,
function( C1G )

    local  Csrc, Crng, h, t, e, f, kerC, kert, kerh, kerth;

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
        Print("condition  [kert,kerh] = 1  is not satisfied \n");
        return false;
    fi;
    if not ( ( Source( f ) = kerC ) and ( Range( f ) = Csrc ) ) then
        Print( "Warning: KernelEmbedding( C1G ) incorrectly defined?\n" );
    fi;
    return true;
end );

#############################################################################
##
#M  IsIdentityCat1
#M  IsEndomorphismPreCat1 
##
InstallMethod( IsEndomorphismPreCat1, "test a pre-cat1-group", true, 
    [ IsPreCat1 ], 0,
function( obj )
    return IsSubgroup( Source(obj), Range(obj) ); 
end );

InstallMethod( IsIdentityCat1, "test a cat1-group", true, [ IsCat1 ], 0,
function( C1G )
    return ( ( TailMap( C1G ) = IdentityMapping( Source( C1G ) ) ) and
             ( HeadMap( C1G ) = IdentityMapping( Source( C1G ) ) ) );
end );

#############################################################################
##
#F  Cat1( <size>, <gpnum>, <num> )   cat1-group from data in CAT1_LIST
#F  Cat1( <t>, <h>, <e> )            cat1-group from given tail, head, embed
#F  Cat1( <t>, <h> )                 cat1-group from tail, head endomorphisms
##
InstallGlobalFunction( Cat1, function( arg )

    local  nargs, C1G, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 3 ) ) then
        Print( "standard usage: Cat1( tail, head [,embed] );\n" );
        Print( "            or: Cat1( size, gpnum, num );\n" );
        return fail;
    elif not IsInt( arg[1] ) then
        if ( nargs = 2 ) then
            C1G := PreCat1( arg[1], arg[2] );
        elif ( nargs = 3 ) then
            C1G := PreCat1( arg[1], arg[2], arg[3] );
        fi;
        ok := IsCat1( C1G );
        if ok then
            return C1G;
        else
            Error( "quotient by Peiffer group not yet implemented" );
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
InstallOtherMethod( Cat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt ], 0,
function( size )
    return Cat1Select( size, 0, 0 );
end );

InstallOtherMethod( Cat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt, IsInt ], 0,
function( size, gpnum )
    return Cat1Select( size, gpnum, 0 );
end );

InstallMethod( Cat1Select, "construct a cat1-group using data in file", true, 
    [ IsInt, IsInt, IsInt ], 0,
function( size, gpnum, num )

    local  ok, type, norm, usage, usage2, maxsize, start, iso, count, comm, 
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
            return PreCat1ByEndomorphisms( t, t ); 
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
    C1G := PreCat1ByEndomorphisms( t, h ); 
    ok := IsCat1( C1G ); 
    if ok then 
        XC := XModOfCat1( C1G ); 
    fi; 
    return C1G; 
end );

InstallMethod( PermCat1Select, "construct a cat1-group using data in file", 
    true, [ IsInt, IsInt, IsInt ], 0,
function( size, gpnum, num )

    local  ok, type, norm, maxsize, start, iso, count, pos, pos2, names,
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
    C1G := PreCat1ByEndomorphisms( t, h ); 
    ok := IsCat1( C1G ); 
    if ok then 
        XC := XModOfCat1( C1G ); 
    fi; 
    return C1G; 
end );

#############################################################################
##
#M  PreCat1ByTailHeadEmbedding
##
InstallMethod( PreCat1ByTailHeadEmbedding,
    "cat1-group from tail, head and embedding", true, 
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( t, h, e )

    local  genG, R, genR, imh, imt, ime, eR, hres, tres, eres,
           kert, kergen, bdy, imbdy, f, C1G, ok, G, PC;

    G := Source( t );
    genG := GeneratorsOfGroup( G );
    if IsSurjective( t ) then
        R := Range( t ); 
    else
        R := Image( t ); 
    fi;
    genR := SmallGeneratingSet( R ); 
    eR := Image( e ); 
    if not ( ( Source( h ) = G )
             and ( Image( h ) = R ) and ( Source( e ) = R )
             and IsInjective( e ) and IsSubgroup( G, eR ) )  then
        return fail;
    fi;
    imh := List( genG, x -> Image( h, x ) );
    imt := List( genG, x -> Image( t, x ) );
    ime := List( genR, x -> Image( e, x ) );
    kert := Kernel( t );
    f := InclusionMappingGroups( G, kert );
    hres := GroupHomomorphismByImages( G, R, genG, imh );
    tres := GroupHomomorphismByImages( G, R, genG, imt );
    eres := GroupHomomorphismByImages( R, G, genR, ime );
    kergen := GeneratorsOfGroup( kert );
    imbdy := List( kergen, x -> Image( h, x) );
    bdy := GroupHomomorphismByImages( kert, R, kergen, imbdy );
    PC := PreCat1Obj( tres, hres, eres );
    SetBoundary( PC, bdy );
    SetKernelEmbedding( PC, f );
    ## check the types 
    if ( IsPermGroup(G) and IsPermGroup(R) ) then 
        SetIsPerm2dGroup( PC, true ); 
    elif ( IsPcGroup(G) and IsPcGroup(R) ) then 
        SetIsPc2dGroup( PC, true ); 
    fi;
    return PC;
end );

#############################################################################
##
#M  PreCat1ByEndomorphisms( <et>, <eh> )
##
InstallMethod( PreCat1ByEndomorphisms,
    "cat1-group from tail and head endomorphisms", true, 
    [ IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( et, eh )

    local  G, gG, R, t, h, e;

    if not ( IsEndoMapping( et ) and IsEndoMapping( eh ) ) then
        Error( "et, eh must both be group endomorphisms" );
    fi;
    if not ( Source( et ) = Source( eh ) ) then
        Error( "et and eh must have same source" );
    fi;
    G := Source( et );
    if not ( Image( et ) = Image( eh ) ) then
        Error( "et and eh must have same image" );
    fi;
    R := Image( et );
    gG := GeneratorsOfGroup( G );
    t := GroupHomomorphismByImages( G, R, gG, List( gG, g->Image(et,g) ) );
    h := GroupHomomorphismByImages( G, R, gG, List( gG, g->Image(eh,g) ) );
    e := InclusionMappingGroups( G, R );
    return PreCat1ByTailHeadEmbedding( t, h, e ); 
end );

#############################################################################
##
#M  PreXModByPreCat1
##
InstallMethod( PreXModByPreCat1, true, 
    [ IsPreCat1 ], 0,
function( C1G )
 
    local  Csrc, Crng, gensrc, genrng, genker, bdy, kert, innaut, autgen,
           imautgen, idkert, a, aut, act, phi, j, r, PM, Cek, Cer, name;

    if not ( IsPermPreCat1( C1G ) or IsPcPreCat1( C1G ) ) then
        Print( "#W: should be a perm-cat1 or a pc-cat1\n" );
        return fail;
    fi;
    Csrc := Source( C1G );
    Crng := Range( C1G );
    bdy := Boundary( C1G );
    Cer := RangeEmbedding( C1G );
    Cek := KernelEmbedding( C1G );
    kert := Kernel( C1G ); 
    if ( Size( kert ) = 1 ) then 
        SetName( kert, "triv" ); 
    fi; 
    if ( ( not HasName( kert ) ) and HasName( C1G ) ) then
        SetName( kert, Concatenation( "ker(", Name( C1G ), ")" ) );
    fi; 
    gensrc := GeneratorsOfGroup( Csrc );
    genrng := GeneratorsOfGroup( Crng );
    genker := GeneratorsOfGroup( kert );
    if IsIdentityCat1( C1G ) then
        # X has trivial source and action
        aut := Group( IdentityMapping( kert ) );
        SetName( aut, "triv_aut" );
        act := MappingToOne( Crng, aut );
        SetName( act, "mapto1" );
    else
        autgen := [ ];
        for r in genrng do
            imautgen := List( genker, s -> Image( Cek, s ) );
            imautgen := List( imautgen, g -> g^( Image( Cer, r ) ) );
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
        SetIsNormalSubgroup2dGroup( PM, true ); 
    fi; 
    if HasName( C1G ) then 
        SetName( PM, Concatenation( "X(", Name( C1G ), ")" ) ); 
    fi; 
    return PM;
end );

#############################################################################
##
#M  Source( C1G ) . . . . . . . . . . . . . . . . . . . .  for a cat1-group
##
InstallOtherMethod( Source,
    "method for a pre-cat1-group",
    true,
    [ IsPreCat1 ], 0,
    C1G -> Source( TailMap( C1G ) ) );

##############################################################################
##
#M  Range( C1G ) . . . . . . . . . . . . . . . . . . . . . for a cat1-group
##
InstallOtherMethod( Range,
    "method for a pre-cat1-group",
    true,
    [ IsPreCat1 ], 0,
    C1G -> Range( TailMap( C1G ) ) );

##############################################################################
##
#M  Kernel( C1G ) . . . . . . . . . . . . . . . . . . . for a pre-cat1-group
##
InstallOtherMethod( Kernel,
    "method for a pre-cat1-group", true, [ IsPreCat1 ], 0,
    C1G -> Kernel( TailMap( C1G ) ) );

#############################################################################
##
#M  Boundary( C1G ) . . . . . . . . . . . . . . . . . . .  for a cat1-group
##
InstallOtherMethod( Boundary,
    "method for a pre-cat1-group", true, [ IsPreCat1 ], 0,
    C1G -> GeneralRestrictedMapping( HeadMap(C1G), Kernel(C1G), Range(C1G) ) );

#############################################################################
##
#M  KernelEmbedding( C1G ) . . .  . . . . . . . . . . . . .  for a cat1-group
##
InstallMethod( KernelEmbedding,
    "method for a pre-cat1-group", true, [ IsPreCat1 ], 0,
    C1G -> InclusionMappingGroups( Source( C1G ), Kernel( C1G ) ) );

##############################################################################
##
#M  Cat1ByPeifferQuotient . . . . . .  cat1 from pre-cat1 and Peiffer subgroup
##
InstallMethod( Cat1ByPeifferQuotient, 
               "cat1-group from a pre-cat1-group and Peiffer subgroup",
               true, [ IsPreCat1 ], 0,
function( PC )

    local  PCrng, PCsrc, PCt, PCh, PCe, genrng, gensrc, Pf, nat, quot,
           qgen, pqgen, tpqgen, hpqgen, tail, head, ime, embed, C1G;

    PCrng := Range( PC ) ;
    genrng := GeneratorsOfGroup( PCrng );
    PCsrc := Source( PC );
    gensrc := GeneratorsOfGroup( PCsrc );
    PCt := TailMap( PC );
    PCh := HeadMap( PC );
    PCe := RangeEmbedding( PC );
    # construct the quotient
    Pf := PeifferSubgroup( PC );
    if not IsNormal( PCsrc, Pf ) then
        Error( "Peiffer subgroup not normal in source group" );
    fi;
    nat := NaturalHomomorphismByNormalSubgroup( PCsrc, Pf );
    quot := Image( nat );
    qgen := GeneratorsOfGroup( quot );
    # construct the head, tail and embedding
    pqgen := List( qgen, q -> PreImagesRepresentative( nat, q ) );
    tpqgen := List( pqgen, p -> Image( PCt, p ) );
    tail := GroupHomomorphismByImages( quot, PCrng, qgen, tpqgen );
    hpqgen := List( pqgen, p -> Image( PCh, p ) );
    head := GroupHomomorphismByImages( quot, PCrng, qgen, hpqgen );
    ime := List( genrng, r -> Image( nat, Image( PCe, r ) ) );
    embed := GroupHomomorphismByImages( PCrng, quot, genrng, ime );
    C1G := PreCat1ByTailHeadEmbedding( tail, head, embed );
    if not IsCat1( C1G ) then
        Error( "fails to be a cat1-group" );
    fi;
    return C1G;
end );

#############################################################################
##
#M  DirectProductInfo( <obj> ) . . . . . . . . . . . . . . . . for 2d-objects
#M  DirectProductOp(  ) . . . . . . .  (bdy1 x bdy2) : (S1 x S2) --> (R1 x R2)
##
InstallOtherMethod( DirectProductInfo, "generic method for 2d-objects",
               true, [ Is2dDomain ], 0,
function( obj )

    return rec( objects := [ ],
                embeddings := [ ],
                projections := [ ] );
end );

##  (19/07/07) : allowed for case when one of Xsrc,Xrng,Ysrc,Yrng trivial ##
##               using parameter list: spec(=[0,0,0,0] by default)        ##

InstallOtherMethod( DirectProductOp,
    "method for pre-crossed modules", true, [ IsList, IsPreXMod ], 0,
function( list, X1 )

    local  Xsrc, Xrng, Y1, Ysrc, Yrng, genXrng, genYrng, genXsrc, genYsrc,
           XSpos, YSpos, XRpos, YRpos, Spos, imaut, autgen, aut, act,
           XY, S, R, genS, lenS, genR, lenR, imbdy, bdy, a, i, j, k,
           Xbdy, Ybdy, Xact, Yact, imXbdy, imYbdy, alpha, info,
           eXS, eYS, pXS, pYS, eXR, eYR, spec;

    if not ( Length( list ) = 2 ) then
        Error( "direct product not yet implemented for more than 2 terms" );
    fi;
    if not ( list[1] = X1 ) then
        Error( "second argument should be first in first argument" );
    fi;
    Y1 := list[2]; 
    #'#  first the source group 
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
        s -> Image( eXR, Image( Xbdy, Image( pXS, s ) ) ) );
    imYbdy := List( genS{ YSpos },
        s -> Image( eYR, Image( Ybdy, Image( pYS, s ) ) ) );
    imbdy := Concatenation( imXbdy, imYbdy );
    bdy := GroupHomomorphismByImages( S, R, genS, imbdy );
    autgen := 0 * [ 1..lenR ];
    for i in XRpos do
        a := Image( Xact, genXrng[i] );
        imaut := 0 * Spos;
        for j in YSpos do
            imaut[j] := genS[j];
        od;
        for j in XSpos do
            imaut[j] := Image( eXS, Image( a, Image( pXS, genS[j] ) ) );
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
        a := Image( Yact, genYrng[i-k] );
        imaut := 0 * Spos;
        for j in XSpos do
            imaut[j] := genS[j];
        od;
        for j in YSpos do
            imaut[j] := Image( eYS, Image( a, Image( pYS, genS[j] ) ) );
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
    true, [ Is2dGroup, IsPosInt ], 0,
function( D, i )
    local  info, eS, eR, mor;

    info := DirectProductInfo( D );
    if IsBound( info!.embeddings[i] ) then
        return info!.embeddings[i];
    fi;
    eS := Embedding( Source( D ), i );
    eR := Embedding( Range( D ), i );
    Info( InfoXMod, 3, "SourceEmbedding: ", eS );
    Info( InfoXMod, 3, " RangeEmbedding: ", eR );
    if IsPreXMod( D ) then
        mor := PreXModMorphism( info!.objects[i], D, eS, eR );
    elif IsPreCat1( D ) then
        mor := PreCat1Morphism( info!.objects[i], D, eS, eR );
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
    true, [ Is2dGroup and HasDirectProductInfo, IsPosInt ], 0,
function( D, i )
    local  info, pS, pR, mor;

    info := DirectProductInfo( D );
    if IsBound( info!.projections[i] ) then
        return info!.projections[i];
    fi;
    pS := Projection( Source( D ), i );
    pR := Projection( Range( D ), i );
    if IsPreXMod( D ) then
        mor := PreXModMorphism( info!.objects[i], D, pS, pR );
    elif IsPreCat1( D ) then
        mor := PreCat1Morphism( info!.objects[i], D, pS, pR );
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
#M  TrivialSub2dGroup . . . . . . . . . . . . . . . of a 2d-object
#M  TrivialSubPreXMod  . . . . . . . . . . . . . . . of a pre-crossed module
#M  TrivialSubXMod     . . . . . . . . . . . . . . . of a crossed module
#M  TrivialSubPreCat1  . . . . . . . . . . . . . . . of a pre-cat1-group
#M  TrivialSubCat1     . . . . . . . . . . . . . . . of a cat1-group
##
InstallMethod( TrivialSub2dGroup, "of a 2d-object", true, [ Is2dGroup ], 0,
function( obj )

    local  idsrc, idrng;

    idsrc := TrivialSubgroup( Source( obj ) );
    idrng := TrivialSubgroup( Range( obj ) );
    if IsPreXMod( obj ) then
        return SubPreXMod( obj, idsrc, idrng );
    elif IsPreCat1( obj ) then
        return SubPreCat1( obj, idsrc );
    else
        Error( "<obj> must be a pre-crossed module or a pre-cat1-group" );
    fi;
end );

InstallMethod( TrivialSubPreXMod, "of a pre-crossed module", true,
    [ IsPreXMod ], 0,
function( obj )
    return TrivialSub2dGroup( obj );
end );

InstallMethod( TrivialSubXMod, "of a crossed module", true, [ IsXMod ], 0,
function( obj )
    return TrivialSub2dGroup( obj );
end );

InstallMethod( TrivialSubPreCat1, "of a pre-cat1-group", true,
[ IsPreCat1 ], 0,
function( obj )
    return TrivialSub2dGroup( obj );
end );

InstallMethod( TrivialSubCat1, "of a cat1-group", true, [ IsCat1 ], 0,
function( obj )
    return TrivialSub2dGroup( obj );
end );

##############################################################################
##
#M  IsNormalSubgroup2dGroup . . . . . . . . . . . . . . . . .  for 2d-objects
##
InstallMethod( IsNormalSubgroup2dGroup, "for crossed modules and cat1-groups",
    [ Is2dGroup ], 0,
function( obj )
    local  src, rng, gensrc, genrng;
    src := Source( obj );
    rng := Range( obj );
    gensrc := GeneratorsOfGroup( src );
    if IsXMod( obj ) then
        return ( IsNormal(rng,src) and
                 ( gensrc = List( gensrc, s -> Image( Boundary(obj), s ) ) ) );
    elif IsCat1( obj ) then
        return IsNormalSubgroup2dGroup( XModOfCat1( obj ) );
    else
        Error( "method not yet implemented" );
    fi;
end );

##############################################################################
##
#M  IsNormal . . . . . . . . . . . . . . . . . . . . . . . . .  for 2d-objects
##
InstallOtherMethod( IsNormal, "for crossed modules", IsIdenticalObj,
    [ IsXMod, IsXMod ], 0,
function( XM, SM )

    local  xr, a, ss, im, xs, sr, Ssrc, Xact;

    if not IsSubXMod( XM, SM ) then
        return false;
    fi;
    Ssrc := Source( SM );
    Xact := XModAction( XM );
    for xr in GeneratorsOfGroup( Range( XM ) ) do
        a := Image( Xact, xr );
        for ss in GeneratorsOfGroup( Ssrc ) do
            im := Image( a, ss );
            if not ( im in Ssrc ) then
                Info( InfoXMod, 2, "ss,xr,ss^xr = ", [ss,xr,im] );
                return false;
            fi;
        od;
    od;
    for sr in GeneratorsOfGroup( Range( SM ) ) do
        a := Image( Xact, sr );
        for xs in GeneratorsOfGroup( Source( XM ) ) do
            im := xs^(-1) * Image( a, xs );
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

    local  Xsrc, Xrng, YM, i, j, slen, rlen, norm, normsrc, normrng, ok;

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
