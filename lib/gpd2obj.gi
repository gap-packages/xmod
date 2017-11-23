##############################################################################
##
#W  gpd2obj.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##  Standard error messages

XMODOBJ_CONSTRUCTORS := Concatenation( 
    "The standard operations which construct an xmod with objects are:\n",
    "1.  SinglePieceXMod( object xmod, list of objects );\n",
    "2.  DomainWithSingleObject( xmod, single object );\n",
    "3.  UnionOfPieces( list of xmods with objects );\n",
    "4.  XModWithObjects( one of the previous parameter options );" );

#############################################################################
##
#F  XModWithObjects( <pieces> )         xmod of groupoids as list of pieces 
#F  XModWithObjects( <xmod>, <obj> )    xmod with a single object
#F  XModWithObjects( <xmod>, <obs> )    single piece xmod with objects 
##
InstallGlobalFunction( XModWithObjects, function( arg )

    local nargs, id, rays;

    nargs := Length( arg ); 
    # list of pieces
    if ( ( nargs = 1 ) and IsList( arg[1] ) 
         and  ForAll( arg[1], G -> IsXModWithObjects(G) ) ) then
        Info( InfoXMod, 2, "ByUnion" );
        return UnionOfPieces( arg[1] );
    # group * tree groupoid
    elif ( ( nargs = 2 ) and IsList( arg[2] ) and IsXMod( arg[1] ) ) then
        Info( InfoXMod, 2, "group plus objects" ); 
        return SinglePieceGroupoid( arg[1], arg[2] );
    # one-object groupoid
    elif ( ( nargs = 2 ) and IsObject( arg[2] ) and IsXMod( arg[1] ) ) then
        Info( InfoXMod, 2, "SingleObjectXMod" );
        return DomainWithSingleObject( arg[1], arg[2] );
    else
        Info( InfoXMod, 1, XMODOBJ_CONSTRUCTORS );
        return fail;
    fi;
end );

##############################################################################
##
#M  PreXModWithObjectsObj( <obs>, <bdy>, <act> ) . . . make pre-crossed module
##
InstallMethod( PreXModWithObjectsObj, "for objects, morphism and action", true,
    [ IsHomogeneousList, IsGeneralMappingWithObjects, 
      IsGeneralMappingWithObjects ], 0,
function( obs, bdy, act )

    local  filter, fam, PM, ok, src, rng, aut, name;

    fam := Family2DimensionalGroupWithObjects;
    filter := IsPreXModWithObjects; 
    if not IsConstantOnObjects( bdy ) then 
        Error( "objects not fixed by the boundary" ); 
    fi;
    src := Source( bdy );
    rng := Range( bdy ); 
    if not ( IsGroupoid( src ) and IsGroupoid( rng ) ) then 
        Error( "source/range of boundary should be groupoids" ); 
    fi; 
    if not ( rng = Source( act ) ) then
        Error( "require Range( bdy ) = Source( act )" );
    fi;
    aut := Range( act ); 
    if not IsGroupOfAutomorphisms( aut!.magma ) then
        Error( "Range( act ) must be a group of automorphisms" );
    fi;
    if ( IsPermGroupoid( src ) and IsPermGroupoid( rng ) ) then
        filter := filter and IsPermPreXMod;
    fi;
    PM := rec();
    ObjectifyWithAttributes( PM, PreXModWithObjectsType,
      ObjectList, obs, 
      Source, src,
      Range, rng,
      Boundary, bdy,
      AutoGroup, aut,
      XModAction, act,
      Is2DimensionalDomain, true, 
      IsPreXModDomain, true );
    if not IsPreXModWithObjects( PM ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed module." );
    fi; 
    SetIsSinglePieceDomain( PM, 
        ( HasIsSinglePieceDomain(rng) and IsSinglePieceDomain(rng) ) ); 
    SetIsPreXModWithObjects( PM, true ); 
##    ok := IsXModWithObjects( PM ); 
    # name := Name( PM );
    return PM;
end );

#############################################################################
##
#M  IsXModWithObjects . . . check that the second crossed module axiom holds
##
InstallMethod( IsXModWithObjects, "generic method for pre-crossed modules",
    true, [ IsPreXModWithObjects ], 0,
function( XM )

    local  gensrc, genrng, x2, y2, a2, w2, z2, bdy, act;

    Info( InfoXMod, 2, "using IsXMod from gpd2obj.gi" ); 
    bdy := Boundary( XM );
    act := XModAction( XM );
    gensrc := GeneratorsOfGroupoid( Source( XM ) );
    genrng := GeneratorsOfGroupoid( Range( XM ) ); 
    for x2 in gensrc do
        for y2 in gensrc do
            Info( InfoXMod, 3, "x2,y2 = ", x2, ",  ", y2 ); 
            a2 := ImageElm( act, ImageElm( bdy, y2 ) ); 
            z2 := ImageElm( a2![1], x2 ); 
            w2 := x2^y2;
            Info( InfoXMod, 3, "w2,z2 = ", w2, ",  ", z2 ); 
            if ( z2 <> w2 ) then
                Info( InfoXMod, 2,
                      "CM2) fails at  x2 = ", x2, ",  y2 = ", y2, "\n",
                      "x2^(bdy(y2)) = ", z2, "\n","      x2^y2 = ", w2, "\n" );
                return false;
            fi;
        od;
    od;
    return true;
end );

##############################################################################
##
#M  DiscreteNormalPreXModWithObjects( <gpd>, <gp> ) .. make pre-crossed module
##
InstallMethod( DiscreteNormalPreXModWithObjects, 
    "for a single piece groupoid and a subgroup of the root group", true,
    [ IsSinglePiece, IsGroup ], 0,
function( R, gpS )

    local  gpR, obs, ro, nobs, imobs, idgpS, S, ok, inc, AR, AS, idS, 
           AS0, conj, action, P0; 

    gpR := R!.magma; 
    obs := R!.objects; 
    ro := obs[1]; 
    nobs := Length( obs ); 
    imobs := List( obs, o -> 0 ); 
    if not IsSubgroup( gpR, gpS ) then 
        Error( "gpS not a subgroup of the root group gpR of gpd" ); 
    fi; 
    idgpS := IdentityMapping( gpS ); 
    S := DiscreteSubgroupoid( R, List( obs, o -> gpS ), obs ); 
    ok := IsHomogeneousDiscreteGroupoid( S );
    inc := InclusionMappingGroupoids( R, S ); 
    AR := AutomorphismGroupOfGroupoid( R ); 
    AS := AutomorphismGroupOfGroupoid( S ); 
    idS := One( AS ); 
    AS0 := DomainWithSingleObject( AS, 0 ); 
    conj := function(a)
                return ArrowNC( true, GroupoidInnerAutomorphism(R,S,a), 0, 0 ); 
            end;
    action := MappingWithObjectsByFunction( R, AS0, conj, imobs ); 
    SetName( Range(action), "Aut(SX0)" ); 
    P0 := PreXModWithObjectsObj( obs, inc, action); 
    return P0; 
end ); 

##############################################################################
##
#M  SinglePiecePreXModWithObjects( <xmod>, <obs> ) .. make prexmod with obs 
#M  SinglePiecePreXModWithObjectsNC( <xmod>, <obs> ) .. make prexmod with obs 
##
InstallMethod( SinglePiecePreXModWithObjects, "for prexmod + list of objects", 
    true, [ IsPreXMod, IsList ], 0,
function( px, obs )
    if not IsSet( obs ) then 
        Sort( obs ); 
    fi; 
    if not IsDuplicateFree( obs ) then 
        Error( "objects must be distinct" );
    fi; 
    return SinglePiecePreXModWithObjectsNC( px, obs ); 
end ); 

InstallMethod( SinglePiecePreXModWithObjectsNC, "for prexmod + list of objects", 
    true, [ IsPreXMod, IsList ], 0,
function( px, obs )

    local rpx, src, rng, gens, bpx, imbdy, i, a, im, bdy, pxwo; 

    rpx := Range( px ); 
    pxwo := rec( 
        objects := obs, 
        prexmod := px, 
        rays := List( obs, function( o ) return One(rpx); end ) 
        ); 
    src := SinglePieceGroupoid( Source( px ), obs ); 
    rng := SinglePieceGroupoid( rpx, obs ); 
    gens := GeneratorsOfGroupoid( src ); 
    bpx := Boundary( px ); 
    imbdy := ShallowCopy( gens ); 
    for i in [1..Length(imbdy)] do 
        a := gens[i]; 
        if ( a![2] = a![3] ) then 
            im := ImageElm( bpx, a![1] ); 
            imbdy[i] := ArrowNC( true, im, a![2], a![3] ); 
        else 
            imbdy[i] := ArrowNC( true, One(rpx), a![2], a![3] ); 
        fi; 
    od; 
    bdy := GroupoidHomomorphismFromSinglePiece( src, rng, gens, imbdy ); 
    ObjectifyWithAttributes( pxwo, PreXModWithObjectsType, 
        IsSinglePieceDomain, true, 
        Root2dGroup, px, 
        Source, src, 
        Range, rng, 
        Boundary, bdy, 
        IsDirectProductWithCompleteDigraphDomain, true ); 
    return pxwo; 
end ); 

##############################################################################
##
#M  \=( <P>, <Q> )  . . . . . . . . . .  test if two 2d-groups over a groupoid
##
InstallMethod( \=, "generic method for two xmods over a groupoid",
    IsIdenticalObj, [ IsPreXModWithObjects, IsPreXModWithObjects ], 0,
function ( P, Q ) 
    return ( ( ObjectList(P) = ObjectList(Q) ) 
             and ( Boundary(P) = Boundary(Q) )
             and ( XModAction(P) = XModAction(Q) ) );
end );

#############################################################################
##
#M  UnionOfPiecesOp . . for connected prexmods with objects plus one of these 
##
InstallOtherMethod( UnionOfPiecesOp, "method for list of prexmods with objects",
    true, [ IsList, IsDomainWithObjects ], 0,
function( comps, dom )

    local len, pieces, L, fam, filter, xwo, i, obs, par;

    if not ForAll( comps, 
               c -> "IsPreXModWithObjects" in CategoriesOfObject( c ) ) then 
        TryNextMethod(); 
    fi; 
    ## order pieces by first object
    len := Length( comps ); 
    obs := List( comps, g -> ObjectList(g)[1] );
    L := [1..len];
    SortParallel( obs, L );
    if ( L = [1..len] ) then 
        pieces := comps; 
    else 
        Info( InfoXMod, 2, "reordering pieces by first object" ); 
        pieces := List( L, i -> comps[i] );
    fi; 
    fam := Family2DimensionalGroupWithObjects; 
    filter := IsPiecesRep and IsPreXModWithObjects and IsAssociative; 
    xwo := Objectify( PreXModWithPiecesType, rec () );
    SetIsSinglePieceDomain( xwo, false ); 
    SetPieces( xwo, pieces ); 
    if HasParent( pieces[1] ) then 
        par := Ancestor( pieces[1] ); 
        if ForAll( pieces, c -> ( Ancestor( c ) = par ) ) then 
            SetParent( xwo, par ); 
        fi; 
    fi; 
    #? removed tests as to whether perm-, pc-, etc xmod with objects 
    return xwo; 
end );

#############################################################################
##
#M  DomainWithSingleObject
##
##  Note that there is another method for [ IsGroup, IsObject ] in gpd.gi 
##
InstallMethod( DomainWithSingleObject, "generic method for domain, object",
    true, [ IsDomain, IsObject ], 0,
function( dom, obj ) 

    local o; 

    if ( IsList( obj ) and ( Length(obj) = 1 ) ) then
        Info( InfoXMod, 2, "object given as a singleton list" );
        o := obj[1]; 
    else
        o := obj;
    fi; 
    if not IsObject( o ) then 
        Error( "<obj> not a scalar or singleton list," ); 
    fi; 
    if ( HasIsAssociative( dom ) and IsAssociative( dom ) 
         and ( "IsMagmaWithInverses" in CategoriesOfObject( dom ) ) 
         and IsMagmaWithInverses( dom ) ) then 
        return SinglePieceGroupoidNC( dom, [o] ); 
    elif ( HasIsMonoid( dom ) and IsMonoid( dom ) ) then 
        return SinglePieceMonoidWithObjects( dom, [o] ); 
    elif ( HasIsSemigroup( dom ) and IsSemigroup( dom ) ) then 
        return SinglePieceSemigroupWithObjects( dom, [o] ); 
    elif ( ( "IsMagma" in CategoriesOfObject(dom) ) and IsMagma(dom) ) then 
        return SinglePieceMagmaWithObjects( dom, [o] ); 
    else 
        Error( "unstructured domains with objects not yet implemented," ); 
    fi; 
end );

############################################################################# 
## 
#M  PieceOfObject
## 
InstallMethod( PieceOfObject, "generic method for magma with objects", 
    true, [ IsDomainWithObjects, IsObject ], 0,
function( dwo, obj )

    local pieces, p, objp;

    if IsSinglePiece( dwo ) then
        if not ( obj in dwo!.objects ) then
            Error( "<obj> not an object of <dwo>," );
        else
            return dwo;
        fi;
    elif not ( obj in ObjectList( dwo ) ) then
        Info( InfoXMod, 1, "<obj> not an object of <dwo>" );
        return fail;
    fi;
    pieces := Pieces( dwo );
    for p in pieces do
        objp := p!.objects;
        if ( obj in objp ) then
            return p;
        fi;
    od;
    Info( InfoXMod, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

############################################################################# 
## 
#M  PieceNrOfObject
## 
InstallMethod( PieceNrOfObject, "generic method for domain with objects",
    true, [ IsDomainWithObjects, IsObject ], 0,
function( dwo, obj )

    local pieces, i, objp, np; 

    pieces := Pieces( dwo );
    for i in [1..Length( pieces )] do
        objp := pieces[i]!.objects;
        if ( obj in objp ) then
            return i;
        fi;
    od;
    Info( InfoXMod, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

############################################################################# 
## 
#M  Root2dGroup 
## 
InstallMethod( Root2dGroup, "generic method for domain with objects",
    true, [ Is2DimensionalDomainWithObjects ], 0,
function( dwo )

    local src, rng, bdy, act, ro, sgo, bdyo, rgo, acto; 

    src := Source( dwo ); 
    rng := Range( dwo ); 
    bdy := Boundary( dwo );
    act := XModAction( dwo ); 
    ro := RootObject( rng ); 
    sgo := FullSubgroupoid( src, [ro] ); 
    bdyo := RestrictedMappingGroupoids( bdy, sgo ); 
    rgo := FullSubgroupoid( rng, [ro] ); 
    acto := RestrictedMappingGroupoids( act, rgo ); 
Error("here");
    return fail; 
end ); 

#############################################################################
##
#O  PrintObj( <pxwo> ) . . print details of a precrossed module of groupoids
#O  ViewObj( <pxwo> ) . . print details of a precrossed module of groupoids
##
InstallMethod( PrintObj, "method for prexmods and precat2groups", true, 
    [ Is2DimensionalGroupWithObjects ], 0,
function ( pxwo )

    Print( "precrossed module with source groupoid:\n" ); 
    Print( Source( pxwo ), "\n" ); 
    Print( "and range groupoid:\n" ); 
    Print( Range( pxwo ) ); 
    return;
end );

InstallMethod( ViewObj, "method for prexmods and precat2groups", true, 
    [ Is2DimensionalGroupWithObjects ], 0,
function ( pxwo )
    PrintObj( pxwo ); 
    return;
end );

#############################################################################
##
#O  Display( <xwo> ) . . . . . print details of a crossed module of groupoids
##
InstallMethod( Display, "method for prexmods and precat2groups", true, 
    [ Is2DimensionalGroupWithObjects ], 0,
function( xwo )
    Print( "crossed module of groupoids, " ); 
    if HasName( xwo ) then 
        Print( Name( xwo ) ); 
    fi; 
    Print( "\n" ); 
    Print( "source groupoid:\n" ); 
    Display( Source( xwo ) ); 
    Print( "range groupoid:\n" ); 
    Display( Range( xwo ) ); 
end ); 

#############################################################################
##
#E  gpd2obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
