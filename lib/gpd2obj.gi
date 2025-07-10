##############################################################################
##
#W  gpd2obj.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  

#############################################################################
##  Standard error messages

XMODOBJ_CONSTRUCTORS := Concatenation( 
    "The standard operations which construct a (pre)xmod with objects are:\n",
    "1.  PreXModWithObjectsByBoundaryAndAction( boundary, action );\n",
    "2.  SinglePiecePreXModWithObjects( (pre)xmod, objects, isdiscrete );\n",
    "3.  MagmaWithSingleObject( (pre)xmod, single object );\n",
    "4.  UnionODomainWithPieces( list of (pre)xmods with objects );\n",
    "5.  PreXModWithObjects( one of the previous parameter options );" );

#############################################################################
##
#F  PreXModWithObjects( <pieces> )            list of (pre)xmods of groupoids  
#F  PreXModWithObjects( <px>, <obj> )         (pre)xmod with a single object
#F  PreXModWithObjects( <px>, <obs>, <disc> ) disc=true when source is discrete 
##
InstallGlobalFunction( PreXModWithObjects, function( arg )

    local nargs, id, rays;

    nargs := Length( arg ); 
    # list of pieces
    if ( ( nargs = 1 ) and IsList( arg[1] ) 
         and  ForAll( arg[1], IsPreXModWithObjects ) ) then
        Info( InfoXMod, 2, "ByUnion" );
        return UnionOfPiecesOp( arg[1], arg[1][1] );
    elif ( nargs = 2 ) then
        # prexmod plus single object 
        if ( IsPreXMod( arg[1] ) and IsObject( arg[2] ) ) then
            Info( InfoXMod, 2, "prexmod plus single object" ); 
            return MagmaWithSingleObject( arg[1], arg[2] );
        # two groupoid homomorphisms
        elif ( IsGroupoidHomomorphism( arg[1] ) 
               and IsGroupoidHomomorphism( arg[2] ) ) then
            Info( InfoXMod, 2, "by boundary and action" );
            return PreXModByBoundaryAndAction( arg[1], arg[2] );
        else
            return fail;
        fi;
    # prexmod + list of objects + boolean (true => discrete source) 
    elif ( ( nargs = 3 ) and IsPreXMod( arg[1] ) 
           and IsList( arg[2] ) and IsBool( arg[3] ) ) then
        Info( InfoXMod, 2, "SinglePieceXPreModWithObjects" );
        return SinglePiecePreXModWithObjects( arg[1], arg[2], arg[3] );
    else
        Info( InfoXMod, 1, XMODOBJ_CONSTRUCTORS );
        return fail;
    fi;
end );

#############################################################################
##
#M  PreXModWithObjectsByBoundaryAndAction
##
InstallMethod( PreXModWithObjectsByBoundaryAndAction,
    "pre-crossed module with objects from boundary and action maps",
    true, [ IsGroupoidHomomorphism, IsGroupoidHomomorphism ], 0,
function( bdy, act )

    local rng, src, genrng, gensrc, aut, genaut, obsaut, root, imact, 
          i, a0, ima, a, PX;

    src := Source( bdy );
    gensrc := GeneratorsOfGroupoid( src );
    rng := Range( bdy );
    genrng := GeneratorsOfGroupoid( rng );
    if not ( Source( act ) = rng ) then
        Info( InfoXMod, 2,
              "The range group is not the source of the action." );
        return fail;
    fi;
    aut := Range( act );
    genaut := GeneratorsOfGroupoid( aut );
    obsaut := ObjectList( aut );
    root := RootGroup( aut );
    if not ( Size( obsaut ) = 1 ) and IsGroupOfAutomorphisms( root ) then
        Info( InfoXMod, 2, "<aut> is not a group of groupoid automorphisms" );
        return fail;
    fi;
    for a in genaut do
        if not ( ( Source( a![2] ) = src ) and ( Range( a![2] ) = src ) ) then
            Info( InfoXMod, 2, "error in source and range of automorphism" );
            return fail;
        fi;
    od;
    if not ( One( root ) = IdentityMapping( src ) ) then
        Info( InfoXMod, 2,
              "root.identity <> IdentityMapping( src )" );
        return fail;
    fi;
    imact := List( genrng, r -> ImageElm( act, r ) );
    PX := PreXModWithObjectsObj( bdy, act ); 
    if not IsPreXModWithObjects( PX ) then 
        Info( InfoXMod, 1, "PX fails to be a groupoid pre-crossed module" ); 
        return fail; 
    fi;
    return PX; 
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
            z2 := ImageElm( a2![2], x2 ); 
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
#M  PreXModWithObjectsObj( <bdy>, <act> ) 
##
InstallMethod( PreXModWithObjectsObj, "for 2 groupoid homomorphisms", 
    true, [ IsGroupoidHomomorphism, IsGroupoidHomomorphism ], 0,
function( bdy, act )

    local pxwo, ok; 

    pxwo := rec( boundary := bdy, action := act ); 
    ObjectifyWithAttributes( pxwo, PreXModWithObjectsType, 
        Is2DimensionalDomain, true, 
        IsSinglePieceDomain, true, 
        IsPreXModWithObjects, true, 
        Source, Source( bdy ), 
        Range, Range( bdy ), 
        Boundary, bdy, 
        XModAction, act ); 
    ok := IsXModWithObjects( pxwo );
    return pxwo; 
end ); 

##############################################################################
##
#M  SinglePiecePreXModWithObjects( <xmod>, <obs> ) .. make prexmod with obs 
#M  SinglePiecePreXModWithObjectsNC( <xmod>, <obs> ) .. make prexmod with obs 
##
InstallMethod( SinglePiecePreXModWithObjects, "for prexmod, objects, isdisc?", 
    true, [ IsPreXMod, IsList, IsBool ], 0,
function( px, obs, isdisc )
    if not IsSet( obs ) then 
        Sort( obs ); 
    fi; 
    if not IsDuplicateFree( obs ) then 
        Error( "objects must be distinct" );
    fi; 
    return SinglePiecePreXModWithObjectsNC( px, obs, isdisc ); 
end ); 

InstallMethod( SinglePiecePreXModWithObjectsNC, "for prexmod, obs, isdisc?", 
    true, [ IsPreXMod, IsList, IsBool ], 0,
function( px, obs, isdisc )

    local nobs, ro, spx, rpx, src, rng, gens, genr, bpx, homs, imbdy, imobs, 
          i, a, im, bdy, apx, AS, AR, AS0, imact, p, g, q, pos, aut, p1, h, 
          idspx, auts, act, pxo; 

    nobs := Length( obs );
    ro := obs[1];  ## root object
    spx := Source( px ); 
    rpx := Range( px ); 
    rng := SinglePieceGroupoid( rpx, obs );
## Print( "rng = ", rng, "\n" );
    if isdisc then 
        src := HomogeneousDiscreteGroupoid( spx, obs );
    else 
        src := SinglePieceGroupoid( spx, obs ); 
    fi;
## Print( "src = ", src, "\n" );
    ## construct the boundary 
    gens := GeneratorsOfGroupoid( src ); 
    genr := GeneratorsOfGroupoid( rng ); 
    bpx := Boundary( px ); 
    if isdisc then 
        homs := ListWithIdenticalEntries( nobs, bpx );
        bdy := GroupoidHomomorphismFromHomogeneousDiscrete(src,rng,homs,obs);
    else
        imbdy := ShallowCopy( gens ); 
        for i in [1..Length(imbdy)] do 
            a := gens[i]; 
            if ( a![3] = a![4] ) then 
                im := ImageElm( bpx, a![2] ); 
                imbdy[i] := ArrowNC( src, true, im, a![3], a![4] ); 
            else 
                imbdy[i] := ArrowNC( src, true, One(rpx), a![3], a![4] ); 
            fi; 
        od; 
        bdy := GroupoidHomomorphismFromSinglePiece( src, rng, gens, imbdy ); 
    fi;
## Print( "boundary = ", bdy, "\n" );
    ## now construct the action 
    apx := XModAction( px ); 
    AS := AutomorphismGroupOfGroupoid( src ); 
    AR := AutomorphismGroupOfGroupoid( rng ); 
    AS0 := MagmaWithSingleObject( AS, 0 ); 
    imact := ListWithIdenticalEntries( nobs, 0 );
    for i in [1..Length(genr)] do
        a := genr[i];
        g := a![2];
        p := a![3];
        q := a![4];
        if ( p <> ro ) then
             Error( "expecting tail of a to be the root object" );
        fi;
        if ( q = ro ) then  ## arrow is a loop
            aut := ImageElm( apx, g );
## Print( "aut = ", aut, "\n" );
            if isdisc then
                p1 := Pieces( src )[1];
                idspx := IdentityMapping( spx );
                auts := ListWithIdenticalEntries( nobs, idspx );
                auts[1] := aut;
                h := GroupoidAutomorphismByGroupAutos( src, auts ); 
            else
                h := GroupoidAutomorphismByGroupAuto( src, aut );
            fi;
## Print( "h = ", h, "\n" );
            imact[i] := Arrow( AS0, h, 0, 0 );
        else 
            imobs := ShallowCopy( obs );
            pos := Position( obs, q );
            imobs[1] := q;
            imobs[pos] := p;
            h := GroupoidAutomorphismByObjectPerm( src, imobs );
            imact[i] := Arrow( AS0, h, 0, 0 );
        fi;
    od;
    act := GroupoidHomomorphism( rng, AS0, genr, imact );
## Print( "act = ", act, "\n" );
    pxo := PreXModWithObjectsByBoundaryAndAction( bdy, act );
    SetRoot2dGroup( pxo, px );
    return pxo;
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
    true, [ IsList, Is2DimensionalGroupWithObjects ], 0,
function( comps, c1 )

    local len, c, obs, obc, pieces, L, fam, filter, xwo, i, par;

    if not ForAll( comps, 
        c -> "Is2DimensionalGroupWithObjects" in CategoriesOfObject( c ) ) then 
        Error( "expecting a list of prexmods with objects" ); 
    fi; 
    len := Length( comps ); 
    if ( len = 1 ) then 
        Info( InfoXMod, 1, "only one component, so returning it" ); 
        return c1; 
    fi;
    ## check object lists are disjoint 
    obs := [ ]; 
    for c in comps do 
        obc := ObjectList( c );
        if ( Intersection( obs, obc ) <> [ ] ) then
            Info( InfoXMod, 1, 
                  "constituents must have disjoint object sets" );
            return fail;
        fi;
        obs := Union( obs, obc ); 
    od;
    ## sorting object lists by leading object 
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
    if ForAll( pieces, p -> HasIsXMod(p) and IsXMod(p) ) then
        SetIsXModWithObjects( xwo, true ); 
    fi; 
    if ForAll( pieces, p -> HasIsPreXMod(p) and IsPreXMod(p) ) then
        SetIsPreXModWithObjects( xwo, true ); 
    fi; 
    if ForAll( pieces, p -> HasIsCat1Groupoid(p) and IsCat1Groupoid(p) ) then
        SetIsCat1Groupoid( xwo, true ); 
    fi; 
    if ForAll( pieces, p -> HasIsPreCat1Groupoid(p) 
                            and IsPreCat1Groupoid(p) ) then
        SetIsPreCat1Groupoid( xwo, true ); 
    fi; 

    #? removed tests as to whether perm-, pc-, etc xmod with objects 
    return xwo; 
end );

#############################################################################
##
#M  MagmaWithSingleObject
##
InstallMethod( MagmaWithSingleObject, "generic method for domain, object",
    true, [ IsMagma, IsObject ], 0,
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
Error("");
    return fail; 
end ); 

############################################################################# 
## 
#M  IsPermPreXModWithObjects  
#M  IsPcPreXModWithObjects  
#M  IsFpPreXModWithObjects  
## 
InstallMethod( IsPermPreXModWithObjects, "for domain with objects", true, 
    [ Is2DimensionalDomainWithObjects ], 0,
function( dwo )
    return IsPerm2DimensionalGroup( Root2dGroup( dwo ) ); 
end ); 

InstallMethod( IsPcPreXModWithObjects, "for domain with objects", true, 
    [ Is2DimensionalDomainWithObjects ], 0,
function( dwo )
    return IsPc2DimensionalGroup( Root2dGroup( dwo ) ); 
end ); 

InstallMethod( IsFpPreXModWithObjects, "for domain with objects", true, 
    [ Is2DimensionalDomainWithObjects ], 0,
function( dwo )
    return IsFp2DimensionalGroup( Root2dGroup( dwo ) ); 
end ); 

#############################################################################
##
#O  PrintObj( <pxwo> ) . . print details of a precrossed module of groupoids
#O  ViewObj( <pxwo> ) . . print details of a precrossed module of groupoids
##
InstallMethod( PrintObj, "method for prexmods and precat2groups with objects", 
    true, [ Is2DimensionalGroupWithObjects ], 0,
function ( pxwo )

    local len, pieces, i, p, type; 

    if ( HasIsXModWithObjects( pxwo ) and IsXModWithObjects( pxwo ) ) then 
        type := "crossed module with objects"; 
    elif ( HasIsPreXModWithObjects( pxwo ) 
           and IsPreXModWithObjects( pxwo ) ) then 
        type := "precrossed module with objects"; 
    elif ( HasIsCat1Groupoid( pxwo ) and IsCat1Groupoid( pxwo ) ) then 
        type := "cat1-groupoid"; 
    elif ( HasIsPreCat1Groupoid( pxwo ) 
           and IsPreCat1Groupoid( pxwo ) ) then 
        type := "pre-cat1-groupoid"; 
    else 
        type := "2dgroup with objects"; 
    fi;
    if ( HasIsSinglePiece( pxwo ) and IsSinglePiece( pxwo ) ) then
        Print( "single piece ", type, "\n" );
        Print( "  source groupoid:\n    " );
        Print( Source( pxwo ), "\n" ); 
        Print( "  and range groupoid:\n    " ); 
        Print( Range( pxwo ) ); 
        return; 
    else 
        pieces := Pieces( pxwo ); 
        len := Length( pieces ); 
        if ( HasIsHomogeneousDomainWithObjects( pxwo ) 
             and IsHomogeneousDomainWithObjects( pxwo ) ) then
            Print( "homogeneous 2dgroup with objects:\n" );
        else
            Print( "2dgroup with ", len, " pieces:\n" ); 
        fi; 
    fi;
    if ForAll( pieces, function ( p )
          return HasName( p );
          end ) then
        Print( pieces );
    else
        for i in [ 1 .. len-1 ] do
            p := pieces[i];
            Print( i, ":  ", p, "\n" );
        od;
        Print( len, ":  ", pieces[len] );
    fi;
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

