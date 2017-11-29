##############################################################################
##
#W  gpd2obj.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##  Standard error messages

XMODOBJ_CONSTRUCTORS := Concatenation( 
    "The standard operations which construct a (pre)xmod with objects are:\n",
    "1.  SinglePiecePreXModWithObjects( (pre)xmod, objects, isdiscrete );\n",
    "2.  DomainWithSingleObject( (pre)xmod, single object );\n",
    "3.  UnionOfPieces( list of (pre)xmods with objects );\n",
    "4.  PreXModWithObjects( one of the previous parameter options );" );

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
         and  ForAll( arg[1], G -> IsPreXModWithObjects(G) ) ) then
        Info( InfoXMod, 2, "ByUnion" );
        return UnionOfPieces( arg[1] );
    # prexmod plus singler object 
    elif ( ( nargs = 2 ) and IsPreXMod( arg[1] ) and IsObject( arg[2] ) ) then
        Info( InfoXMod, 2, "prexmod plus single object" ); 
        return DomainWithSingleObject( arg[1], arg[2] );
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

    local spx, rpx, src, rng, gens, bpx, imbdy, i, a, im, bdy, 
          apx, AS, AR, AS0, obs0, imobs, fact, act, pxwo; 

    spx := Source( px ); 
    rpx := Range( px ); 
    pxwo := rec( 
        objects := obs, 
        prexmod := px, 
        rays := List( obs, function( o ) return One(rpx); end ) 
        ); 
    rng := SinglePieceGroupoid( rpx, obs ); 
    if isdisc then 
        src := HomogeneousDiscreteGroupoid( spx, obs );
    else 
        src := SinglePieceGroupoid( spx, obs ); 
    fi;
    ## construct the boundary 
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
    if isdisc then 
        bdy := GroupoidHomomorphismFromHomogeneousDiscrete(src,rng,gens,imbdy);
    else 
        bdy := GroupoidHomomorphismFromSinglePiece(src,rng,gens,imbdy); 
    fi; 
    ## construct the action 
    apx := XModAction( px ); 
    AS := AutomorphismGroupOfGroupoid( src ); 
    AR := AutomorphismGroupOfGroupoid( rng ); 
    AS0 := DomainWithSingleObject( AS, 0 ); 
    obs0 := List( obs, o -> 0 ); 
    fact := function(a) local i,j,g,imobs,aut1,autg,aut2,aut; 
                g := a![1]; 
                i := Position( obs, a![2] ); 
                j := Position( obs, a![3] ); 
                imobs := ShallowCopy( obs ); 
                imobs[i] := obs[j]; 
                imobs[j] := obs[i];
                aut1 := GroupoidAutomorphismByObjectPerm( src, imobs ); 
                autg := ImageElm( apx, g ); 
                aut2 := GroupoidAutomorphismByGroupAuto( src, autg ); 
                aut := aut1 * aut2; 
                return ArrowNC( true, aut, 0, 0 ); 
            end; 
    act := MappingWithObjectsByFunction( rng, AS0, fact, obs0 ); 
    ObjectifyWithAttributes( pxwo, PreXModWithObjectsType, 
        Is2DimensionalDomain, true, 
        IsSinglePieceDomain, true, 
        IsPreXModWithObjects, true, 
        ObjectList, obs, 
        Root2dGroup, px, 
        Source, src, 
        Range, rng, 
        Boundary, bdy, 
        XModAction, act, 
        IsDirectProductWithCompleteDigraphDomain, true ); 
    SetIsXModWithObjects( pxwo, IsXMod( px ) ); 
    #?  name := Name( pxwo ); 
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
