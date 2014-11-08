##############################################################################
##
#W  dom2d3d.gi                 GAP4 package `XMod'               Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 


###########################  DOMAIN WITH OBJECTS  ########################### 

############################################################################# 
## 
#M  TypeOf2dDomain( <m2d> ) 
##
InstallMethod( TypeOf2dDomain, "for list of 2d-domains", true, [ IsList ], 0, 

    function( pieces ) 
    local  type; 
    ## type:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dom 
    type := 0; 
    return type; 
end );

#####################  MULT ELTS WITH OBJECTS  ############################## 

############################################################################# 
## 
#M  MultiplicativeElementWithTuple( <pxmod>, <tup>, <elt> ) 
#M  MultiplicativeElementWithTupleNC( <ispx>, <tup>, <elt> ) 
##
InstallMethod( MultiplicativeElementWithTupleNC, 
    "for boolean, dimension and element", true,  
    [ IsBool, IsHomogeneousList, IsMultiplicativeElement ], 
    0, 
    function( ok, d, e ) 
    local  obs, elt, fam;
    if ok then 
        fam := GroupElementWithTupleFamily; 
        elt := Objectify( 
            NewType( fam, IsGroupElementWithTuple ), [ d, e ] ); 
    else 
        fam := MultiplicativeElementWithTupleFamily; 
        elt := Objectify( 
            NewType( fam, IsMultiplicativeElementWithTuple ), [ d, e ] ); 
    fi; 
    return elt; 
end ); 

InstallMethod( MultiplicativeElementWithTuple, 
    "for general 2d-magma, element, tail and head", true, 
    [ Is2dGroup, IsHomogeneousList, IsMultiplicativeElement ], 0, 
    function( m2d, t, e ) 
    if ( t = [0] ) then 
        if not ( e in Range( m2d ) ) then 
            Error( "element not in the range of m2d" ); 
        fi; 
    elif ( t = [1] ) then 
        if not ( e in Source( m2d ) ) then 
            Error( "element not in source of m2d" ); 
        fi; 
    else 
        Error( "tuple t should be [0] or [1]" ); 
    fi; 
    return MultiplicativeElementWithTupleNC( true, t, e ); 
end );

#############################################################################
##
#M  PrintObj . . . . . . . . . . . . . . . . . . . for elements in a 2d-group 
#M  ViewObj  . . . . . . . . . . . . . . . . . . . for elements in a 2d-group
##
InstallMethod( PrintObj, "for an element in a 2d-group etc",
    [ IsMultiplicativeElementWithTuple ],
function ( e )
    Print( e![1], ":", e![2] );
end );

InstallMethod( ViewObj, "for an element in a 2d-group",
    [ IsMultiplicativeElementWithTuple ], PrintObj );

#############################################################################
##
#M  \=( <e1>, <e2> ) . . . . . . equality of elements in a 2d-group
##
InstallMethod( \=, "for two multiplicative 2d-elements", IsIdenticalObj, 
    [ IsMultiplicativeElementWithTuple, IsMultiplicativeElementWithTuple ], 0,
function( e1, e2 )
    return ForAll( [1..2], i -> ( e1![i] = e2![i] ) ); 
end );

############################################################################# 
## 
#M  \*( e1, e2 ) . . . . . . composition of elements in a 2d-magma 
## 
InstallMethod( \*, "for two elements in a 2d-group", IsIdenticalObj,
    [ IsMultiplicativeElementWithTuple, IsMultiplicativeElementWithTuple ], 
    0, 
    function( e1, e2 ) 
    local  d, e; 
    d := e1![1]; 
    if ( d <> e2![1] ) then 
        return fail; 
    fi; 
    e := e1![2] * e2![2]; 
    return MultiplicativeElementWithTupleNC( true, d, e );     
end );

#############################################################################
##
#M  Order( <e> )  . . . . . . . . . . . . . . . . . . . of a 2d-group element etc
##
InstallOtherMethod( Order, "for a multiplicative element ina  2d-group", 
    true, [ IsMultiplicativeElementWithTuple ], 0,
function( e )
    return Order( e![2] ); 
end );

#############################################################################
##
#M  GeneratorsOfMagma( <m2d> ). . . . . . . . . . . . . of a 2d-magma element
##
InstallOtherMethod( GeneratorsOfMagma, "for a multiplicative 2d-group", 
    true, [ Is2dGroup ], 0,
function( m2d )
    local  ok, gensrc, gens2, genrng, gens1; 
    ok := Is2dGroup( m2d ); 
    gensrc := GeneratorsOfMagma( Source( m2d ) ); 
    gens2 := List( gensrc, g -> MultiplicativeElementWithTupleNC(ok,[1],g) ); 
    genrng := GeneratorsOfMagma( Range( m2d ) ); 
    gens1 := List( genrng, g -> MultiplicativeElementWithTupleNC(ok,[0],g) ); 
    return Concatenation( gens2, gens1 ); 
end );

#############################################################################
##
#M  Iterator( <mwo> ) . . . . . . . . . . . . . . . . iterator for a 2d-group 
#M  Enumerator( <mwo> ) . . . . . . . . . . . . . . enumerator for a 2d-group 
##
InstallMethod( Iterator, "for a 2d-group", [ Is2dGroup ], 
function( g2d )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return IsDoneIterator( iter!.rngIterator );
            end, 
        NextIterator := function( iter )
            if ( iter!.tup = [1] ) then
                if IsDoneIterator( iter!.srcIterator ) then 
                    iter!.tup := [0]; 
                else 
                    iter!.elt := NextIterator( iter!.srcIterator );
                fi; 
            fi; 
            if ( iter!.tup = [0] ) then 
                iter!.elt := NextIterator( iter!.rngIterator ); 
            fi; 
            return MultiplicativeElementWithTupleNC(true,iter!.tup,iter!.elt );
            end, 
        ShallowCopy := iter -> 
            rec( srcIterator := ShallowCopy( iter!.srcIterator ), 
                 rngIterator := ShallowCopy( iter!.rngIterator ), 
                 elt := iter!.elt,
                 tup := iter!.tup ),
        srcIterator := Iterator( Source( g2d ) ), 
        rngIterator := Iterator( Range( g2d ) ), 
        elt := One( Source( g2d ) ), 
        tup := [1] ) );
end );

InstallMethod( Enumerator, "for a 2d-group", [ Is2dGroup ], 
function( g2d )
    Print( "#I  no Enumerator coded for 2d-groups: use Iterator instead\n" ); 
    return fail; 
end ); 

InstallMethod( EnumeratorSorted, "for a 2d-group", [Is2dGroup], Enumerator );


################################  MAGMAS  ################################### 

############################################################################# 
## 
#F  Make2dMagma( <mag>, <obs> ) 
##
InstallGlobalFunction( Make2dMagma, function( arg ) 

    local  obs, mag;
    Error( "Current usage: Make2dMagma( <src>, <rng> )" ); 
    return fail; 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . test if two 2d-magmas are equal
##
InstallMethod( \=, "for 2d-magmas", IsIdenticalObj,
    [ Is2dMagma, Is2dMagma ], 0, 
function ( m1, m2 ) 
    local  i, p1, p2;
    return fail; 
end );

#############################################################################
##
#M  GeneratorsOf2dMagma( <m2d> )  for a 2d-magma
##
InstallMethod( GeneratorsOf2dMagma, "for a 2d-magma", 
    true, [ Is2dMagma ], 0,
function( m2d )
    local  g;
    return fail; 
end );


#################################  SUBDOMAINS  ############################## 

#############################################################################
##
#F  IsSub2dDomain( <M>, <U> )
##
InstallMethod( IsSub2dDomain, "for two 2d-domains", true, 
    [ Is2dDomain, Is2dDomain ], 0, 
    function( D, U )

    local  compU, obj, p, ok; 
    return fail; 
end );


################################  SEMIGROUPS  ############################### 

############################################################################# 
## 
#F  Make2dSemigroup( <sgp>, <obs> ) 
##
InstallGlobalFunction( Make2dSemigroup, function( arg ) 

    local  obs, sgp; 
    return fail; 
end ); 


##################################  MONOIDS  ################################ 

############################################################################# 
## 
#F  Make2dMonoid( <mon>, <obs> ) 
##
InstallGlobalFunction( Make2dMonoid, function( arg ) 

    local  obs, mon;
    return fail; 
end ); 

#############################################################################
##
#E  dom2d3d.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
