##############################################################################
##
#W  gp3map.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for 3Dimensional Mappings for 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  IsPreCrossedSquareMorphism      check the axioms for a pre-crossed square
##
InstallMethod( IsPreCrossedSquareMorphism,
    "generic method for pre-crossed module homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PS, QS, homs, upmor, ltmor, dnmor, rtmor, ok;

    PS := Source( mor );
    QS := Range( mor );
    homs := ListOfHomomorphisms( mor ); 
    if not ( IsPreCrossedSquare( PS ) and IsPreCrossedSquare( QS ) ) then
        return false;
    fi;
    ### (1) check that the morphisms commute
    upmor := Up2DimensionalMorphism( mor );
    ltmor := Left2DimensionalMorphism( mor );
    dnmor := Down2DimensionalMorphism( mor );
    rtmor := Right2DimensionalMorphism( mor );
    ok := ( ( SourceHom( upmor ) = SourceHom( ltmor ) ) and
            ( RangeHom( upmor ) = SourceHom( rtmor ) ) and
            ( RangeHom( ltmor ) = SourceHom( dnmor ) ) and
            ( RangeHom( rtmor ) = RangeHom( dnmor ) ) );
    if not ok then
        return false;
    fi;
    ### (2) check the remaining axioms
    Info( InfoXMod, 1, 
        "Warning: IsPreCrossedSquareMorphism not fully implemented!" );
    return true;
end );

#############################################################################
##
#M  IsCrossedSquareMorphism
##
InstallMethod( IsCrossedSquareMorphism, 
    "generic method for pre-crossed square morphisms", true, 
    [ IsPreCrossedSquareMorphism ], 0,
function( mor )
    return ( IsCrossedSquare( Source( mor ) ) 
             and IsCrossedSquare(  Range( mor ) ) );
end );

InstallMethod( IsCrossedSquareMorphism, "generic method for 3d-mappings", true,
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local ispre;

    ispre := IsPreCrossedSquareMorphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCrossedSquare( Source( mor ) ) 
                 and IsCrossedSquare(  Range( mor ) ) );
    fi;
end );

#############################################################################
##
#M  IsPreCat2Morphism      check the axioms for a pre-cat2 
##
InstallMethod( IsPreCat2Morphism,
    "generic method for pre-cat2 homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PC, QC, upmor, dnmor, ok, d1, d2, u1, u2, G1, G2, P1, P2, p1, q1, 
          comp1, G11, G12, P11, P12, p2, q2, comp2;

    if not ( IsPreCatnMorphism( mor ) and ( HigherDimension = 2 ) ) then
        return true;
    else
        return false;
    fi;
end );

#############################################################################
##
#M  IsCat2Morphism
##
InstallMethod( IsCat2Morphism, "generic method for cat2 morphisms", true, 
    [ IsPreCat2Morphism ], 0,
function( mor )
    return ( IsCat2Group( Source( mor ) ) and IsCat2Group(  Range( mor ) ) );
end );

InstallMethod( IsCat2Morphism, "generic method for 3d-mappings", true,
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local ispre;

    ispre := IsPreCat2Morphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCat2Group( Source(mor) ) and IsCat2Group( Range(mor) ) );
    fi;
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . . . . test if two morphisms of 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-morphisms", IsIdenticalObj, 
    [ IsPreCrossedSquareMorphism, IsPreCrossedSquareMorphism ], 0,
function ( mor, phi )
    return ( ( Source( mor ) = Source( phi ) )
         and ( Range( mor ) = Range( phi ) )
         and ( Up2DimensionalMorphism( mor ) 
               = Up2DimensionalMorphism( phi ) )
         and ( Left2DimensionalMorphism( mor ) 
               = Left2DimensionalMorphism( phi ) )
         and ( Right2DimensionalMorphism( mor ) 
               = Right2DimensionalMorphism( phi ) )
         and ( Down2DimensionalMorphism( mor ) 
               = Down2DimensionalMorphism( phi ) ) );
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . for a HigherDimensionalMapping
##
InstallOtherMethod( MappingGeneratorsImages, "for a HigherDimensionalMapping", 
    true, [ IsPreCrossedSquareMorphism ], 0,
function( map )
    return [ MappingGeneratorsImages( Up2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Left2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Right2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Down2DimensionalMorphism( map ) ) ];
end );

#############################################################################
##
#M  Name                                              for a pre-CrossedSquare
##
InstallMethod( Name, "method for a 3d-mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local nsrc, nrng, name;

    if HasName( Source( mor ) ) then
        nsrc := Name( Source( mor ) );
    else
        nsrc := "[..]";
    fi;
    if HasName( Range( mor ) ) then
        nrng := Name( Range( mor ) );
    else
        nrng := "[..]";
    fi;
    name := Concatenation( "[", nsrc, " => ", nrng, "]" );
    SetName( mor, name );
    return name;
end );

#############################################################################
##
#F  Display3dMorphism( <mor> ) . . . . . print details of a 3d-group morphism 
##
InstallMethod( Display3dMorphism, "display a morphism of 3d-groups", true,
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local dim, upmor, downmor, P, Q;

    P := Source( mor );
    Q := Range( mor );
    dim := HigherDimension( P ); 
    if not ( dim = 3 ) then 
        Error( "expecting a 3-dimensional group morphism\n" ); 
    fi;
    upmor := Up2DimensionalMorphism( mor );
    downmor := Down2DimensionalMorphism( mor );
    if ( HasIsPreCrossedSquareMorphism( mor ) and 
         IsPreCrossedSquareMorphism( mor ) ) then 
        if ( HasIsCrossedSquareMorphism( mor ) and 
            IsCrossedSquareMorphism( mor ) ) then
            Print( "Morphism of crossed squares :- \n" );
        else
            Print( "Morphism of pre-crossed squares :- \n" );
        fi; 
    else 
        if ( HasIsCat2Morphism( mor ) and IsCat2Morphism( mor ) ) then
            Print( "Morphism of cat2-groups :- \n" );
        else
            Print( "Morphism of pre-cat2-groups :- \n" );
        fi; 
    fi; 
    if HasName( P ) then
        Print( ": Source = ", Name( P ), "\n" );
    else
        Print( ": Source has ", P, "\n" );
    fi;
    if HasName( Q ) then
        Print( ": Range = ", Name( Q ), "\n" );
    else
        Print( ": Range has ", Q, "\n" );
    fi;
    if HasOrder( mor ) then
        Print( ":     order = ", Order( mor ), "\n" );
    fi;
    Print( ":    up-left: ", MappingGeneratorsImages(SourceHom(upmor)),"\n");
    Print( ":   up-right: ", MappingGeneratorsImages(RangeHom(upmor)),"\n");
    Print( ":  down-left: ", MappingGeneratorsImages(SourceHom(downmor)),"\n");
    Print( ": down-right: ", MappingGeneratorsImages(RangeHom(downmor)),"\n");
end ); 

##############################################################################
##
#M  InclusionMorphismHigherDimensionalDomains( <obj>, <sub> )
##
InstallMethod( InclusionMorphismHigherDimensionalDomains, 
    "one n-dimensional object in another", true, 
    [ IsHigherDimensionalDomain, IsHigherDimensionalDomain ], 0,
function( obj, sub )

    local up, lt, rt, dn;

    up := InclusionMorphism2DimensionalDomains( Up2DimensionalGroup(obj), 
              Up2DimensionalGroup(sub) );
    dn := InclusionMorphism2DimensionalDomains( Down2DimensionalGroup(obj), 
              Down2DimensionalGroup(sub) );    
    if IsPreCrossedSquare( obj ) then
        lt := InclusionMorphism2DimensionalDomains( Left2DimensionalGroup(obj), 
              Left2DimensionalGroup(sub) );
        rt := InclusionMorphism2DimensionalDomains( Right2DimensionalGroup(obj), 
              Right2DimensionalGroup(sub) );
        return PreCrossedSquareMorphismByMorphisms( sub, obj, [up,lt,rt,dn] );
    elif IsPreCat2Group( obj ) then
        return PreCat2MorphismByMorphisms( sub, obj, [ up, dn ] );
    else
        return fail;
    fi;
end );

##############################################################################
##
#F  CrossedSquareMorphism( <src>, <rng>, <list> ) 
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( CrossedSquareMorphism, function( arg )

    local nargs;
    nargs := Length( arg );

    # two CrossedSquares and four homomorphisms
    if ( ( nargs = 3 ) 
             and IsCrossedSquare( arg[1] ) and IsCrossedSquare( arg[2]) 
             and ForAll( arg[3], m -> IsXModMorphism(m) ) ) then 
        return  CrossedSquareMorphismByMorphisms( arg[1], arg[2], arg[3] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, 
        "usage: CrossedSquareMorphism( src, rng, list of maps );" );
    return fail;
end );

##############################################################################
##
#F  Cat2Morphism( <src>, <rng>, <up>, <dn> ) . . cat2 morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( Cat2Morphism, function( arg )

    local nargs;

    nargs := Length( arg );
    # two cat2-groups and two homomorphisms
    if ( ( nargs = 3 ) and IsCat2Group( arg[1] ) and IsCat2Group( arg[2])
        and IsCat1Morphism( arg[3][1] ) and IsCat1Morphism( arg[3][2] ) ) then
        return  Cat2MorphismByMorphisms( arg[1], arg[2], arg[3] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: Cat2Morphism( src, rng, list of maps );" );
    return fail;
end );

###############################################################################
##
#M  PreCrossedSquareMorphismByMorphisms( <src>, <rng>, <list of maps> ) 
##
InstallMethod( PreCrossedSquareMorphismByMorphisms,
    "for two pre-crossed squares and four pre-xmod morphisms,", true,
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ], 0,
function( src, rng, list )

    local filter, fam, mor, ok, nsrc, nrng, name;

    if not ForAll( list, m -> IsPreXModMorphism(m) ) then 
        Error( "third argument should be a list of pre-xmod-morphisms" ); 
    fi; 
    mor := MakeHigherDimensionalGroupMorphism( src, rng, list );
    if not IsPreCrossedSquareMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-crossed squares.\n" );
        return fail;
    fi;
    ok := IsCrossedSquareMorphism( mor );
    return mor;
end );

###############################################################################
##
#M  PreCat2MorphismByMorphisms( <src>, <rng>, <list> ) 
##
InstallMethod( PreCat2MorphismByMorphisms,
    "for two pre-cat2 and two pre-cat1 morphisms,", true,
    [ IsPreCat2Group, IsPreCat2Group, IsList ], 0,
function( src, rng, list )

    local filter, fam, mor, ok, nsrc, nrng, name;

    if not ForAll( list, m-> IsPreCat2Morphism(m) ) then 
        Error( "third argument should be a list of pre-cat2-morphisms" );
    fi;
    mor := MakeHigherDimensionalGroupMorphism( src, rng, list );
    if not IsPreCat2Morphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-cat2.\n" );
        return fail;
    fi;
    ok := IsCat2Morphism( mor );
    return mor;
end );

##############################################################################
##
#M  CrossedSquareMorphismByMorphisms( <Xs>, <Xr>, <list> )  
##
InstallMethod( CrossedSquareMorphismByMorphisms, 
    "for 2 CrossedSquares and 4 morphisms", true,
    [ IsCrossedSquare, IsCrossedSquare, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ForAll( list, m -> IsXModMorphism(m) ) then 
        Error( "third argument should be a list of xmod morphisms" ); 
    fi; 
    mor := PreCrossedSquareMorphismByMorphisms( src, rng, list );
    ok := IsCrossedSquareMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  Cat2MorphismByMorphisms( <Cs>, <Cr>, <list> ) . . make cat2-group mapping
##
InstallMethod( Cat2MorphismByMorphisms, "for two cat2-groups and 2 morphisms", 
    true, [ IsCat2Group, IsCat2Group, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ForAll( list, m -> IsCat1Morphism(m) ) then 
        Error( "third argument should be a list of cat1-morphisms" ); 
    fi; 
    mor := PreCat2MorphismByMorphisms( src, rng, list );
    ok := IsCat2Morphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#E  gp3map.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
