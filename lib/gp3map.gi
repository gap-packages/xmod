##############################################################################
##
#W  gp3map.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for 3Dimensional Mappings for 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  Make3DimensionalGroupMorphism( <src>, <rng>, <up>, <lt>, <rt>, <dn> ) 
##
InstallMethod( Make3DimensionalGroupMorphism,
    "for two 3d-objects and four 2d-morphisms", true, 
    [ Is3DimensionalGroup, Is3DimensionalGroup, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism ], 0,
function( src, rng, upmor, leftmor, rightmor, downmor )

    local  filter, fam, mor;

    fam := Family3DimensionalGroupMorphism;
    filter := Is3DimensionalMappingRep;
    mor := rec();
    ObjectifyWithAttributes( mor, 
      NewType( fam, filter ),
      Source, src,
      Range, rng,
      Up2DimensionalMorphism, upmor,
      Left2DimensionalMorphism, leftmor, 
      Right2DimensionalMorphism, rightmor, 
      Down2DimensionalMorphism, downmor );
    return mor; 
end );

##############################################################################
##
#M  Make3DimensionalGroupMorphism( <src>, <rng>, <up>, <lt>, <rt>, <dn> ) 
##
InstallMethod( Make3DimensionalGroupMorphism,
    "for two 3d-objects and four 2d-morphisms", true, 
    [ Is3DimensionalGroup, Is3DimensionalGroup, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism ], 0,
function( src, rng, upmor, downmor )

    local  filter, fam, mor;

    fam := Family3DimensionalGroupMorphism;
    filter := Is3DimensionalMappingRep;
    mor := rec();
    ObjectifyWithAttributes( mor, 
      NewType( fam, filter ),
      Source, src,
      Range, rng,
      Up2DimensionalMorphism, upmor,
      Down2DimensionalMorphism, downmor );
    return mor; 
end );


#############################################################################
##
#M  IsPreCrossedSquareMorphism      check the axioms for a pre-crossed square
##
InstallMethod( IsPreCrossedSquareMorphism,
    "generic method for pre-crossed module homomorphisms", true, 
    [ Is3DimensionalGroupMorphism ], 0,
function( mor )

    local  PS, QS, upmor, ltmor, dnmor, rtmor, ok;

    PS := Source( mor );
    QS := Range( mor );
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
    [ Is3DimensionalGroupMorphism ], 0,
function( mor )
    local  ispre;
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
    [ Is3DimensionalGroupMorphism ], 0,
function( mor )

    local  PC, QC, upmor, dnmor, ok, d1, d2, u1, u2,G1,G2,P1,P2,p1,q1,comp1,G11,G12,P11,P12,p2,q2,comp2;

    PC := Source( mor );
    QC := Range( mor );
    if not ( IsPreCat2Group( PC ) and IsPreCat2Group( QC ) ) then
        return false;
    fi;
    
    upmor := Up2DimensionalMorphism( mor );
    dnmor := Down2DimensionalMorphism( mor );
    if not ( IsPreCat1Morphism( upmor ) and IsPreCat1Morphism( dnmor ) ) then
        return false;
    fi;
    
    u1 := Up2DimensionalGroup( PC );
    d1 := Down2DimensionalGroup( PC );
    u2 := Up2DimensionalGroup( QC );
    d2 := Down2DimensionalGroup( QC );
    
    if not (( Source( RangeHom( upmor ) ) = Range( u1 ) ) and 
            ( Source( RangeHom( dnmor ) ) = Range( d1 ) ) and 
            ( Range( RangeHom( upmor ) ) = Range( u2 ) ) and 
            ( Range( RangeHom( dnmor ) ) = Range( d2 ) )) then
        return false;
    fi;
	
	# check that equality of SourceHoms
	
	G1 := Source(SourceHom( upmor ));
	G2 := Range(SourceHom( upmor ));
	P1 := Image(IsomorphismPermGroup(G1));
	P2 := Image(IsomorphismPermGroup(G2));
	p1 := IsomorphismGroups(P1,G1);
	q1 := IsomorphismGroups(G2,P2);
	comp1 := p1 * SourceHom( upmor ) * q1;
	
	G11 := Source(SourceHom( dnmor ));
	G12 := Range(SourceHom( dnmor ));
	P11 := Image(IsomorphismPermGroup(G11));
	P12 := Image(IsomorphismPermGroup(G12));
	p2 := IsomorphismGroups(P11,G11);
	q2 := IsomorphismGroups(G12,P12);
	comp2 := p2 * SourceHom( dnmor ) * q2;
	
    
#  Issue => ok := ( SourceHom( upmor ) = SourceHom( dnmor ) );
#  The cause of the wrong result : SmallGroup(n,m) <> SmallGroup(n,m) 
	ok := ( comp1 = comp2 );
    if not ok then

        return false;
    fi;
    return true;
end );

#############################################################################
##
#M  IsCat2Morphism
##
InstallMethod( IsCat2Morphism, 
    "generic method for cat2 morphisms", true, 
    [ IsPreCrossedSquareMorphism ], 0,
function( mor )
    return ( IsCat2Group( Source( mor ) ) and IsCat2Group(  Range( mor ) ) );
end );

InstallMethod( IsCat2Morphism, "generic method for 3d-mappings", true,
    [ Is3DimensionalGroupMorphism ], 0,
function( mor )
    local  ispre;
    ispre := IsPreCat2Morphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCat2Group( Source( mor ) ) and IsCat2Group(  Range( mor ) ) );
    fi;
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . . . . test if two morphisms of 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-morphisms",
    IsIdenticalObj, [ Is3DimensionalMapping, Is3DimensionalMapping ], 0,
    function ( mor, phi )
    
    if ( IsPreCat2Morphism( mor ) and IsPreCat2Morphism( phi ) ) then
        return ( ( Source( mor ) = Source( phi ) )
             and ( Range( mor ) = Range( phi ) )
             and ( Up2DimensionalMorphism( mor ) 
                   = Up2DimensionalMorphism( phi ) )
             and ( Down2DimensionalMorphism( mor ) 
                   = Down2DimensionalMorphism( phi ) ) );
    else
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
    fi;
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . . . . . . . .  for a 3DimensionalMapping
##
InstallOtherMethod( MappingGeneratorsImages, "for a 3DimensionalMapping", true,
    [ Is3DimensionalMapping ], 0,
    function( map )
    if ( IsPreCat2Morphism( map ) ) then
        return [ MappingGeneratorsImages( Up2DimensionalMorphism( map ) ), 
                 MappingGeneratorsImages( Down2DimensionalMorphism( map ) ) ];
    else
        return [ MappingGeneratorsImages( Up2DimensionalMorphism( map ) ), 
                 MappingGeneratorsImages( Left2DimensionalMorphism( map ) ), 
                 MappingGeneratorsImages( Right2DimensionalMorphism( map ) ), 
                 MappingGeneratorsImages( Down2DimensionalMorphism( map ) ) ];
    fi;
end );

#############################################################################
##
#M  Name                                              for a pre-CrossedSquare
##
InstallMethod( Name, "method for a 3d-mapping", true, 
    [ Is3DimensionalMapping ], 0,
function( mor )

    local  nsrc, nrng, name;

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
#F  Display( <mor> ) . . . . . . . . . print details of a 3d-group moprphism 
##
InstallMethod( Display, "display a morphism of 3d-groups", true,
    [ Is3DimensionalGroupMorphism ], 0,
    function( mor )

    local  upmor, downmor, P, Q;

    P := Source( mor );
    Q := Range( mor );
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
        Print( ":    Source = ", Name( P ), "\n" );
    else
        Print( ":    Source = \n", P, "\n" );
    fi;
    if HasName( Q ) then
        Print( ":     Range = ", Name( Q ), "\n" );
    else
        Print( ":     Range = \n", Q, "\n" );
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
#M  IdentityMapping( <obj> )
##
InstallOtherMethod( IdentityMapping, "for 3d-group object", true,
    [ Is3DimensionalGroup ], 0,
function( obj )

    local  up, lt, dn, rt;

    up := IdentityMapping( Up2DimensionalGroup( obj ) );
    dn := IdentityMapping( Down2DimensionalGroup( obj ) );
    if ( HasIsPreCrossedSquare( obj ) and IsPreCrossedSquare( obj ) ) then
        lt := IdentityMapping( Left2DimensionalGroup( obj ) );
        rt := IdentityMapping( Right2DimensionalGroup( obj ) );
        return PreCrossedSquareMorphismByMorphisms( obj, obj, up, lt, rt, dn );
    elif ( HasIsPreCat2Group( obj ) and IsPreCat2Group( obj ) ) then
        return PreCat2MorphismByMorphisms( obj, obj, up, dn );
    else
        return fail;
    fi;
end );
    
##############################################################################
##
#M  InclusionMorphism3DimensionalDomains( <obj>, <sub> )
##
InstallMethod( InclusionMorphism3DimensionalDomains, "of one 3d-object in another", 
    true, [ Is3DimensionalDomain, Is3DimensionalDomain ], 0,
function( obj, sub )

    local  up, lt, rt, dn;

    up := InclusionMorphism2DimensionalDomains( Up2DimensionalGroup(obj), 
              Up2DimensionalGroup(sub) );
    dn := InclusionMorphism2DimensionalDomains( Down2DimensionalGroup(obj), 
              Down2DimensionalGroup(sub) );    
    if IsPreCrossedSquare( obj ) then
        lt := InclusionMorphism2DimensionalDomains( Left2DimensionalGroup(obj), 
              Left2DimensionalGroup(sub) );
        rt := InclusionMorphism2DimensionalDomains( Right2DimensionalGroup(obj), 
              Right2DimensionalGroup(sub) );
        return PreCrossedSquareMorphismByMorphisms( sub, obj, up, lt, rt, dn );
    elif IsPreCat2Group( obj ) then
        return PreCat2MorphismByMorphisms( sub, obj, up, dn );
    else
        return fail;
    fi;
end );

##############################################################################
##
#F  CrossedSquareMorphism( <src>, <rng>, <up>, <lt>, <rt>, <dn> ) . . x-square morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( CrossedSquareMorphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two CrossedSquares and two homomorphisms
    if ( ( nargs = 6 ) and IsCrossedSquare( arg[1] ) and IsCrossedSquare( arg[2])
             and IsXModMorphism( arg[3] ) and IsXModMorphism( arg[4] )
             and IsXModMorphism( arg[5] ) and IsXModMorphism( arg[6] ) ) then
        return  CrossedSquareMorphismByMorphisms( arg[1], arg[2], arg[3], 
                                        arg[4], arg[5], arg[6] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: CrossedSquareMorphism( src, rng, up, lt, rt, dn );" );
    return fail;
end );

##############################################################################
##
#F  Cat2Morphism( <src>, <rng>, <up>, <dn> ) . . cat2 morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( Cat2Morphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two cat2-groups and two homomorphisms
    if ( ( nargs = 4 ) and IsCat2Group( arg[1] ) and IsCat2Group( arg[2])
             and IsCat1Morphism( arg[3] ) and IsCat1Morphism( arg[4] ) ) then
        return  Cat2MorphismByMorphisms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: Cat2Morphism( src, rng, up, dn );" );
    return fail;
end );

###############################################################################
##
#M  PreCrossedSquareMorphismByMorphisms( <src>, <rng>, <up>, <left>, <right>, <down> ) 
##
InstallMethod( PreCrossedSquareMorphismByMorphisms,
    "for two pre-crossed squares and four pre-xmod morphisms,", true,
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsPreXModMorphism, IsPreXModMorphism, 
      IsPreXModMorphism, IsPreXModMorphism ], 0,
function( src, rng, up, lt, rt, dn )

    local  filter, fam, mor, ok, nsrc, nrng, name;

    mor := Make3DimensionalGroupMorphism( src, rng, up, lt, rt, dn );
    if not IsPreCrossedSquareMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-crossed squares.\n" );
        return fail;
    fi;
    ok := IsCrossedSquareMorphism( mor );
    return mor;
end );

###############################################################################
##
#M  PreCat2MorphismByMorphisms( <src>, <rng>, <up>, <down> ) 
##
InstallMethod( PreCat2MorphismByMorphisms,
    "for two pre-cat2 and two pre-cat1 morphisms,", true,
    [ IsPreCat2Group, IsPreCat2Group, IsPreCat1Morphism, IsPreCat1Morphism ], 0,
function( src, rng, up, dn )

    local  filter, fam, mor, ok, nsrc, nrng, name;

    mor := Make3DimensionalGroupMorphism( src, rng, up, dn );
    if not IsPreCat2Morphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-cat2.\n" );
        return fail;
    fi;
    ok := IsCrossedSquareMorphism( mor );
    return mor;
end );

##############################################################################
##
#M  CrossedSquareMorphismByMorphisms( <Xs>, <Xr>, <up>, <lt>, <rt>, <dn> )  make CrossedSquare map
##
InstallMethod( CrossedSquareMorphismByMorphisms, "for 2 CrossedSquares and 4 morphisms", true,
    [ IsCrossedSquare, IsCrossedSquare, IsXModMorphism, IsXModMorphism, IsXModMorphism, 
      IsXModMorphism ], 0,
function( src, rng, up, lt, rt, dn )

    local  mor, ok;
    mor := PreCrossedSquareMorphismByMorphisms( src, rng, up, lt, rt, dn );
    ok := IsCrossedSquareMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  Cat2MorphismByMorphisms( <Cs>, <Cr>, <up>, <dn> )  make cat2-group mapping
##
InstallMethod( Cat2MorphismByMorphisms, "for two cat2-groups and 2 morphisms", 
    true, [ IsCat2Group, IsCat2Group, IsCat1Morphism, IsCat1Morphism ], 0,
function( src, rng, up, dn )

    local  mor, ok;
    mor := PreCat2MorphismByMorphisms( src, rng, up, dn );
    ok := IsCat2Morphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CompositionMorphism  . . . . . . . . . . . . . . . . . for two 3d-mappings
##
InstallOtherMethod( CompositionMorphism, "generic method for 3d-mappings",
    IsIdenticalObj, [ Is3DimensionalMapping, Is3DimensionalMapping ], 0,
function( m2, m1 )

    local  up, lt, rt, dn, comp, ok;

    if not ( Range( m1 ) = Source( m2 ) ) then
        Info( InfoXMod, 2, "Range(m1) <> Source(m2)" );
        return fail;
    fi;
    up := CompositionMapping( Up2DimensionalMorphism(m2), 
                              Up2DimensionalMorphism(m1) );
    dn := CompositionMapping( Down2DimensionalMorphism(m2), 
                              Down2DimensionalMorphism(m1) );
    if IsPreCat2Group( Source( m1 ) ) then
        comp := Make3DimensionalMapping( Source( m1 ), Range( m2 ), up, dn );
        if ( IsPreCat2Morphism( m1 ) and IsPreCat2Morphism( m2 ) ) then
            SetIsPreCat2Morphism( comp, true );
        fi;
        if ( IsCat2Morphism( m1 ) and IsCat2Morphism( m2 ) ) then
            SetIsCat2Morphism( comp, true );
        fi;
    else
        lt := CompositionMapping( Left2DimensionalMorphism(m2), 
                                  Left2DimensionalMorphism(m1) );
        rt := CompositionMapping( Right2DimensionalMorphism(m2), 
                                  Right2DimensionalMorphism(m1) );
        comp := Make3DimensionalMapping( Source( m1 ), Range( m2 ), up, lt, rt, dn );
        if ( IsPreCrossedSquareMorphism( m1 ) and IsPreCrossedSquareMorphism( m2 ) ) then
            SetIsPreCrossedSquareMorphism( comp, true );
        fi;
        if ( IsCrossedSquareMorphism( m1 ) 
             and IsCrossedSquareMorphism( m2 ) ) then
            SetIsCrossedSquareMorphism( comp, true );
        fi;
    fi;
    return comp;
end );

##############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . . . . . . . . . for a 3d-mapping
##
InstallOtherMethod( Order, "generic method for 3d-mapping", true, 
    [ Is3DimensionalMapping ], 0,
function( mor )

    local ok;

    if not ( IsEndomorphism3DimensionalDomain(mor) and IsBijective(mor) ) then
       Info( InfoXMod, 2, "Parameter is not an automorphism" );
       return fail;
    fi;
    if ( IsPreCat2Morphism( mor ) ) then
        return  Lcm( Order( Up2DimensionalMorphism(mor) ), 
                     Order( Down2DimensionalMorphism(mor) ) );
    else
        return Lcm( Order( Up2DimensionalMorphism(mor) ), 
                    Order( Left2DimensionalMorphism(mor) ),  
                    Order( Down2DimensionalMorphism(mor) ), 
                    Order( Right2DimensionalMorphism(mor) ) );
    fi;
end );

##############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . . . . . . . . .  for a 3d-mapping
##
InstallOtherMethod( IsInjective,
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
        ok := ( IsInjective( Up2DimensionalMorphism( map ) ) and
                IsInjective( Down2DimensionalMorphism( map ) ) );
    else
        ok := ( IsInjective( Up2DimensionalMorphism( map ) ) and
                IsInjective( Left2DimensionalMorphism( map ) ) and
                IsInjective( Right2DimensionalMorphism( map ) ) and
                IsInjective( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . . . . . . . . . for a 3d-mapping
##
InstallOtherMethod( IsSurjective,
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
        ok := ( IsSurjective( Up2DimensionalMorphism( map ) ) and
                IsSurjective( Down2DimensionalMorphism( map ) ) );
    else
        ok := ( IsSurjective( Up2DimensionalMorphism( map ) ) and
                IsSurjective( Left2DimensionalMorphism( map ) ) and
                IsSurjective( Right2DimensionalMorphism( map ) ) and
                IsSurjective( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . . . . . . . . . for a 3d-mapping
##
InstallOtherMethod( IsSingleValued,
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
        ok := ( IsSingleValued( Up2DimensionalMorphism( map ) ) and
                IsSingleValued( Down2DimensionalMorphism( map ) ) );
    else
        ok := ( IsSingleValued( Up2DimensionalMorphism( map ) ) and
                IsSingleValued( Left2DimensionalMorphism( map ) ) and
                IsSingleValued( Right2DimensionalMorphism( map ) ) and
                IsSingleValued( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . . . . . . . . .  for a 3d-mapping
##
InstallOtherMethod( IsTotal,
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
        ok := ( IsTotal( Up2DimensionalMorphism( map ) ) and
                IsTotal( Down2DimensionalMorphism( map ) ) );
    else
        ok := ( IsTotal( Up2DimensionalMorphism( map ) ) and
                IsTotal( Left2DimensionalMorphism( map ) ) and
                IsTotal( Right2DimensionalMorphism( map ) ) and
                IsTotal( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . . . . . . . .  for a 3d-mapping
##
InstallOtherMethod( IsBijective,
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
        ok := ( IsBijective( Up2DimensionalMorphism( map ) ) and
                IsBijective( Down2DimensionalMorphism( map ) ) );
    else
        ok := ( IsBijective( Up2DimensionalMorphism( map ) ) and
                IsBijective( Left2DimensionalMorphism( map ) ) and
                IsBijective( Right2DimensionalMorphism( map ) ) and
                IsBijective( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsEndomorphism3DimensionalDomain( map ) . . . . . . . . . . . . . . for a 3d-mapping
##  temporary fix 08/01/04  ---  need to check correctness
#M  IsAutomorphism3DimensionalDomain( map ) . . . . . . . . . . . . . . for a 3d-mapping
##
InstallMethod( IsEndomorphism3DimensionalDomain, 
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
function( map )

    local ok;
    
    if ( IsPreCat2Morphism( map ) ) then
      ok := 
        ( IsEndomorphism2DimensionalDomain( Up2DimensionalMorphism( map ) ) 
      and IsEndomorphism2DimensionalDomain( Down2DimensionalMorphism( map ) ) );
    else
      ok := 
        ( IsEndomorphism2DimensionalDomain( Up2DimensionalMorphism( map ) ) 
      and IsEndomorphism2DimensionalDomain( Left2DimensionalMorphism( map ) ) 
      and IsEndomorphism2DimensionalDomain( Right2DimensionalMorphism( map ) ) 
      and IsEndomorphism2DimensionalDomain( Down2DimensionalMorphism( map ) ) );
    fi;

    if not ok then
        return false;
    fi;    
    return true;
end );

InstallMethod( IsAutomorphism3DimensionalDomain, 
    "method for a 3d-mapping", true, [ Is3DimensionalMapping ], 0,
    map -> IsEndomorphism3DimensionalDomain( map ) and IsBijective( map ) );

##############################################################################
##
#E  gp3map.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
