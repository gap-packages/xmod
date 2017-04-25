##############################################################################
##
#W  gp3map.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for 3dimensional-mappings: 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#F  PreCrossedSquareMorphism( <args> )
#O  PreCrossedSquareMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  PreCat2Morphism( <args> )
#O  PreCat2MorphismByMorphisms( <src>, <rng>, <up>, <dn> )
##
DeclareGlobalFunction( "PreCrossedSquareMorphism" );
DeclareOperation( "PreCrossedSquareMorphismByMorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism ] );
DeclareGlobalFunction( "PreCat2Morphism" );
DeclareOperation( "PreCat2MorphismByMorphisms",
    [ IsPreCat2Group, IsPreCat2Group, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism ] );

#############################################################################
##
#O  Make3DimensionalGroupMorphism( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
##
##  A 3d-group morphism is a quadruple of 2d-group homs
##
DeclareOperation( "Make3DimensionalGroupMorphism",
    [ Is3DimensionalGroup, Is3DimensionalGroup, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism ] );
							
DeclareOperation( "Make3DimensionalGroupMorphism",
    [ Is3DimensionalGroup, Is3DimensionalGroup, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism ] );

#############################################################################
##
#F  CrossedSquareMorphism( <args> )
#O  CrossedSquareMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  Cat2Morphism( <args> )
#O  Cat2MorphismByMorphisms( <src>, <rng>, <up>, <dn> )
#O  InclusionMorphism3DimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "CrossedSquareMorphism" );
DeclareOperation( "CrossedSquareMorphismByMorphisms",
    [ IsCrossedSquare, IsCrossedSquare, Is2DimensionalGroupMorphism, 
      Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism ] );
DeclareGlobalFunction( "Cat2Morphism" );
DeclareOperation( "Cat2MorphismByMorphisms",
    [ IsCat2Group, IsCat2Group, Is2DimensionalGroupMorphism, Is2DimensionalGroupMorphism ] );
DeclareOperation( "InclusionMorphism3DimensionalDomains", 
    [ Is3DimensionalDomain, Is3DimensionalDomain ] );

#############################################################################
##
#O  InnerAutomorphismCrossedSquare( <XM>, <r> )
#O  InnerAutomorphismCat2Group( <C1G>, <r> )
##
DeclareOperation( "InnerAutomorphismCrossedSquare",
    [ IsPreCrossedSquare, IsMultiplicativeElementWithInverse ] );
DeclareOperation( "InnerAutomorphismCat2Group",
    [ IsPreCat2Group, IsMultiplicativeElementWithInverse ] );

#############################################################################
##
#A  CrossedSquareMorphismOfCat2Morphism( <mor> )
#O  CrossedSquareMorphismByCat2Morphism( <mor> )
#A  Cat2MorphismOfCrossedSquareMorphism( <mor> )
#O  Cat2MorphismByCrossedSquareMorphism( <mor> )
##
DeclareAttribute( "CrossedSquareMorphismOfCat2Morphism", IsCat2Morphism );
DeclareOperation( "CrossedSquareMorphismByCat2Morphism", [ IsCat2Morphism ] );
DeclareAttribute( "Cat2MorphismOfCrossedSquareMorphism", 
    IsCrossedSquareMorphism );
DeclareOperation( "Cat2MorphismByCrossedSquareMorphism", 
    [ IsCrossedSquareMorphism ] );

#############################################################################
##
#P  IsEndomorphism3DimensionalDomain( <mor> )
#P  IsAutomorphism3DimensionalDomain( <mor> )
##
DeclareProperty( "IsEndomorphism3DimensionalDomain", Is3DimensionalMapping );
DeclareProperty( "IsAutomorphism3DimensionalDomain", Is3DimensionalMapping );

#############################################################################
##
#E  gp3map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
