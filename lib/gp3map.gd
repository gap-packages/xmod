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
#O  PreCrossedSquareMorphismByMorphisms( <src>, <rng>, <list> )
#F  PreCat2Morphism( <args> )
#O  PreCat2MorphismByMorphisms( <src>, <rng>, <list> )
##
DeclareGlobalFunction( "PreCrossedSquareMorphism" );
DeclareOperation( "PreCrossedSquareMorphismByMorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ] );
DeclareGlobalFunction( "PreCat2Morphism" );
DeclareOperation( "PreCat2MorphismByMorphisms",
    [ IsPreCat2Group, IsPreCat2Group, IsList ] );

#############################################################################
##
#F  CrossedSquareMorphism( <args> )
#O  CrossedSquareMorphismByMorphisms( <src>, <rng>, <list> )
#F  Cat2Morphism( <args> )
#O  Cat2MorphismByMorphisms( <src>, <rng>, <list> )
#O  InclusionMorphismHigherDimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "CrossedSquareMorphism" );
DeclareOperation( "CrossedSquareMorphismByMorphisms",
    [ IsCrossedSquare, IsCrossedSquare, IsList ] );
DeclareGlobalFunction( "Cat2Morphism" );
DeclareOperation( "Cat2MorphismByMorphisms",
    [ IsCat2Group, IsCat2Group, IsList ] );
DeclareOperation( "InclusionMorphismHigherDimensionalDomains", 
    [ IsHigherDimensionalDomain, IsHigherDimensionalDomain ] );

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
#A  Cat2MorphismOfCrossedSquareMorphism( <mor> )
##
DeclareAttribute( "CrossedSquareMorphismOfCat2Morphism", IsCat2Morphism );
DeclareAttribute( "Cat2MorphismOfCrossedSquareMorphism", 
    IsCrossedSquareMorphism );

#############################################################################
##
#O  Display3dMorphism( <mor )
##
DeclareOperation( "Display3dMorphism", [ IsHigherDimensionalMapping ] );

#############################################################################
##
#E  gp3map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
