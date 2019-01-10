##############################################################################
##
#W  gp3map.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for 3dimensional-mappings: 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#O  PreCrossedSquareMorphismByPreXModMorphisms( <src>, <rng>, <list> )
#O  PreCrossedSquareMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
#O  PreCat2GroupMorphismByPreCat1GroupMorphisms( <src>, <rng>, <list> )
#O  PreCat2GroupMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
##
DeclareGlobalFunction( "PreCrossedSquareMorphism" );
DeclareOperation( "PreCrossedSquareMorphismByPreXModMorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ] );
DeclareOperation( "PreCrossedSquareMorphismByGroupHomomorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ] );
DeclareGlobalFunction( "PreCat2GroupMorphism" );
DeclareOperation( "PreCat2GroupMorphismByPreCat1GroupMorphisms",
    [ IsPreCat2Group, IsPreCat2Group, IsList ] );
DeclareOperation( "PreCat2GroupMorphismByGroupHomomorphisms",
    [ IsPreCat2Group, IsPreCat2Group, IsList ] );

#############################################################################
##
#F  CrossedSquareMorphism( <args> )
#O  CrossedSquareMorphismByXModMorphisms( <src>, <rng>, <list> )
#O  CrossedSquareMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
#F  Cat2GroupMorphism( <args> )
#O  Cat2GroupMorphismByCat1GroupMorphisms( <src>, <rng>, <list> )
#O  Cat2GroupMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
#O  InclusionMorphismHigherDimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "CrossedSquareMorphism" );
DeclareOperation( "CrossedSquareMorphismByXModMorphisms",
    [ IsCrossedSquare, IsCrossedSquare, IsList ] );
DeclareOperation( "CrossedSquareMorphismByGroupHomomorphisms",
    [ IsCrossedSquare, IsCrossedSquare, IsList ] );
DeclareGlobalFunction( "Cat2GroupMorphism" );
DeclareOperation( "Cat2GroupMorphismByCat1GroupMorphisms",
    [ IsCat2Group, IsCat2Group, IsList ] );
DeclareOperation( "Cat2GroupMorphismByGroupHomomorphisms",
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
#A  CrossedSquareMorphismOfCat2GroupMorphism( <mor> )
#A  Cat2GroupMorphismOfCrossedSquareMorphism( <mor> )
##
DeclareAttribute( "CrossedSquareMorphismOfCat2GroupMorphism", 
    IsCat2GroupMorphism );
DeclareAttribute( "Cat2GroupMorphismOfCrossedSquareMorphism", 
    IsCrossedSquareMorphism );

#############################################################################
##
#O  Display3DimensionalMorphism( <mor )
##
DeclareOperation( "Display3DimensionalMorphism", 
    [ IsHigherDimensionalMapping ] );

#############################################################################
##
#E  gp3map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
