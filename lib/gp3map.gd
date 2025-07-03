##############################################################################
##
#W  gp3map.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for 3dimensional-mappings: 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

#############################################################################
##
#O  PreCrossedSquareMorphismByPreXModMorphisms( <src>, <rng>, <list> )
#O  PreCrossedSquareMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
#F  PreCat2GroupMorphism( <args> )
#O  PreCat2GroupMorphismByPreCat1GroupMorphisms( <src>, <rng>, <upm>, <ltm> )
#O  PreCat2GroupMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
##
DeclareGlobalFunction( "PreCrossedSquareMorphism" );
DeclareOperation( "PreCrossedSquareMorphismByPreXModMorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ] );
DeclareOperation( "PreCrossedSquareMorphismByGroupHomomorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ] );
DeclareGlobalFunction( "PreCat2GroupMorphism" );
DeclareOperation( "PreCat2GroupMorphismByPreCat1GroupMorphisms",
    [ IsPreCat2Group, IsPreCat2Group, IsPreCat1GroupMorphism, 
      IsPreCat1GroupMorphism ] );
DeclareOperation( "PreCat2GroupMorphismByGroupHomomorphisms",
    [ IsPreCat2Group, IsPreCat2Group, IsList ] );

#############################################################################
##
#F  CrossedSquareMorphism( <args> )
#O  CrossedSquareMorphismByXModMorphisms( <src>, <rng>, <list> )
#O  CrossedSquareMorphismByGroupHomomorphisms( <src>, <rng>, <list> )
#F  Cat2GroupMorphism( <args> )
#O  Cat2GroupMorphismByCat1GroupMorphisms( <src>, <rng>, <upm>, <ltm> )
#O  Cat2GroupMorphismByGroupHomomorphisms( <src>, <rng>, <homs> )
#O  InclusionMorphismHigherDimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "CrossedSquareMorphism" );
DeclareOperation( "CrossedSquareMorphismByXModMorphisms",
    [ IsCrossedSquare, IsCrossedSquare, IsList ] );
DeclareOperation( "CrossedSquareMorphismByGroupHomomorphisms",
    [ IsCrossedSquare, IsCrossedSquare, IsList ] );
DeclareGlobalFunction( "Cat2GroupMorphism" );
DeclareOperation( "Cat2GroupMorphismByCat1GroupMorphisms",
    [ IsCat2Group, IsCat2Group, IsCat1GroupMorphism, IsCat1GroupMorphism ] );
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
#O  IsomorphismPreCat2GroupsNoTranspose( <precat2>, <precat2> )
#O  IsomorphismCat2GroupsNoTranspose( <cat2>, <cat2> )     ?? is this needed ??
#O  IsomorphismPreCat2Groups( <precat2>, <precat2> )
#O  IsomorphismCat2Groups( <cat2>, <cat2> )
##
DeclareOperation( "IsomorphismPreCat2GroupsNoTranspose", 
    [ IsPreCat2Group, IsPreCat2Group ] ); 
DeclareOperation( "IsomorphismCat2GroupsNoTranspose", 
    [ IsCat2Group, IsCat2Group ] );
DeclareOperation( "IsomorphismPreCat2Groups", 
    [ IsPreCat2Group, IsPreCat2Group ] );
DeclareOperation( "IsomorphismCat2Groups", 
    [ IsCat2Group, IsCat2Group ] );

#############################################################################
##
#O  AllCat2GroupMorphisms( <cat2>, <cat2> ) 
## 
DeclareOperation( "AllCat2GroupMorphisms", [ IsCat2Group, IsCat2Group ] ); 
