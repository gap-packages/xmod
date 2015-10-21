##############################################################################
##
#W  gp3map.gd                   GAP4 package `XMod'              Chris Wensley
##
##  version 2.43, 21/10/2015 
##
##  This file declares functions for 3d-mappings: (pre-)crossed squares and 
##  (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#F  PreCrossedSquareMorphism( <args> )
#O  PreCrossedSquareMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  PreCat2Morphism( <args> )
#O  PreCat2MorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
##
DeclareGlobalFunction( "PreCrossedSquareMorphism" );
DeclareOperation( "PreCrossedSquareMorphismByMorphisms",
    [ IsPreCrossedSquare, IsPreCrossedSquare, Is2dGroupMorphism, 
      Is2dGroupMorphism, Is2dGroupMorphism, Is2dGroupMorphism ] );
DeclareGlobalFunction( "PreCat2Morphism" );
DeclareOperation( "PreCat2MorphismByMorphisms",
    [ IsPreCat2, IsPreCat2, Is2dGroupMorphism, Is2dGroupMorphism, 
                            Is2dGroupMorphism, Is2dGroupMorphism ] );

#############################################################################
##
#O  Make3dGroupMorphism( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
##
##  A 3d-group morphism is a quadruple of 2d-group homs
##
DeclareOperation( "Make3dGroupMorphism",
    [ Is3dGroup, Is3dGroup, Is2dGroupMorphism, Is2dGroupMorphism, 
                            Is2dGroupMorphism, Is2dGroupMorphism ] );

#############################################################################
##
#F  CrossedSquareMorphism( <args> )
#O  CrossedSquareMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  Cat2Morphism( <args> )
#O  Cat2MorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#O  InclusionMorphism3dDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "CrossedSquareMorphism" );
DeclareOperation( "CrossedSquareMorphismByMorphisms",
    [ IsCrossedSquare, IsCrossedSquare, Is2dGroupMorphism, 
      Is2dGroupMorphism, Is2dGroupMorphism, Is2dGroupMorphism ] );
DeclareGlobalFunction( "Cat2Morphism" );
DeclareOperation( "Cat2MorphismByMorphisms",
    [ IsCat2, IsCat2, Is2dGroupMorphism, Is2dGroupMorphism, 
                      Is2dGroupMorphism, Is2dGroupMorphism ] );
DeclareOperation( "InclusionMorphism3dDomains", [ Is3dDomain, Is3dDomain ] );

#############################################################################
##
#O  InnerAutomorphismCrossedSquare( <XM>, <r> )
#O  InnerAutomorphismCat2( <C1G>, <r> )
##
DeclareOperation( "InnerAutomorphismCrossedSquare",
    [ IsPreCrossedSquare, IsMultiplicativeElementWithInverse ] );
DeclareOperation( "InnerAutomorphismCat2",
    [ IsPreCat2, IsMultiplicativeElementWithInverse ] );

#############################################################################
##
#A  CrossedSquareMorphismOfCat2Morphism( <mor> )
#O  CrossedSquareMorphismByCat2Morphism( <mor> )
#A  Cat2MorphismOfCrossedSquareMorphism( <mor> )
#O  Cat2MorphismByCrossedSquareMorphism( <mor> )
##
DeclareAttribute( "CrossedSquareMorphismOfCat2Morphism", IsCat2Morphism );
DeclareOperation( "CrossedSquareMorphismByCat2Morphism", [ IsCat2Morphism ] );
DeclareAttribute( "Cat2MorphismOfCrossedSquareMorphism", IsCrossedSquareMorphism );
DeclareOperation( "Cat2MorphismByCrossedSquareMorphism", [ IsCrossedSquareMorphism ] );

#############################################################################
##
#P  IsEndomorphism3dDomain( <mor> )
#P  IsAutomorphism3dDomain( <mor> )
##
DeclareProperty( "IsEndomorphism3dDomain", Is3dMapping );
DeclareProperty( "IsAutomorphism3dDomain", Is3dMapping );

#############################################################################
##
#E  gp3map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
