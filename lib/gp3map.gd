##############################################################################
##
#W  gp3map.gd                   GAP4 package `XMod'              Chris Wensley
##
##  version 2.31, 08/11/2014 
##
##  This file declares functions for 3d-mappings: (pre-)crossed squares and 
##  (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#F  PreXSqMorphism( <args> )
#O  PreXSqMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  PreCat2Morphism( <args> )
#O  PreCat2MorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
##
DeclareGlobalFunction( "PreXSqMorphism" );
DeclareOperation( "PreXSqMorphismByMorphisms",
    [ IsPreXSq, IsPreXSq, Is2dGroupMorphism, Is2dGroupMorphism, 
                          Is2dGroupMorphism, Is2dGroupMorphism ] );
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
#F  XSqMorphism( <args> )
#O  XSqMorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#F  Cat2Morphism( <args> )
#O  Cat2MorphismByMorphisms( <src>, <rng>, <up>, <lt>, <rt>, <dn> )
#O  InclusionMorphism3dDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "XSqMorphism" );
DeclareOperation( "XSqMorphismByMorphisms",
    [ IsXSq, IsXSq, Is2dGroupMorphism, Is2dGroupMorphism, 
                    Is2dGroupMorphism, Is2dGroupMorphism ] );
DeclareGlobalFunction( "Cat2Morphism" );
DeclareOperation( "Cat2MorphismByMorphisms",
    [ IsCat2, IsCat2, Is2dGroupMorphism, Is2dGroupMorphism, 
                      Is2dGroupMorphism, Is2dGroupMorphism ] );
DeclareOperation( "InclusionMorphism3dDomains", [ Is3dDomain, Is3dDomain ] );

#############################################################################
##
#O  InnerAutomorphismXSq( <XM>, <r> )
#O  InnerAutomorphismCat2( <C1G>, <r> )
##
DeclareOperation( "InnerAutomorphismXSq",
    [ IsPreXSq, IsMultiplicativeElementWithInverse ] );
DeclareOperation( "InnerAutomorphismCat2",
    [ IsPreCat2, IsMultiplicativeElementWithInverse ] );

#############################################################################
##
#A  XSqMorphismOfCat2Morphism( <mor> )
#O  XSqMorphismByCat2Morphism( <mor> )
#A  Cat2MorphismOfXSqMorphism( <mor> )
#O  Cat2MorphismByXSqMorphism( <mor> )
##
DeclareAttribute( "XSqMorphismOfCat2Morphism", IsCat2Morphism );
DeclareOperation( "XSqMorphismByCat2Morphism", [ IsCat2Morphism ] );
DeclareAttribute( "Cat2MorphismOfXSqMorphism", IsXSqMorphism );
DeclareOperation( "Cat2MorphismByXSqMorphism", [ IsXSqMorphism ] );

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
