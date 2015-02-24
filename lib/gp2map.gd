##############################################################################
##
#W  gp2map.gd                   GAP4 package `XMod'              Chris Wensley
#W                                                                 & Murat Alp
##  version 2.32, 29/01/2015 
##
#Y  Copyright (C) 2001-2015, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##
##  This file contains implementations of 2dMappings for crossed modules and 
##  cat1-groups.
##

#############################################################################
##
#F  PreXModMorphism( <args> )
#O  PreXModMorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
#F  PreCat1Morphism( <args> )
#O  PreCat1MorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
##
DeclareGlobalFunction( "PreXModMorphism" );
DeclareOperation( "PreXModMorphismByHoms",
    [ IsPreXMod, IsPreXMod, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareGlobalFunction( "PreCat1Morphism" );
DeclareOperation( "PreCat1MorphismByHoms",
    [ IsPreCat1, IsPreCat1, IsGroupHomomorphism, IsGroupHomomorphism ] );

#############################################################################
##
#O  Make2dGroupMorphism( <src>, <rng>, <srchom>, <rnghom> )
##
##  A pre-crossed module or pre-cat1-group morphism is a pair of group homs
##
DeclareOperation( "Make2dGroupMorphism",
    [ Is2dGroup, Is2dGroup, IsGroupHomomorphism, IsGroupHomomorphism ] );

#############################################################################
##
#F  XModMorphism( <args> )
#O  XModMorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
#F  Cat1Morphism( <args> )
#O  Cat1MorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
#O  InclusionMorphism2dDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "XModMorphism" );
DeclareOperation( "XModMorphismByHoms",
    [ IsXMod, IsXMod, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareGlobalFunction( "Cat1Morphism" );
DeclareOperation( "Cat1MorphismByHoms",
    [ IsCat1, IsCat1, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "InclusionMorphism2dDomains", [ Is2dDomain, Is2dDomain ] );

#############################################################################
##
#O  InnerAutomorphismXMod( <XM>, <r> )
#O  InnerAutomorphismCat1( <C1G>, <r> )
##
DeclareOperation( "InnerAutomorphismXMod",
    [ IsPreXMod, IsMultiplicativeElementWithInverse ] );
DeclareOperation( "InnerAutomorphismCat1",
    [ IsPreCat1, IsMultiplicativeElementWithInverse ] );

#############################################################################
##
#P  IsEndomorphism2dDomain( <mor> )
#P  IsAutomorphism2dDomain( <mor> )
##
DeclareProperty( "IsEndomorphism2dDomain", Is2dMapping );
DeclareProperty( "IsAutomorphism2dDomain", Is2dMapping );

#############################################################################
##
#P  IsPermPreXModMorphism( <mor> )
#P  IsPermPreCat1Morphism( <mor> )
##
DeclareProperty( "IsPermPreXModMorphism", IsPreXModMorphism );
DeclareProperty( "IsPermPreCat1Morphism", IsPreCat1Morphism );

#############################################################################
##
#A  ReverseIsomorphism( <PCG> )
#O  CompositionMorphism( <mor2>, <mor1> )
##
DeclareAttribute( "ReverseIsomorphism", IsPreCat1 );
DeclareOperation( "CompositionMorphism", [ Is2dMapping, Is2dMapping ] );

#############################################################################
##
#P  IsSourceMorphism( <mor> )
#O  PreXModBySourceHom( <mor> )
##
DeclareProperty( "IsSourceMorphism", IsPreXModMorphism );
DeclareOperation( "PreXModBySourceHom", [ IsPreXModMorphism ] );

#############################################################################
##
#A  XModMorphismOfCat1Morphism( <mor> )
#O  XModMorphismByCat1Morphism( <mor> )
#A  Cat1MorphismOfXModMorphism( <mor> )
#O  Cat1MorphismByXModMorphism( <mor> )
##
DeclareAttribute( "XModMorphismOfCat1Morphism", IsCat1Morphism );
DeclareOperation( "XModMorphismByCat1Morphism", [ IsCat1Morphism ] );
DeclareAttribute( "Cat1MorphismOfXModMorphism", IsXModMorphism );
DeclareOperation( "Cat1MorphismByXModMorphism", [ IsXModMorphism ] );

#############################################################################
##
#A  IsomorphismPermPreXMod( <pre-xmod> )
#A  IsomorphismFpPreXMod( <pre-xmod> )
#A  IsomorphismPcPreXMod( <pre-xmod> )
##
DeclareAttribute( "IsomorphismPermPreXMod", IsPreXMod );
DeclareAttribute( "IsomorphismFpPreXMod", IsPreXMod );
DeclareAttribute( "IsomorphismPcPreXMod", IsPreXMod );

#############################################################################
##
#A  IsomorphismPermPreCat1( <pre-cat1> )
#A  IsomorphismFpPreCat1( <pre-cat1> )
#A  IsomorphismPcPreCat1( <pre-cat1> )
##
DeclareAttribute( "IsomorphismPermPreCat1", IsPreCat1 );
DeclareAttribute( "IsomorphismFpPreCat1", IsPreCat1 );
DeclareAttribute( "IsomorphismPcPreCat1", IsPreCat1 );

#############################################################################
##
#A  IsomorphismXModByNormalSubgroup( <xmod> )
##
DeclareAttribute( "IsomorphismXModByNormalSubgroup", IsXMod );

#############################################################################
##
#O  PreXModIsomorphismByIsomorphisms( <pre-xmod>, <sigma>, <rho> )
#O  PreCat1IsomorphismByIsomorphisms( <pre-cat1>, <sigma>, <rho> )
#F  SmallerDegreePerm2dGroup( <args> )
#A  SmallerDegreePermPreXMod( <XM> )
##
DeclareOperation( "PreXModIsomorphismByIsomorphisms", 
    [ IsPreXMod, IsBijective, IsBijective ] );
DeclareOperation( "PreCat1IsomorphismByIsomorphisms", 
    [ IsPreCat1, IsBijective, IsBijective ] );
DeclareGlobalFunction( "SmallerDegreePerm2dGroup" );
DeclareAttribute( "SmallerDegreePermPreXMod", IsPreXMod );

#############################################################################
##
#O  IsomorphismPreCat1s( <precat1>, <precat1> )
##
DeclareOperation( "IsomorphismPreCat1s", [ IsPreCat1, IsPreCat1 ] );

#############################################################################
##
#E  gp2map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
