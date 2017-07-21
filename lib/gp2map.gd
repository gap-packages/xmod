##############################################################################
##
#W  gp2map.gd                   GAP4 package `XMod'              Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##
##  This file contains implementations of 2DimensionalMappings 
##  for crossed modules and cat1-groups.
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
    [ IsPreCat1Group, IsPreCat1Group, IsGroupHomomorphism, 
      IsGroupHomomorphism ] );

#############################################################################
##
#O  Is2DimensionalGroupMorphismData( <list> )
#O  Make2DimensionalGroupMorphism( <list> )
##
##  A pre-crossed module or pre-cat1-group morphism is a pair of commuting 
##  group homomorphisms: at this stage actions not checked 
##
DeclareOperation( "Is2DimensionalGroupMorphismData", [ IsList ] );
DeclareOperation( "Make2DimensionalGroupMorphism", [ IsList ] );

#############################################################################
##
#F  XModMorphism( <args> )
#O  XModMorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
#F  Cat1Morphism( <args> )
#O  Cat1MorphismByHoms( <src>, <rng>, <srchom>, <rnghom> )
#O  InclusionMorphism2DimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "XModMorphism" );
DeclareOperation( "XModMorphismByHoms",
    [ IsXMod, IsXMod, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareGlobalFunction( "Cat1Morphism" );
DeclareOperation( "Cat1MorphismByHoms",
    [ IsCat1Group, IsCat1Group, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "InclusionMorphism2DimensionalDomains", 
    [ Is2DimensionalDomain, Is2DimensionalDomain ] );

#############################################################################
##
#O  InnerAutomorphismXMod( <XM>, <r> )
#O  InnerAutomorphismCat1Group( <C1G>, <r> )
##
DeclareOperation( "InnerAutomorphismXMod",
    [ IsPreXMod, IsMultiplicativeElementWithInverse ] );
DeclareOperation( "InnerAutomorphismCat1Group",
    [ IsPreCat1Group, IsMultiplicativeElementWithInverse ] );

#############################################################################
##
#P  IsEndomorphism2DimensionalDomain( <mor> )
#P  IsAutomorphism2DimensionalDomain( <mor> )
##
DeclareProperty( "IsEndomorphism2DimensionalDomain", Is2DimensionalMapping );
DeclareProperty( "IsAutomorphism2DimensionalDomain", Is2DimensionalMapping );

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
DeclareAttribute( "ReverseIsomorphism", IsPreCat1Group );
DeclareOperation( "CompositionMorphism", [ Is2DimensionalMapping, Is2DimensionalMapping ] );

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
#A  Cat1MorphismOfXModMorphism( <mor> )
##
DeclareAttribute( "XModMorphismOfCat1Morphism", IsCat1Morphism );
DeclareAttribute( "Cat1MorphismOfXModMorphism", IsXModMorphism );

#############################################################################
##
#O  IsomorphismByIsomorphisms( <n-dim-obj>, <list> )
#A  IsomorphismPerm2DimensionalGroup( <obj> )
#A  IsomorphismFp2DimensionalGroup( <obj> )
#A  IsomorphismPc2DimensionalGroup( <obj> )
##
DeclareOperation( "IsomorphismByIsomorphisms", [ IsObject, IsList ] );
DeclareAttribute( "IsomorphismPerm2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "IsomorphismFp2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "IsomorphismPc2DimensionalGroup", Is2DimensionalGroup );

#############################################################################
##
#A  IsomorphismXModByNormalSubgroup( <xmod> )
##
DeclareAttribute( "IsomorphismXModByNormalSubgroup", IsXMod );

#############################################################################
##
#F  SmallerDegreePerm2DimensionalGroup( <args> )
#A  SmallerDegreePermPreXMod( <XM> )
##
DeclareGlobalFunction( "SmallerDegreePerm2DimensionalGroup" );
DeclareAttribute( "SmallerDegreePermPreXMod", IsPreXMod );

#############################################################################
##
#O  IsomorphismPreCat1Groups( <precat1>, <precat1> )
##
DeclareOperation( "IsomorphismPreCat1Groups", 
    [ IsPreCat1Group, IsPreCat1Group ] );

#############################################################################
##
#E  gp2map.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
