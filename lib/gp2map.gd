##############################################################################
##
#W  gp2map.gd                   GAP4 package `XMod'              Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2021, Chris Wensley et al,  
##
##  This file contains implementations of 2DimensionalMappings 
##  for crossed modules and cat1-groups.
##

#############################################################################
##
#F  PreXModMorphism( <args> )
#O  PreXModMorphismByGroupHomomorphisms( <src>, <rng>, <srchom>, <rnghom> )
#F  PreCat1GroupMorphism( <args> )
#O  PreCat1GroupMorphismByGroupHomomorphisms( <src>, <rng>, <srchom>, <rnghom> )
##
DeclareGlobalFunction( "PreXModMorphism" );
DeclareOperation( "PreXModMorphismByGroupHomomorphisms",
    [ IsPreXMod, IsPreXMod, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareGlobalFunction( "PreCat1GroupMorphism" );
DeclareOperation( "PreCat1GroupMorphismByGroupHomomorphisms",
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
#O  XModMorphismByGroupHomomorphisms( <src>, <rng>, <srchom>, <rnghom> )
#F  Cat1GroupMorphism( <args> )
#O  Cat1GroupMorphismByGroupHomomorphisms( <src>, <rng>, <srchom>, <rnghom> )
#O  InclusionMorphism2DimensionalDomains( <obj>, <sub> )
##
DeclareGlobalFunction( "XModMorphism" );
DeclareOperation( "XModMorphismByGroupHomomorphisms",
    [ IsXMod, IsXMod, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareGlobalFunction( "Cat1GroupMorphism" );
DeclareOperation( "Cat1GroupMorphismByGroupHomomorphisms",
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
#P  IsPermPreCat1GroupMorphism( <mor> )
##
DeclareProperty( "IsPermPreXModMorphism", IsPreXModMorphism );
DeclareProperty( "IsPermPreCat1GroupMorphism", IsPreCat1GroupMorphism );

#############################################################################
##
#A  TransposeIsomorphism( <PCG> )
#O  CompositionMorphism( <mor2>, <mor1> )
##
DeclareAttribute( "TransposeIsomorphism", IsPreCat1Group );
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
#A  XModMorphismOfCat1GroupMorphism( <mor> )
#A  Cat1GroupMorphismOfXModMorphism( <mor> )
##
DeclareAttribute( "XModMorphismOfCat1GroupMorphism", IsCat1GroupMorphism );
DeclareAttribute( "Cat1GroupMorphismOfXModMorphism", IsXModMorphism );

#############################################################################
##
#O  IsomorphismByIsomorphisms( <n-dim-obj>, <list> )
#A  IsomorphismPerm2DimensionalGroup( <obj> )
#A  IsomorphismFp2DimensionalGroup( <obj> )
#A  IsomorphismPc2DimensionalGroup( <obj> )
#A  RegularActionHomomorphism2DimensionalGroup( <obj> )
##
DeclareOperation( "IsomorphismByIsomorphisms", [ IsObject, IsList ] );
DeclareAttribute( "IsomorphismPerm2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "IsomorphismFp2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "IsomorphismPc2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "RegularActionHomomorphism2DimensionalGroup", 
                       Is2DimensionalGroup ); 

#############################################################################
##
#A  IsomorphismXModByNormalSubgroup( <xmod> )
##
DeclareAttribute( "IsomorphismXModByNormalSubgroup", IsXMod );

#############################################################################
##
#A  SmallerDegreePermutationRepresentation2DimensionalGroup( <2dgp> )
#A  SmallerDegree2DimensionalGroup( <2dgp> )
##
DeclareAttribute( 
    "SmallerDegreePermutationRepresentation2DimensionalGroup", 
    IsPerm2DimensionalGroup );

#############################################################################
##
#O  IsomorphismPreCat1Groups( <precat1>, <precat1> )
#O  IsomorphismCat1Groups( <cat1>, <cat1> )
##
DeclareOperation( "IsomorphismPreCat1Groups", 
    [ IsPreCat1Group, IsPreCat1Group ] );
DeclareOperation( "IsomorphismCat1Groups", [ IsCat1Group, IsCat1Group ] );

#############################################################################
##
#A  MorphismOfPullback( <xmod> )
##
DeclareAttribute( "MorphismOfPullback", IsXMod );

#############################################################################
##
#O  AllCat1GroupMorphisms( <cat1>, <cat1> ) 
## 
DeclareOperation( "AllCat1GroupMorphisms", [ IsCat1Group, IsCat1Group ] ); 

##############################################################################
##
#O  SubQuasiIsomorphism( <cat1>, <show> )
#O  QuotientQuasiIsomorphism( <cat1>, <show> )
#O  QuasiIsomorphism( <2dgp>, <id>, <show> )
## 
DeclareOperation( "SubQuasiIsomorphism", [ IsCat1Group, IsBool ] );
DeclareOperation( "QuotientQuasiIsomorphism", [ IsCat1Group, IsBool ] );
DeclareOperation( "QuasiIsomorphism", [ Is2DimensionalGroup, IsList, IsBool ] );
