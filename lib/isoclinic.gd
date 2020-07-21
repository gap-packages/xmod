#############################################################################
##
#W  isoclinic.gd              GAP4 package `XMod'                Alper Odabas
#W                                                               & Enver Uslu
#Y  Copyright (C) 2001-2020, Chris Wensley et al 

#############################################################################
##
#A  FixedPointSubgroupXMod . . . . . . . . {s in S | s^r = s for all r in R}
#A  StabilizerSubgroupXMod . . . . . . . . {r in R | s^r = s for all s in S}
##
DeclareOperation( "FixedPointSubgroupXMod", [ IsPreXMod, IsGroup, IsGroup ] );
DeclareOperation( "StabilizerSubgroupXMod", [ IsPreXMod, IsGroup, IsGroup ] );

#############################################################################
##
#O Displacement( act, r, s )
#O DisplacementGroup( XM, R, S )
#A DisplacementSubgroup( XM )
#O CrossActionSubgroup( XM )
##
DeclareOperation( "Displacement", [ IsGroupHomomorphism, IsObject, IsObject ] );
DeclareOperation( "DisplacementGroup", [ IsXMod, IsGroup, IsGroup ] );
DeclareAttribute( "DisplacementSubgroup", IsXMod );
DeclareOperation( "CrossActionSubgroup", [ IsXMod, IsXMod, IsXMod ] ); 

#############################################################################
##
#A DerivedSubXMod  . . . . . . . . . the commutator of the crossed module
##
DeclareAttribute( "DerivedSubXMod", IsXMod );

#############################################################################
##
#O IntersectionSubXMods . . . . .intersection of subcrossed modules SH and RK
##
DeclareOperation( "IntersectionSubXMods", [  IsXMod, IsXMod, IsXMod ] );

#############################################################################
##
#O FactorPreXMod  . . . . . . . . . . . . . . the quotient precrossed module
#O NaturalMorphismByNormalSubPreXMod . . . . . the quotient prexmod morphism
##
DeclareOperation( "FactorPreXMod", [ IsPreXMod, IsPreXMod ] );
DeclareOperation( "NaturalMorphismByNormalSubPreXMod", 
    [ IsPreXMod, IsPreXMod ] );
DeclareAttribute( "ProjectionOfFactorPreXMod", IsPreXMod ); 
 
#############################################################################
##
#O CommutatorSubXMod  . . . . . . . commutator subxmod of two normal subxmods
##
DeclareOperation( "CommutatorSubXMod", [  IsXMod, IsXMod, IsXMod ] );

#############################################################################
##
#O  CentreXMod  . . . . . . . . . . . . . . . the center of a crossed module
##
DeclareAttribute( "CentreXMod", IsXMod );
DeclareSynonym( "CenterXMod", CentreXMod );

#############################################################################
##
#O IsAbelian2DimensionalGroup  
#O IsAspherical2DimensionalGroup  
#O IsSimplyConnected2DimensionalGroup 
#O IsFaithful2DimensionalGroup 
##
DeclareProperty( "IsAbelian2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsAspherical2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsSimplyConnected2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsFaithful2DimensionalGroup", Is2DimensionalGroup );

#############################################################################
##
#P IsNilpotent2DimensionalGroup . . check an xmod or cat1-group is nilpotent
#A NilpotencyClassOf2DimensionalGroup . nilpotency degree of xmod/cat1-group 
## 
DeclareProperty( "IsNilpotent2DimensionalGroup", Is2DimensionalGroup );
DeclareAttribute( "NilpotencyClassOf2DimensionalGroup", Is2DimensionalGroup );

#############################################################################
##
#O IsomorphismXMods  . . . . . check that two crossed modules are isomorphic
##
DeclareOperation( "IsomorphismXMods", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );

#############################################################################
##
#F AllXMods
#O AllXModsWithGroups  . . . . . . . . . all xmods with given source and range
##
DeclareGlobalFunction( "AllXMods" );
DeclareOperation( "AllXModsWithGroups", [ IsGroup, IsGroup ] );

############################################################################# 
#####                FUNCTIONS FOR ISOCLINISM OF GROUPS                 ##### 
############################################################################# 

#############################################################################
##
#P IsStemDomain . . check that the centre is a subgroup of the derived domain
#O AllStemGroupFamilies . . . . . all IdGroups of stem groups of chosen order 
## 
DeclareProperty( "IsStemDomain", IsGroup );
DeclareProperty( "IsStemDomain", IsXMod );
DeclareOperation( "AllStemGroupIds", [ IsPosInt ] );
DeclareOperation( "AllStemGroupFamilies", [ IsPosInt ] );
DeclareAttribute( "CentralQuotient", IsDomain ); 

#############################################################################
##
#A IsoclinicStemDomain  . . . . . . . . . . find a stem group for the group G 
#O Isoclinism . . . find a homomorphism between the stem groups of two groups
#O AreIsoclinicDomains . . . does an isoclinism exist between two (2d-)groups
##
DeclareAttribute( "IsoclinicStemDomain", IsGroup );
DeclareOperation( "Isoclinism", [ IsGroup, IsGroup ] );
DeclareOperation( "Isoclinism", [ IsXMod, IsXMod ] );
DeclareOperation( "AreIsoclinicDomains", [ IsDomain, IsDomain ] );


############################################################################# 
#####           FUNCTIONS FOR ISOCLINISM OF CROSSED MODULES             ##### 
############################################################################# 

#############################################################################
##
#O IsoclinicXModFamily  . . . . . . all xmods in list isoclinic to given xmod
##
DeclareOperation( "IsoclinicXModFamily", [  Is2DimensionalGroup, IsList ] );

#############################################################################
##
#O IsomorphicXModFamily  
## . . . . . . . all xmods in the list which are isomorphic to the given xmod
##
DeclareOperation( "IsomorphicXModFamily", [  Is2DimensionalGroup, IsList ] );

#############################################################################
##
#O AllXModsUpToIsomorphism . . . . . . all crossed modules up to isomorphism
##
DeclareOperation( "AllXModsUpToIsomorphism", [ IsGroup, IsGroup ] ); 
DeclareOperation( "IsomorphismClassRepresentatives2dGroups", [ IsList ] ); 

#############################################################################
##
#A IsoclinicMiddleLength . .  the middle length of a group or crossed module
#A IsoclinicRank   . . . . . . . . . . the rank of a group or crossed module
##
DeclareAttribute( "IsoclinicMiddleLength", IsGroup ); 
DeclareAttribute( "IsoclinicMiddleLength", Is2DimensionalGroup ); 
DeclareAttribute( "IsoclinicRank", IsGroup );
DeclareAttribute( "IsoclinicRank", Is2DimensionalGroup );

#############################################################################
##
#O TableRowXMod  . . . . table row for isoclinism families of crossed modules
##
DeclareOperation( "TableRowXMod", [ Is2DimensionalGroup, IsList ] );
