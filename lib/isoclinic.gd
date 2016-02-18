#############################################################################
##
#W  isoclinic.gd              GAP4 package `XMod'                Alper Odabas
#W                                                               & Enver Uslu
#Y  Copyright (C) 2001-2016, Chris Wensley et al 
##

#############################################################################
##
#A  FixedPointSubgroupXMod . . . . . . . . {s in S | s^r = s for all r in R}
#A  StabilizerSubgroupXMod . . . . . . . . {r in R | s^r = s for all s in S}
##
DeclareOperation( "FixedPointSubgroupXMod", [ IsPreXMod, IsGroup, IsGroup ] );
DeclareOperation( "StabilizerSubgroupXMod", [ IsPreXMod, IsGroup, IsGroup ] );

#############################################################################
##
#O Displacement(  )
#A DisplacementSubgroup( XM )
#O CrossActionSubgroup( XM )
##
DeclareOperation( "Displacement", [ IsGroupHomomorphism, IsObject, IsObject ] );
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
#O FactorXMod  . . . . . . . . . . . . . . . . . the quotient crossed module
#O NaturalMorphismByNormalSubXMod . . . . . . . . the quotient xmod morphism
##
DeclareOperation( "FactorXMod", [  IsXMod, IsXMod ] );
DeclareOperation( "NaturalMorphismByNormalSubXMod", [  IsXMod, IsXMod ] );

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
#O IsAbelian2dGroup  
#O IsAspherical2dGroup  
#O IsSimplyConnected2dGroup 
#O IsFaithful2dGroup 
##
DeclareProperty( "IsAbelian2dGroup", Is2dGroup );
DeclareProperty( "IsAspherical2dGroup", Is2dGroup );
DeclareProperty( "IsSimplyConnected2dGroup", Is2dGroup );
DeclareProperty( "IsFaithful2dGroup", Is2dGroup );

#############################################################################
##
#P IsNilpotent2dGroup . . . . . check that an xmod or cat1-group is nilpotent
#A NilpotencyClassOf2dGroup  . . . nilpotency degree of an xmod or cat1-group 
## 
DeclareProperty( "IsNilpotent2dGroup", Is2dGroup );
DeclareAttribute( "NilpotencyClassOf2dGroup", Is2dGroup );

#############################################################################
##
#O IsomorphismXMods  . . . . . check that two crossed modules are isomorphic
##
DeclareOperation( "IsomorphismXMods", [  Is2dGroup, Is2dGroup ] );

#############################################################################
##
#F AllXMods
#O AllXModsWithGroups  . . . . . . . . . all xmods with given source and range
##
DeclareGlobalFunction( "AllXMods" );
DeclareOperation( "AllXModsWithGroups", [ IsGroup, IsGroup ] );
DeclareOperation( "AllXModsWithGroups0", [ IsGroup, IsGroup ] );
DeclareOperation( "AllXModsWithGroups1", [ IsGroup, IsGroup ] );
DeclareOperation( "AllXModsWithGroups2", [ IsGroup, IsGroup ] );
DeclareOperation( "AllXModsWithGroups3", [ IsGroup, IsGroup ] );


############################################################################# 
#####                FUNCTIONS FOR ISOCLINISM OF GROUPS                 ##### 
############################################################################# 

#############################################################################
##
#P IsStemDomain . . check that the centre is a subgroup of the derived domain
#O AllStemGroupFamilies . . . . . all IdGroups of stem groups of chosen order 
## 
DeclareProperty( "IsStemDomain", IsGroup );
DeclareOperation( "AllStemGroupIds", [ IsPosInt ] );
DeclareOperation( "AllStemGroupFamilies", [ IsPosInt ] );
DeclareAttribute( "CentralQuotient", IsGroup ); 

#############################################################################
##
#A IsoclinicStemDomain  . . . . . . . . . . find a stem group for the group G 
#O Isoclinism . . . find a homomorphism between the stem groups of two groups
#O AreIsoclinicDomains . . . does an isoclinism exist between two (2d-)groups
##
DeclareAttribute( "IsoclinicStemDomain", IsGroup );
DeclareOperation( "Isoclinism", [ IsGroup, IsGroup ] );
DeclareOperation( "AreIsoclinicDomains", [ IsDomain, IsDomain ] );


############################################################################# 
#####           FUNCTIONS FOR ISOCLINISM OF CROSSED MODULES             ##### 
############################################################################# 

#############################################################################
##
#O IsoclinicXModFamily  . . . . . . all xmods in list isoclinic to given xmod
##
DeclareOperation( "IsoclinicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O IsomorphicXModFamily  
## . . . . . . . all xmods in the list which are isomorphic to the given xmod
##
DeclareOperation( "IsomorphicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O AllXModsUpToIsomorphism . . . . . . all crossed modules up to isomorphism
##
DeclareOperation( "AllXModsUpToIsomorphism", [ IsList ] );

#############################################################################
##
#A IsoclinicMiddleLength . .  the middle length of a group or crossed module
#A IsoclinicRank   . . . . . . . . . . the rank of a group or crossed module
##
DeclareAttribute( "IsoclinicMiddleLength", IsGroup ); 
DeclareAttribute( "IsoclinicRank", IsGroup );

#############################################################################
##
#O TableRowXMod  . . . . table row for isoclinism families of crossed modules
##
DeclareOperation( "TableRowXMod", [ Is2dGroup, IsList ] );
