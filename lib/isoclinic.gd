#############################################################################
##
#W  isoclinic.gd              GAP4 package `XMod'                Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 07/10/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al 
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
#O IntersectionSubXMod  . . . . .intersection of subcrossed modules SH and RK
##
DeclareOperation( "IntersectionSubXMod", [  IsXMod, IsXMod, IsXMod ] );

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
#O LowerCentralSeriesOfXMod  . . . . . . the lower central series of an xmod
##
DeclareAttribute( "LowerCentralSeriesOfXMod", IsXMod );

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
#A NilpotencyClass2dGroup . . . . nilpotency degree of an xmod or cat1-group 
## 
DeclareProperty( "IsNilpotent2dGroup", Is2dGroup );
DeclareAttribute( "NilpotencyClass2dGroup", Is2dGroup );


############################################################################# 
#####                FUNCTIONS FOR ISOCLINISM OF GROUPS                 ##### 
############################################################################# 

#############################################################################
##
#P IsStemGroup . . . check that the centre is a subgroup of the derived group
#O AllStemGroupFamilies . . . list of all IdGroup's of stem groups of chosen order 
#A MiddleLength . . . 
## 
DeclareProperty( "IsStemGroup", IsGroup );
DeclareOperation( "AllStemGroupIds", [ IsPosInt ] );
DeclareOperation( "AllStemGroupFamilies", [ IsPosInt ] );
DeclareAttribute( "CentralQuotient", IsGroup ); 
DeclareAttribute( "CentralQuotientHomomorphism", IsGroup ); 
DeclareAttribute( "MiddleLength", IsGroup ); 
DeclareOperation( "Isoclinism", [ IsGroup, IsGroup ] );
DeclareOperation( "AreIsoclinicGroups", [ IsGroup, IsGroup ] );
DeclareAttribute( "IsoclinicStemGroup", IsGroup );


############################################################################# 
#####                FUNCTIONS FOR ISOCLINISM OF GROUPS                 ##### 
############################################################################# 

#############################################################################
##
#O AreIsoclinicXMods  . . . . . . check that two crossed modules are isoclinic
##
DeclareOperation( "AreIsoclinicXMods", [ IsXMod, IsXMod ] );

#############################################################################
##
#O IsomorphismXMods  . . . . . check that two crossed modules are isomorphic
##
DeclareOperation( "IsomorphismXMods", [  Is2dGroup, Is2dGroup ] );

#############################################################################
##
#O AllPreXMods  . . . . . . all precrossed modules in the given order interval
##
DeclareOperation( "AllPreXMods", [  IsInt, IsInt ] );

#############################################################################
##
#F AllXMods
#O AllXModsWithGroups  . . . . . . . . . all xmods with given source and range
##
DeclareGlobalFunction( "AllXMods" );
DeclareOperation( "AllXModsWithGroups", [ IsGroup, IsGroup ] );

#############################################################################
##
#O IsoclinicXModFamily  . . . . . . all xmods in list isoclinic to given xmod
##
DeclareOperation( "IsoclinicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O IsomorphicXModFamily  . . . . . . . . . all crossed modules in the list which are isomorphic to the crossed module
##
DeclareOperation( "IsomorphicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O IsoAllXMods . . . . . . . . . . . . all crossed modules up to isomorphism
##
DeclareOperation( "IsoAllXMods", [  IsList ] );

#############################################################################
##
#O RankXMod  . . . . . . . . . the rank of the crossed module
##
DeclareOperation( "RankXMod", [ Is2dGroup ] );

#############################################################################
##
#O TableRowXMod  . . . . table row for isoclinism families of crossed modules
##
DeclareOperation( "TableRowXMod", [ Is2dGroup, IsList ] );
