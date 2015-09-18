#############################################################################
##
#W  isoclinic.gd              GAP4 package `XMod'                Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 18/09/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al 
##

#############################################################################
##
#A  PreXModFixedPointSubgroup( <PM> ) 
##
DeclareAttribute( "PreXModFixedPointSubgroup", IsPreXMod );

#############################################################################
##
#A  PreXModStabilizer( <PM> ) 
##
DeclareAttribute( "PreXModStabilizer", IsPreXMod );

#############################################################################
##
#A DisplacementSubgroup( XM )
##
DeclareAttribute( "DisplacementSubgroup", IsXMod );

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
##
DeclareOperation( "FactorXMod", [  IsXMod, IsXMod ] );

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
#O  AllIsomorphisms  . . . . . . . . . . all isomorphisms between two groups 
##
DeclareOperation( "AllIsomorphisms", [ IsGroup, IsGroup ] );

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
##
#O AreIsoclinicXMods  . . . . . . check that two crossed modules are isoclinic
##
DeclareOperation( "AreIsoclinicXMods", [ IsXMod, IsXMod ] );

#############################################################################
##
#O IsIsomorphicXMod  . . . . . check that two crossed modules are isomorphic
##
#? should really be IsomorphismXMods (which might return fail) 
##
DeclareOperation( "IsIsomorphicXMod", [  Is2dGroup, Is2dGroup ] );

#############################################################################
##
#O PreAllXMods  . . . . . . . . . all precrossed modules in the given order interval
##
DeclareOperation( "PreAllXMods", [  IsInt, IsInt ] );

#############################################################################
##
#O AllXMods  . . . . . . . . . all crossed modules in the given order interval
##
DeclareOperation( "AllXMods", [  IsInt, IsInt ] );

#############################################################################
##
#O IsoclinicXModFamily  . . . . . . . . . all crossed modules in the list which are isoclinic to the crossed module
##
DeclareOperation( "IsoclinicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O IsomorphicXModFamily  . . . . . . . . . all crossed modules in the list which are isomorphic to the crossed module
##
DeclareOperation( "IsomorphicXModFamily", [  Is2dGroup, IsList ] );

#############################################################################
##
#O IsoAllXMods  . . . . . . . . . all crossed modules up to isomorphism
##
DeclareOperation( "IsoAllXMods", [  IsList ] );

#############################################################################
##
#O RankXMod  . . . . . . . . . the rank of the crossed module
##
DeclareOperation( "RankXMod", [ Is2dGroup ] );

#############################################################################
##
#O MiddleLengthXMod  . . . . . . . . . the middle length of the crossed module
##
DeclareOperation( "MiddleLengthXMod", [ Is2dGroup ] );

#############################################################################
##
#O TableRowXMod  . . . . . . . . . table row for isoclinism families of crossed modules
##
DeclareOperation( "TableRowXMod", [ Is2dGroup, IsList ] );

