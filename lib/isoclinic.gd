#############################################################################
##
#W  isoclinic.gd              GAP4 package `XMod'                Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 17/09/2015 
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
#O CommutatorSubXMod  . . . . . . . commutator subxmod of two normal subxmods
##
DeclareOperation( "CommutatorSubXMod", [  IsXMod, IsXMod, IsXMod ] );

#############################################################################
##
#O LowerCentralSeriesXMod  . . . . . . the lower central series of an xmod
##
DeclareAttribute( "LowerCentralSeriesXMod", IsXMod );

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
#O IsNilpotentXMod  . . . . . . . check that the crossed module is nilpotent
##
DeclareOperation( "IsNilpotentXMod", [ Is2dGroup ] );

#############################################################################
##
#O NilpotencyClassXMod  . . . . . . . the nilpotency degree of the crossed module
##
DeclareOperation( "NilpotencyClassXMod", [ Is2dGroup ] );

#############################################################################
##
#O CorrespondingMap  . . . . . . . . . the tool for quotient crossed module
##
DeclareOperation( "CorrespondingMap", [  IsGroup, IsGroup ] );

#############################################################################
##
#O FactorXMod  . . . . . . . . . the quotient crossed module
##
DeclareOperation( "FactorXMod", [  IsXMod, IsXMod ] );

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
#O IsAbelianXMod  . . . . . . . . . check that the crossed module is abelian
##
DeclareOperation( "IsAbelianXMod", [ Is2dGroup ] );

#############################################################################
##
#O IsAsphericalXMod  . . . . . . . . . check that the crossed module is aspherical
##
DeclareOperation( "IsAsphericalXMod", [ Is2dGroup ] );

#############################################################################
##
#O IsSimplyConnectedXMod  . . . . . . . . . check that the crossed module is simply connected
##
DeclareOperation( "IsSimplyConnectedXMod", [ Is2dGroup ] );

#############################################################################
##
#O IsFaithfulXMod  . . . . . . . . . check that the crossed module is faithful
##
DeclareOperation( "IsFaithfulXMod", [ Is2dGroup ] );

#############################################################################
##
#O TableRowXMod  . . . . . . . . . table row for isoclinism families of crossed modules
##
DeclareOperation( "TableRowXMod", [ Is2dGroup, IsList ] );

