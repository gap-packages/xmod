##############################################################################
##
#W  hap.gd                     GAP4 package `XMod'               Chris Wensley
#W                                                                
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

##############################################################################
##
#O  Cat1GroupToHAP( <cat1> )
#O  CatOneGroupToXMod( <cat1> ) 
##
DeclareOperation( "Cat1GroupToHAP", [ IsCat1Group ] ); 
DeclareOperation( "CatOneGroupToXMod", [ IsHapCatOneGroupRep ] ); 

##############################################################################
##
#O  SmallCat1Group( <cat1> )
##
DeclareOperation( "SmallCat1Group", [ IsPosInt, IsPosInt, IsPosInt ] ); 

##############################################################################
##
#A  IdCat1Group( <cat1> ) 
#A  IdQuasiCat1Group( <cat1> ) 
##
DeclareAttribute( "IdCat1Group", IsCat1Group ); 
DeclareAttribute( "IdQuasiCat1Group", IsCat1Group ); 

##############################################################################
##
#O  QuasiIsomorphCat1Group( <cat1> )
##
DeclareOperation( "QuasiIsomorphCat1Group", [ IsCat1Group ] ); 

##############################################################################
##
#P  IsPreCat1GroupQuasiIsomorphism( <precat1> )
##
DeclareProperty( "IsPreCat1QuasiIsomorphism", IsPreCat1GroupMorphism ); 

