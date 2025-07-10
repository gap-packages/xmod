#############################################################################
##
##  gp4obj.gd                 GAP4 package `XMod'               Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  
##  
##  This file declares generic methods for (pre-)crossed cubes and
##  (pre-)cat3-groups.

#############################################################################
##
#A  Front3DimensionalGroup( <PS> ) 
#A  Left3DimensionalGroup( <PS> ) 
#A  Up3DimensionalGroup( <PS> ) 
#A  Right3DimensionalGroup( <PS> ) 
#A  Down3DimensionalGroup ( <PS> ) 
#A  Back3DimensionalGroup( <PS> )
##
DeclareAttribute( "Front3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Left3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Up3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Right3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Down3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Back3DimensionalGroup", IsHigherDimensionalGroup );

#############################################################################
## 
#A  Size4d( <obj> ) 
##
DeclareAttribute( "Size4d", IsHigherDimensionalDomain ); 

#############################################################################
##
#R  IsPreCat3GroupObj ( <obj> ) 
#T  PreCat3GroupObjType 
#O  PreCat3GroupObj( <cat2gp>, <cat2 gp> ) 
##  A pre-cat3-group is a cube of commuting pre-cat1-groups 
##
DeclareRepresentation( "IsPreCat3GroupObj", 
    IsHigherDimensionalGroup and IsAttributeStoringRep,
    [ "front3d", "left3d", "up3d", "right3d", "down3d", "back3d" ] );
BindGlobal( "PreCat3GroupObjType", 
            NewType( FamilyHigherDimensionalGroup, IsPreCat3GroupObj ) ); 
DeclareOperation( "PreCat3GroupObj", [ IsList] );

#############################################################################
##
#P  IsPreCat3Group( <PCG> ) 
#P  IsCat3Group( <C1G> ) 
##
DeclareProperty( "IsPreCat3Group", IsHigherDimensionalGroup );
DeclareProperty( "IsCat3Group", IsHigherDimensionalGroup );
InstallTrueMethod( IsPreCat3Group, IsCat3Group );

#############################################################################
##
#F  PreCat3Group( <arg> ) 
#F  Cat3Group( <arg> }
#F  DetermineRemainingFaces( <arg> )
#O  DetermineRemainingCat2Groups( <front> <left> )
#O  PreCat3GroupByPreCat2Groups( <front>,<left>,<up>,<right>,<down>,<back> )
##
DeclareGlobalFunction( "PreCat3Group" );
DeclareGlobalFunction( "Cat3Group" );
DeclareGlobalFunction( "DetermineRemainingFaces" );
DeclareOperation( "DetermineRemainingCat2Groups", 
    [ IsPreCat2Group, IsPreCat2Group ] );
DeclareOperation( "PreCat3GroupByPreCat2Groups", 
    [ IsPreCat2Group, IsPreCat2Group, IsPreCat2Group, 
      IsPreCat2Group, IsPreCat2Group, IsPreCat2Group ] );

#############################################################################
##
#O  AllCat3Groups( <G> ) 
#O  AllCat3GroupsIterator( <gp> )
#A  AllCat3GroupsNumber( <gp> )
#O  AllCat3GroupsUpToIsomorphism( <G> ) 
#O  AllCat3GroupTriples( <G> ) 
## 
DeclareOperation( "AllCat3Groups", [ IsGroup ] ); 
DeclareOperation( "AllCat3GroupsIterator", [ IsGroup ] ); DeclareAttribute( "AllCat3GroupsNumber", IsGroup ); 
DeclareOperation( "AllCat3GroupsUpToIsomorphism", [ IsGroup ] ); 
DeclareOperation( "AllCat3GroupTriples", [ IsGroup ] ); 
