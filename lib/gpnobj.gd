#############################################################################
##
##  gpnobj.gd                 GAP4 package `XMod'               Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  
##  
##  This file declares generic methods for (pre-)catn-groups.


#############################################################################
##
#O  GroupsOfHigherDimensionalGroup( <obj> ) 
##  These are the vertices of the n-cube 
##
DeclareAttribute( "GroupsOfHigherDimensionalGroup",
    IsHigherDimensionalGroup );

#############################################################################
##
#R  IsPreCatnObj( <obj> ) 
##  A pre-catn-group is an n-cube of pre-cat1 groups
##
DeclareRepresentation( "IsPreCatnObj", 
    IsHigherDimensionalGroup and IsAttributeStoringRep, 
    [ "GeneratingCat1Groups", "HigherDimension" ] );

#############################################################################
##
#P  IsPreCatnGroup( <PCG> ) 
#P  IsCatnGroup( <C1G> ) 
##
DeclareProperty( "IsPreCatnGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsCatnGroup", IsHigherDimensionalGroup );

#############################################################################
##
#P  IsPermHigherDimensionalGroup( <obj> ) 
#P  IsFpHigherDimensionalGroup( <obj> ) 
#P  IsPcHigherDimensionalGroup( <obj> )
##
DeclareProperty( "IsPermHigherDimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsFpHigherDimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsPcHigherDimensionalGroup", IsHigherDimensionalGroup );

#############################################################################
##
#P  IsPreCatnGroupWithIdentityEmbeddings( <obj> )
##
DeclareProperty( "IsPreCatnGroupWithIdentityEmbeddings", IsPreCatnGroup ); 

#############################################################################
##
#T  PreCatnObjType . . . . . . . . . . . . . . . . . . . type for catn-groups
#T  PermPreCatnObjType . . . . . .  . . . . . . . . type for perm catn-groups
#T  PcPreCatnObjType . . . . . . . . . . . . . . . .  type for pc catn-groups
## 
BindGlobal( "PreCatnObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCatnObj ) ); 
BindGlobal( "PermPreCatnObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCatnObj and IsPermHigherDimensionalGroup ) ); 
BindGlobal( "PcPreCatnObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCatnObj and IsPcHigherDimensionalGroup ) ); 

#############################################################################
##
#O  PreCatnObj ( <arg> ) 
#A  GeneratingCat1Groups( <P> ) 
#A  HigherDimension ( <P> ) 
#A  GroupsOfHigherDimensonalGroup( <obj> ) 
#A  EdgesOfHigherDimensonalGroup( <obj> ) 
#A  FacesOfHigherDimensonalGroup( <obj> ) 
##
DeclareOperation( "PreCatnObj", [ IsList ] );
DeclareAttribute( "GeneratingCat1Groups", IsHigherDimensionalGroup );
DeclareAttribute( "HigherDimension", IsHigherDimensionalGroup );
DeclareAttribute( "GroupsOfHigherDimensionalGroup", 
    IsHigherDimensionalGroup ); 
DeclareAttribute( "EdgesOfHigherDimensionalGroup", 
    IsHigherDimensionalGroup ); 
DeclareAttribute( "FacesOfHigherDimensionalGroup", 
    IsHigherDimensionalGroup ); 

#############################################################################
##
#F  PreCatnGroup( <arg> ) 
#F  CatnGroup( <arg> }
##
DeclareGlobalFunction( "PreCatnGroup" );
DeclareGlobalFunction( "CatnGroup" );

#############################################################################
##
#O  IsomorphicRepresentatives( <arg> ) 
##
DeclareOperation( "IsomorphicRepresentatives", [ IsList ] );
