##############################################################################
##
##  gpnobj.gd                 GAP4 package `XMod'                Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for (pre-)catn-groups.


#############################################################################
##
#R  IsPreCatnObj( <obj> ) 
##  A pre-catn-group is an n-cube of pre-cat1 groups
##
DeclareRepresentation( "IsPreCatnObj", IsHigherDimensionalGroup and IsAttributeStoringRep, 
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
#A  VerticesOfHigherDimensonalGroup( <obj> ) 
#A  EdgesesOfHigherDimensonalGroup( <obj> ) 
#A  FacesOfHigherDimensonalGroup( <obj> ) 
##
DeclareOperation( "PreCatnObj", [ IsList ] );
DeclareAttribute( "GeneratingCat1Groups", IsHigherDimensionalGroup );
DeclareAttribute( "HigherDimension", IsHigherDimensionalGroup );
DeclareAttribute( "VerticesOfHigherDimensionalGroup", 
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
#E gpnobj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
