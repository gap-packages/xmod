##############################################################################
##
##  gpnobj.gd                 GAP4 package `XMod'                Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for (pre-)catn-groups.


#############################################################################
##
#R  IsPreCatnObj( <obj> ) 
##  A pre-cat2-group is a square of pre-cat1 groups
##
DeclareRepresentation( "IsPreCatnObj", IsHigherDimensionalGroup and IsAttributeStoringRep, 
    [ "2DimensionalGroups", "PreCatnDimension" ] );

#############################################################################
##
#P  IsPreCatnGroup( <PCG> ) 
#P  IsCatnGroup( <C1G> ) 
##
DeclareProperty( "IsPreCatnGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsCatnGroup", IsHigherDimensionalGroup );

#############################################################################
##
#O  PreCatnObj ( <arg> ) 
#A  2DimensionalGroups( <P> ) 
#A  PreCatnDimension ( <P> ) 
##
DeclareOperation( "PreCatnObj", [ IsList ] );
DeclareAttribute( "GeneratingCat1Groups", IsHigherDimensionalGroup );
DeclareAttribute( "2DimensionalGroups", IsHigherDimensionalGroup );
DeclareAttribute( "PreCatnDimension", IsHigherDimensionalGroup );

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
