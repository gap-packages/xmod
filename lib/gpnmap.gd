##############################################################################
##
#W  gpnmap.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for n-dimensional-mappings: 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

###############################  nd MAPPINGS  ################################ 

#############################################################################
##
#P  IsPreCatnGroupMorphism( <map> ) 
#P  IsCatnGroupMorphism( <map> ) 
##  
DeclareProperty( "IsPreCatnGroupMorphism", IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsCatnGroupMorphism", IsHigherDimensionalGroupMorphism );

#############################################################################
##
#A  2DimensionalGroupMorphisms( <mor> )
#O  MakeHigherDimensionalMapping( <src>, <rng>, <list of maps> )
##
DeclareAttribute( "ListOf2DimensionalMappings", IsHigherDimensionalMapping );
DeclareOperation( "MakeHigherDimensionalMapping",
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup, IsList ] );

############################################################################# 
##  
##  HigherDimensionalMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "HigherDimensionalMagmaMorphism" ); 

#############################################################################
##
#A  KernelHigherDimensionalMapping( <mor> )
##
DeclareAttribute( "KernelHigherDimensionalMapping", 
    IsHigherDimensionalGroupMorphism );

#############################################################################
##
#F  PreCatnGroupMorphism( <args> )
#O  PreCatnGroupMorphismByMorphisms( [list] )
##
DeclareGlobalFunction( "PreCatnGroupMorphism" );
DeclareOperation( "PreCatnGroupMorphismByMorphisms", 
    [ IsPreCatnGroup, IsPreCatnGroup, IsList ] );

#############################################################################
##
#O  MakeHigherDimensionalGroupMorphism( [hd-group,hd-group,list] )
##
##
DeclareOperation( "MakeHigherDimensionalGroupMorphism", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup, IsList ] );

#############################################################################
##
#F  CatnGroupMorphism( <args> )
#O  CatnGroupMorphismByMorphisms( <src>, <rng>, <up>, <dn> )
##
DeclareGlobalFunction( "CatnGroupMorphism" );
DeclareOperation( "CatnGroupMorphismByMorphisms", 
    [ IsCatnGroup, IsCatnGroup, IsList ] );

#############################################################################
##
#P  IsEndomorphismHigherDimensionalDomain( <mor> )
#P  IsAutomorphismHigherDimensionalDomain( <mor> )
##
DeclareProperty( "IsEndomorphismHigherDimensionalDomain", 
    IsHigherDimensionalMapping );
DeclareProperty( "IsAutomorphismHigherDimensionalDomain", 
    IsHigherDimensionalMapping );
