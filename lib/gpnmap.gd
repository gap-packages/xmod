##############################################################################
##
#W  gpnmap.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for n-dimensional-mappings: 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

###############################  nd MAPPINGS  ################################ 

#############################################################################
##
#P  IsPreCatnMorphism( <map> ) 
#P  IsCatnMorphism( <map> ) 
##  
DeclareProperty( "IsPreCatnMorphism", IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsCatnMorphism", IsHigherDimensionalGroupMorphism );

#############################################################################
##
#A  2DimensionalGroupMorphisms( <mor> )
#O  MakeHigherDimensionalMapping( <src>, <rng>, <list of maps> )
##
DeclareAttribute( "ListOfHomomorphisms", IsHigherDimensionalMapping );
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

# -----------------------

#############################################################################
##
#F  PreCatnMorphism( <args> )
#O  PreCatnMorphismByMorphisms( [list] )
##
DeclareGlobalFunction( "PreCatnMorphism" );
DeclareOperation( "PreCatnMorphismByMorphisms", 
    [ IsPreCatnGroup, IsPreCatnGroup, IsList ] );

#############################################################################
##
#O  MakeHigherDimensionalGroupMorphism( [list])
##
##
DeclareOperation( "MakeHigherDimensionalGroupMorphism", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup, IsList ] );

#############################################################################
##
#F  CatnMorphism( <args> )
#O  CatnMorphismByMorphisms( <src>, <rng>, <up>, <dn> )
##
DeclareGlobalFunction( "CatnMorphism" );
DeclareOperation( "CatnMorphismByMorphisms", 
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

#############################################################################
##
#E  gpnmap.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
