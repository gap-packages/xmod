##############################################################################
##
#W  gpnmap.gd                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file declares functions for n-dimensional-mappings: 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

###############################  nd MAPPINGS  ################################ 

#############################################################################
##
#C  IsGeneralHigherDimensionalMapping( <map> )
#C  IsNonSPGeneralHigherDimensionalMapping( <map> )
#C  IsSPGeneralHigherDimensionalMapping( <map> )
##  
DeclareCategory( "IsGeneralHigherDimensionalMapping", IsGeneral2DimensionalMapping );
DeclareCategory( "IsSPGeneralHigherDimensionalMapping", 
    IsGeneralHigherDimensionalMapping and IsSPGeneralMapping );
DeclareCategory( "IsNonSPGeneralHigherDimensionalMapping", 
    IsGeneralHigherDimensionalMapping and IsNonSPGeneralMapping );

#############################################################################
##
#C  IsGeneralHigherDimensionalMapping . . . . . category of colls of higher dimensional magma maps
#C  IsGeneralHigherDimensionalMappingCollColl . . . . . . category of colls of colls 
##
DeclareCategoryCollections( "IsGeneralHigherDimensionalMapping" );
DeclareCategoryCollections( "IsGeneralHigherDimensionalMappingCollection" );
DeclareCategoryCollections( "IsGeneralHigherDimensionalMappingCollColl" );

############################################################################# 
##  
#V  GeneralHigherDimensionalMappingFamily . . . . family for homs of higher dimensional magmas with objects 
##  
BindGlobal( "GeneralHigherDimensionalMappingFamily", 
    NewFamily( "GeneralHigherDimensionalMappingFamily", IsGeneralHigherDimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsHigherDimensionalMapping( <map> )
#P  IsEndoGeneralHigherDimensionalMapping( <map> )
#P  IsEndoHigherDimensionalMapping( <map> )
#P  IsHigherDimensionalMagmaGeneralMapping( <map> ) 
#P  IsHigherDimensionalMagmaMorphism( <map> ) 
#P  IsHigherDimensionalSemigroupMorphism( <map> )
#P  IsHigherDimensionalMonoidMorphism( <map> )
DeclareSynonymAttr( "IsHigherDimensionalMapping", 
    IsGeneralHigherDimensionalMapping and IsMapping );
DeclareSynonymAttr( "IsEndoGeneralHigherDimensionalMapping",
    IsGeneralHigherDimensionalMapping and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndoHigherDimensionalMapping", 
    IsHigherDimensionalMapping and IsEndoMapping );
DeclareSynonymAttr( "IsHigherDimensionalMagmaGeneralMapping", 
    IsGeneralHigherDimensionalMapping and RespectsMultiplication );
DeclareSynonymAttr( "IsHigherDimensionalMagmaMorphism", 
    IsHigherDimensionalMagmaGeneralMapping and IsMapping );
DeclareProperty( "IsHigherDimensionalSemigroupMorphism", 
    IsHigherDimensionalMagmaMorphism  );
DeclareProperty( "IsHigherDimensionalMonoidMorphism", 
    IsHigherDimensionalMagmaMorphism );

############################################################################## 
## 
#C  IsHigherDimensionalGroupMorphism( <map> )
#C  IsHigherDimensionalGroupMorphismCollection . category of colls of higher dimensional groups
#C  IsHigherDimensionalGroupMorphismCollColl . . . . . . category of colls of colls 
#V  FamilyHigherDimensionalGroupMorphism . .  family for homomorphisms of higher dimensional groups 
##
DeclareCategory( "IsHigherDimensionalGroupMorphism", IsGeneralHigherDimensionalMapping ); 
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphism" );
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphismCollection" );
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphismCollColl" );
BindGlobal( "FamilyHigherDimensionalGroupMorphism", 
    NewFamily( "FamilyHigherDimensionalGroupMorphism", IsHigherDimensionalGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreCatnMorphism( <map> ) 
#P  IsCatnMorphism( <map> ) 
##  
DeclareProperty( "IsPreCatnMorphism", IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsCatnMorphism", IsHigherDimensionalGroupMorphism );

#############################################################################
##
#R  IsHigherDimensionalMappingRep( <mor> )
#A  2DimensionalGroupMorphisms( <mor> )
#A  PreCatnGroupMorphismDimension( <mor> )
#O  MakeHigherDimensionalMapping( [list] )
##
##  A pre-catn-group morphism 
##
DeclareRepresentation( "IsHigherDimensionalMappingRep", 
    IsHigherDimensionalMapping and IsAttributeStoringRep,
    [ "Source", "Range", "2DimensionalGroupMorphisms", "PreCatnGroupMorphismDimension" ] );
DeclareAttribute( "2DimensionalGroupMorphisms", IsHigherDimensionalMapping );
DeclareAttribute( "PreCatnGroupMorphismDimension", IsHigherDimensionalMapping );
DeclareOperation( "MakeHigherDimensionalMapping",
    [ IsList ] );

############################################################################# 
##  
##  HigherDimensionalMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "HigherDimensionalMagmaMorphism" ); 

#############################################################################
##
#A  KernelHigherDimensionalMapping( <mor> )
##
DeclareAttribute( "KernelHigherDimensionalMapping", IsHigherDimensionalGroupMorphism );

# -----------------------

#############################################################################
##
#F  PreCatnMorphism( <args> )
#O  PreCatnMorphismByMorphisms( [list] )
##
DeclareGlobalFunction( "PreCatnMorphism" );
DeclareOperation( "PreCatnMorphismByMorphisms", [ IsList ] );

#############################################################################
##
#O  MakeHigherDimensionalGroupMorphism( [list])
##
##
DeclareOperation( "MakeHigherDimensionalGroupMorphism", [ IsList ] );

#############################################################################
##
#F  CatnMorphism( <args> )
#O  CatnMorphismByMorphisms( <src>, <rng>, <up>, <dn> )
##
DeclareGlobalFunction( "CatnMorphism" );
DeclareOperation( "CatnMorphismByMorphisms", [ IsList ] );

#############################################################################
##
#P  IsEndomorphismHigherDimensionalDomain( <mor> )
#P  IsAutomorphismHigherDimensionalDomain( <mor> )
##
DeclareProperty( "IsEndomorphismHigherDimensionalDomain", IsHigherDimensionalMapping );
DeclareProperty( "IsAutomorphismHigherDimensionalDomain", IsHigherDimensionalMapping );

#############################################################################
##
#E  gpnmap.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
