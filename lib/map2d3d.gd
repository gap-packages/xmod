##############################################################################
##
#W  map2d3d.gd                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 

############################################################################## 
## 
#C  IsGeneral2DimensionalMapping( <map> )
#C  IsNonSPGeneral2DimensionalMapping( <map> )
#C  IsSPGeneral2DimensionalMapping( <map> )
## 
#T  do we need the SP and nonSP division ?
##  Crossed module and cat1-group mapping declarations 

DeclareCategory( "IsGeneral2DimensionalMapping", IsGeneralMapping ); 
DeclareCategory( "IsSPGeneral2DimensionalMapping", 
    IsSPGeneralMapping and IsGeneral2DimensionalMapping ); 
DeclareCategory( "IsNonSPGeneral2DimensionalMapping", 
    IsNonSPGeneralMapping and IsGeneral2DimensionalMapping ); 

#############################################################################
##
#C  IsGeneral2DimensionalMappingCollection . . category of colls of mwo maps
#C  IsGeneral2DimensionalMappingCollColl . . . category of colls of colls 
#C  IsGeneral2DimensionalMappingCollCollColl . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGeneral2DimensionalMapping" );
DeclareCategoryCollections( "IsGeneral2DimensionalMappingCollection" );
DeclareCategoryCollections( "IsGeneral2DimensionalMappingCollColl" );

############################################################################# 
##  
#V  General2DimensionalMappingFamily . . . . . . family for homs of magmas with objects 
##  
BindGlobal( "General2DimensionalMappingFamily", 
    NewFamily( "General2DimensionalMappingFamily", IsGeneral2DimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  Is2DimensionalMapping( <map> )
#P  IsEndoGeneral2DimensionalMapping( <map> )
#P  IsEndo2DimensionalMapping( <map> )
#P  Is2DimensionalMagmaGeneralMapping( <map> ) 
#P  Is2DimensionalMagmaMorphism( <map> ) 
#P  Is2DimensionalSemigroupMorphism( <map> )
#P  Is2DimensionalMonoidMorphism( <map> )
DeclareSynonymAttr( "Is2DimensionalMapping", 
    IsGeneral2DimensionalMapping and IsMapping );
DeclareSynonymAttr( "IsEndoGeneral2DimensionalMapping",
    IsGeneral2DimensionalMapping and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndo2DimensionalMapping", 
    Is2DimensionalMapping and IsEndoMapping );
DeclareSynonymAttr( "Is2DimensionalMagmaGeneralMapping", 
    IsGeneral2DimensionalMapping and RespectsMultiplication );
DeclareSynonymAttr( "Is2DimensionalMagmaMorphism", 
    Is2DimensionalMagmaGeneralMapping and IsMapping );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "Is2DimensionalSemigroupMorphism", 
    Is2DimensionalMagmaMorphism  );
DeclareProperty( "Is2DimensionalMonoidMorphism", 
    Is2DimensionalMagmaMorphism );

############################################################################## 
## 
#C  Is2DimensionalGroupMorphism( <map> )
#C  Is2DimensionalGroupMorphismCollection . category of colls of pre-xmod homs
#C  Is2DimensionalGroupMorphismCollColl . . . . . . category of colls of colls 
#C  Is2DimensionalGroupMorphismCollCollColl .  category of colls, colls, colls
#V  Family2DimensionalGroupMorphism . .  family for homomorphisms of pre-xmods 
##
DeclareCategory( "Is2DimensionalGroupMorphism", IsGeneral2DimensionalMapping ); 
DeclareCategoryCollections( "Is2DimensionalGroupMorphism" );
DeclareCategoryCollections( "Is2DimensionalGroupMorphismCollection" );
DeclareCategoryCollections( "Is2DimensionalGroupMorphismCollColl" );
BindGlobal( "Family2DimensionalGroupMorphism", 
    NewFamily( "Family2DimensionalGroupMorphism", Is2DimensionalGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreXModMorphism( <map> ) 
#P  IsPreCat1Morphism( <map> ) 
#P  IsXModMorphism( <map> ) 
#P  IsCat1Morphism( <map> ) 
##  
DeclareProperty( "IsPreXModMorphism", Is2DimensionalGroupMorphism );
DeclareProperty( "IsPreCat1Morphism", Is2DimensionalGroupMorphism );
DeclareProperty( "IsXModMorphism", IsPreXModMorphism );
DeclareProperty( "IsCat1Morphism", IsPreCat1Morphism );

############################################################################## 
## 
#R  Is2DimensionalMappingRep( <map> )
#A  SourceHom( <mor> )
#A  RangeHom( <mor> )
## 
DeclareRepresentation( "Is2DimensionalMappingRep", 
    Is2DimensionalMagmaMorphism and IsAttributeStoringRep and IsGeneralMapping, 
    [ "Source", "Range", "SourceHom", "RangeHom" ] );
DeclareAttribute( "SourceHom", Is2DimensionalGroupMorphism );
DeclareAttribute( "RangeHom", Is2DimensionalGroupMorphism );

############################################################################# 
##  
##  Make2DimensionalMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "Make2DimensionalMagmaMorphism" ); 

#############################################################################
##
#A  Kernel2DimensionalMapping( <mor> )
##
DeclareAttribute( "Kernel2DimensionalMapping", Is2DimensionalGroupMorphism );


###############################  3d MAPPINGS  ################################ 

#############################################################################
##
#C  IsGeneral3DimensionalMapping( <map> )
#C  IsNonSPGeneral3DimensionalMapping( <map> )
#C  IsSPGeneral3DimensionalMapping( <map> )
##  
DeclareCategory( "IsGeneral3DimensionalMapping", IsGeneral2DimensionalMapping );
DeclareCategory( "IsSPGeneral3DimensionalMapping", 
    IsGeneral3DimensionalMapping and IsSPGeneralMapping );
DeclareCategory( "IsNonSPGeneral3DimensionalMapping", 
    IsGeneral3DimensionalMapping and IsNonSPGeneralMapping );

#############################################################################
##
#C  IsGeneral3DimensionalMappingCollection . . . . . category of colls of 3d-magma maps
#C  IsGeneral3DimensionalMappingCollColl . . . . . . category of colls of colls 
#C  IsGeneral3DimensionalMappingCollCollColl . . . . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGeneral3DimensionalMapping" );
DeclareCategoryCollections( "IsGeneral3DimensionalMappingCollection" );
DeclareCategoryCollections( "IsGeneral3DimensionalMappingCollColl" );

############################################################################# 
##  
#V  General3DimensionalMappingFamily . . . . family for homs of 3d-magmas with objects 
##  
BindGlobal( "General3DimensionalMappingFamily", 
    NewFamily( "General3DimensionalMappingFamily", IsGeneral3DimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  Is3DimensionalMapping( <map> )
#P  IsEndoGeneral3DimensionalMapping( <map> )
#P  IsEndo3DimensionalMapping( <map> )
#P  Is3DimensionalMagmaGeneralMapping( <map> ) 
#P  Is3DimensionalMagmaMorphism( <map> ) 
#P  Is3DimensionalSemigroupMorphism( <map> )
#P  Is3DimensionalMonoidMorphism( <map> )
DeclareSynonymAttr( "Is3DimensionalMapping", 
    IsGeneral3DimensionalMapping and IsMapping );
DeclareSynonymAttr( "IsEndoGeneral3DimensionalMapping",
    IsGeneral3DimensionalMapping and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndo3DimensionalMapping", 
    Is3DimensionalMapping and IsEndoMapping );
DeclareSynonymAttr( "Is3DimensionalMagmaGeneralMapping", 
    IsGeneral3DimensionalMapping and RespectsMultiplication );
DeclareSynonymAttr( "Is3DimensionalMagmaMorphism", 
    Is3DimensionalMagmaGeneralMapping and IsMapping );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "Is3DimensionalSemigroupMorphism", 
    Is3DimensionalMagmaMorphism  );
DeclareProperty( "Is3DimensionalMonoidMorphism", 
    Is3DimensionalMagmaMorphism );

############################################################################## 
## 
#C  Is3DimensionalGroupMorphism( <map> )
#C  Is3DimensionalGroupMorphismCollection . category of colls of 3d-group homs
#C  Is3DimensionalGroupMorphismCollColl . . . . . . category of colls of colls 
#C  Is3DimensionalGroupMorphismCollCollColl .  category of colls, colls, colls
#V  Family3DimensionalGroupMorphism . .  family for homomorphisms of 3d-groups 
##
DeclareCategory( "Is3DimensionalGroupMorphism", IsGeneral3DimensionalMapping ); 
DeclareCategoryCollections( "Is3DimensionalGroupMorphism" );
DeclareCategoryCollections( "Is3DimensionalGroupMorphismCollection" );
DeclareCategoryCollections( "Is3DimensionalGroupMorphismCollColl" );
BindGlobal( "Family3DimensionalGroupMorphism", 
    NewFamily( "Family3DimensionalGroupMorphism", Is3DimensionalGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreSqMorphism( <map> ) 
#P  IsPreCat2Morphism( <map> ) 
#P  IsCrossedSquareMorphism( <map> ) 
#P  IsCat2Morphism( <map> ) 
##  
DeclareProperty( "IsPreCrossedSquareMorphism", Is3DimensionalGroupMorphism );
DeclareProperty( "IsPreCat2Morphism", Is3DimensionalGroupMorphism );
DeclareProperty( "IsCrossedSquareMorphism", Is3DimensionalGroupMorphism );
DeclareProperty( "IsCat2Morphism", Is3DimensionalGroupMorphism );

#############################################################################
##
#R  Is3DimensionalMappingRep( <mor> )
#A  Up2DimensionalMorphism( <mor> )
#A  Left2DimensionalMorphism( <mor> )
#A  Down2DimensionalMorphism( <mor> )
#A  Right2DimensionalMorphism( <mor> )
#O  Make3DimensionalMapping( <src>, <rng>, <srchom>, <rnghom> )
##
##  A pre-crossed square or pre-cat2-group morphism contains 4 group homs
##
DeclareRepresentation( "Is3DimensionalMappingRep", 
    Is3DimensionalMapping and IsAttributeStoringRep,
    [ "Source", "Range", "Up2DimensionalMorphism", "Left2DimensionalMorphism", 
      "Right2DimensionalMorphism", "Down2DimensionalMorphism" ] );
DeclareAttribute( "Up2DimensionalMorphism", Is3DimensionalMapping );
DeclareAttribute( "Left2DimensionalMorphism", Is3DimensionalMapping );
DeclareAttribute( "Down2DimensionalMorphism", Is3DimensionalMapping );
DeclareAttribute( "Right2DimensionalMorphism", Is3DimensionalMapping );
DeclareOperation( "Make3DimensionalMapping",
    [ Is3DimensionalDomain, Is3DimensionalDomain, Is2DimensionalMapping, 
      Is2DimensionalMapping, Is2DimensionalMapping, Is2DimensionalMapping ] );

############################################################################# 
##  
##  3DimensionalMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "3DimensionalMagmaMorphism" ); 

#############################################################################
##
#A  Kernel3DimensionalMapping( <mor> )
##
DeclareAttribute( "Kernel3DimensionalMapping", Is3DimensionalGroupMorphism );


############################################################################## 
## 
#E  map2d3d.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
