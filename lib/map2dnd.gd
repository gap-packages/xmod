##############################################################################
##
#W  map2dnd.gd                 GAP4 package `XMod'               Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 

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

############################################################################# 
##  
#V  General2DimensionalMappingFamily . family for homs of magmas with objects 
#R  Is2DimensionalMappingRep( <map> )
##  
BindGlobal( "General2DimensionalMappingFamily", 
    NewFamily( "General2DimensionalMappingFamily", IsGeneral2DimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 
DeclareRepresentation( "Is2DimensionalMappingRep", 
    Is2DimensionalMagmaMorphism and IsAttributeStoringRep and IsGeneralMapping, 
    [ "Source", "Range", "SourceHom", "RangeHom" ] );

############################################################################## 
## 
#C  Is2DimensionalGroupMorphism( <map> )
#C  Is2DimensionalGroupMorphismCollection . category of colls of pre-xmod homs
#C  Is2DimensionalGroupMorphismCollColl . . . . . . category of colls of colls 
#C  Is2DimensionalGroupMorphismCollCollColl .  category of colls, colls, colls
#V  Family2DimensionalGroupMorphism . .  family for homomorphisms of pre-xmods 
#T  Type2DimensionalGroupMorphism . . . .  type for homomorphisms of pre-xmods 
##
DeclareCategory( "Is2DimensionalGroupMorphism", IsGeneral2DimensionalMapping ); 
DeclareCategoryCollections( "Is2DimensionalGroupMorphism" );
DeclareCategoryCollections( "Is2DimensionalGroupMorphismCollection" );
DeclareCategoryCollections( "Is2DimensionalGroupMorphismCollColl" );
BindGlobal( "Family2DimensionalGroupMorphism", 
    NewFamily( "Family2DimensionalGroupMorphism", Is2DimensionalGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "Type2DimensionalGroupMorphism", 
            NewType( Family2DimensionalGroupMorphism, 
                     Is2DimensionalMappingRep ) ); 

#############################################################################
##
#P  IsPreXModMorphism( <map> ) 
#P  IsPreCat1GroupMorphism( <map> ) 
#P  IsXModMorphism( <map> ) 
#P  IsCat1GroupMorphism( <map> ) 
##  
DeclareProperty( "IsPreXModMorphism", Is2DimensionalGroupMorphism );
DeclareProperty( "IsPreCat1GroupMorphism", Is2DimensionalGroupMorphism );
DeclareProperty( "IsXModMorphism", IsPreXModMorphism );
DeclareProperty( "IsCat1GroupMorphism", IsPreCat1GroupMorphism );

############################################################################## 
## 
#A  SourceHom( <mor> )
#A  RangeHom( <mor> )
## 
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

########################  HIGHER DIMENSIONAL MAPPINGS  ###################### 

#############################################################################
##
#C  IsGeneralHigherDimensionalMapping( <map> )
#C  IsNonSPGeneralHigherDimensionalMapping( <map> )
#C  IsSPGeneralHigherDimensionalMapping( <map> )
##  
DeclareCategory( "IsGeneralHigherDimensionalMapping", 
    IsGeneral2DimensionalMapping );
DeclareCategory( "IsSPGeneralHigherDimensionalMapping", 
    IsGeneralHigherDimensionalMapping and IsSPGeneralMapping );
DeclareCategory( "IsNonSPGeneralHigherDimensionalMapping", 
    IsGeneralHigherDimensionalMapping and IsNonSPGeneralMapping );

#############################################################################
##
#C  IsGeneralHigherDimensionalMapping 
##               . . . . . category of colls of higher dimensional magma maps
#C  IsGeneralHigherDimensionalMappingCollColl . .  category of colls of colls 
##
DeclareCategoryCollections( "IsGeneralHigherDimensionalMapping" );
DeclareCategoryCollections( "IsGeneralHigherDimensionalMappingCollection" );
DeclareCategoryCollections( "IsGeneralHigherDimensionalMappingCollColl" );

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

############################################################################# 
##  
#V  GeneralHigherDimensionalMappingFamily 
##          . . . . family for homs of higher dimensional magmas with objects 
#R  IsHigherDimensionalMappingRep( <mor> )
##  
BindGlobal( "GeneralHigherDimensionalMappingFamily", 
    NewFamily( "GeneralHigherDimensionalMappingFamily", IsGeneralHigherDimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 
DeclareRepresentation( "IsHigherDimensionalMappingRep", 
    IsHigherDimensionalMapping and IsAttributeStoringRep,
    [ "Source", "Range", "ListOf2DimensionalMappings", "HigherDimension" ] );

############################################################################## 
## 
#C  IsHigherDimensionalGroupMorphism( <map> )
#C  IsHigherDimensionalGroupMorphismCollection 
#C  IsHigherDimensionalGroupMorphismCollColl . 
#V  FamilyHigherDimensionalGroupMorphism  
#T  Type2DimensionalGroupMorphism 
##
DeclareCategory( "IsHigherDimensionalGroupMorphism", 
    IsGeneralHigherDimensionalMapping ); 
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphism" );
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphismCollection" );
DeclareCategoryCollections( "IsHigherDimensionalGroupMorphismCollColl" );
BindGlobal( "FamilyHigherDimensionalGroupMorphism", 
    NewFamily( "FamilyHigherDimensionalGroupMorphism", 
               IsHigherDimensionalGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "TypeHigherDimensionalGroupMorphism", 
            NewType( FamilyHigherDimensionalGroupMorphism, 
                     IsHigherDimensionalMappingRep ) ); 

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
#P  IsPreSqMorphism( <map> ) 
#P  IsPreCat2GroupMorphism( <map> ) 
#P  IsCrossedSquareMorphism( <map> ) 
#P  IsCat2GroupMorphism( <map> ) 
##  
DeclareProperty( "IsPreCrossedSquareMorphism", 
    IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsPreCat2GroupMorphism", IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsCrossedSquareMorphism", IsHigherDimensionalGroupMorphism );
DeclareProperty( "IsCat2GroupMorphism", IsHigherDimensionalGroupMorphism );

#############################################################################
##
#A  Up2DimensionalMorphism( <mor> )
#A  Left2DimensionalMorphism( <mor> )
#A  Down2DimensionalMorphism( <mor> )
#A  Right2DimensionalMorphism( <mor> )
##
##  A pre-crossed square or pre-cat2-group morphism contains 4 group homs
##
DeclareAttribute( "Up2DimensionalMorphism", IsHigherDimensionalMapping );
DeclareAttribute( "Left2DimensionalMorphism", IsHigherDimensionalMapping );
DeclareAttribute( "Down2DimensionalMorphism", IsHigherDimensionalMapping );
DeclareAttribute( "Right2DimensionalMorphism", IsHigherDimensionalMapping );
