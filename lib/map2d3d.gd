##############################################################################
##
#W  map2d3d.gd                 GAP4 package `XMod'               Chris Wensley
##
##  version 2.32, 02/02/2015 
##
#Y  Copyright (C) 2001-2015, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 

############################################################################## 
## 
#C  IsGeneral2dMapping( <map> )
#C  IsNonSPGeneral2dMapping( <map> )
#C  IsSPGeneral2dMapping( <map> )
## 
#T  do we need the SP and nonSP division ?
##  Crossed module and cat1-group mapping declarations 

DeclareCategory( "IsGeneral2dMapping", IsGeneralMapping ); 
DeclareCategory( "IsSPGeneral2dMapping", 
    IsSPGeneralMapping and IsGeneral2dMapping ); 
DeclareCategory( "IsNonSPGeneral2dMapping", 
    IsNonSPGeneralMapping and IsGeneral2dMapping ); 

#############################################################################
##
#C  IsGeneral2dMappingCollection . . category of colls of mwo maps
#C  IsGeneral2dMappingCollColl . . . category of colls of colls 
#C  IsGeneral2dMappingCollCollColl . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGeneral2dMapping" );
DeclareCategoryCollections( "IsGeneral2dMappingCollection" );
DeclareCategoryCollections( "IsGeneral2dMappingCollColl" );

############################################################################# 
##  
#V  General2dMappingFamily . . . . . . family for homs of magmas with objects 
##  
BindGlobal( "General2dMappingFamily", 
    NewFamily( "General2dMappingFamily", IsGeneral2dMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  Is2dMapping( <map> )
#P  IsEndoGeneral2dMapping( <map> )
#P  IsEndo2dMapping( <map> )
#P  Is2dMagmaGeneralMapping( <map> ) 
#P  Is2dMagmaMorphism( <map> ) 
#P  Is2dSemigroupMorphism( <map> )
#P  Is2dMonoidMorphism( <map> )
DeclareSynonymAttr( "Is2dMapping", IsGeneral2dMapping and IsMapping );
DeclareSynonymAttr( "IsEndoGeneral2dMapping",
    IsGeneral2dMapping and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndo2dMapping", Is2dMapping and IsEndoMapping );
DeclareSynonymAttr( "Is2dMagmaGeneralMapping", 
    IsGeneral2dMapping and RespectsMultiplication );
DeclareSynonymAttr( "Is2dMagmaMorphism", 
    Is2dMagmaGeneralMapping and IsMapping );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "Is2dSemigroupMorphism", Is2dMagmaMorphism  );
DeclareProperty( "Is2dMonoidMorphism", Is2dMagmaMorphism );

############################################################################## 
## 
#C  Is2dGroupMorphism( <map> )
#C  Is2dGroupMorphismCollection . . . . category of colls of pre-xmod homs
#C  Is2dGroupMorphismCollColl . . . . . category of colls of colls 
#C  Is2dGroupMorphismCollCollColl . . . category of colls, colls, colls
#V  Family2dGroupMorphism . . . . . . . family for homomorphisms of pre-xmods 
##
DeclareCategory( "Is2dGroupMorphism", IsGeneral2dMapping ); 
DeclareCategoryCollections( "Is2dGroupMorphism" );
DeclareCategoryCollections( "Is2dGroupMorphismCollection" );
DeclareCategoryCollections( "Is2dGroupMorphismCollColl" );
BindGlobal( "Family2dGroupMorphism", 
    NewFamily( "Family2dGroupMorphism", Is2dGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreXModMorphism( <map> ) 
#P  IsPreCat1Morphism( <map> ) 
#P  IsXModMorphism( <map> ) 
#P  IsCat1Morphism( <map> ) 
##  
DeclareProperty( "IsPreXModMorphism", Is2dGroupMorphism );
DeclareProperty( "IsPreCat1Morphism", Is2dGroupMorphism );
DeclareProperty( "IsXModMorphism", IsPreXModMorphism );
DeclareProperty( "IsCat1Morphism", IsPreCat1Morphism );

############################################################################## 
## 
#R  Is2dMappingRep( <map> )
#A  SourceHom( <mor> )
#A  RangeHom( <mor> )
## 
DeclareRepresentation( "Is2dMappingRep", 
    Is2dMagmaMorphism and IsAttributeStoringRep and IsGeneralMapping, 
    [ "Source", "Range", "SourceHom", "RangeHom" ] );
DeclareAttribute( "SourceHom", Is2dGroupMorphism );
DeclareAttribute( "RangeHom", Is2dGroupMorphism );

############################################################################# 
##  
##  Make2dMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "Make2dMagmaMorphism" ); 

#############################################################################
##
#A  Kernel2dMapping( <mor> )
##
DeclareAttribute( "Kernel2dMapping", Is2dGroupMorphism );


###############################  3d MAPPINGS  ################################ 

#############################################################################
##
#C  IsGeneral3dMapping( <map> )
#C  IsNonSPGeneral3dMapping( <map> )
#C  IsSPGeneral3dMapping( <map> )
##  
DeclareCategory( "IsGeneral3dMapping", IsGeneral2dMapping );
DeclareCategory( "IsSPGeneral3dMapping", 
    IsGeneral3dMapping and IsSPGeneralMapping );
DeclareCategory( "IsNonSPGeneral3dMapping", 
    IsGeneral3dMapping and IsNonSPGeneralMapping );

#############################################################################
##
#C  IsGeneral3dMappingCollection . . . . . category of colls of 3d-magma maps
#C  IsGeneral3dMappingCollColl . . . . . . category of colls of colls 
#C  IsGeneral3dMappingCollCollColl . . . . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGeneral3dMapping" );
DeclareCategoryCollections( "IsGeneral3dMappingCollection" );
DeclareCategoryCollections( "IsGeneral3dMappingCollColl" );

############################################################################# 
##  
#V  General3dMappingFamily . . . . family for homs of 3d-magmas with objects 
##  
BindGlobal( "General3dMappingFamily", 
    NewFamily( "General3dMappingFamily", IsGeneral3dMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  Is3dMapping( <map> )
#P  IsEndoGeneral3dMapping( <map> )
#P  IsEndo3dMapping( <map> )
#P  Is3dMagmaGeneralMapping( <map> ) 
#P  Is3dMagmaMorphism( <map> ) 
#P  Is3dSemigroupMorphism( <map> )
#P  Is3dMonoidMorphism( <map> )
DeclareSynonymAttr( "Is3dMapping", IsGeneral3dMapping and IsMapping );
DeclareSynonymAttr( "IsEndoGeneral3dMapping",
    IsGeneral3dMapping and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndo3dMapping", Is3dMapping and IsEndoMapping );
DeclareSynonymAttr( "Is3dMagmaGeneralMapping", 
    IsGeneral3dMapping and RespectsMultiplication );
DeclareSynonymAttr( "Is3dMagmaMorphism", 
    Is3dMagmaGeneralMapping and IsMapping );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "Is3dSemigroupMorphism", Is3dMagmaMorphism  );
DeclareProperty( "Is3dMonoidMorphism", Is3dMagmaMorphism );

############################################################################## 
## 
#C  Is3dGroupMorphism( <map> )
#C  Is3dGroupMorphismCollection . . . . category of colls of 3d-group homs
#C  Is3dGroupMorphismCollColl . . . . . category of colls of colls 
#C  Is3dGroupMorphismCollCollColl . . . category of colls, colls, colls
#V  Family3dGroupMorphism . . . . . . . family for homomorphisms of 3d-groups 
##
DeclareCategory( "Is3dGroupMorphism", IsGeneral3dMapping ); 
DeclareCategoryCollections( "Is3dGroupMorphism" );
DeclareCategoryCollections( "Is3dGroupMorphismCollection" );
DeclareCategoryCollections( "Is3dGroupMorphismCollColl" );
BindGlobal( "Family3dGroupMorphism", 
    NewFamily( "Family3dGroupMorphism", Is3dGroupMorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreSqMorphism( <map> ) 
#P  IsPreCat2Morphism( <map> ) 
#P  IsXSqMorphism( <map> ) 
#P  IsCat2Morphism( <map> ) 
##  
DeclareProperty( "IsPreXSqMorphism", Is3dGroupMorphism );
DeclareProperty( "IsPreCat2Morphism", Is3dGroupMorphism );
DeclareProperty( "IsXSqMorphism", Is3dGroupMorphism );
DeclareProperty( "IsCat2Morphism", Is3dGroupMorphism );

#############################################################################
##
#R  Is3dMappingRep( <mor> )
#A  Up2dMorphism( <mor> )
#A  Left2dMorphism( <mor> )
#A  Down2dMorphism( <mor> )
#A  Right2dMorphism( <mor> )
#O  Make3dMapping( <src>, <rng>, <srchom>, <rnghom> )
##
##  A pre-crossed square or pre-cat2-group morphism contains 4 group homs
##
DeclareRepresentation( "Is3dMappingRep", 
    Is3dMapping and IsAttributeStoringRep,
    [ "Source", "Range", "Up2dMorphism", "Left2dMorphism", 
      "Right2dMorphism", "Down2dMorphism" ] );
DeclareAttribute( "Up2dMorphism", Is3dMapping );
DeclareAttribute( "Left2dMorphism", Is3dMapping );
DeclareAttribute( "Down2dMorphism", Is3dMapping );
DeclareAttribute( "Right2dMorphism", Is3dMapping );
DeclareOperation( "Make3dMapping",
    [ Is3dDomain, Is3dDomain, Is2dMapping, Is2dMapping, 
      Is2dMapping, Is2dMapping ] );

############################################################################# 
##  
##  3dMagmaMorphism( <args> )
## 
DeclareGlobalFunction( "3dMagmaMorphism" ); 

#############################################################################
##
#A  Kernel3dMapping( <mor> )
##
DeclareAttribute( "Kernel3dMapping", Is3dGroupMorphism );


############################################################################## 
## 
#E  map2d3d.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
