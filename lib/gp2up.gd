##############################################################################
##
#W  gp2up.gd                   GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##
##  This file contains declarations for UpMappings, Derivations and Sections
##
#Y  Copyright (C) 2001-2022, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#C  IsUp2DimensionalMapping( <map> )
#R  IsUp2DimensionalMappingRep( <map> ) 
##
##  A section|derivation is determined by a cat1-group|xmod + generator images
##
DeclareCategory( "IsUp2DimensionalMapping", IsGeneralMapping );
DeclareRepresentation( "IsUp2DimensionalMappingRep", 
    IsUp2DimensionalMapping and IsAttributeStoringRep, 
    [ "Object2d", "UpGeneratorImages", "UpHomomorphism", "UpImagePositions" ] );

#############################################################################
##
#C  IsUp2DimensionalMappingCollection . . . . category of colls of up-2d-maps
#C  IsUp2DimensionalMappingCollColl . . . . . . .  category of colls of colls 
#C  IsUp2DimensionalMappingCollCollColl . . . category of colls, colls, colls
#V  Up2DimensionalMappingFamily . . . . . family for derivations and sections
#T  Up2DimensionalMappingType( <map> ) 
##
DeclareCategoryCollections( "IsUp2DimensionalMapping" );
DeclareCategoryCollections( "IsUp2DimensionalMappingCollection" );
DeclareCategoryCollections( "IsUp2DimensionalMappingCollColl" );
BindGlobal( "Up2DimensionalMappingFamily", 
    NewFamily( "Up2DimensionalMappingFamily", IsUp2DimensionalMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "Up2DimensionalMappingType", 
            NewType( Up2DimensionalMappingFamily, 
                     IsUp2DimensionalMappingRep ) ); 

##############################################################################
##
#A  Object2d( <map> )
#A  UpHomomorphism( <map> ) 
#A  UpGeneratorImages( <map> )
#A  UpImagePositions( <map> )
##
DeclareAttribute( "Object2d", IsUp2DimensionalMapping ); 
DeclareAttribute( "UpHomomorphism", IsUp2DimensionalMapping ); 
DeclareAttribute( "UpGeneratorImages", IsUp2DimensionalMapping );
DeclareAttribute( "UpImagePositions", IsUp2DimensionalMapping );

#############################################################################
##
#P  IsDerivation( <map> )
#P  IsSection( <map> )
##
DeclareProperty( "IsDerivation", IsUp2DimensionalMapping );
DeclareProperty( "IsSection", IsUp2DimensionalMapping );

InstallTrueMethod( IsUp2DimensionalMapping, IsDerivation );
InstallTrueMethod( IsUp2DimensionalMapping, IsSection );

##############################################################################
##                               Derivations                                ##
##############################################################################

##############################################################################
##
#O  DerivationByImages                                     sets up the mapping
#O  DerivationByImagesNC                                   sets up the mapping
##
DeclareOperation( "DerivationByImages", 
    [ Is2DimensionalDomain, IsHomogeneousList ] );
DeclareOperation( "DerivationByImagesNC", 
    [ Is2DimensionalDomain, IsHomogeneousList ] );
##  usage: DerivationByImages( XM, im, [, true|false ] )

##############################################################################
##
#O  DerivationImage                  image of  r \in R  by the derivation  chi
##
DeclareOperation( "DerivationImage", [ IsDerivation, IsObject ] );

###############################################################################
##
#O  DerivationBySection  construct an XMod derivation from a cat1-group section
##
DeclareOperation( "DerivationBySection", [ IsSection ] );

##############################################################################
##
#O  IdentityDerivation          construct the identity derivation for an  XMod 
#O  IdentitySection            construct the identity section for a cat1-group 
##
DeclareAttribute( "IdentityDerivation", IsXMod );
DeclareAttribute( "IdentitySection", IsCat1Group );

##############################################################################
##
#O  PrincipalDerivation              derivation determined by choice of s in S
#A  PrincipalDerivations                         list of principal derivations
##
DeclareOperation( "PrincipalDerivation", [ IsXMod, IsObject ] );
DeclareAttribute( "PrincipalDerivations", IsXMod );

##############################################################################
##
#O  CompositeDerivation                 Whitehead composite of two derivations
#O  WhiteheadProduct                    Whitehead composite of two derivations
#O  WhiteheadOrder            order of a derivation using the WhitefeadProduct
##
DeclareOperation( "CompositeDerivation", [ IsDerivation, IsDerivation ] );
DeclareOperation( "WhiteheadProduct", 
    [ IsUp2DimensionalMapping, IsUp2DimensionalMapping ] ); 
DeclareOperation( "WhiteheadOrder", [ IsUp2DimensionalMapping ] ); 

##############################################################################
##
#P  IsRegularDerivation                   so an element of the Whitehead group
##
DeclareProperty( "IsRegularDerivation", IsDerivation );

InstallTrueMethod( IsDerivation, IsRegularDerivation );

##############################################################################
##
#A  SourceEndomorphism       upmapping determines endomorphism of source group
#A  RangeEndomorphism        upmapping determines endomorphism of range group
#A  Object2dEndomorphism     upmapping determines endomorphism of xmod or cat1
##
DeclareAttribute( "SourceEndomorphism", IsUp2DimensionalMapping );
DeclareAttribute( "RangeEndomorphism", IsUp2DimensionalMapping );
DeclareAttribute( "Object2dEndomorphism", IsUp2DimensionalMapping );

#############################################################################
##
#O  InverseDerivations      Finds all semigroup inverses XJ for derivation Xi
##                                                i.e.  XiXjXi=Xi & XjXiXj=Xj
DeclareOperation( "InverseDerivations", [ IsDerivation ] );

##############################################################################
##
#O  ListInverseDerivations               List all inverses for each derivation
##
DeclareAttribute( "ListInverseDerivations", IsXMod );


##############################################################################
##                                 Sections                                 ##
##############################################################################

##############################################################################
##
#O  SectionByHomomorphism                 converts a homomorphism to a section
#O  SectionByHomomorphismNC               converts a homomorphism to a section
##
DeclareOperation( "SectionByHomomorphism", 
    [ IsPreCat1Group, IsGroupHomomorphism ] );
DeclareOperation( "SectionByHomomorphismNC", 
    [ IsPreCat1Group, IsGroupHomomorphism ] );

##############################################################################
##
#O  SectionByDerivation      the cat1-group section determined by a derivation
##
DeclareOperation( "SectionByDerivation", [ IsDerivation ] );

##############################################################################
##
#O  CompositeSection                       Whitehead composite of two sections
##
DeclareOperation( "CompositeSection", [ IsSection, IsSection ] );


#############################################################################
##                      Monoids of Derivations or Sections                 ##
#############################################################################

#############################################################################
##
#P  IsMonoidOfUp2DimensionalMappings( <obj> )
#R  IsMonoidOfUp2DimensionalMappingsObj( <obj> )
##
##  An Up2DimensionalMappings record stores images lists and composition table
##
DeclareProperty( "IsMonoidOfUp2DimensionalMappings", IsObject );
DeclareRepresentation( "IsMonoidOfUp2DimensionalMappingsObj",
    IsMonoidOfUp2DimensionalMappings and IsAttributeStoringRep,
    [ "Object2d", "ImagesList" ] );

##############################################################################
##
#A  ImagesList                                returns list of DerivationImages
#A  DerivationClass                                 type of derivations record
#A  ImagesTable                                   returns lists of image lists
##
DeclareAttribute( "ImagesList", IsMonoidOfUp2DimensionalMappings );
DeclareAttribute( "DerivationClass", IsMonoidOfUp2DimensionalMappings );
DeclareAttribute( "ImagesTable", IsMonoidOfUp2DimensionalMappings );

#############################################################################
##
#O  MonoidOfUp2DimensionalMappingsObj( <obj>, <images>, <str> )
#F  MonoidOfUp2DimensionalMappingsFamily . . .  family for up-mappings monoid 
#T  MonoidOfUp2DimensionalMappingsType . . . . .  type for up-mappings monoid 
#P  IsMonoidOfDerivations                               
#P  IsMonoidOfSections
##
DeclareOperation( "MonoidOfUp2DimensionalMappingsObj",
    [ Is2DimensionalDomain, IsHomogeneousList, IsString ] );
MonoidOfUp2DimensionalMappingsFamily := 
    CollectionsFamily( Up2DimensionalMappingFamily ); 
BindGlobal( "MonoidOfUp2DimensionalMappingsType", 
            NewType( MonoidOfUp2DimensionalMappingsFamily, 
                     IsMonoidOfUp2DimensionalMappingsObj ) ); 
DeclareProperty( "IsMonoidOfDerivations", IsMonoidOfUp2DimensionalMappings );
DeclareProperty( "IsMonoidOfSections", IsMonoidOfUp2DimensionalMappings );

##############################################################################
##
#A  RegularDerivations    find all invertible derivations for a crossed module
#A  AllDerivations                   find all derivations for a crossed module
#A  RegularSections              find all invertible sections for a cat1-group
#A  AllSections                             find all sections for a cat1-group
##
DeclareAttribute( "RegularDerivations", IsXMod );
DeclareAttribute( "AllDerivations", IsXMod );
DeclareAttribute( "RegularSections", IsCat1Group );
DeclareAttribute( "AllSections", IsCat1Group );

##############################################################################
##
#O  BacktrackDerivationsJ          recursive function for BacktrackDerivations
#O  BacktrackDerivations            recursive construction for all derivations
#O  BacktrackSectionsJ         recursion used by RegularSections & AllSections
##
DeclareOperation( "BacktrackDerivationsJ",  [ IsXMod, IsHomogeneousList,
    IsHomogeneousList, IsHomogeneousList, IsInt, IsString ] );
DeclareOperation( "BacktrackDerivations", [ IsXMod, IsString ] );
DeclareOperation( "BacktrackSectionsJ", [ IsRecord, IsInt, IsObject, IsInt ] );

#############################################################################
##
#A  WhiteheadMonoidTable( XM )               Table of products of derivations
#A  WhiteheadGroupTable( XM )        Table of products of regular derivations
##
##  ?? should these refer just to 2DimensionalDomains ??
##
DeclareAttribute( "WhiteheadMonoidTable", IsXMod );
DeclareAttribute( "WhiteheadGroupTable", IsXMod );

#############################################################################
##
#A  WhiteheadPermGroup( XM )                     a permutation representation
#A  WhiteheadGroupGeneratingDerivations                   generators for W(X)
#A  WhiteheadGroupGeneratorPositions         positions of generators for W(X)
#A  WhiteheadTransformationMonoid( XM )                a transformation representation
##
DeclareAttribute( "WhiteheadPermGroup", IsXMod );
DeclareAttribute( "WhiteheadGroupGeneratingDerivations", IsXMod );
DeclareAttribute( "WhiteheadGroupGeneratorPositions", IsXMod );
DeclareAttribute( "WhiteheadTransformationMonoid", IsXMod );
