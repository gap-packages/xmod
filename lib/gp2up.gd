##############################################################################
##
#W  gp2up.gd                   GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##
##  version 2.43, 21/10/2015 
##
##  This file contains declarations for UpMappings, Derivations and Sections
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#C  IsUp2dMapping( <map> )
#R  IsUp2dMappingRep( <map> )
#O  Up2dMappingObj( <obj>, <ims> )
##
##  A section|derivation is determined by a cat1-group|xmod + generator images
##
DeclareCategory( "IsUp2dMapping", IsGeneralMapping );
DeclareRepresentation( "IsUp2dMappingRep", IsUp2dMapping and 
    IsAttributeStoringRep, [ "Object2d", "UpGeneratorImages", 
                             "UpHomomorphism", "UpImagePositions" ] );
DeclareOperation( "Up2dMappingObj", [ Is2dDomain, IsHomogeneousList ] );

#############################################################################
##
#C  IsUp2dMappingCollection . . . . . . . . . category of colls of up-2d-maps
#C  IsUp2dMappingCollColl . . . . . . . . . . category of colls of colls 
#C  IsUp2dMappingCollCollColl . . . . . . . . category of colls, colls, colls
#V  Up2dMappingFamily . . . . . . . . . . family for derivations and sections
##
DeclareCategoryCollections( "IsUp2dMapping" );
DeclareCategoryCollections( "IsUp2dMappingCollection" );
DeclareCategoryCollections( "IsUp2dMappingCollColl" );
BindGlobal( "Up2dMappingFamily", 
    NewFamily( "Up2dMappingFamily", IsUp2dMapping, 
               CanEasilySortElements, CanEasilySortElements ) ); 

##############################################################################
##
#A  Object2d( <map> )
#A  UpHomomorphism( <map> ) 
#A  UpGeneratorImages( <map> )
#A  UpImagePositions( <map> )
##
DeclareAttribute( "Object2d", IsUp2dMapping ); 
DeclareAttribute( "UpHomomorphism", IsUp2dMapping ); 
DeclareAttribute( "UpGeneratorImages", IsUp2dMapping );
DeclareAttribute( "UpImagePositions", IsUp2dMapping );

#############################################################################
##
#P  IsDerivation( <map> )
#P  IsSection( <map> )
##
DeclareProperty( "IsDerivation", IsUp2dMapping );
DeclareProperty( "IsSection", IsUp2dMapping );


##############################################################################
##                               Derivations                                ##
##############################################################################

##############################################################################
##
#O  DerivationByImages                                     sets up the mapping
#O  DerivationByImagesNC                                   sets up the mapping
##
DeclareOperation( "DerivationByImages", [ Is2dDomain, IsHomogeneousList ] );
DeclareOperation( "DerivationByImagesNC", [ Is2dDomain, IsHomogeneousList ] );
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
#O  PrincipalDerivation              derivation determined by choice of s in S
#A  PrincipalDerivations                         list of principal derivations
##
DeclareOperation( "PrincipalDerivation", [ IsXMod, IsObject ] );
DeclareAttribute( "PrincipalDerivations", IsXMod );

##############################################################################
##
#O  CompositeDerivation                 Whitehead composite of two derivations
##
DeclareOperation( "CompositeDerivation", [ IsDerivation, IsDerivation ] );

##############################################################################
##
#P  IsRegularDerivation                   so an element of the Whitehead group
##
DeclareProperty( "IsRegularDerivation", IsDerivation );

##############################################################################
##
#A  SourceEndomorphism       upmapping determines endomorphism of source group
#A  RangeEndomorphism        upmapping determines endomorphism of range group
#A  Object2dEndomorphism     upmapping determines endomorphism of xmod or cat1
##
DeclareAttribute( "SourceEndomorphism", IsUp2dMapping );
DeclareAttribute( "RangeEndomorphism", IsUp2dMapping );
DeclareAttribute( "Object2dEndomorphism", IsUp2dMapping );

#############################################################################
##
#O  InverseDerivations      Finds all semigroup inverses XJ for derivation Xi
##                                                i.e.  XiXjXi=Xi & XjXiXj=Xj
DeclareOperation( "InverseDerivations", [ IsDerivation ] );

##############################################################################
##
#O  ListInverseDerivations               List all inverses for each derivation
##
DeclareOperation( "ListInverseDerivations", [ IsXMod ] );


##############################################################################
##                                 Sections                                 ##
##############################################################################

##############################################################################
##
#O  SectionByImages                                   sets up GroupHomByImages
#O  SectionByImagesNC                                 sets up GroupHomByImages
##
DeclareOperation( "SectionByImages", [ Is2dDomain, IsGroupHomomorphism ] );
DeclareOperation( "SectionByImagesNC", [ Is2dDomain, IsGroupHomomorphism ] );
##  usage: SectionByImages( C, im, [, true|false ] )

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
#P  IsMonoidOfUp2dMappings( <obj> )
#R  IsMonoidOfUp2dMappingsObj( <obj> )
##
##  An Up2dMappings record stores images lists and composition table
##
DeclareProperty( "IsMonoidOfUp2dMappings", IsObject );
DeclareRepresentation( "IsMonoidOfUp2dMappingsObj",
    IsMonoidOfUp2dMappings and IsAttributeStoringRep,
    [ "Object2d", "ImagesList" ] );

##############################################################################
##
#A  ImagesList                                returns list of DerivationImages
#A  AllOrRegular                                    type of derivations record
#A  ImagesTable                                   returns lists of image lists
##
DeclareAttribute( "ImagesList", IsMonoidOfUp2dMappings );
DeclareAttribute( "AllOrRegular", IsMonoidOfUp2dMappings );
DeclareAttribute( "ImagesTable", IsMonoidOfUp2dMappings );

#############################################################################
##
#O  MonoidOfUp2dMappingsObj( <obj>, <images>, <str> )
#P  IsMonoidOfDerivations                               
#P  IsMonoidOfSections
##
DeclareOperation( "MonoidOfUp2dMappingsObj",
    [ Is2dDomain, IsHomogeneousList, IsString ] );
DeclareProperty( "IsMonoidOfDerivations", IsMonoidOfUp2dMappings );
DeclareProperty( "IsMonoidOfSections", IsMonoidOfUp2dMappings );

##############################################################################
##
#A  RegularDerivations    find all invertible derivations for a crossed module
#A  AllDerivations                   find all derivations for a crossed module
#A  RegularSections              find all invertible sections for a cat1-group
#A  AllSections                             find all sections for a cat1-group
##
DeclareAttribute( "RegularDerivations", IsXMod );
DeclareAttribute( "AllDerivations", IsXMod );
DeclareAttribute( "RegularSections", IsCat1 );
DeclareAttribute( "AllSections", IsCat1 );

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
##  ?? should these refer just to 2dDomains ??
##
DeclareAttribute( "WhiteheadMonoidTable", IsXMod );
DeclareAttribute( "WhiteheadGroupTable", IsXMod );

#############################################################################
##
#A  WhiteheadPermGroup( XM )                     a permutation representation
#A  WhiteheadGroupGeneratingDerivations                   generators for W(X)
#A  WhiteheadGroupGeneratorPositions         positions of generators for W(X)
#A  WhiteheadTransMonoid( XM )                a transformation representation
##
DeclareAttribute( "WhiteheadPermGroup", IsXMod );
DeclareAttribute( "WhiteheadGroupGeneratingDerivations", IsXMod );
DeclareAttribute( "WhiteheadGroupGeneratorPositions", IsXMod );
DeclareAttribute( "WhiteheadTransMonoid", IsXMod );

###############################################################################
##
#E  gp2up.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
