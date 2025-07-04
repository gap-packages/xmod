#############################################################################
##
#W  dom2dnd.gd                 GAP4 package `XMod'              Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  

#############################################################################
##
#V  InfoXMod
##
DeclareInfoClass( "InfoXMod" );


######################  HIGHER DIMENSIONAL DOMAINS  ######################### 

############################################################################# 
## 
#C  IsHigherDimensionalDomain( <obj> ) . . . . test if object is n-dim domain 
#F  MakeHigherDimensionalDomain( <src>, <rng> ) 
## 
DeclareCategory( "IsHigherDimensionalDomain", IsDomain ); 
DeclareGlobalFunction( "MakeHigherDimensionalDomain" ); 

######################  HIGHER DIMENSIONAL MAGMAS  ########################## 

############################################################################# 
## 
#C  IsHigherDimensionalMagma( <m3d> ) . . . . . . .  category of n-dim magmas 
#C  IsHigherDimensionalMagmaWithOne( <m3d> )  . . . . . . . . . . .  with one
#C  IsHigherDimensionalMagmaWithInverses( <m3d> ) . . . . . . .  and inverses
##
DeclareCategory( "IsHigherDimensionalMagma", IsHigherDimensionalDomain  
    and CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "IsHigherDimensionalMagma" ); 
DeclareCategory( "IsHigherDimensionalMagmaWithOne", IsHigherDimensionalMagma 
    and CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "IsHigherDimensionalMagmaWithInverses", 
    IsHigherDimensionalMagmaWithOne and 
    CategoryCollections( IsMultiplicativeElementWithInverse ) ); 

############################################################################# 
##  
#V  FamilyHigherDimensionalMagma . . . . family for higher dimensional magmas 
##  
BindGlobal( "FamilyHigherDimensionalMagma", 
    NewFamily( "FamilyHigherDimensionalMagma", IsHigherDimensionalMagma, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################ 
## 
#F  MakeHigherDimensionalMagma( <mag>, <obs> ) 
## 
DeclareGlobalFunction( "MakeHigherDimensionalMagma" ); 


################ HIGHER DIMENSIONAL SEMIGROUPS and MONOIDS ##################

#############################################################################
##
#C  IsHigherDimensionalSemigroup( <mag> )
#F  MakeHigherDimensionalSemigroup( <mag>, <obs> )  
#O  SinglePieceHigherDimensionalSemigroup( <sgp>, <obs> ) 
##
DeclareCategory( "IsHigherDimensionalSemigroup", 
    IsHigherDimensionalMagma and CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "MakeHigherDimensionalSemigroup" ); 

#############################################################################
##
#C  IsHigherDimensionalMonoid( <ndmagma> )
#F  MakeHigherDimensionalMonoid( <mag>, <obs> )  
#O  SinglePieceHigherDimensionalMonoid( <mon>, <obs> ) 
##
DeclareCategory( "IsHigherDimensionalMonoid", 
    IsHigherDimensionalMagmaWithOne 
    and CategoryCollections(IsAssociativeElement) );
DeclareGlobalFunction( "MakeHigherDimensionalMonoid" ); 


########################  HIGHER DIMENSIONAL GROUPS  ########################
##  A *higher dimensional group* is a higher dimensional magma where 
##  all the objects are groups: see file gpnobj.gd.

#############################################################################
##
#C  IsHigherDimensionalGroup( <obj> ) 
##
DeclareCategory( "IsHigherDimensionalGroup", 
    IsHigherDimensionalMagmaWithInverses 
    and CategoryCollections( IsAssociativeElement ) );

############################################################################# 
##  
#V  FamilyHigherDimensionalGroup . . . . family for higher dimensional groups
##  
BindGlobal( "FamilyHigherDimensionalGroup", 
    NewFamily( "Family2DimensionalGroup", IsHigherDimensionalGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#O  DisplayLeadMaps( <obj> ) 
##
DeclareOperation( "DisplayLeadMaps", [ IsHigherDimensionalDomain ] ); 


#######################  HIGHER DIMENSIONAL SUBDOMAINS  #####################

############################################################################# 
## 
#O  IsSubHigherDimensionalDomain( <D>, <U> )
#F  SubHigherDimensionalDomain( <args> )              
## 
DeclareOperation( "IsSubHigherDimensionalDomain", 
    [ IsHigherDimensionalDomain, IsHigherDimensionalDomain ] );
DeclareOperation( "SubHigherDimensionalDomain", 
    [ IsHigherDimensionalDomain, IsList ] );

#############################################################################
##
#A  DirectProductHigherDimensionalInfo( <D> }
##
DeclareAttribute( "DirectProductHigherDimensionalInfo", 
     IsHigherDimensionalDomain, "mutable" );


#########################  2 DIMENSIONAL DOMAINS  ########################### 

############################################################################# 
## 
#C  Is2DimensionalDomain( <obj> ) . . test if object is a 2Dimensional-domain 
#F  Make2DimensionalDomain( <src>, <rng> ) 
## 
DeclareCategory( "Is2DimensionalDomain", IsHigherDimensionalDomain ); 
DeclareGlobalFunction( "Make2DimensionalDomain" ); 

#########################  2 DIMENSIONAL MAGMAS  ############################ 

############################################################################# 
## 
#C  Is2DimensionalMagma( <m2d> ) . . . . . . category of 2Dimensional magmas 
#C  Is2DimensionalMagmaWithOne( <m2d> ) . . . . . . . . . . . . . . with one
#C  Is2DimensionalMagmaWithInverses( <m2d> )  . . . . . . . . . and inverses
##
DeclareCategory( "Is2DimensionalMagma", Is2DimensionalDomain and 
    CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "Is2DimensionalMagma" ); 
DeclareCategory( "Is2DimensionalMagmaWithOne", Is2DimensionalMagma 
    and CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "Is2DimensionalMagmaWithInverses", 
    Is2DimensionalMagmaWithOne 
    and CategoryCollections( IsMultiplicativeElementWithInverse ) ); 

############################################################################# 
##  
#V  Family2DimensionalMagma . . . . . . . . .  family for 2Dimensional-magmas 
##  
BindGlobal( "Family2DimensionalMagma", 
    NewFamily( "Family2DimensionalMagma", Is2DimensionalMagma, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################ 
## 
#F  Make2DimensionalMagma( <mag>, <obs> ) 
## 
DeclareGlobalFunction( "Make2DimensionalMagma" ); 


################## 2 DIMENSIONAL SEMIGROUPS and MONOIDS #####################

#############################################################################
##
#C  Is2DimensionalSemigroup( <2dmag> )
#F  Make2DimensionalSemigroup( <mag>, <obs> )  
#O  SinglePiece2DimensionalSemigroup( <sgp>, <obs> ) 
##
DeclareCategory( "Is2DimensionalSemigroup", 
    Is2DimensionalMagma and CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "Make2DimensionalSemigroup" ); 
## DeclareOperation( "SinglePiece2DimensionalSemigroup", 
##     [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#C  Is2DimensionalMonoid( <2dmagma> )
#F  Make2DimensionalMonoid( <mag>, <obs> )  
#O  SinglePiece2DimensionalMonoid( <mon>, <obs> ) 
##
DeclareCategory( "Is2DimensionalMonoid", 
    Is2DimensionalMagmaWithOne and CategoryCollections(IsAssociativeElement) );
DeclareGlobalFunction( "Make2DimensionalMonoid" ); 
##  DeclareOperation( "SinglePiece2DimensionalMonoid", 
##      [ IsMonoid, IsCollection ] ); 


##########################  2 DIMENSIONAL GROUPS  ###########################

##  A *2Dimensional-group* is a 2Dimensional-magma where source and range are 
##  both groups, and so is a pre-crossed module or pre-cat1-group: 
##  see file gp2obj.gd.

#############################################################################
##
#S  Is2DimensionalGroup( <m2d> ) 
##
DeclareSynonymAttr( "Is2DimensionalGroup", Is2DimensionalMagmaWithInverses
    and CategoryCollections( IsAssociativeElement ) );

############################################################################# 
##  
#V  Family2DimensionalGroup . . . . . . . . .  family for 2Dimensional-groups 
#T  Type2DimensionalGroup . . . . . . . . . . .  type for 2Dimensional-groups 
##  
BindGlobal( "Family2DimensionalGroup", NewFamily( "Family2DimensionalGroup", 
    Is2DimensionalGroup, CanEasilySortElements, CanEasilySortElements ) ); 

#########################  2 DIMENSIONAL SUBDOMAINS  ######################## 

############################################################################# 
## 
#O  IsSub2DimensionalDomain( <D>, <U> )
#F  Sub2DimensionalDomain( <args> )              
## 
DeclareOperation( "IsSub2DimensionalDomain", 
    [ Is2DimensionalDomain, Is2DimensionalDomain ] );
DeclareOperation( "Sub2DimensionalDomain", 
    [ Is2DimensionalDomain, IsGroup, IsGroup ] );

#########################  3 DIMENSIONAL DOMAINS  ########################### 

############################################################################# 
## 
#C  Is3DimensionalDomain( <obj> ) . . test if object is a 3Dimensional-domain 
#F  Make3DimensionalDomain( <up>, <lt>, <rt>, <dn> ) 
## 
DeclareCategory( "Is3DimensionalDomain", IsHigherDimensionalDomain ); 
DeclareGlobalFunction( "Make3DimensionalDomain" ); 

#########################  3 DIMENSIONAL MAGMAS  ############################ 

############################################################################# 
## 
#C  Is3DimensionalMagma( <m3d> ) . . . . . . category of 3Dimensional magmas 
#C  Is3DimensionalMagmaWithOne( <m3d> ) . . . . . . . . . . . . . . with one
#C  Is3DimensionalMagmaWithInverses( <m3d> )  . . . . . . . . . and inverses
##
DeclareCategory( "Is3DimensionalMagma", Is3DimensionalDomain and 
    CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "Is3DimensionalMagma" ); 
DeclareCategory( "Is3DimensionalMagmaWithOne", Is3DimensionalMagma 
    and CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "Is3DimensionalMagmaWithInverses", Is3DimensionalMagmaWithOne 
    and CategoryCollections( IsMultiplicativeElementWithInverse ) ); 

############################################################################# 
##  
#V  Family3DimensionalMagma . . . . . . . . .  family for 3Dimensional-magmas 
##  
BindGlobal( "Family3DimensionalMagma", 
    NewFamily( "Family3DimensionalMagma", Is3DimensionalMagma, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################ 
## 
#F  Make3DimensionalMagma( <mag>, <obs> ) 
## 
DeclareGlobalFunction( "Make3DimensionalMagma" ); 


################## 3 DIMENSIONAL SEMIGROUPS and MONOIDS #####################

#############################################################################
##
#C  Is3DimensionalSemigroup( <3dmag> )
#F  Make3DimensionalSemigroup( <mag>, <obs> )  
#O  SinglePiece3DimensionalSemigroup( <sgp>, <obs> ) 
##
DeclareCategory( "Is3DimensionalSemigroup", 
    Is3DimensionalMagma and CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "Make3DimensionalSemigroup" ); 
## DeclareOperation( "SinglePiece3DimensionalSemigroup", 
##     [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#C  Is3DimensionalMonoid( <3dmagma> )
#F  Make3DimensionalMonoid( <mag>, <obs> )  
#O  SinglePiece3DimensionalMonoid( <mon>, <obs> ) 
##
DeclareCategory( "Is3DimensionalMonoid", 
    Is3DimensionalMagmaWithOne and CategoryCollections(IsAssociativeElement) );
DeclareGlobalFunction( "Make3DimensionalMonoid" ); 
##  DeclareOperation( "SinglePiece3DimensionalMonoid", 
##      [ IsMonoid, IsCollection ] ); 


##########################  3 DIMENSIONAL GROUPS  ###########################

##  A *3Dimensional-group* is a 3Dimensional-magma where the up, left, right 
##  and down components are all 2Dimensional-groups, and so is a pre-crossed 
##  square or pre-cat2-group: see file gp3obj.gd.

#############################################################################
##
#S  Is3DimensionalGroup( <m3d> ) 
##
DeclareSynonymAttr( "Is3DimensionalGroup", Is3DimensionalMagmaWithInverses and 
    CategoryCollections( IsAssociativeElement ) );

############################################################################# 
##  
#V  Family3DimensionalGroup . . . . . . . . .  family for 3Dimensional-groups 
#T  Type3DimensionalGroup . . . . . . . . . . .  type for 3Dimensional-groups 
##  
BindGlobal( "Family3DimensionalGroup", NewFamily( "Family3DimensionalGroup", 
    Is3DimensionalGroup, CanEasilySortElements, CanEasilySortElements ) ); 

#########################  3 DIMENSIONAL SUBDOMAINS  ######################## 

############################################################################# 
## 
#O  IsSub3DimensionalDomain( <D>, <U> )
#F  Sub3DimensionalDomain( <args> )              
## 
DeclareOperation( "IsSub3DimensionalDomain", 
    [ Is3DimensionalDomain, Is3DimensionalDomain ] );
DeclareOperation( "Sub3DimensionalDomain", 
    [ Is3DimensionalDomain, IsGroup, IsGroup ] );
