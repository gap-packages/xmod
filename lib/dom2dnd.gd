##############################################################################
##
#W  dom2dnd.gd                 GAP4 package `XMod'               Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

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
#P  IsHigherDimensionalSemigroup( <ndmag> )
#F  MakeHigherDimensionalSemigroup( <mag>, <obs> )  
#O  SinglePieceHigherDimensionalSemigroup( <sgp>, <obs> ) 
##
DeclareSynonymAttr( "IsHigherDimensionalSemigroup", 
    IsHigherDimensionalMagma and CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "MakeHigherDimensionalSemigroup" ); 
## DeclareOperation( "SinglePieceHigherDimensionalSemigroup", 
##     [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#P  IsHigherDimensionalMonoid( <ndmagma> )
#F  MakeHigherDimensionalMonoid( <mag>, <obs> )  
#O  SinglePieceHigherDimensionalMonoid( <mon>, <obs> ) 
##
DeclareSynonymAttr( "IsHigherDimensionalMonoid", 
    IsHigherDimensionalMagmaWithOne 
    and CategoryCollections(IsAssociativeElement) );
DeclareGlobalFunction( "MakeHigherDimensionalMonoid" ); 
##  DeclareOperation( "SinglePieceHigherDimensionalMonoid", 
##      [ IsMonoid, IsCollection ] ); 


########################  HIGHER DIMENSIONAL GROUPS  ########################
##  A *higher dimensional group* is a higher dimensional magma where 
##  all the objects are groups: see file gpnobj.gd.

#############################################################################
##
#S  IsHigherDimensionalGroup( <obj> ) 
##
DeclareSynonymAttr( "IsHigherDimensionalGroup", 
    IsHigherDimensionalMagmaWithInverses 
    and CategoryCollections( IsAssociativeElement ) );

############################################################################# 
##  
#V  FamilyHigherDimensionalGroup . . . . family for higher dimensional groups
##  
BindGlobal( "FamilyHigherDimensionalGroup", 
    NewFamily( "Family2DimensionalGroup", IsHigherDimensionalGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 


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


#########################  2 DIMENSIONAL DOMAINS  ########################### 

############################################################################# 
## 
#C  Is2DimensionalDomain( <obj> ) . . test if object is a 2Dimensional-domain 
#F  Make2DimensionalDomain( <src>, <rng> ) 
#O  KindOf2DimensionalDomain( <m2d> ) 
## 
DeclareCategory( "Is2DimensionalDomain", IsHigherDimensionalDomain ); 
DeclareGlobalFunction( "Make2DimensionalDomain" ); 
DeclareOperation( "KindOf2DimensionalDomain", [ IsList ] );  

##  temporary measure to keep XModAlg working:
DeclareSynonym( "Is2dDomain", Is2DimensionalDomain ); 

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
DeclareCategory( "Is2DimensionalMagmaWithInverses", Is2DimensionalMagmaWithOne 
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
#P  Is2DimensionalSemigroup( <2dmag> )
#F  Make2DimensionalSemigroup( <mag>, <obs> )  
#O  SinglePiece2DimensionalSemigroup( <sgp>, <obs> ) 
##
DeclareSynonymAttr( "Is2DimensionalSemigroup", 
    Is2DimensionalMagma and CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "Make2DimensionalSemigroup" ); 
## DeclareOperation( "SinglePiece2DimensionalSemigroup", 
##     [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#P  Is2DimensionalMonoid( <2dmagma> )
#F  Make2DimensionalMonoid( <mag>, <obs> )  
#O  SinglePiece2DimensionalMonoid( <mon>, <obs> ) 
##
DeclareSynonymAttr( "Is2DimensionalMonoid", 
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
DeclareSynonymAttr( "Is2DimensionalGroup", Is2DimensionalMagmaWithInverses and 
    CategoryCollections( IsAssociativeElement ) );

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

#############################################################################
##
#E  dom2dnd.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
