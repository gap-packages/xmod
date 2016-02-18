##############################################################################
##
#W  dom2d3d.gd                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#V  InfoXMod
##
DeclareInfoClass( "InfoXMod" );

###############################  2d DOMAIN   ################################ 

############################################################################# 
## 
#C  Is2dDomain( <obj> ) . . . . . . . . . . . . test if object is a 2d-domain 
#F  Make2dDomain( <src>, <rng> ) 
#O  TypeOf2dDomain( <m2d> ) 
## 
DeclareCategory( "Is2dDomain", IsDomain ); 
DeclareGlobalFunction( "Make2dDomain" ); 
DeclareOperation( "TypeOf2dDomain", [ IsList ] );  


###############################  2d MAGMAS  ################################# 

############################################################################# 
## 
#C  Is2dMagma( <m2d> ) . . . . . . . . . . . . . . . . category of 2d magmas 
#C  Is2dMagmaWithOne( <m2d> ) . . . . . . . . . . . . . . . . . . . with one
#C  Is2dMagmaWithInverses( <m2d> )  . . . . . . . . . . . . . . and inverses
##
DeclareCategory( "Is2dMagma", Is2dDomain and 
    CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "Is2dMagma" ); 
DeclareCategory( "Is2dMagmaWithOne", Is2dMagma and 
    CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "Is2dMagmaWithInverses", Is2dMagmaWithOne and 
    CategoryCollections( IsMultiplicativeElementWithInverse ) ); 

############################################################################# 
##  
#V  Family2dMagma . . . . . . . . . . . . . . . . . . . family for 2d-magmas 
##  
BindGlobal( "Family2dMagma", 
    NewFamily( "Family2dMagma", Is2dMagma, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################ 
## 
#F  Make2dMagma( <mag>, <obs> ) 
## 
DeclareGlobalFunction( "Make2dMagma" ); 


###########################  SEMIGROUPS and MONOIDS #########################

#############################################################################
##
#P  Is2dSemigroup( <2dMagma> )
#F  Make2dSemigroup( <mag>, <obs> )  
#O  SinglePiece2dSemigroup( <sgp>, <obs> ) 
##
DeclareSynonymAttr( "Is2dSemigroup", Is2dMagma and 
    CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "Make2dSemigroup" ); 
## DeclareOperation( "SinglePiece2dSemigroup", [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#P  Is2dMonoid( <2dMagma> )
#F  Make2dMonoid( <mag>, <obs> )  
#O  SinglePiece2dMonoid( <mon>, <obs> ) 
##
DeclareSynonymAttr( "Is2dMonoid", Is2dMagmaWithOne and 
    CategoryCollections( IsAssociativeElement ) );
DeclareGlobalFunction( "Make2dMonoid" ); 
##  DeclareOperation( "SinglePiece2dMonoid", [ IsMonoid, IsCollection ] ); 


#################################  GROUPS  ##################################

##  A *2d-group* is a 2d-magma where source and range are both groups, 
##  and so is a pre-crossed module or pre-cat1-group - see file gp2obj.gd.

#############################################################################
##
#S  Is2dGroup( <m2d> ) 
##
DeclareSynonymAttr( "Is2dGroup", Is2dMagmaWithInverses and 
    CategoryCollections( IsAssociativeElement ) );

############################################################################# 
##  
#V  Family2dGroup . . . . . . . . . . . . . . . . . . . family for 2d-groups
##  
BindGlobal( "Family2dGroup", NewFamily( "Family2dGroup", Is2dGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 


#################################  SUBDOMAINS  ############################## 

############################################################################# 
## 
#O  IsSub2dDomain( <D>, <U> )
#F  Sub2dDomain( <args> )              
## 
DeclareOperation( "IsSub2dDomain", [ Is2dDomain, Is2dDomain ] );
DeclareOperation( "Sub2dDomain", [ Is2dDomain, IsGroup, IsGroup ] );


###############################  3d DOMAIN   ################################ 

############################################################################# 
## 
#C  Is3dDomain( <obj> ) . . . . . . . . . . . . test if object is a 3d-domain 
#F  Make3dDomain( <src>, <rng> ) 
#O  TypeOf3dDomain( <m3d> ) 
## 
DeclareCategory( "Is3dDomain", Is2dDomain ); 
DeclareGlobalFunction( "Make3dDomain" ); 
DeclareOperation( "TypeOf3dDomain", [ IsList ] );  


###############################  3d MAGMAS  ################################# 

############################################################################# 
## 
#C  Is3dMagma( <m3d> ) . . . . . . . . . . . . . . . . category of 3d magmas 
#C  Is3dMagmaWithOne( <m3d> )  . . . . . . . . . . . . . . . . . . with one
#C  Is3dMagmaWithInverses( <m3d> ) . . . . . . . . . . . . . . and inverses
#C  Is3dGroup( <m3d> ) 
##
DeclareCategory( "Is3dMagma", Is3dDomain and 
    CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "Is3dMagma" ); 
DeclareCategory( "Is3dMagmaWithOne", Is3dMagma and 
    CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "Is3dMagmaWithInverses", Is3dMagmaWithOne and 
    CategoryCollections( IsMultiplicativeElementWithInverse ) ); 
DeclareCategory( "Is3dGroup", Is3dMagmaWithInverses and 
    CategoryCollections( IsAssociativeElement ) ); 

############################################################################# 
##  
#V  Family3dGroup . . . . . . . . . . . . . . . . . . . family for 3d-groups
##  
BindGlobal( "Family3dGroup", NewFamily( "Family3dGroup", Is3dGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 


#############################################################################
##
#E  dom2d3d.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
