##############################################################################
##
#W  dom2d3d.gd                 GAP4 package `XMod'               Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
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


##############################  MULT 2d ELEMENTS  ########################### 

############################################################################# 
## 
#C  IsMultiplicativeElementWithTuple( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeElementWithTuple", IsMultiplicativeElement ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTuple" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTupleCollection" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTupleCollColl" ); 

############################################################################# 
## 
#C  IsMultiplicativeElementWithTupleAndOnes( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeElementWithTupleAndOnes", 
    IsMultiplicativeElementWithTuple ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTupleAndOnes" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTupleAndOnesCollection" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithTupleAndOnesCollColl" ); 

############################################################################# 
## 
#C  IsMultiplicativeElementWithTupleAndInversesIfNonzero( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeElementWithTupleAndInversesIfNonzero", 
    IsMultiplicativeElementWithTupleAndOnes ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithTupleAndInversesIfNonzero" ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithTupleAndInversesIfNonzeroCollection" ); 
DeclareCategoryCollections(     
    "IsMultiplicativeElementWithTupleAndInversesIfNonzeroCollColl" ); 

############################################################################# 
## 
#C  IsGroupElementWithTuple( <elt> ) 
## 
DeclareCategory( "IsGroupElementWithTuple", 
    IsMultiplicativeElementWithTupleAndInversesIfNonzero ); 
DeclareCategoryCollections( "IsGroupElementWithTuple" ); 
DeclareCategoryCollections( "IsGroupElementWithTupleCollection" ); 
DeclareCategoryCollections( "IsGroupElementWithTupleCollColl" ); 

############################################################################# 
## 
#R  IsMultiplicativeElementWithTuplePosRep 
## 
DeclareRepresentation( "IsMultiplicativeElementWithTuplePosRep", 
  IsPositionalObjectRep and IsMultiplicativeElementWithTuple, [1..2] ); 

############################################################################# 
##  
#V  MultiplicativeElementWithTupleFamily . .  family for elements of 2dMagmas 
#V  GroupElementWithTupleFamily  . . . . . . . . . . family for elements of PreXMods
##  
BindGlobal( "MultiplicativeElementWithTupleFamily", 
    NewFamily( "MultiplicativeElementWithTupleFamily", 
               IsMultiplicativeElementWithTuple, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "GroupElementWithTupleFamily", 
    NewFamily( "GroupElementWithTupleFamily", IsGroupElementWithTuple, 
               CanEasilySortElements, CanEasilySortElements ) ); 


###############################  2d MAGMAS  ################################# 

############################################################################# 
## 
#C  Is2dMagma( <m2d> ) . . . . . . . . . . . . . . . . category of 2d magmas 
#C  Is2dMagmaWithOnes( <m2d> ) . . . . . . . . . . . . . . . . . . with ones
#C  Is2dMagmaWithInversesIfNonzero( <m2d> )  . . . . . . . and some inverses
#C  Is2dGroup( <m2d> )  . . . . . . . . . . . . . . . . . . and all inverses
##
DeclareCategory( "Is2dMagma", Is2dDomain and 
    IsMagma and IsMultiplicativeElementWithTupleCollection ); 
DeclareCategoryCollections( "Is2dMagma" ); 
DeclareCategory( "Is2dMagmaWithOnes", Is2dMagma and 
    IsMultiplicativeElementWithTupleAndOnesCollection ); 
DeclareCategory( "Is2dMagmaWithInversesIfNonzero", Is2dMagmaWithOnes and 
    IsMultiplicativeElementWithTupleAndInversesIfNonzeroCollection ); 
DeclareCategory( "Is2dGroup", Is2dMagmaWithInversesIfNonzero and 
    IsGroupElementWithTupleCollection ); 

############################################################################# 
##  
#V  Family2dMagma . . . . . . . . . . . . . . . . . . . family for 2d-magmas 
#V  Family2dGroup . . . . . . . . . . . . . . . . . . . family for 2d-groups
##  
BindGlobal( "Family2dMagma", 
    NewFamily( "Family2dMagma", Is2dMagma, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "Family2dGroup", NewFamily( "Family2dGroup", Is2dGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################ 
## 
#F  Make2dMagma( <mag>, <obs> ) 
#O  GeneratorsOf2dMagma( <2dMagma> ) 
## 
DeclareGlobalFunction( "Make2dMagma" ); 
DeclareOperation( "GeneratorsOf2dMagma", [ Is2dMagma ] ); 

############################################################################# 
## 
#O  MultiplicativeElementWithTuple( <2dmag>, <tup>, <elt> ) 
#O  MultiplicativeElementWithTupleNC( <ispx>, <tup>, <elt> ) 
## 
DeclareOperation( "MultiplicativeElementWithTuple", 
    [ Is2dGroup, IsHomogeneousList, IsMultiplicativeElement ] ); 
DeclareOperation( "MultiplicativeElementWithTupleNC", 
    [ IsBool, IsHomogeneousList, IsMultiplicativeElement ] ); 
    


###########################  SEMIGROUPS and MONOIDS #########################

#############################################################################
##
#P  Is2dSemigroup( <2dMagma> )
#F  Make2dSemigroup( <mag>, <obs> )  
#O  SinglePiece2dSemigroup( <sgp>, <obs> ) 
##
DeclareSynonymAttr( "Is2dSemigroup", Is2dMagma and IsAssociative );
DeclareGlobalFunction( "Make2dSemigroup" ); 
## DeclareOperation( "SinglePiece2dSemigroup", [ IsSemigroup, IsCollection ] ); 

#############################################################################
##
#P  Is2dMonoid( <2dMagma> )
#F  Make2dMonoid( <mag>, <obs> )  
#O  SinglePiece2dMonoid( <mon>, <obs> ) 
##
DeclareSynonymAttr( "Is2dMonoid", Is2dMagmaWithOnes and IsAssociative );
DeclareGlobalFunction( "Make2dMonoid" ); 
##  DeclareOperation( "SinglePiece2dMonoid", [ IsMonoid, IsCollection ] ); 


#################################  GROUPS  ##################################

##  A *2d-group* is a 2d-magma where source and range are both groups, 
##  and so is a *crossed module* - see file gp2obj.gd.


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
#C  Is3dMagmaWithOnes( <m3d> ) . . . . . . . . . . . . . . . . . . with ones
#C  Is3dMagmaWithInversesIfNonzero( <m3d> )  . . . . . . . and some inverses
#C  Is3dGroup( <m3d> )  . . . . . . . . . . . . . . . . . . and all inverses
##
DeclareCategory( "Is3dMagma", Is3dDomain and 
    IsMagma and IsMultiplicativeElementWithTupleCollection ); 
DeclareCategoryCollections( "Is3dMagma" ); 
DeclareCategory( "Is3dMagmaWithOnes", Is3dMagma and 
    IsMultiplicativeElementWithTupleAndOnesCollection ); 
DeclareCategory( "Is3dMagmaWithInversesIfNonzero", Is3dMagmaWithOnes and 
    IsMultiplicativeElementWithTupleAndInversesIfNonzeroCollection ); 
DeclareCategory( "Is3dGroup", Is3dMagmaWithInversesIfNonzero and 
    IsGroupElementWithTupleCollection ); 

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
