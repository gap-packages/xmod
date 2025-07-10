##############################################################################
##
#W  gpgpd.gd                   GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

############################################################################# 
## 
#C  IsGroupGroupoidElement( <elt> ) 
## 
DeclareCategory( "IsGroupGroupoidElement", 
                 IsMultiplicativeElementWithInverse ); 
DeclareCategoryCollections( "IsGroupGroupoidElement" ); 

############################################################################# 
##  
#R  IsGroupGroupoidElementRep( <elt> ) 
DeclareRepresentation( "IsGroupGroupoidElementRep",
  IsGroupGroupoidElement and IsComponentObjectRep, 
  [ "precat1", "root", "element", "tail", "tailid", "head", "headid" ] ); 

############################################################################# 
##  
#V  IsGroupGroupoidElementFamily  . . family for elements of group groupoids
##  
BindGlobal( "IsGroupGroupoidElementFamily", 
    NewFamily( "IsGroupGroupoidElementFamily", 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################# 
##  
#T  IsGroupGroupoidElementType  default type for elements of group groupoids
##  
BindGlobal( "IsGroupGroupoidElementType", 
            NewType( IsGroupGroupoidElementFamily, 
                     IsGroupGroupoidElement and IsGroupGroupoidElementRep ) );

############################################################################## 
## 
#O  GroupGroupoidElement( <cat1>, <obj>, <obj> ) 
## 
DeclareOperation( "GroupGroupoidElement", 
    [ IsPreCat1Group, IsObject, IsObject ] ); 

############################################################################## 
## 
#O  GroupGroupoidGroup( <gens> ) ??? 
## 
DeclareOperation( "GroupGroupoidGroup", [ IsCollection ] ); 

############################################################################## 
## 
#O  GroupGroupoid( <cat1> ) 
## 
DeclareAttribute( "GroupGroupoid", IsPreCat1Group ); 

