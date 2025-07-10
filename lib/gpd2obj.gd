##############################################################################
##
#W  gpd2obj.gd                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  

#############################################################################
## 
#C  Is2DimensionalDomainWithObjects( <obj> )
##  
DeclareCategory( "Is2DimensionalDomainWithObjects", 
    Is2DimensionalDomain and IsDomainWithObjects ); 

############################################################################## 
## 
#C  Is2DimensionalMagmaWithObjects( <m2d> ) category of 2d magmas with objects
#C  Is2DimensionalMagmaWithObjectsAndOnes( <m2d> ) . . . . . . . and with ones
#C  Is2DimensionalMagmaWithObjectsAndInverses( <m2d> ) . . . and some inverses
#C  Is2DimensionalGroupWithObjects( <m2d> )  . . . . . . . .  and all inverses
##
DeclareCategory( "Is2DimensionalMagmaWithObjects", 
    Is2DimensionalDomainWithObjects and IsMagmaWithObjects ); 
DeclareCategoryCollections( "Is2DimensionalMagmaWithObjects" ); 
DeclareCategory( "Is2DimensionalMagmaWithObjectsAndOne", 
    Is2DimensionalMagmaWithObjects 
    and CategoryCollections( IsMultiplicativeElementWithObjectsAndOnes ) ); 
DeclareCategory( "Is2DimensionalMagmaWithObjectsAndInverses", 
    Is2DimensionalMagmaWithObjectsAndOne 
    and CategoryCollections( IsMultiplicativeElementWithObjectsAndInverses ) ); 
DeclareCategory( "Is2DimensionalGroupWithObjects", 
    Is2DimensionalMagmaWithObjectsAndInverses 
    and CategoryCollections( IsAssociativeElement ) ); 

############################################################################# 
##  
#V  Family2DimensionalGroupWithObjects  . . family for 2d-groups with objects
##  
BindGlobal( "Family2DimensionalGroupWithObjects", 
    NewFamily( "Family2DimensionalGroupWithObjects", 
               Is2DimensionalGroupWithObjects, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsPreXModWithObjects( <PM> ) . . . . . . . . for a 2d-domain with objects 
#P  IsXModWithObjects( <PM> )  . . . . . . . . . for a 2d-domain with objects
#P  IsPreCat1Groupoid( <PM> )  . . . . . . . . . for a 2d-domain with objects 
#P  IsCat1Groupoid( <PM> ) . . . . . . . . . . . for a 2d-domain with objects
##
DeclareProperty( "IsPreXModWithObjects", 
    IsPreXMod and Is2DimensionalDomainWithObjects );
DeclareProperty( "IsXModWithObjects", IsPreXModWithObjects ); 
DeclareProperty( "IsPreCat1Groupoid",   #? another condition?? 
    Is2DimensionalDomainWithObjects );
DeclareProperty( "IsCat1Groupoid", IsPreCat1Groupoid ); 

#############################################################################
##
#R  IsPreXModWithObjectsObj( <obj> ) . . . . for objects, boundary and action 
##  
##  A pre-crossed module of groupoids is a morphism preserving an action
##
DeclareRepresentation( "IsPreXModWithObjectsObj", 
    Is2DimensionalDomainWithObjects and IsAttributeStoringRep, 
    [ "boundary", "action" ] );

#############################################################################
##
#T  PreXModWithObjectsType . . . . . . . . . . type for prexmods with objects
#T  PreXModWithPiecesType  . . . . . type for unions of prexmods with objects
##
BindGlobal( "PreXModWithObjectsType", 
            NewType( Family2DimensionalGroupWithObjects, 
                     IsPreXModWithObjectsObj ) ); 
BindGlobal( "PreXModWithPiecesType", 
            NewType( Family2DimensionalGroupWithObjects, 
                     IsPreXModWithObjectsObj and IsPiecesRep ) ); 

#############################################################################
##
#O  PreXModWithObjectsByBoundaryAndAction( <bdy>, <act> )
## 
DeclareOperation( "PreXModWithObjectsByBoundaryAndAction",
   [ IsGroupoidHomomorphism, IsGroupoidHomomorphism ] );

############################################################################# 
## 
#A  Root2dGroup( <dwo> ) 
## 
DeclareAttribute( "Root2dGroup", Is2DimensionalDomainWithObjects ); 

############################################################################# 
## 
#F  PreXModWithObjects( <args> ) 
#O  PreXModWithObjectsObj( <bdy>, <act> )         
## 
DeclareGlobalFunction( "PreXModWithObjects" );
DeclareOperation( "PreXModWithObjectsObj",
    [ IsGroupoidHomomorphism, IsGroupoidHomomorphism ] ); 

#############################################################################
##
#O  SinglePiecePreXModWithObjects( <xmod>, <obs>, <isdiscrete> )
#O  SinglePiecePreXModWithObjectsNC( <xmod>, <obs>, <isdiscrete>  
##  . . for precrossed modules, a set of objects, discrete or connected source
##  
DeclareOperation( "SinglePiecePreXModWithObjects", 
    [ IsPreXMod, IsList, IsBool ] );
DeclareOperation( "SinglePiecePreXModWithObjectsNC", 
    [ IsPreXMod, IsList, IsBool ] );

#############################################################################
##
#P  IsPermPreXModWithObjects 
#P  IsPcPreXModWithObjects 
#P  IsFpPreXModWithObjects 
## 
DeclareProperty( "IsPermPreXModWithObjects", Is2DimensionalDomainWithObjects ); 
DeclareProperty( "IsPcPreXModWithObjects", Is2DimensionalDomainWithObjects ); 
DeclareProperty( "IsFpPreXModWithObjects", Is2DimensionalDomainWithObjects ); 
