##############################################################################
##
##  gpnobj.gd                 GAP4 package `XMod'                Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for and ##  (pre-)catn-groups.


###############################  nd DOMAIN   ################################ 

############################################################################# 
## 
#C  IsndDomain( <obj> ) . . . . . . . . . . . . test if object is a nd-domain 
#F  MakendDomain( <src>, <rng> ) 
#O  TypeOfndDomain( <m3d> ) 
## 
DeclareCategory( "IsndDomain", Is2dDomain ); 
DeclareGlobalFunction( "MakendDomain" ); 
DeclareOperation( "TypeOfndDomain", [ IsList ] );  

###############################  nd MAGMAS  ################################# 

############################################################################# 
## 
#C  IsndMagma( <m3d> ) . . . . . . . . . . . . . . . . category of nd magmas 
#C  IsndMagmaWithOne( <m3d> )  . . . . . . . . . . . . . . . . . . with one
#C  IsndMagmaWithInverses( <m3d> ) . . . . . . . . . . . . . . and inverses
#C  IsndGroup( <m3d> ) 
##
DeclareCategory( "IsndMagma", IsndDomain and 
    CategoryCollections( IsMultiplicativeElement ) ); 
DeclareCategoryCollections( "IsndMagma" ); 
DeclareCategory( "IsndMagmaWithOne", IsndMagma and 
    CategoryCollections( IsMultiplicativeElementWithOne ) ); 
DeclareCategory( "IsndMagmaWithInverses", IsndMagmaWithOne and 
    CategoryCollections( IsMultiplicativeElementWithInverse ) ); 
DeclareCategory( "IsndGroup", IsndMagmaWithInverses and 
    CategoryCollections( IsAssociativeElement ) ); 

############################################################################# 
##  
#V  FamilyndGroup . . . . . . . . . . . . . . . . . . . family for nd-groups
##  
BindGlobal( "FamilyndGroup", NewFamily( "FamilyndGroup", IsndGroup, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#R  IsPreCatnObj( <obj> ) 
##  A pre-cat2-group is a square of pre-cat1 groups
##
DeclareRepresentation( "IsPreCatnObj", IsndGroup and IsAttributeStoringRep,
[ "2dGroups", "PreCatnDimension" ] );

#############################################################################
##
#P  IsPreCatnGroup( <PCG> ) 
##
DeclareProperty( "IsPreCatnGroup", IsndGroup );


#############################################################################
##
#P  IsCatnGroup( <C1G> ) 
##
DeclareProperty( "IsCatnGroup", IsndGroup );


#############################################################################
##
#O  PreCatnObj ( <arg> ) 
#A  2dGroups( <P> ) 
#A  PreCatnDimension ( <P> ) 
##
DeclareOperation( "PreCatnObj", 
    [ IsList ] );
DeclareAttribute( "2dGroups", IsndGroup );
DeclareAttribute( "PreCatnDimension", IsndGroup );


#############################################################################
##
#F  PreCatnGroup( <arg> ) 
#F  PreCatnGroupByPreCat1Groups( <first>, <second>, ... )
##
DeclareGlobalFunction( "PreCatnGroup" );
DeclareGlobalFunction( "PreCatnGroupByPreCat1Groups" );


#############################################################################
##
#F  CatnGroup( <arg> }
##
DeclareGlobalFunction( "CatnGroup" );


#############################################################################
##
#E gpnobj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
