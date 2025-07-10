##############################################################################
##
#W  gp2obj.gd                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2023, Chris Wensley et al,  

#############################################################################
##
#P  IsPreXModDomain( <obj> )
#P  IsPreCat1Domain( <obj> )
## 
##  these apply to groups, algebras, whatever ... 
##
DeclareProperty( "IsPreXModDomain", Is2DimensionalDomain );
DeclareProperty( "IsPreCat1Domain", Is2DimensionalDomain );
InstallTrueMethod( Is2DimensionalDomain, IsPreXModDomain );
InstallTrueMethod( Is2DimensionalDomain, IsPreCat1Domain );

#############################################################################
## 
#A  Size2d( <obj> ) 
##
DeclareAttribute( "Size2d", Is2DimensionalDomain ); 

#############################################################################
##
#R  IsPreXModObj( <obj> )
##    A pre-crossed module is a group homomorphism which preserves an action
#R  IsPreCat1Obj( <obj> )
##    A pre-cat1-group is a pair of group endomorphisms with a common image
##  
DeclareRepresentation( "IsPreXModObj", Is2DimensionalGroup and 
    IsAttributeStoringRep, [ "boundary", "action" ] );
DeclareRepresentation( "IsPreCat1Obj", Is2DimensionalGroup and 
    IsAttributeStoringRep, [ "tailMap", "headMap", "rangeEmbedding" ] );

#############################################################################
##
#P  IsPerm2DimensionalGroup( <obj> )
#P  IsFp2DimensionalGroup( <obj> )
#P  IsPc2DimensionalGroup( <obj> )
##
DeclareProperty( "IsPerm2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsFp2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsPc2DimensionalGroup", Is2DimensionalGroup );

InstallTrueMethod( Is2DimensionalGroup, IsPerm2DimensionalGroup );
InstallTrueMethod( Is2DimensionalGroup, IsFp2DimensionalGroup );
InstallTrueMethod( Is2DimensionalGroup, IsPc2DimensionalGroup );

#############################################################################
##
#T  PreXModObjType . . . . . . . . . . . . . . . . . type for crossed modules
#T  PermPreXModObjType . . . . . . . . . . . .  type for perm crossed modules 
#T  PcPreXModObjType . . . . . . . . . . . . . .  type for pc crossed modules 
#T  PreCat1ObjType . . . . . . . . . . . . . . . . . . . type for cat1-groups
#T  PermPreCat1ObjType . . . . . . . . . . . . . .  type for perm cat1-groups 
#T  PcPreCat1ObjType . . . . . . . . . . . . . . . .  type for pc cat1-groups 
## 
BindGlobal( "PreXModObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreXModObj ) ); 
BindGlobal( "PermPreXModObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreXModObj and IsPerm2DimensionalGroup ) ); 
BindGlobal( "PcPreXModObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreXModObj and IsPc2DimensionalGroup ) ); 
BindGlobal( "PreCat1ObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreCat1Obj ) ); 
BindGlobal( "PermPreCat1ObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreCat1Obj and IsPerm2DimensionalGroup ) ); 
BindGlobal( "PcPreCat1ObjType", 
            NewType( Family2DimensionalGroup, 
                     IsPreCat1Obj and IsPc2DimensionalGroup ) ); 

#############################################################################
##
#P  IsPreXMod( <PM> )
#P  IsPermPreXMod( <PM> )
#P  IsFpPreXMod( <PM> )
#P  IsPcPreXMod( <PM> )
##
DeclareProperty( "IsPreXMod", Is2DimensionalGroup );
InstallTrueMethod( Is2DimensionalGroup, IsPreXMod );
DeclareSynonym( "IsPermPreXMod", IsPreXMod and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpPreXMod", IsPreXMod and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcPreXMod", IsPreXMod and IsPc2DimensionalGroup );

#############################################################################
##
#P  IsXMod( <PM> )
#P  IsPermXMod( <XM> )
#P  IsFpXMod( <XM> )
#P  IsPcXMod( <XM> )
##
DeclareProperty( "IsXMod", IsPreXMod ); 
InstallTrueMethod( IsPreXMod, IsXMod );
DeclareSynonym( "IsPermXMod", IsXMod and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpXMod", IsXMod and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcXMod", IsXMod and IsPc2DimensionalGroup );

#############################################################################
##
#P  IsPreCat1Group( <PCG> )
#P  IsPermPreCat1Group( <PCG> )
#P  IsFpPreCat1Group( <PCG> )
#P  IsPcPreCat1Group( <PCG> )
##
DeclareProperty( "IsPreCat1Group", Is2DimensionalGroup );
InstallTrueMethod( Is2DimensionalGroup, IsPreCat1Group );
DeclareSynonym( "IsPermPreCat1Group", 
    IsPreCat1Group and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpPreCat1Group", IsPreCat1Group and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcPreCat1Group", IsPreCat1Group and IsPc2DimensionalGroup );

#############################################################################
##
#P  IsCat1Group( <C1G> )
#P  IsPermCat1Group( <CG> )
#P  IsFpCat1Group( <CG> )
#P  IsPcCat1Group( <CG> )
##
DeclareProperty( "IsCat1Group", IsPreCat1Group );
InstallTrueMethod( IsPreCat1Group, IsCat1Group );
DeclareSynonym( "IsPermCat1Group", IsCat1Group and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpCat1Group", IsCat1Group and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcCat1Group", IsCat1Group and IsPc2DimensionalGroup );

#############################################################################
##
#O  PreXModObj( <bdy>, <act> )
#A  Boundary( <PM> )
#A  XModAction( <PM> )
#A  ExternalSetXMod( <PM> )
#O  ImageElmXModAction( <PM>, <srcobj>, <rngobj> ) 
##
DeclareOperation( "PreXModObj", [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareAttribute( "Boundary", IsPreXMod );
DeclareAttribute( "XModAction", IsPreXMod );
DeclareAttribute( "ExternalSetXMod", IsPreXMod ); 
DeclareOperation( "ImageElmXModAction", 
    [ Is2DimensionalDomain, IsObject, IsObject ] ); 

#############################################################################
##
#A  PeifferSubgroup( <obj> )
#A  PeifferSub2DimensionalGroup( <obj> )
#O  PeifferSubgroupPreXMod( <PM> )
#O  PeifferSubgroupPreCat1Group( <P1C> )
##
DeclareAttribute( "PeifferSubgroup", Is2DimensionalGroup );
DeclareAttribute( "PeifferSub2DimensionalGroup", Is2DimensionalGroup );
DeclareOperation( "PeifferSubgroupPreXMod", [ IsPreXMod ] );
DeclareOperation( "PeifferSubgroupPreCat1Group", [ IsPreCat1Group ] );

#############################################################################
##
#O  PreXModByBoundaryAndAction( <bdy>, <act> )
#O  PreXModWithTrivialRange( <bdy>, <act> )
## 
DeclareOperation( "PreXModByBoundaryAndAction",
   [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreXModWithTrivialRange", [ IsGroup, IsGroup ] );

#############################################################################
##
#F  XMod( <args> )
#O  AsXMod( <arg> )
#O  XModByBoundaryAndAction( <bdy>, <act> )
#O  XModByTrivialAction( <f> )
#O  XModByNormalSubgroup( <G>, <N> )
#O  XModByCentralExtension( <hom> )
#O  XModByGroupOfAutomorphisms( <G>, <A> )
#A  XModByAutomorphismGroup( <G> )
#A  XModByInnerAutomorphismGroup( <G> )
#O  XModByAbelianModule( <R> )
#A  XModByPeifferQuotient( <PM> )
#O  XModByPullback( <PM>, <hom> )
##
DeclareGlobalFunction( "XMod" );
DeclareOperation( "AsXMod", [ IsDomain ] );
DeclareOperation( "XModByBoundaryAndAction",
   [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "XModByTrivialAction", [ IsGroupHomomorphism ] );
DeclareOperation( "XModByNormalSubgroup", [ IsGroup, IsGroup ] );
DeclareOperation( "XModByCentralExtension", [ IsGroupHomomorphism ] );
DeclareOperation( "XModByGroupOfAutomorphisms", [ IsGroup, IsGroup ] );
DeclareAttribute( "XModByAutomorphismGroup", IsGroup );
DeclareAttribute( "XModByInnerAutomorphismGroup", IsGroup );
DeclareOperation( "XModByAbelianModule", [ IsAbelianModule ] );
DeclareAttribute( "XModByPeifferQuotient", IsPreXMod );
DeclareOperation( "XModByPullback", [ IsXMod, IsGroupHomomorphism ] ); 

#############################################################################
##
#A  KernelCokernelXMod( <2dgp> )
##
DeclareAttribute( "KernelCokernelXMod", Is2DimensionalGroup );

#############################################################################
##
#P  IsTrivialAction2DimensionalGroup( <obj> )
#P  IsNormalSubgroup2DimensionalGroup( <obj> ) 
#P  IsCentralExtension2DimensionalGroup( <obj> )
#P  IsAutomorphismGroup2DimensionalGroup( <XM> )
#P  IsAbelianModule2DimensionalGroup( <obj> )
#P  IsFreeXMod( <XM> )
##
DeclareProperty( "IsTrivialAction2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsNormalSubgroup2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsCentralExtension2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsAutomorphismGroup2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsAbelianModule2DimensionalGroup", Is2DimensionalGroup );
DeclareProperty( "IsFreeXMod", IsPreXModObj );

#############################################################################
##
#O  IsSub2DimensionalGroup( <obj>, <sub> )
#O  IsSubPreXMod( <obj>, <subobj>  )
#O  IsSubXMod( <obj>, <subobj> )
#O  IsSubPreCat1Group( <obj>, <subobj> )
#O  IsSubCat1Group( <obj>, <subobj> )
#O  IsNormalSub2DimensionalDomain( <obj>, <subobj> ) 
##
DeclareOperation( "IsSub2DimensionalGroup", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubPreXMod", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubXMod", [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubPreCat1Group", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubCat1Group", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsNormalSub2DimensionalDomain", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );

##############################################################################
##
#O  Sub2DimensionalGroup( <obj>, <src>, <rng> )
#O  SubPreXMod( <PM, Ssrc, Srng> )
#O  SubXMod( <PM, Ssrc, Srng> )
#O  SubPreCat1Group( <C>, <H> )                           
#O  SubCat1Group( <C>, <H> )                           
##
DeclareOperation( "Sub2DimensionalGroup", 
    [ Is2DimensionalGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreXMod", [ IsPreXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubXMod", [ IsXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat1Group", [ IsPreCat1Group, IsGroup, IsGroup ] );
DeclareOperation( "SubCat1Group", [ IsCat1Group, IsGroup, IsGroup ] );

#############################################################################
##
#O  TrivialSub2DimensionalGroup( <obj> )
#A  TrivialSubPreXMod( <obj> )
#A  TrivialSubXMod( <obj> )
#A  TrivialSubPreCat1Group( <obj> )
#A  TrivialSubCat1Group( <obj> )
#P  IsIdentityPreCat1Group( <obj> )
#P  IsPreCat1GroupWithIdentityEmbedding( <obj> ) 
#P  IsSymmetric2DimensionalGroup( <obj> )
##
DeclareOperation( "TrivialSub2DimensionalGroup", [ Is2DimensionalGroup ] );
DeclareAttribute( "TrivialSubPreXMod", IsPreXMod );
DeclareAttribute( "TrivialSubXMod", IsXMod );
DeclareAttribute( "TrivialSubPreCat1Group", IsPreCat1Group );
DeclareAttribute( "TrivialSubCat1Group", IsCat1Group );
DeclareProperty( "IsIdentityPreCat1Group", IsPreCat1Group );
DeclareProperty( "IsPreCat1GroupWithIdentityEmbedding", IsPreCat1Group ); 
DeclareProperty( "IsSymmetric2DimensionalGroup", Is2DimensionalGroup ); 

#############################################################################
##
#O  PreCat1Obj( <arg> )
#A  HeadMap( <PCG> )
#A  TailMap( <PCG> )
#A  RangeEmbedding( <PCG> )
#A  KernelEmbedding( <C> )
##
DeclareOperation( "PreCat1Obj",
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareAttribute( "HeadMap", IsPreCat1Group );
DeclareAttribute( "TailMap", IsPreCat1Group );
DeclareAttribute( "RangeEmbedding", IsPreCat1Group );
DeclareAttribute( "KernelEmbedding", IsPreCat1Group );

#############################################################################
##
#F  PreCat1Group( <arg> )
#O  PreCat1GroupByTailHeadEmbedding( <t>, <h>, <e> )
#O  PreCat1GroupWithIdentityEmbedding( <tail>, <head> )
#A  IsomorphicPreCat1GroupWithIdentityEmbedding( <PCG> ) 
#A  IsomorphismToPreCat1GroupWithIdentityEmbedding( <PCG> ) 
#A  TransposeCat1Group( <PCG> )
##
DeclareGlobalFunction( "PreCat1Group" );
DeclareOperation( "PreCat1GroupByTailHeadEmbedding",
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreCat1GroupWithIdentityEmbedding",
    [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareAttribute( "IsomorphicPreCat1GroupWithIdentityEmbedding", 
    IsPreCat1Group ); 
DeclareAttribute( "IsomorphismToPreCat1GroupWithIdentityEmbedding", 
    IsPreCat1Group ); 
DeclareAttribute( "TransposeCat1Group", IsPreCat1Group );

#############################################################################
##
#A  PreXModRecordOfPreCat1Group( <PCG> )
#A  PreCat1GroupRecordOfPreXMod( <PM> )
#A  XModOfCat1Group( <C1G> )
#A  Cat1GroupOfXMod( <XM> )
##
DeclareAttribute( "PreXModRecordOfPreCat1Group", IsPreCat1Group );
DeclareAttribute( "PreCat1GroupRecordOfPreXMod", IsPreXMod );
DeclareAttribute( "XModOfCat1Group", IsCat1Group );
DeclareAttribute( "Cat1GroupOfXMod", IsXMod );

#############################################################################
##
#F  Cat1Group( <arg> )
#O  Cat1Select( <size>, <gpnum>, <num> )
#O  PermCat1Select( <size>, <gpnum>, <num> )
#O  Cat1GroupByPeifferQuotient( <PM> )
#O  DiagonalCat1Group( <list> )
##
DeclareGlobalFunction( "Cat1Group" );
DeclareOperation( "Cat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "Cat1Select", [ IsInt, IsInt ] );
DeclareOperation( "Cat1Select", [ IsInt ] );
DeclareOperation( "PermCat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "Cat1GroupByPeifferQuotient", [ IsPreCat1Group ] );
DeclareOperation( "DiagonalCat1Group", [ IsList ] ); 

#############################################################################
##
#O  AllCat1GroupsWithImage( <gp>, <subgp> )
#O  AllCat1GroupsWithImageIterator( <gp>, <subgp> )
#O  AllCat1GroupsWithImageNumber( <gp>, <subgp> )
#F  DoAllCat1GroupsWithImageIterator
#O  AllCat1GroupsWithImageUpToIsomorphism( <gp> )
##
DeclareOperation( "AllCat1GroupsWithImage", [ IsGroup, IsGroup ] ); 
DeclareOperation( "AllCat1GroupsWithImageIterator", [ IsGroup, IsGroup ] ); 
DeclareOperation( "AllCat1GroupsWithImageNumber", [ IsGroup, IsGroup ] ); 
DeclareGlobalFunction( "DoAllCat1GroupsWithImageIterator" ); 
DeclareOperation( "AllCat1GroupsWithImageUpToIsomorphism", [ IsGroup ] ); 

#############################################################################
##
#O  AllCat1Groups( <gp> )
#O  AllCat1GroupsIterator( <gp> )
#F  DoAllCat1GroupsIterator 
#O  AllCat1GroupsMatrix( <gp> )
#A  AllCat1GroupsNumber( <gp> )
#O  AllCat1GroupsUpToIsomorphism( <gp> )
#O  AllCat1GroupFamilies( <gp> )
##
DeclareOperation( "AllCat1Groups", [ IsGroup ] ); 
DeclareOperation( "AllCat1GroupsIterator", [ IsGroup ] ); DeclareGlobalFunction( "DoAllCat1GroupsIterator" ); 
DeclareOperation( "AllCat1GroupsMatrix", [ IsGroup ] ); DeclareAttribute( "AllCat1GroupsNumber", IsGroup ); 
DeclareOperation( "AllCat1GroupsUpToIsomorphism", [ IsGroup ] ); 
DeclareOperation( "AllCat1GroupFamilies", [ IsGroup ] ); 

#############################################################################
##
#A  DirectProduct2dInfo( <D> )
#A  Coproduct2dInfo( <D> )
##
DeclareAttribute( "DirectProduct2dInfo", Is2DimensionalDomain, "mutable" );
DeclareAttribute( "Coproduct2dInfo", Is2DimensionalDomain, "mutable" );

#############################################################################
##
#A  NormalSubXMods( <XM> )
#A  NormalSubCat1Groups( <C1G> )
##
DeclareAttribute( "NormalSubXMods", IsXMod );
DeclareAttribute( "NormalSubCat1Groups", IsCat1Group );

#############################################################################
##
#A  CatnGroupNumbers( <G> )
#A  CatnGroupLists( <G> )
#A  InitCatnGroupRecords( <G> )
##
DeclareAttribute( "CatnGroupNumbers", IsGroup, "mutable" );
DeclareAttribute( "CatnGroupLists", IsGroup, "mutable" );
DeclareOperation( "InitCatnGroupRecords", [ IsGroup ] );
