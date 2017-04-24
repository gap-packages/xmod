##############################################################################
##
#W  gp2obj.gd                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#P  IsPreXModDomain( <obj> )
#P  IsPreCat1Domain( <obj> )
## 
##  these apply to groups, algebras, whatever ... 
##
DeclareProperty( "IsPreXModDomain", Is2DimensionalDomain );
DeclareProperty( "IsPreCat1Domain", Is2DimensionalDomain );

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

#############################################################################
##
#P  IsPreXMod( <PM> )
#P  IsPermPreXMod( <PM> )
#P  IsFpPreXMod( <PM> )
#P  IsPcPreXMod( <PM> )
##
DeclareProperty( "IsPreXMod", Is2DimensionalGroup );
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
DeclareSynonym( "IsPermXMod", IsXMod and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpXMod", IsXMod and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcXMod", IsXMod and IsPc2DimensionalGroup );

#############################################################################
##
#P  IsPreCat1( <PCG> )
#P  IsPermPreCat1( <PCG> )
#P  IsFpPreCat1( <PCG> )
#P  IsPcPreCat1( <PCG> )
##
DeclareProperty( "IsPreCat1", Is2DimensionalGroup );
DeclareSynonym( "IsPermPreCat1", IsPreCat1 and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpPreCat1", IsPreCat1 and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcPreCat1", IsPreCat1 and IsPc2DimensionalGroup );

#############################################################################
##
#P  IsCat1( <C1G> )
#P  IsPermCat1( <CG> )
#P  IsFpCat1( <CG> )
#P  IsPcCat1( <CG> )
##
DeclareProperty( "IsCat1", IsPreCat1 );
DeclareSynonym( "IsPermCat1", IsCat1 and IsPerm2DimensionalGroup );
DeclareSynonym( "IsFpCat1", IsCat1 and IsFp2DimensionalGroup );
DeclareSynonym( "IsPcCat1", IsCat1 and IsPc2DimensionalGroup );

#############################################################################
##
#O  PreXModObj( <bdy>, <act> )
#A  Boundary( <PM> )
#A  AutoGroup( <PM> )
#A  XModAction( <PM> )
#A  ExternalSetXMod( <PM> )
##
DeclareOperation( "PreXModObj", [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareAttribute( "Boundary", IsPreXMod );
DeclareAttribute( "AutoGroup", IsPreXMod );
DeclareAttribute( "XModAction", IsPreXMod );
DeclareAttribute( "ExternalSetXMod", IsPreXMod ); 

#############################################################################
##
#A  PeifferSubgroup( <obj> )
#A  PeifferSub2DimensionalGroup( <obj> )
#O  PeifferSubgroupPreXMod( <PM> )
#O  PeifferSubgroupPreCat1( <P1C> )
##
DeclareAttribute( "PeifferSubgroup", Is2DimensionalGroup );
DeclareAttribute( "PeifferSub2DimensionalGroup", Is2DimensionalGroup );
DeclareOperation( "PeifferSubgroupPreXMod", [ IsPreXMod ] );
DeclareOperation( "PeifferSubgroupPreCat1", [ IsPreCat1 ] );

#############################################################################
##
#O  PreXModByBoundaryAndAction( <bdy>, <act> )
##
DeclareOperation( "PreXModByBoundaryAndAction",
   [ IsGroupHomomorphism, IsGroupHomomorphism ] );

#############################################################################
##
#F  XMod( <args> )
#O  AsXMod( <arg> )
#O  XModByBoundaryAndAction( <bdy>, <act> )
#O  XModByTrivialAction( <f> )
#O  XModByNormalSubgroup( <G>, <N> )
#O  XModByCentralExtension( <hom> )
#O  XModByGroupOfAutomorphisms( <G>, <A> )
#F  XModByAutomorphismGroup( <args> )
#A  XModByInnerAutomorphismGroup( <G> )
#O  XModByAbelianModule( <R> )
#A  XModByPeifferQuotient( <PM> )
##
DeclareGlobalFunction( "XMod" );
DeclareOperation( "AsXMod", [ IsDomain ] );
DeclareOperation( "XModByBoundaryAndAction",
   [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "XModByTrivialAction", [ IsGroupHomomorphism ] );
DeclareOperation( "XModByNormalSubgroup", [ IsGroup, IsGroup ] );
DeclareOperation( "XModByCentralExtension", [ IsGroupHomomorphism ] );
DeclareOperation( "XModByGroupOfAutomorphisms", 
    [ IsGroup, IsGroupOfAutomorphisms ] );
DeclareGlobalFunction( "XModByAutomorphismGroup" );
DeclareAttribute( "XModByInnerAutomorphismGroup", IsGroup );
DeclareOperation( "XModByAbelianModule", [ IsAbelianModule ] );
DeclareAttribute( "XModByPeifferQuotient", IsPreXMod );

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
#O  IsSubPreXMod( <obj> )
#O  IsSubXMod( <obj> )
#O  IsSubPreCat1( <obj> )
#O  IsSubCat1( <obj> )
##
DeclareOperation( "IsSubPreXMod", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubXMod", [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubPreCat1", 
    [ Is2DimensionalGroup, Is2DimensionalGroup ] );
DeclareOperation( "IsSubCat1", [ Is2DimensionalGroup, Is2DimensionalGroup ] );

##############################################################################
##
#O  Sub2DimensionalGroup( <obj>, <src>, <rng> )
#O  SubPreXMod( <PM, Ssrc, Srng> )
#O  SubXMod( <PM, Ssrc, Srng> )
#O  SubPreCat1( <C>, <H> )                           
##
DeclareOperation( "Sub2DimensionalGroup", 
    [ Is2DimensionalGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreXMod", [ IsPreXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubXMod", [ IsXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat1", [ IsPreCat1, IsGroup, IsGroup ] );
DeclareOperation( "SubCat1", [ IsCat1, IsGroup, IsGroup ] );

#############################################################################
##
#O  TrivialSub2DimensionalGroup( <obj> )
#A  TrivialSubPreXMod( <obj> )
#A  TrivialSubXMod( <obj> )
#A  TrivialSubPreCat1( <obj> )
#A  TrivialSubCat1( <obj> )
#P  IsIdentityCat1( <obj> )
#P  IsEndomorphismPreCat1( <obj> )
##
DeclareOperation( "TrivialSub2DimensionalGroup", [ Is2DimensionalGroup ] );
DeclareAttribute( "TrivialSubPreXMod", IsPreXMod );
DeclareAttribute( "TrivialSubXMod", IsXMod );
DeclareAttribute( "TrivialSubPreCat1", IsPreCat1 );
DeclareAttribute( "TrivialSubCat1", IsCat1 );
DeclareProperty( "IsIdentityCat1", IsCat1 );
DeclareProperty( "IsEndomorphismPreCat1", IsPreCat1 ); 

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
DeclareAttribute( "HeadMap", IsPreCat1 );
DeclareAttribute( "TailMap", IsPreCat1 );
DeclareAttribute( "RangeEmbedding", IsPreCat1 );
DeclareAttribute( "KernelEmbedding", IsPreCat1 );

#############################################################################
##
#F  PreCat1( <arg> )
#O  PreCat1ByTailHeadEmbedding( <t>, <h>, <e> )
#O  PreCat1ByEndomorphisms( <tail>, <head> )
#O  PreCat1ByNormalSubgroup( <G>, <N> )
#A  ReverseCat1( <PCG> )
##
DeclareGlobalFunction( "PreCat1" );
DeclareOperation( "PreCat1ByTailHeadEmbedding",
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreCat1ByEndomorphisms",
    [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreCat1ByNormalSubgroup", [ IsGroup, IsGroup ] );
DeclareAttribute( "ReverseCat1", IsPreCat1 );

#############################################################################
##
#O  PreXModByPreCat1( <PCG> )
#O  PreCat1ByPreXMod( <PM> )
#A  XModOfCat1( <C1G> )
#O  XModByCat1( <C1G> )
#A  Cat1OfXMod( <XM> )
#O  Cat1ByXMod( <XM> )
##
DeclareOperation( "PreXModByPreCat1", [ IsPreCat1 ] );
DeclareOperation( "PreCat1ByPreXMod", [ IsPreXMod ] );
DeclareAttribute( "XModOfCat1", IsCat1 );
DeclareOperation( "XModByCat1", [ IsCat1 ] );
DeclareAttribute( "Cat1OfXMod", IsXMod );
DeclareOperation( "Cat1ByXMod", [ IsXMod ] );

#############################################################################
##
#A  SourceEmbedding( <XM> )
##
##  homomorphism from Source(XM) to Source(Cat1OfXMod(XM))
##
DeclareAttribute( "SourceEmbedding", IsPreXMod );

#############################################################################
##
#F  Cat1( <arg> )
#O  Cat1Select( <size>, <gpnum>, <num> )
#O  PermCat1Select( <size>, <gpnum>, <num> )
#O  Cat1ByPeifferQuotient( <PM> )
#O  DiagonalCat1( <list> )
##
DeclareGlobalFunction( "Cat1" );
DeclareOperation( "Cat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "PermCat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "Cat1ByPeifferQuotient", [ IsPreCat1 ] );
DeclareOperation( "DiagonalCat1", [ IsList ] ); 

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
#A  NormalSubXCat1s( <C1G> )
##
DeclareAttribute( "NormalSubXMods", IsXMod );
DeclareAttribute( "NormalSubCat1s", IsCat1 );

#############################################################################
##
#E  gp2obj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
