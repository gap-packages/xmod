##############################################################################
##
#W  gp2obj.gd                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##
##  version 2.32, 03/02/2015 
##
#Y  Copyright (C) 2001-2015, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#V  InfoXMod
##
DeclareInfoClass( "InfoXMod" );

#############################################################################
##
#P  IsPreXModDomain( <obj> )
#P  IsPreCat1Domain( <obj> )
## 
##  these apply to groups, algebras, whatever ... 
##
DeclareProperty( "IsPreXModDomain", Is2dDomain );
DeclareProperty( "IsPreCat1Domain", Is2dDomain );

#############################################################################
##
#R  IsPreXModObj( <obj> )
##    A pre-crossed module is a group homomorphism which preserves an action
#R  IsPreCat1Obj( <obj> )
##    A pre-cat1-group is a pair of group endomorphisms with a common image
##  
DeclareRepresentation( "IsPreXModObj", Is2dGroup and IsAttributeStoringRep,
      [ "boundary", "action" ] );
DeclareRepresentation( "IsPreCat1Obj", Is2dGroup and IsAttributeStoringRep,
    [ "tailMap", "headMap", "rangeEmbedding" ] );

#############################################################################
##
#P  IsPerm2dGroup( <obj> )
#P  IsFp2dGroup( <obj> )
#P  IsPc2dGroup( <obj> )
##
#?  DeclareSynonym( "IsPerm2dGroup", IsPermObject and Is2dGroup );  etc ??
DeclareProperty( "IsPerm2dGroup", Is2dGroup );
DeclareProperty( "IsFp2dGroup", Is2dGroup );
DeclareProperty( "IsPc2dGroup", Is2dGroup );

#############################################################################
##
#P  IsPreXMod( <PM> )
#P  IsPermPreXMod( <PM> )
#P  IsFpPreXMod( <PM> )
#P  IsPcPreXMod( <PM> )
##
DeclareProperty( "IsPreXMod", Is2dGroup );
DeclareSynonym( "IsPermPreXMod", IsPreXMod and IsPerm2dGroup );
DeclareSynonym( "IsFpPreXMod", IsPreXMod and IsFp2dGroup );
DeclareSynonym( "IsPcPreXMod", IsPreXMod and IsPc2dGroup );

#############################################################################
##
#P  IsXMod( <PM> )
#P  IsPermXMod( <XM> )
#P  IsFpXMod( <XM> )
#P  IsPcXMod( <XM> )
##
DeclareProperty( "IsXMod", IsPreXMod ); 
DeclareSynonym( "IsPermXMod", IsXMod and IsPerm2dGroup );
DeclareSynonym( "IsFpXMod", IsXMod and IsFp2dGroup );
DeclareSynonym( "IsPcXMod", IsXMod and IsPc2dGroup );

#############################################################################
##
#P  IsPreCat1( <PCG> )
#P  IsPermPreCat1( <PCG> )
#P  IsFpPreCat1( <PCG> )
#P  IsPcPreCat1( <PCG> )
##
DeclareProperty( "IsPreCat1", Is2dGroup );
DeclareSynonym( "IsPermPreCat1", IsPreCat1 and IsPerm2dGroup );
DeclareSynonym( "IsFpPreCat1", IsPreCat1 and IsFp2dGroup );
DeclareSynonym( "IsPcPreCat1", IsPreCat1 and IsPc2dGroup );

#############################################################################
##
#P  IsCat1( <C1G> )
#P  IsPermCat1( <CG> )
#P  IsFpCat1( <CG> )
#P  IsPcCat1( <CG> )
##
DeclareProperty( "IsCat1", IsPreCat1 );
DeclareSynonym( "IsPermCat1", IsCat1 and IsPerm2dGroup );
DeclareSynonym( "IsFpCat1", IsCat1 and IsFp2dGroup );
DeclareSynonym( "IsPcCat1", IsCat1 and IsPc2dGroup );

#############################################################################
##
#O  PreXModObj( <bdy>, <act> )
#A  Boundary( <PM> )
#A  AutoGroup( <PM> )
#A  XModAction( <PM> )
##
DeclareOperation( "PreXModObj",
    [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareAttribute( "Boundary", IsPreXMod );
DeclareAttribute( "AutoGroup", IsPreXMod );
DeclareAttribute( "XModAction", IsPreXMod );

#############################################################################
##
#A  PeifferSubgroup( <obj> )
#O  PeifferSubgroupPreXMod( <PM> )
#O  PeifferSubgroupPreCat1( <P1C> )
##
DeclareAttribute( "PeifferSubgroup", Is2dGroup );
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
#P  IsTrivialAction2dGroup( <obj> )
#P  IsNormalSubgroup2dGroup( <obj> )
#P  IsCentralExtension2dGroup( <obj> )
#P  IsAutomorphismGroup2dGroup( <XM> )
#P  IsAbelianModule2dGroup( <obj> )
#P  IsFreeXMod( <XM> )
##
DeclareProperty( "IsTrivialAction2dGroup", Is2dGroup );
DeclareProperty( "IsNormalSubgroup2dGroup", Is2dGroup );
DeclareProperty( "IsCentralExtension2dGroup", Is2dGroup );
DeclareProperty( "IsAutomorphismGroup2dGroup", Is2dGroup );
DeclareProperty( "IsAbelianModule2dGroup", Is2dGroup );
DeclareProperty( "IsFreeXMod", IsPreXModObj );

#############################################################################
##
#O  IsSubPreXMod( <obj> )
#O  IsSubXMod( <obj> )
#O  IsSubPreCat1( <obj> )
#O  IsSubCat1( <obj> )
##
DeclareOperation( "IsSubPreXMod", [ Is2dGroup, Is2dGroup ] );
DeclareOperation( "IsSubXMod", [ Is2dGroup, Is2dGroup ] );
DeclareOperation( "IsSubPreCat1", [ Is2dGroup, Is2dGroup ] );
DeclareOperation( "IsSubCat1", [ Is2dGroup, Is2dGroup ] );

##############################################################################
##
#O  Sub2dGroup( <obj>, <src>, <rng> )
#O  SubPreXMod( <PM, Ssrc, Srng> )
#O  SubXMod( <PM, Ssrc, Srng> )
#O  SubPreCat1( <C>, <H> )                           
##
DeclareOperation( "Sub2dGroup", [ Is2dGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreXMod", [ IsPreXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubXMod", [ IsXMod, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat1", [ IsPreCat1, IsGroup, IsGroup ] );
DeclareOperation( "SubCat1", [ IsCat1, IsGroup, IsGroup ] );

#############################################################################
##
#O  TrivialSub2dGroup( <obj> )
#A  TrivialSubPreXMod( <obj> )
#A  TrivialSubXMod( <obj> )
#A  TrivialSubPreCat1( <obj> )
#A  TrivialSubCat1( <obj> )
#P  IsIdentityCat1( <C1G> )
##
DeclareOperation( "TrivialSub2dGroup", [ Is2dGroup ] );
DeclareAttribute( "TrivialSubPreXMod", IsPreXMod );
DeclareAttribute( "TrivialSubXMod", IsXMod );
DeclareAttribute( "TrivialSubPreCat1", IsPreCat1 );
DeclareAttribute( "TrivialSubCat1", IsCat1 );
DeclareProperty( "IsIdentityCat1", IsCat1 );

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
#A  Reverse( <PCG> )
##
DeclareGlobalFunction( "PreCat1" );
DeclareOperation( "PreCat1ByTailHeadEmbedding",
    [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreCat1ByEndomorphisms",
    [ IsGroupHomomorphism, IsGroupHomomorphism ] );
DeclareOperation( "PreCat1ByNormalSubgroup", [ IsGroup, IsGroup ] );
DeclareAttribute( "Reverse", IsPreCat1 );

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
##
DeclareGlobalFunction( "Cat1" );
DeclareOperation( "Cat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "PermCat1Select", [ IsInt, IsInt, IsInt ] );
DeclareOperation( "Cat1ByPeifferQuotient", [ IsPreCat1 ] );

#############################################################################
##
#A  DirectProduct2dInfo( <D> )
##
DeclareAttribute( "DirectProduct2dInfo", Is2dDomain, "mutable" );

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
