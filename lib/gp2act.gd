##############################################################################
##
#W  gp2act.gd                   GAP4 package `XMod'              Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##
##  This file declares methods for actor crossed squares of crossed modules. 
##  

#############################################################################
##
#A  AutomorphismPermGroup( <obj> )
##
DeclareAttribute( "AutomorphismPermGroup", Is2dDomain );
DeclareAttribute( "GeneratingAutomorphisms", Is2dDomain );
DeclareAttribute( "AutoGroupIsomorphism", IsGroup );
DeclareProperty( "IsAutomorphismPermGroupOfXMod", IsGroup );
DeclareAttribute( "EmbedSourceAutos", IsAutomorphismPermGroupOfXMod );
DeclareAttribute( "EmbedRangeAutos", IsAutomorphismPermGroupOfXMod );
DeclareAttribute( "SourceProjection", IsAutomorphismPermGroupOfXMod );
DeclareAttribute( "RangeProjection", IsAutomorphismPermGroupOfXMod );

#############################################################################
##
#O  PermAutomorphismAsXModMorphism( <xmod>, <permaut> )
##
DeclareOperation( "PermAutomorphismAsXModMorphism", [ IsXMod, IsPerm ] );

#############################################################################
##
#A  WhiteheadXMod( <XM> )
#A  NorrieXMod( <XM> )
#A  LueXMod( <XM> )
##
DeclareAttribute( "WhiteheadXMod", IsXMod );
DeclareAttribute( "NorrieXMod", IsXMod );
DeclareAttribute( "LueXMod", IsXMod );

#############################################################################
##
#F  Actor( <args> )
#F  InnerActor( <args> )
#A  ActorXMod( <XM> )
#A  ActorCat1( <C> )
##
DeclareGlobalFunction( "Actor" );
DeclareGlobalFunction( "InnerActor" );
DeclareAttribute( "ActorXMod", IsXMod );
DeclareAttribute( "ActorCat1", IsCat1 );

#############################################################################
##
#A  InnerActorXMod( <XM> )
#A  InnerMorphism( <XM> )
#A  XModCentre( <XM> )     #? (18/07/11) : cannot get plain Centre to work ?? 
##
DeclareAttribute( "InnerActorXMod", IsXMod );
DeclareAttribute( "InnerMorphism", IsXMod );
DeclareAttribute( "XModCentre", IsXMod ); 

#############################################################################
##
#O  ImageAutomorphismDerivation( <mor>, <chi> )
##
DeclareOperation( "ImageAutomorphismDerivation", 
    [ IsXModMorphism, IsDerivation ] );

#############################################################################
##
#E  gp2act.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
