##############################################################################
##
##  gp3obj.gd                 GAP4 package `XMod'                Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.

#############################################################################
##
#R  IsPreXSqObj ( <obj> ) 
##  A pre-crossed square is a square of pre-crossed modules 
#R  IsPreCat2Obj( <obj> ) 
##  A pre-cat2-group is a square of pre-cat1 groups
##
DeclareRepresentation( "IsPreXSqObj", Is3dGroup and IsAttributeStoringRep,
    [ "up2d", "down2d", "left2d", "right2d", "action", "pairing" ] );
DeclareRepresentation( " IsPreCat2Obj", Is3dGroup and IsAttributeStoringRep,
    [ "up2d", "down2d", "left2d", "right2d" ] );

#############################################################################
##
#P  IsPerm3dGroup( <obj> ) 
#P  IsFp3dGroup( <obj> ) 
#P  IsPc3dGroup( <obj> )
##
DeclareProperty( "IsPerm3dGroup", Is3dGroup );
DeclareProperty( "IsFp3dGroup", Is3dGroup );
DeclareProperty( "IsPc3dGroup", Is3dGroup );

#############################################################################
##
#P  IsPreXSq( <PM> } 
#P  IsPermPreXSq( <PM> ) 
#P  IsFpPreXSq( <PM> ) 
#P  IsPcPreXSq( <PM> }
##
DeclareProperty( "IsPreXSq", Is3dDomain );
DeclareSynonym( "IsPermPreXSq", IsPreXSq and IsPerm3dGroup );
DeclareSynonym( "IsFpPreXSq", IsPreXSq and IsFp3dGroup );
DeclareSynonym( "IsPcPreXSq", IsPreXSq and IsPc3dGroup );

#############################################################################
##
#P  IsXSq( <PM> ) 
#P  IsPermXSq( <XS> } 
#P  IsFpXSq( <XS> ) 
#P  IsPcXSq( <XS> )
##
DeclareProperty( "IsXSq", Is3dGroup );
InstallTrueMethod( IsPreXSq, IsXSq );
DeclareSynonym( "IsPermXSq", IsXSq and IsPerm3dGroup );
DeclareSynonym( "IsFpXSq", IsXSq and IsFp3dGroup );
DeclareSynonym( "IsPcXSq", IsXSq and IsPc3dGroup );

#############################################################################
##
#P  IsXPair( <map> )
#R  IsXPairObj( <obj> )
#O  XPairObj( <src>, <rng>, <map> )
#A  XPairMap( <xp> )
#O  ImageElmXPair( <xp>, <elm> ) 
##
DeclareProperty( "IsXPair", IsGeneralMapping );
DeclareRepresentation( "IsXPairObj", IsXPair and IsAttributeStoringRep,
    [ "Source", "Range", "XPairMap" ] );
DeclareOperation( "XPairObj", [ IsList, IsGroup, IsGeneralMapping ] );
DeclareAttribute( "XPairMap", IsXPair );
DeclareOperation( "ImageElmXPair", [ IsXPair, IsObject ] );

#############################################################################
##
#O  XPairByNormalSubgroups
#O  XPairByDerivations
##
DeclareOperation( "XPairByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "XPairByDerivations", [ IsXMod ] );

#############################################################################
##
#P  IsPreCat2( <PCG> ) 
#P  IsPermPreCat2( <PCG> ) 
#P  IsFpPreCat2( <PCG> ) 
#P  IsPcPreCat2( <PCG> )
##
DeclareProperty( "IsPreCat2", Is3dGroup );
DeclareSynonym( "IsPermPreCat2", IsPreCat2 and IsPerm3dGroup );
DeclareSynonym( "IsFpPreCat2", IsPreCat2 and IsFp3dGroup ); 
DeclareSynonym( "IsPcPreCat2", IsPreCat2 and IsPc3dGroup );

#############################################################################
##
#P  IsCat2( <C1G> ) 
#P  IsPermCat2( <CG> ) 
#P  IsFpCat2( <CG> ) 
#P  IsPcCat2( <CG> )
##
DeclareProperty( "IsCat2", Is3dGroup );
DeclareSynonym( "IsPermCat2", IsCat2 and IsPerm3dGroup );
DeclareSynonym( "IsFpCat2", IsCat2 and IsFp3dGroup );
DeclareSynonym( "IsPcCat2", IsCat2 and IsPc3dGroup );

#############################################################################
##
#O  PreXSqObj( <bdy>, <act> ) 
#O  PreCat20bj ( <arg> ) 
#A  Up2dGroup( <PS> ) 
#A  Down2dGroup ( <PS> ) 
#A  Left2dGroup( <PS> ) 
#A  Right2dGroup( <PS> ) 
#A  DiagonalAction( <PS> ) 
#A  XPair( <PS> )
##
DeclareOperation( "PreXSqObj", 
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject] );
DeclareOperation( "PreCat20bj", 
    [ IsPreCat1, IsPreCat1, IsPreCat1, IsPreCat1 ] );
DeclareAttribute( "Up2dGroup", Is3dGroup );
DeclareAttribute( "Left2dGroup", Is3dGroup );
DeclareAttribute( "Down2dGroup", Is3dGroup );
DeclareAttribute( "Right2dGroup", Is3dGroup );
DeclareAttribute( "DiagonalAction", Is3dGroup );
DeclareAttribute( "XPair", Is3dGroup );

#############################################################################
##
#F  XSq( <args> ) 
#0  AsXSq( <arg> ) 
#0  XSqByXMods( <up>, <left>, <down>, <right>, <action>, <pairing> ) 
#0  XSqByNormalSubgroups( <P>, <N>, <M>, <L> )
#A  ActorXSq( <xmod> )
#A  Transpose3dGroup( <xsq> )
##
DeclareGlobalFunction( "XSq" );
DeclareOperation( "XSqByXMods", 
  [ IsXMod, IsXMod, IsXMod, IsXMod, IsGroupHomomorphism, IsXPair ] );
DeclareOperation( "XSqByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareAttribute( "ActorXSq", IsXMod );
DeclareAttribute( "Transpose3dGroup", Is3dGroup );

#############################################################################
##
#P  IsTrivialAction3dGroup( <obj> ) 
#P  IsNormalSub3dGroup( <obj> ) 
#P  IsAutomorphismGroup3dGroup( <XS> ) 
#P  IsAbelianSquare3dGroup( <obj> ) 
#P  IsFreeXSq( <XS> )
##
DeclareProperty( "IsTrivialAction3dGroup", Is3dGroup );
DeclareProperty( "IsNormalSub3dGroup", Is3dGroup );
DeclareProperty( "IsCentralExtension3dGroup", Is3dGroup );
DeclareProperty( "IsAutomorphismGroup3dGroup", Is3dGroup );
DeclareProperty( "IsAbelianSquare3dGroup", Is3dGroup );
DeclareProperty( "IsFreeXSq", IsPreXSqObj );

#############################################################################
##
#0  IsSubPreXSq( <obj> ) 
#0  IsSubXSq( <obj> ) 
#0  IsSubPreCat2( <obj> ) 
#0  IsSubCat2( <obj> )
##
DeclareOperation( "IsSubPreXSq", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubXSq", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubPreCat2", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubCat2", [ Is3dGroup, Is3dGroup ] );

##############################################################################
##
#0  Sub3dGroup( <obj>, <src>, <rng> ) 
#0  SubPreXSq( <PM, Ssrc, Srng> ) 
#0  SubXSq( <PM, Ssrc, Srng> ) 
#0  SubPreCat2( <C>, <H> )
##
DeclareOperation( "Sub3dGroup", [ Is3dGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreXSq", [ IsPreXSq, IsGroup, IsGroup ] );
DeclareOperation( "SubXSq", [ IsXSq, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat2", [ IsPreCat2, IsGroup, IsGroup ] );
DeclareOperation( "SubCat2", [ IsCat2, IsGroup, IsGroup ] );

#############################################################################
##
#0  TrivialSub3dGroup( <obj> ) 
#A  TrivialSubPreXSq( <obj> ) 
#A  TrivialSubXSq( <obj> ) 
#A  TrivialSubPreCat2( <obj> ) 
#A  TrivialSubCat2( <obj> ) 
#P  IsIdentityCat2( <C1G> )
##
DeclareOperation( "TrivialSub3dGroup", [ Is3dGroup ] );
DeclareAttribute( "TrivialSubPreXSq", IsPreXSq );
DeclareAttribute( "TrivialSubXSq", IsXSq );
DeclareAttribute( "TrivialSubPreCat2", IsPreCat2 );
DeclareAttribute( "TrivialSubCat2", IsCat2 );
DeclareProperty( "IsIdentityCat2", IsCat2 );

#############################################################################
##
#F  PreCat2( <arg> ) 
#0  PreCat2ByPreCat1s( <up>, <down>, <left>, <right> )
##
DeclareGlobalFunction( "PreCat2" );
DeclareOperation( "PreCat2ByPreCat1s ", 
    [ IsPreCat1, IsPreCat1, IsPreCat1, IsPreCat1 ] );

#############################################################################
##
#0  PreXSqByPreCat2( <PCG> ) 
#0  PreCat2ByPreXSq( <PS> } 
#A  XSqOfCat2( <C1G> } 
#0  XSqByCat2( <C1G> } 
#A  Cat20fXSq( <XS> ) 
#0  Cat2ByXSq( <XS> )
##
DeclareOperation( "PreXSqByPreCat2", [ IsPreCat2 ] );
DeclareOperation( "PreCat2ByPreXSq", [ IsPreXSq ] );
DeclareAttribute( "XSqOfCat2", IsCat2 );
DeclareOperation( "XSqByCat2", [ IsCat2 ] );
DeclareAttribute( "Cat20fXSq", IsXSq );
DeclareOperation( "Cat2ByXSq", [ IsXSq ] );

#############################################################################
##
#F  Cat2( <arg> }
##
DeclareGlobalFunction( "Cat2" );

#############################################################################
##
#A  DirectProduct3dInfo( <D> }
##
DeclareAttribute( "DirectProduct3dInfo", Is3dDomain, "mutable" );

#############################################################################
##
#A  NormalSubXSqs( <XS> } #A NormalSubXCat2s( <C1G> }
##
DeclareAttribute( "NormalSubXSqs", IsXSq ); 
DeclareAttribute( "NormalSubCat2s", IsCat2 );

#############################################################################
##
#E gp3obj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
