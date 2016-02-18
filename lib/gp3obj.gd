##############################################################################
##
##  gp3obj.gd                 GAP4 package `XMod'                Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.

#############################################################################
##
#R  IsPreCrossedSquareObj ( <obj> ) 
##  A pre-crossed square is a square of pre-crossed modules 
#R  IsPreCat2Obj( <obj> ) 
##  A pre-cat2-group is a square of pre-cat1 groups
##
DeclareRepresentation( "IsPreCrossedSquareObj", Is3dGroup and IsAttributeStoringRep,
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
#P  IsPreCrossedSquare( <PM> } 
#P  IsPermPreCrossedSquare( <PM> ) 
#P  IsFpPreCrossedSquare( <PM> ) 
#P  IsPcPreCrossedSquare( <PM> }
##
DeclareProperty( "IsPreCrossedSquare", Is3dGroup );
DeclareSynonym( "IsPermPreCrossedSquare", IsPreCrossedSquare and IsPerm3dGroup );
DeclareSynonym( "IsFpPreCrossedSquare", IsPreCrossedSquare and IsFp3dGroup );
DeclareSynonym( "IsPcPreCrossedSquare", IsPreCrossedSquare and IsPc3dGroup );

#############################################################################
##
#P  IsCrossedSquare( <PM> ) 
#P  IsPermCrossedSquare( <XS> } 
#P  IsFpCrossedSquare( <XS> ) 
#P  IsPcCrossedSquare( <XS> )
##
DeclareProperty( "IsCrossedSquare", Is3dGroup );
InstallTrueMethod( IsPreCrossedSquare, IsCrossedSquare );
DeclareSynonym( "IsPermCrossedSquare", IsCrossedSquare and IsPerm3dGroup );
DeclareSynonym( "IsFpCrossedSquare", IsCrossedSquare and IsFp3dGroup );
DeclareSynonym( "IsPcCrossedSquare", IsCrossedSquare and IsPc3dGroup );

#############################################################################
##
#P  IsXPairing( <map> )
#R  IsXPairingObj( <obj> )
#O  XPairingObj( <src>, <rng>, <map> )
#A  XPairingMap( <xp> )
#O  ImageElmXPairing( <xp>, <elm> ) 
##
DeclareProperty( "IsXPairing", IsGeneralMapping );
DeclareRepresentation( "IsXPairingObj", IsXPairing and IsAttributeStoringRep,
    [ "Source", "Range", "XPairingMap" ] );
DeclareOperation( "XPairingObj", [ IsList, IsGroup, IsGeneralMapping ] );
DeclareAttribute( "XPairingMap", IsXPairing );
DeclareOperation( "ImageElmXPairing", [ IsXPairing, IsObject ] );

#############################################################################
##
#O  XPairingByNormalSubgroups
#O  XPairingByDerivations
##
DeclareOperation( "XPairingByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "XPairingByDerivations", [ IsXMod ] );

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
#O  PreCrossedSquareObj( <bdy>, <act> ) 
#O  PreCat20bj ( <arg> ) 
#A  Up2dGroup( <PS> ) 
#A  Down2dGroup ( <PS> ) 
#A  Left2dGroup( <PS> ) 
#A  Right2dGroup( <PS> ) 
#A  DiagonalAction( <PS> ) 
#A  XPairing( <PS> )
##
DeclareOperation( "PreCrossedSquareObj", 
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject] );
DeclareOperation( "PreCat20bj", 
    [ IsPreCat1, IsPreCat1, IsPreCat1, IsPreCat1 ] );
DeclareAttribute( "Up2dGroup", Is3dGroup );
DeclareAttribute( "Left2dGroup", Is3dGroup );
DeclareAttribute( "Down2dGroup", Is3dGroup );
DeclareAttribute( "Right2dGroup", Is3dGroup );
DeclareAttribute( "DiagonalAction", Is3dGroup );
DeclareAttribute( "XPairing", Is3dGroup );
DeclareAttribute( "LeftRightMorphism", Is3dGroup );
DeclareAttribute( "UpDownMorphism", Is3dGroup ); 

#############################################################################
##
#F  CrossedSquare( <args> ) 
#0  AsCrossedSquare( <arg> ) 
#0  CrossedSquareByXMods( <up>, <left>, <down>, <right>, <action>, <pairing> ) 
#0  CrossedSquareByNormalSubgroups( <P>, <N>, <M>, <L> )
#A  ActorCrossedSquare( <xmod> )
#A  Transpose3dGroup( <CrossedSquare> )
##
DeclareGlobalFunction( "CrossedSquare" );
DeclareOperation( "CrossedSquareByXMods", 
  [ IsXMod, IsXMod, IsXMod, IsXMod, IsGroupHomomorphism, IsXPairing ] );
DeclareOperation( "CrossedSquareByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareAttribute( "ActorCrossedSquare", IsXMod );
DeclareAttribute( "Transpose3dGroup", Is3dGroup );

#############################################################################
##
#P  IsTrivialAction3dGroup( <obj> ) 
#P  IsNormalSubgroup3dGroup( <obj> ) 
#P  IsAutomorphismGroup3dGroup( <XS> ) 
#P  IsAbelianSquare3dGroup( <obj> ) 
#P  IsFreeCrossedSquare( <XS> )
##
DeclareProperty( "IsTrivialAction3dGroup", Is3dGroup );
DeclareProperty( "IsNormalSubgroup3dGroup", Is3dGroup );
DeclareProperty( "IsCentralExtension3dGroup", Is3dGroup );
DeclareProperty( "IsAutomorphismGroup3dGroup", Is3dGroup );
DeclareProperty( "IsAbelianSquare3dGroup", Is3dGroup );
DeclareProperty( "IsFreeCrossedSquare", IsPreCrossedSquareObj );

#############################################################################
##
#0  IsSubPreCrossedSquare( <obj> ) 
#0  IsSubCrossedSquare( <obj> ) 
#0  IsSubPreCat2( <obj> ) 
#0  IsSubCat2( <obj> )
##
DeclareOperation( "IsSubPreCrossedSquare", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubCrossedSquare", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubPreCat2", [ Is3dGroup, Is3dGroup ] );
DeclareOperation( "IsSubCat2", [ Is3dGroup, Is3dGroup ] );

##############################################################################
##
#0  Sub3dGroup( <obj>, <src>, <rng> ) 
#0  SubPreCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubPreCat2( <C>, <H> )
##
DeclareOperation( "Sub3dGroup", [ Is3dGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCrossedSquare", [ IsPreCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubCrossedSquare", [ IsCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat2", [ IsPreCat2, IsGroup, IsGroup ] );
DeclareOperation( "SubCat2", [ IsCat2, IsGroup, IsGroup ] );

#############################################################################
##
#0  TrivialSub3dGroup( <obj> ) 
#A  TrivialSubPreCrossedSquare( <obj> ) 
#A  TrivialSubCrossedSquare( <obj> ) 
#A  TrivialSubPreCat2( <obj> ) 
#A  TrivialSubCat2( <obj> ) 
#P  IsIdentityCat2( <C1G> )
##
DeclareOperation( "TrivialSub3dGroup", [ Is3dGroup ] );
DeclareAttribute( "TrivialSubPreCrossedSquare", IsPreCrossedSquare );
DeclareAttribute( "TrivialSubCrossedSquare", IsCrossedSquare );
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
#0  PreCrossedSquareByPreCat2( <PCG> ) 
#0  PreCat2ByPreCrossedSquare( <PS> } 
#A  CrossedSquareOfCat2( <C1G> } 
#0  CrossedSquareByCat2( <C1G> } 
#A  Cat20fCrossedSquare( <XS> ) 
#0  Cat2ByCrossedSquare( <XS> )
##
DeclareOperation( "PreCrossedSquareByPreCat2", [ IsPreCat2 ] );
DeclareOperation( "PreCat2ByPreCrossedSquare", [ IsPreCrossedSquare ] );
DeclareAttribute( "CrossedSquareOfCat2", IsCat2 );
DeclareOperation( "CrossedSquareByCat2", [ IsCat2 ] );
DeclareAttribute( "Cat20fCrossedSquare", IsCrossedSquare );
DeclareOperation( "Cat2ByCrossedSquare", [ IsCrossedSquare ] );

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
#A  NormalSubCrossedSquares( <XS> } #A NormalSubXCat2s( <C1G> }
##
DeclareAttribute( "NormalSubCrossedSquares", IsCrossedSquare ); 
DeclareAttribute( "NormalSubCat2s", IsCat2 );

#############################################################################
##
#E gp3obj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
