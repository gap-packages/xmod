##############################################################################
##
##  gp3obj.gd                 GAP4 package `XMod'                Chris Wensley
##                                                                Alper Odabas
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file declares generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.

#############################################################################
##
#R  IsPreCrossedSquareObj ( <obj> ) 
##  A pre-crossed square is a square of pre-crossed modules 
#R  IsPreCat2Obj( <obj> ) 
##  A pre-cat2-group is a square of pre-cat1-groups
##
DeclareRepresentation( "IsPreCrossedSquareObj", 
    Is3DimensionalGroup and IsAttributeStoringRep,
    [ "up2d", "down2d", "left2d", "right2d", "action", "pairing" ] );
DeclareRepresentation( "IsPreCat2Obj", 
    Is3DimensionalGroup and IsAttributeStoringRep, [ "up2d", "down2d" ] );

#############################################################################
##
#P  IsPerm3DimensionalGroup( <obj> ) 
#P  IsFp3DimensionalGroup( <obj> ) 
#P  IsPc3DimensionalGroup( <obj> )
##
DeclareProperty( "IsPerm3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsFp3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsPc3DimensionalGroup", Is3DimensionalGroup );

#############################################################################
##
#P  IsPreCrossedSquare( <PM> } 
#P  IsPermPreCrossedSquare( <PM> ) 
#P  IsFpPreCrossedSquare( <PM> ) 
#P  IsPcPreCrossedSquare( <PM> }
##
DeclareProperty( "IsPreCrossedSquare", Is3DimensionalGroup );
DeclareSynonym( "IsPermPreCrossedSquare", 
    IsPreCrossedSquare and IsPerm3DimensionalGroup );
DeclareSynonym( "IsFpPreCrossedSquare", 
    IsPreCrossedSquare and IsFp3DimensionalGroup );
DeclareSynonym( "IsPcPreCrossedSquare", 
    IsPreCrossedSquare and IsPc3DimensionalGroup );

#############################################################################
##
#P  IsCrossedSquare( <PM> ) 
#P  IsPermCrossedSquare( <XS> } 
#P  IsFpCrossedSquare( <XS> ) 
#P  IsPcCrossedSquare( <XS> )
##
DeclareProperty( "IsCrossedSquare", Is3DimensionalGroup );
InstallTrueMethod( IsPreCrossedSquare, IsCrossedSquare );
DeclareSynonym( "IsPermCrossedSquare", 
    IsCrossedSquare and IsPerm3DimensionalGroup );
DeclareSynonym( "IsFpCrossedSquare", 
    IsCrossedSquare and IsFp3DimensionalGroup );
DeclareSynonym( "IsPcCrossedSquare", 
    IsCrossedSquare and IsPc3DimensionalGroup );

#############################################################################
##
#P  IsCrossedPairing( <map> )
#R  IsCrossedPairingObj( <obj> )
#O  CrossedPairingObj( <src>, <rng>, <map> )
#A  CrossedPairingMap( <xp> )
#O  ImageElmCrossedPairing( <xp>, <elm> ) 
##
DeclareProperty( "IsCrossedPairing", IsGeneralMapping );
DeclareRepresentation( "IsCrossedPairingObj", 
    IsCrossedPairing and IsAttributeStoringRep,
    [ "Source", "Range", "CrossedPairingMap" ] );
DeclareOperation( "CrossedPairingObj", [ IsList, IsGroup, IsGeneralMapping ] );
DeclareAttribute( "CrossedPairingMap", IsCrossedPairing );
DeclareOperation( "ImageElmCrossedPairing", [ IsCrossedPairing, IsObject ] );

#############################################################################
##
#O  CrossedPairingByNormalSubgroups
#O  CrossedPairingByDerivations
##
DeclareOperation( "CrossedPairingByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "CrossedPairingByDerivations", [ IsXMod ] );

#############################################################################
##
#P  IsPreCat2Group( <PCG> ) 
#P  IsPermPreCat2Group( <PCG> ) 
#P  IsFpPreCat2Group( <PCG> ) 
#P  IsPcPreCat2Group( <PCG> )
##
DeclareProperty( "IsPreCat2Group", Is3DimensionalGroup );
DeclareSynonym( "IsPermPreCat2Group", 
    IsPreCat2Group and IsPerm3DimensionalGroup );
DeclareSynonym( "IsFpPreCat2Group", IsPreCat2Group and IsFp3DimensionalGroup ); 
DeclareSynonym( "IsPcPreCat2Group", IsPreCat2Group and IsPc3DimensionalGroup );

#############################################################################
##
#P  IsCat2Group( <C1G> ) 
#P  IsPermCat2Group( <CG> ) 
#P  IsFpCat2Group( <CG> ) 
#P  IsPcCat2Group( <CG> )
##
DeclareProperty( "IsCat2Group", Is3DimensionalGroup );
DeclareSynonym( "IsPermCat2Group", IsCat2Group and IsPerm3DimensionalGroup );
DeclareSynonym( "IsFpCat2Group", IsCat2Group and IsFp3DimensionalGroup );
DeclareSynonym( "IsPcCat2Group", IsCat2Group and IsPc3DimensionalGroup );

#############################################################################
##
#O  PreCrossedSquareObj( <bdy>, <act> ) 
#O  PreCat2Group0bj ( <arg> ) 
#A  Up2DimensionalGroup( <PS> ) 
#A  Down2DimensionalGroup ( <PS> ) 
#A  Left2DimensionalGroup( <PS> ) 
#A  Right2DimensionalGroup( <PS> ) 
#A  DiagonalAction( <PS> ) 
#A  CrossedPairing( <PS> )
##
DeclareOperation( "PreCrossedSquareObj", 
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject] );
DeclareOperation( "PreCat2Obj", 
    [ IsPreCat1Group, IsPreCat1Group ] );
DeclareAttribute( "Up2DimensionalGroup", Is3DimensionalGroup );
DeclareAttribute( "Left2DimensionalGroup", Is3DimensionalGroup );
DeclareAttribute( "Down2DimensionalGroup", Is3DimensionalGroup );
DeclareAttribute( "Right2DimensionalGroup", Is3DimensionalGroup );
DeclareAttribute( "DiagonalAction", Is3DimensionalGroup );
DeclareAttribute( "CrossedPairing", Is3DimensionalGroup );
DeclareAttribute( "LeftRightMorphism", Is3DimensionalGroup );
DeclareAttribute( "UpDownMorphism", Is3DimensionalGroup ); 

#############################################################################
##
#F  CrossedSquare( <args> ) 
#0  AsCrossedSquare( <arg> ) 
#0  CrossedSquareByXMods( <up>, <left>, <down>, <right>, <action>, <pairing> ) 
#0  CrossedSquareByNormalSubgroups( <P>, <N>, <M>, <L> )
#A  ActorCrossedSquare( <xmod> )
#A  Transpose3DimensionalGroup( <CrossedSquare> )
##
DeclareGlobalFunction( "CrossedSquare" );
DeclareOperation( "CrossedSquareByXMods", 
  [ IsXMod, IsXMod, IsXMod, IsXMod, IsGroupHomomorphism, IsCrossedPairing ] );
DeclareOperation( "CrossedSquareByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareAttribute( "ActorCrossedSquare", IsXMod );
DeclareAttribute( "Transpose3DimensionalGroup", Is3DimensionalGroup );

#############################################################################
##
#P  IsTrivialAction3DimensionalGroup( <obj> ) 
#P  IsNormalSubgroup3DimensionalGroup( <obj> ) 
#P  IsAutomorphismGroup3DimensionalGroup( <XS> ) 
#P  IsAbelianSquare3DimensionalGroup( <obj> ) 
#P  IsFreeCrossedSquare( <XS> )
##
DeclareProperty( "IsTrivialAction3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsNormalSubgroup3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsCentralExtension3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsAutomorphismGroup3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsAbelianSquare3DimensionalGroup", Is3DimensionalGroup );
DeclareProperty( "IsFreeCrossedSquare", IsPreCrossedSquareObj );

#############################################################################
##
#0  IsSubPreCrossedSquare( <obj> ) 
#0  IsSubCrossedSquare( <obj> ) 
#0  IsSubPreCat2Group( <obj> ) 
#0  IsSubCat2Group( <obj> )
##
DeclareOperation( "IsSubPreCrossedSquare", 
    [ Is3DimensionalGroup, Is3DimensionalGroup ] );
DeclareOperation( "IsSubCrossedSquare", 
    [ Is3DimensionalGroup, Is3DimensionalGroup ] );
DeclareOperation( "IsSubPreCat2Group", 
    [ Is3DimensionalGroup, Is3DimensionalGroup ] );
DeclareOperation( "IsSubCat2Group", 
    [ Is3DimensionalGroup, Is3DimensionalGroup ] );

##############################################################################
##
#0  Sub3DimensionalGroup( <obj>, <src>, <rng> ) 
#0  SubPreCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubPreCat2Group( <C>, <H> )
##
DeclareOperation( "Sub3DimensionalGroup", 
    [ Is3DimensionalGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCrossedSquare", 
    [ IsPreCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubCrossedSquare", [ IsCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat2Group", [ IsPreCat2Group, IsGroup, IsGroup ] );
DeclareOperation( "SubCat2Group", [ IsCat2Group, IsGroup, IsGroup ] );

#############################################################################
##
#0  TrivialSub3DimensionalGroup( <obj> ) 
#A  TrivialSubPreCrossedSquare( <obj> ) 
#A  TrivialSubCrossedSquare( <obj> ) 
#A  TrivialSubPreCat2Group( <obj> ) 
#A  TrivialSubCat2Group( <obj> ) 
#P  IsIdentityCat2Group( <C1G> )
##
DeclareOperation( "TrivialSub3DimensionalGroup", [ Is3DimensionalGroup ] );
DeclareAttribute( "TrivialSubPreCrossedSquare", IsPreCrossedSquare );
DeclareAttribute( "TrivialSubCrossedSquare", IsCrossedSquare );
DeclareAttribute( "TrivialSubPreCat2Group", IsPreCat2Group );
DeclareAttribute( "TrivialSubCat2Group", IsCat2Group );
DeclareProperty( "IsIdentityCat2Group", IsCat2Group );

#############################################################################
##
#F  PreCat2Group( <arg> ) 
#0  PreCat2GroupByPreCat1Groups( <first>, <second> )
##
DeclareGlobalFunction( "PreCat2GroupGroup" );
DeclareOperation( "PreCat2GroupGroupByPreCat1Groups", 
    [ IsPreCat1Group, IsPreCat1Group ] );

#############################################################################
##
#0  PreCrossedSquareByPreCat2Group( <PCG> ) 
#0  PreCat2GroupByPreCrossedSquare( <PS> } 
#A  CrossedSquareOfCat2Group( <C1G> } 
#0  CrossedSquareByCat2Group( <C1G> } 
#A  Cat2GroupOfCrossedSquare( <XS> ) 
#0  Cat2GroupByCrossedSquare( <XS> )
##
DeclareOperation( "PreCrossedSquareByPreCat2Group", [ IsPreCat2Group ] );
DeclareOperation( "PreCat2GroupByPreCrossedSquare", [ IsPreCrossedSquare ] );
DeclareAttribute( "CrossedSquareOfCat2Group", IsCat2Group );
DeclareOperation( "CrossedSquareByCat2Group", [ IsCat2Group ] );
DeclareAttribute( "Cat2GroupOfCrossedSquare", IsCrossedSquare );
DeclareOperation( "Cat2GroupByCrossedSquare", [ IsCrossedSquare ] );

#############################################################################
##
#F  Cat2Group( <arg> }
##
DeclareGlobalFunction( "Cat2Group" );

#############################################################################
##
#A  DirectProduct3dInfo( <D> }
##
DeclareAttribute( "DirectProduct3dInfo", Is3DimensionalDomain, "mutable" );

#############################################################################
##
#A  NormalSubCrossedSquares( <XS> } #A NormalSubXCat2Groups( <C1G> }
##
DeclareAttribute( "NormalSubCrossedSquares", IsCrossedSquare ); 
DeclareAttribute( "NormalSubCat2Groups", IsCat2Group );

#############################################################################
##
#E gp3obj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
