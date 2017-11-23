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
##
DeclareRepresentation( "IsPreCrossedSquareObj", 
    IsHigherDimensionalGroup and IsAttributeStoringRep,
    [ "up2d", "down2d", "left2d", "right2d", "action", "pairing" ] );

#############################################################################
##
#P  IsPerm3DimensionalGroup( <obj> ) 
#P  IsFp3DimensionalGroup( <obj> ) 
#P  IsPc3DimensionalGroup( <obj> )
##
DeclareProperty( "IsPerm3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsFp3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsPc3DimensionalGroup", IsHigherDimensionalGroup );

#############################################################################
##
#T  PreCrossedSquareObjType . . . . . . . . . . . .  type for crossed squares
#T  PermPreCrossedSquareObjType . . . . . .  .  type for perm crossed squares
#T  PcPreCrossedSquareObjType . . . . . . . . . . type for pc crossed squares
## 
BindGlobal( "PreCrossedSquareObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCrossedSquareObj ) ); 
BindGlobal( "PermPreCrossedSquareObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCrossedSquareObj and IsPerm3DimensionalGroup ) ); 
BindGlobal( "PcPreCrossedSquareObjType", 
            NewType( FamilyHigherDimensionalGroup, 
                     IsPreCrossedSquareObj and IsPc3DimensionalGroup ) ); 

#############################################################################
##
#P  IsPreCrossedSquare( <PM> } 
#P  IsPermPreCrossedSquare( <PM> ) 
#P  IsFpPreCrossedSquare( <PM> ) 
#P  IsPcPreCrossedSquare( <PM> }
##
DeclareProperty( "IsPreCrossedSquare", IsHigherDimensionalGroup );
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
DeclareProperty( "IsCrossedSquare", IsHigherDimensionalGroup );
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
#V  CrossedPairingFamily
#T  CrossedPairingObjType
#A  CrossedPairingMap( <xp> )
#O  ImageElmCrossedPairing( <xp>, <elm> ) 
##
DeclareProperty( "IsCrossedPairing", IsGeneralMapping );
DeclareRepresentation( "IsCrossedPairingObj", 
    IsCrossedPairing and IsAttributeStoringRep,
    [ "Source", "Range", "CrossedPairingMap" ] ); 
BindGlobal( "CrossedPairingFamily", 
    NewFamily( "CrossedPairingFamily", IsCrossedPairing, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "CrossedPairingType", 
    NewType( CrossedPairingFamily, IsCrossedPairingObj ) ); 
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
DeclareProperty( "IsPreCat2Group", IsHigherDimensionalGroup );
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
DeclareProperty( "IsCat2Group", IsHigherDimensionalGroup );
InstallTrueMethod( IsPreCat2Group, IsCat2Group );
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
DeclareAttribute( "Up2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Left2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Down2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Right2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "DiagonalAction", IsHigherDimensionalGroup );
DeclareAttribute( "CrossedPairing", IsHigherDimensionalGroup );
DeclareAttribute( "LeftRightMorphism", IsHigherDimensionalGroup );
DeclareAttribute( "UpDownMorphism", IsHigherDimensionalGroup ); 

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
DeclareAttribute( "Transpose3DimensionalGroup", IsHigherDimensionalGroup );

#############################################################################
##
#P  IsTrivialAction3DimensionalGroup( <obj> ) 
#P  IsNormalSubgroup3DimensionalGroup( <obj> ) 
#P  IsAutomorphismGroup3DimensionalGroup( <XS> ) 
#P  IsAbelianSquare3DimensionalGroup( <obj> ) 
#P  IsFreeCrossedSquare( <XS> )
##
DeclareProperty( "IsTrivialAction3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsNormalSubgroup3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsCentralExtension3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsAutomorphismGroup3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsAbelianSquare3DimensionalGroup", IsHigherDimensionalGroup );
DeclareProperty( "IsFreeCrossedSquare", IsPreCrossedSquareObj );

#############################################################################
##
#0  IsSubPreCrossedSquare( <obj> ) 
#0  IsSubCrossedSquare( <obj> ) 
#0  IsSubPreCat2Group( <obj> ) 
#0  IsSubCat2Group( <obj> )
##
DeclareOperation( "IsSubPreCrossedSquare", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubCrossedSquare", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubPreCat2Group", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubCat2Group", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );

##############################################################################
##
#0  SubPreCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubCrossedSquare( <PM, Ssrc, Srng> ) 
#0  SubPreCat2Group( <C>, <H> )
##
DeclareOperation( "SubPreCrossedSquare", 
    [ IsPreCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubCrossedSquare", [ IsCrossedSquare, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat2Group", [ IsPreCat2Group, IsGroup, IsGroup ] );
DeclareOperation( "SubCat2Group", [ IsCat2Group, IsGroup, IsGroup ] );

#############################################################################
##
#0  TrivialSubHigherDimensionalGroup( <obj> ) 
#A  TrivialSubPreCrossedSquare( <obj> ) 
#A  TrivialSubCrossedSquare( <obj> ) 
#A  TrivialSubPreCat2Group( <obj> ) 
#A  TrivialSubCat2Group( <obj> ) 
#P  IsIdentityCat2Group( <C1G> )
##
DeclareOperation( "TrivialSubHigherDimensionalGroup", 
    [ IsHigherDimensionalGroup ] );
DeclareAttribute( "TrivialSubPreCrossedSquare", IsPreCrossedSquare );
DeclareAttribute( "TrivialSubCrossedSquare", IsCrossedSquare );
DeclareAttribute( "TrivialSubPreCat2Group", IsPreCat2Group );
DeclareAttribute( "TrivialSubCat2Group", IsCat2Group );
DeclareProperty( "IsIdentityCat2Group", IsCat2Group );

#############################################################################
##
#A  ElementsRelationsForSemidirectProduct( <G> ) 
##
DeclareAttribute( "ElementsRelationsForSemidirectProduct", IsGroup );

#############################################################################
##
#F  PreCat2Group( <arg> ) 
#0  PreCat2GroupByPreCat1Groups( <first>, <second> )
##
DeclareGlobalFunction( "PreCat2Group" );
DeclareOperation( "PreCat2GroupByPreCat1Groups", 
    [ IsPreCat1Group, IsPreCat1Group ] );

#############################################################################
##
#F  Cat2Group( <arg> }
##
DeclareGlobalFunction( "Cat2Group" );

#############################################################################
##
#O  ConjugationActionForCrossedSquare( <G>, <N> )
#A  PreCrossedSquareOfPreCat2Group( <PCG> ) 
#A  PreCat2GroupOfPreCrossedSquare( <PS> } 
#A  CrossedSquareOfCat2Group( <C1G> } 
#A  Cat2GroupOfCrossedSquare( <XS> ) 
##
DeclareOperation( "ConjugationActionForCrossedSquare", [ IsGroup, IsGroup ] );
DeclareAttribute( "PreCrossedSquareOfPreCat2Group", IsPreCat2Group );
DeclareAttribute( "PreCat2GroupOfPreCrossedSquare", IsPreCrossedSquare );
DeclareAttribute( "CrossedSquareOfCat2Group", IsCat2Group );
DeclareAttribute( "Cat2GroupOfCrossedSquare", IsCrossedSquare );

#############################################################################
##
#A  DirectProductHigherDimensionalInfo( <D> }
##
DeclareAttribute( "DirectProductHigherDimensionalInfo", 
     IsHigherDimensionalDomain, "mutable" );

#############################################################################
##
#A  NormalSubCrossedSquares( <XS> } 
#A  NormalSubXCat2Groups( <C1G> }
##
DeclareAttribute( "NormalSubCrossedSquares", IsCrossedSquare ); 
DeclareAttribute( "NormalSubCat2Groups", IsCat2Group );

#############################################################################
##
#E gp3obj.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
