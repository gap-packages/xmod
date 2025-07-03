#############################################################################
##
##  gp3obj.gd                 GAP4 package `XMod'               Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  
##  
##  This file declares generic methods for (pre-)crossed squares and
##  (pre-)cat2-groups.

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
#A  Size3d( <obj> ) 
#A  CrossedPairing( <3dgp> )
##
DeclareAttribute( "Size3d", Is3DimensionalDomain ); 
DeclareAttribute( "CrossedPairing", Is3DimensionalDomain );

#############################################################################
##
#O  CrossedPairingByCommutators
#O  CrossedPairingByDerivation 
#O  CrossedPairingBySingleXModAction 
#A  PrincipalCrossedPairing
#O  CrossedPairingByConjugators
#O  CrossedPairingByPreImages
##
DeclareOperation( "CrossedPairingByCommutators", 
    [ IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "CrossedPairingByDerivations", [ IsXMod ] );
DeclareOperation( "CrossedPairingBySingleXModAction", [ IsXMod, IsXMod ] );
DeclareAttribute( "PrincipalCrossedPairing", IsXMod ); 
DeclareOperation( "CrossedPairingByConjugators", [ IsGroup ] );
DeclareOperation( "CrossedPairingByPreImages", [ IsXMod, IsXMod ] ); 

#############################################################################
##
#P  IsSymmetric3DimensionalGroup( <obj> ) 
DeclareProperty( "IsSymmetric3DimensionalGroup", IsHigherDimensionalGroup );

#############################################################################
##
#A  Up2DimensionalGroup( <PS> ) 
#A  Down2DimensionalGroup ( <PS> ) 
#A  Left2DimensionalGroup( <PS> ) 
#A  Right2DimensionalGroup( <PS> ) 
#A  Diagonal2DimensionalGroup( <PS> ) 
#A  Subdiagonal2DimensionalGroup( <PS> ) 
#A  CrossDiagonalActions( <PS> ) 
#A  CrossedPairing( <PS> )
#A  Transpose3DimensionalGroup( <PS> )
#A  LeftRightMorphism( <PS> )
#A  UpDownMorphism( <PS> )
##
DeclareAttribute( "Up2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Left2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Down2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Right2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Diagonal2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "Subdiagonal2DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "CrossDiagonalActions", IsHigherDimensionalGroup );
DeclareAttribute( "CrossedPairing", IsHigherDimensionalGroup );
DeclareAttribute( "Transpose3DimensionalGroup", IsHigherDimensionalGroup );
DeclareAttribute( "LeftRightMorphism", IsHigherDimensionalGroup );
DeclareAttribute( "UpDownMorphism", IsHigherDimensionalGroup ); 

#############################################################################
##
#R  IsPreCrossedSquareObj ( <obj> ) 
#T  PreCrossedSquareObjType . . . . . . . . . . . .  type for crossed squares
#O  PreCrossedSquareObj( <bdy>, <act> ) 
##  A pre-crossed square is a square of pre-crossed modules 
##
DeclareRepresentation( "IsPreCrossedSquareObj", 
    IsHigherDimensionalGroup and IsAttributeStoringRep,
    [ "up2d", "left2d", "right2d", "down2d", "action", "pairing" ] );
BindGlobal( "PreCrossedSquareObjType", 
            NewType( FamilyHigherDimensionalGroup, IsPreCrossedSquareObj ) ); 
DeclareOperation( "PreCrossedSquareObj", 
    [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsObject, IsObject] );

#############################################################################
##
#P  IsPreCrossedSquare( <PM> } 
#P  IsCrossedSquare( <PM> ) 
##
DeclareProperty( "IsPreCrossedSquare", IsHigherDimensionalGroup );
DeclareProperty( "IsCrossedSquare", IsHigherDimensionalGroup );
InstallTrueMethod( IsHigherDimensionalGroup, IsPreCrossedSquare );
InstallTrueMethod( IsPreCrossedSquare, IsCrossedSquare );

#############################################################################
##
#F  PreCrossedSquare( <args> ) 
#F  CrossedSquare( <args> ) 
#O  PreCrossedSquareByPreXMods( <up>, <left>, <down>, <right>, <diag>, <xpair> ) 
#O  CrossedSquareByXMods( <up>, <left>, <down>, <right>, <diag>, <xpair> ) 
#O  CrossedSquareByNormalSubgroups( <L>, <M>, <N>, <P> )
#O  CrossedSquareByNormalSubXMod( <xmod>, <subxmod> ) 
#O  CrossedSquareByPullback( <xmod>, <xmod> )
#A  CrossedSquareByAutomorphismGroup( <gp> )
#A  ActorCrossedSquare( <xmod> )
#A  CrossedSquareByXModSplitting( <xmod> ) 
##
DeclareGlobalFunction( "PreCrossedSquare" );
DeclareGlobalFunction( "CrossedSquare" );
DeclareOperation( "PreCrossedSquareByPreXMods", 
  [ IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsPreXMod, IsFunction ] );
DeclareOperation( "CrossedSquareByXMods", 
  [ IsXMod, IsXMod, IsXMod, IsXMod, IsXMod, IsFunction ] );
DeclareOperation( "CrossedSquareByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "CrossedSquareByNormalSubgroups", 
    [ IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "CrossedSquareByNormalSubXMod", [ IsXMod, IsXMod ] );
DeclareOperation( "CrossedSquareByPullback", [ IsXMod, IsXMod ] );
DeclareAttribute( "CrossedSquareByAutomorphismGroup", IsGroup );
DeclareAttribute( "ActorCrossedSquare", IsXMod );
DeclareAttribute( "CrossedSquareByXModSplitting", IsXMod );

#############################################################################
##
#R  IsPreCat2GroupObj ( <obj> ) 
#T  PreCat2GroupObjType 
#O  PreCat2GroupObj( <cat1gp>, <cat1gp> ) 
##  A pre-cat2-group is a square of commuting pre-cat1-groups 
##
DeclareRepresentation( "IsPreCat2GroupObj", 
    IsHigherDimensionalGroup and Is3DimensionalDomain and IsAttributeStoringRep,
    [ "up", "left", "right", "down", "diag" ] );
BindGlobal( "PreCat2GroupObjType", 
            NewType( FamilyHigherDimensionalGroup, IsPreCat2GroupObj ) ); 
DeclareOperation( "PreCat2GroupObj", [ IsList ] );

#############################################################################
##
#P  IsPreCat2Group( <PCG> ) 
#P  IsCat2Group( <C1G> ) 
##
DeclareProperty( "IsPreCat2Group", IsHigherDimensionalGroup );
DeclareProperty( "IsCat2Group", IsHigherDimensionalGroup );
InstallTrueMethod( IsPreCat2Group, IsCat2Group );

#############################################################################
##
#F  PreCat2Group( <arg> ) 
#F  Cat2Group( <arg> }
#O  DetermineRemainingCat1Groups( <up>, <left> )
#O  PreCat2GroupByPreCat1Groups( <up>, <left>, <right>, <down>, <diag> )
##
DeclareGlobalFunction( "PreCat2Group" );
DeclareGlobalFunction( "Cat2Group" );
DeclareOperation( "DetermineRemainingCat1Groups", 
    [ IsPreCat1Group, IsPreCat1Group ] );
DeclareOperation( "PreCat2GroupByPreCat1Groups", [ IsPreCat1Group, 
    IsPreCat1Group, IsPreCat1Group, IsPreCat1Group, IsPreCat1Group ] );

#############################################################################
##
##  functions for the paper "Computing 3-Dimensional Groups ..." 
## 
#O  AllCat2GroupsWithImages( <G> <R> <Q> ) 
#O  AllCat2GroupsWithImagesIterator( <G> <R> <Q> ) 
#F  DoAllCat2GroupsWithImagesIterator
#O  AllCat2GroupsWithFixedUp( <G> <C> ) 
#O  AllCat2GroupsWithFixedUpIterator( <G> <C> ) 
#F  DoAllCat2GroupsWithFixedUpIterator
#O  AllCat2GroupsWithImagesNumber( <G> <R> <Q> ) 
#O  AllCat2GroupsWithImagesUpToIsomorphism( <G> <R> <Q> ) 
## 
DeclareOperation( "AllCat2GroupsWithImages", 
    [ IsGroup, IsGroup, IsGroup ] ); 
DeclareOperation( "AllCat2GroupsWithImagesIterator", 
    [ IsGroup, IsGroup, IsGroup ] ); 
DeclareGlobalFunction( "DoAllCat2GroupsWithImagesIterator" ); 
DeclareOperation( "AllCat2GroupsWithImagesNumber", 
    [ IsGroup, IsGroup, IsGroup ] ); 
DeclareOperation( "AllCat2GroupsWithImagesUpToIsomorphism", 
    [ IsGroup, IsGroup, IsGroup ] ); 
DeclareOperation( "AllCat2GroupsWithFixedUpAndLeftRange", 
    [ IsPreCat1Group, IsGroup ] ); 
DeclareOperation( "AllCat2GroupsWithFixedUp", 
    [ IsPreCat1Group ] ); 

#############################################################################
##
##  more functions for the paper "Computing 3-Dimensional Groups ..." 
## 
#O  AllCat2Groups( <G> ) 
#O  AllCat2GroupsIterator( <gp> )
#O  AllCat2GroupsMatrix( <G> ) 
#A  AllCat2GroupsNumber( <gp> )
#O  AllCat2GroupsUpToIsomorphism( <G> ) 
#O  TableRowForCat1Groups( <G> ) 
#O  TableRowForCat2Groups( <G> ) 
## 
DeclareOperation( "AllCat2Groups", [ IsGroup ] ); 
DeclareOperation( "AllCat2GroupsIterator", [ IsGroup ] ); DeclareOperation( "AllCat2GroupsMatrix", [ IsGroup ] ); 
DeclareAttribute( "AllCat2GroupsNumber", IsGroup ); 
DeclareOperation( "AllCat2GroupsUpToIsomorphism", [ IsGroup ] ); 
DeclareOperation( "AllCat2GroupFamilies", [ IsGroup ] ); 
DeclareOperation( "TableRowForCat1Groups", [ IsGroup ] ); 
DeclareOperation( "TableRowForCat2Groups", [ IsGroup ] ); 

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
## The following subfunctions were implemented in April 2025 for version 2.93
#############################################################################

#############################################################################
##
#O  IsSubPreCrossedSquare( <pxs> <subpxs> ) 
#O  IsSubCrossedSquare( <xs> <subxs> ) 
#O  IsSubPreCat2Group( <pc1> <subpc1> ) 
#O  IsSubCat2Group( <cat1> <subcat1 )
##
DeclareOperation( "IsSubPreCrossedSquare", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubCrossedSquare", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubPreCat2Group", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );
DeclareOperation( "IsSubCat2Group", 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ] );

#############################################################################
##
#O  SubPreCrossedSquare( <PXS>, <ul>, <ur>, <dl>, <dr> ) 
#O  SubCrossedSquare( <XS>, <ul>, <ur>, <dl>, <dr> ) 
#O  SubPreCat2Group( <PC2>, <ul>, <ur>, <dl>  )
#O  SubCat2Group( <C2>, <ul>, <ur>, <dl>  )
##
DeclareOperation( "SubPreCrossedSquare", 
    [ IsPreCrossedSquare, IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubCrossedSquare", 
    [ IsCrossedSquare, IsGroup, IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubPreCat2Group", 
    [ IsPreCat2Group, IsGroup, IsGroup, IsGroup ] );
DeclareOperation( "SubCat2Group", 
    [ IsCat2Group, IsGroup, IsGroup, IsGroup ] );

#############################################################################
##
#O  TrivialSub3DimensionalGroup( <obj> ) 
#A  TrivialSubPreCrossedSquare( <obj> ) 
#A  TrivialSubCrossedSquare( <obj> ) 
#A  TrivialSubPreCat2Group( <obj> ) 
#A  TrivialSubCat2Group( <obj> ) 
#P  IsIdentityCat2Group( <C1G> )
##
DeclareOperation( "TrivialSub3DimensionalGroup", [ Is3DimensionalDomain ] );
DeclareAttribute( "TrivialSubPreCrossedSquare", IsPreCrossedSquare );
DeclareAttribute( "TrivialSubCrossedSquare", IsCrossedSquare );
DeclareAttribute( "TrivialSubPreCat2Group", IsPreCat2Group );
DeclareAttribute( "TrivialSubCat2Group", IsCat2Group );
DeclareProperty( "IsIdentityCat2Group", IsCat2Group );

#############################################################################
##
#A  NormalSubCrossedSquares( <XS> } 
#A  NormalSubXCat2Groups( <C1G> }
##
DeclareAttribute( "NormalSubCrossedSquares", IsCrossedSquare ); 
DeclareAttribute( "NormalSubCat2Groups", IsCat2Group );
