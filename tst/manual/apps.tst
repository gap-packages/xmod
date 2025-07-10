#############################################################################
##
#W  apps.tst                    GAP4 package `XMod'             Chris Wensley
##  
#Y  Copyright (C) 2001-2022, Chris Wensley et al, 

gap> START_TEST( "XMod package: apps.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 11

## Subsection 11.1.1 
gap> k8 := Group( (3,4), (5,6), (7,8) );; 
gap> SetName( k8, "k8" ); 
gap> Y8 := XModByAutomorphismGroup( k8 );; 
gap> X8 := Image( IsomorphismPerm2DimensionalGroup( Y8 ) );;
gap> SetName( X8, "X8" );
gap> Print( "X8: ", Size2d( X8 ), " : ", StructureDescription( X8 ), "\n" );  
X8: [ 8, 168 ] : [ "C2 x C2 x C2", "PSL(3,2)" ]
gap> classes := LoopClasses( X8 );;
gap> List( classes, c -> Length(c) );
[ 1, 21, 56, 42, 24, 24 ]
gap> LX := LoopsXMod( X8, (1,2)(5,6) );;
gap> Size2d( LX ); 
[ 8, 64 ]
gap> IdGroup( LX );
[ [ 8, 5 ], [ 64, 138 ] ]
gap> SetInfoLevel( InfoXMod, 1 );
gap> LX8 := AllLoopsXMod( X8 );;
#I  LoopsXMod with a = (),  IdGroup = [ [ 8, 5 ], [ 1344, 11686 ] ]
#I  LoopsXMod with a = (4,5)(6,7),  IdGroup = [ [ 8, 5 ], [ 64, 138 ] ]
#I  LoopsXMod with a = (2,3)(4,6,5,7),  IdGroup = [ [ 8, 5 ], [ 32, 6 ] ]
#I  LoopsXMod with a = (2,4,6)(3,5,7),  IdGroup = [ [ 8, 5 ], [ 24, 13 ] ]
#I  LoopsXMod with a = (1,2,4,3,6,7,5),  IdGroup = [ [ 8, 5 ], [ 56, 11 ] ]
#I  LoopsXMod with a = (1,2,4,5,7,3,6),  IdGroup = [ [ 8, 5 ], [ 56, 11 ] ]
gap> iso := IsomorphismGroups( Range( LX ), Range( LX8[2] ) );;
gap> iso = fail;
false

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "apps.tst", 10000 );
