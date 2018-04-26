##############################################################################
##
#W  apps.tst                    GAP4 package `XMod'              Chris Wensley
##  
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 

gap> START_TEST( "XMod package: apps.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

gap> k8 := Group( (3,4), (5,6), (7,8) );; 
gap> SetName( k8, "k8" ); 
gap> Y8 := XModByAutomorphismGroup( k8 );; 
gap> X8 := Image( IsomorphismPerm2DimensionalGroup( Y8 ) );;
gap> SetName( X8, "X8" );
gap> Print( "X8: ", Size( X8 ), " : ", StructureDescription( X8 ), "\n" );  
X8: [ 8, 168 ] : [ "C2 x C2 x C2", "PSL(3,2)" ]
gap> LX := LoopsXMod( X8, (1,2)(5,6) );;
gap> Size( LX ); 
[ 8, 64 ]
gap> str := StructureDescription( LX );;
gap> str in  [ [ "C2 x C2 x C2", "((C2 x C2 x C2 x C2) : C2) : C2" ], 
>              [ "C2 x C2 x C2", "(C4 x C2) : D8" ] ]; 
true
gap> SetInfoLevel( InfoXMod, 1 );
gap> LX8 := AllLoopsXMod( X8 );;
#I  LoopsXMod with a = (), [ 8, 1344 ] 
#I  LoopsXMod with a = (4,5)(6,7), [ 8, 64 ]
#I  LoopsXMod with a = (2,3)(4,6,5,7), [ 8, 32 ]
#I  LoopsXMod with a = (2,4,6)(3,5,7), [ 8, 24 ] 
#I  LoopsXMod with a = (1,2,4,3,6,7,5), [ 8, 56 ] 
#I  LoopsXMod with a = (1,2,4,5,7,3,6), [ 8, 56 ]
gap> iso := IsomorphismGroups( Range( LX ), Range( LX8[2] ) );;
gap> iso = fail;
false

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "apps.tst", 10000 );

##############################################################################
##
#E  apps.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
