#############################################################################
##
#W  allxmods.tst                  XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2022, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: allxmods.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

## Chapter 4

#### 4.1.11
gap> x36 := AllXMods( 36 );; 
gap> Length( x36 ); 
205
gap> size36 := List( x36, x -> Size2d( x ) );;
gap> Collected( size36 );
[ [ [ 1, 36 ], 14 ], [ [ 2, 18 ], 7 ], [ [ 3, 12 ], 21 ], [ [ 4, 9 ], 14 ], 
  [ [ 6, 6 ], 17 ], [ [ 9, 4 ], 102 ], [ [ 12, 3 ], 8 ], [ [ 18, 2 ], 18 ], 
  [ [ 36, 1 ], 4 ] ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "allxmods.tst", 10000 );

#############################################################################
##
#E  allxmods.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
