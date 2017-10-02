#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gpd2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 9

gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> SetName( a4, "a4" );
gap> Ga4 := SinglePieceGroupoid( a4, [-9,-8,-7] );;
gap> Display( Ga4 );
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> SetName( k4, "k4" );
gap> PXO := DiscreteNormalPreXModWithObjects( Ga4, k4 );;
gap> ## Print( PXO, "\n" );
gap> ## IsXMod( PXO ); 
gap> SetName( Ga4, "Ga4" );; 
gap> ## SetName( Source(PXO), "Dk4" );; 
gap> ## Name( PXO );

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gpd2obj.tst", 10000 );

#############################################################################
##
#E  gpd2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
