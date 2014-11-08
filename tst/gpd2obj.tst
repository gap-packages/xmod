#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_gpd := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );;
gap> d8 := Group( (1,2,3,4), (1,3) );;
gap> SetName( d8, "d8" );
gap> Gd8 := SinglePieceGroupoid( d8, [-9,-8,-7] );; 
gap> Display( Gd8 ); 
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]>
gap> k4 := Subgroup( d8, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> PX0 := DiscreteNormalPreXModWithObjects( Gd8, k4 ); 
homogeneous, discrete groupoid with:
  group: Group( [ (1,2)(3,4), (1,3)(2,4) ] ) >
objects: [ -9, -8, -7 ]
#I  now need to be able to test:   ok := IsXMod( PM );
<magma>
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGpd, saved_infolevel_gpd );; 

#############################################################################
##
#E  gpd2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
