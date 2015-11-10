#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_gpd := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );;

## Chapter 9

gap> Ga4 := SinglePieceGroupoid( a4, [-9,-8,-7] );;
gap> Display( Ga4 );
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> GeneratorsOfGroup( k4 );
[ (1,2)(3,4), (1,3)(2,4) ]
gap> PXO := DiscreteNormalPreXModWithObjects( Ga4, k4 );
homogeneous, discrete groupoid with:
  group: k4 = <[ (1,2)(3,4), (1,3)(2,4) ]> >
objects: [ -9, -8, -7 ]
#I  now need to be able to test:   ok := IsXMod( PM );
<semigroup>
gap> Source( PXO ); 
perm homogeneous, discrete groupoid: < k4, [ -9, -8, -7 ] >

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGpd, saved_infolevel_gpd );; 

#############################################################################
##
#E  gpd2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
