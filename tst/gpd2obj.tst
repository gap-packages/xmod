#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al, 
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
gap> PXO := DiscreteNormalPreXModWithObjects( Ga4, k4 );;
gap> Print( PXO, "\n" );
[perm homogeneous, discrete groupoid: < k4, [ -9, -8, -7 ] >
->  perm single piece groupoid: < a4, [ -9, -8, -7 ] >]

gap> IsXMod( PXO );
true
gap> SetName( Ga4, "Ga4" );; 
gap> SetName( Source(PXO), "Dk4" );; 
gap> Name( PXO );
"[Dk4->Ga4]"

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGpd, saved_infolevel_gpd );; 

#############################################################################
##
#E  gpd2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
