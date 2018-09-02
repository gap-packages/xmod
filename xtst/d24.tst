#############################################################################
##
#W  d24.tst                       XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: d24.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> d24 := DihedralGroup( IsPermGroup, 24 );;
gap> SetName( d24, "d24" );
gap> norm := NormalSubgroups( d24 );; 
gap> d12a := norm[3];; 
gap> SetName( d12a, "d12a" ); 
gap> d12b := norm[4];; 
gap> SetName( d12b, "d12b" ); 
gap> c6 := Intersection( d12a, d12b );; 
gap> SetName( c6, "c6" ); 
gap> xs24 := CrossedSquareByNormalSubgroups( d24, d12a, d12b, c6 ); 
[   c6 -> d12a ]
[   |      |  ]
[ d12b -> d24 ]
gap> cxs24 := PreCat2GroupOfPreCrossedSquare( xs24 ); 
cat2-group with generating (pre-)cat1-groups:
1 : [d24 |X d12a |X d12b |X c6=>d24 |X d12a]
2 : [d24 |X d12a |X d12b |X c6=>d24 |X d12b]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "d24.tst", 10000 );

#############################################################################
##
#E  d24.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
