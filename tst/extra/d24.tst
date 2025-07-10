#############################################################################
##
#W  d24.tst                       XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2023, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: d24.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> a := (1,2,3,4,5,6,7,8,9,10,11,12);; 
gap> b := (2,12)(3,11)(4,10)(5,9)(6,8);;
gap> d24 := Group( a, b );;
gap> SetName( d24, "d24" );
gap> d12a := Subgroup( d24, [ a^2, b ] );; 
gap> SetName( d12a, "d12a" ); 
gap> d12b := Subgroup( d24, [ a^2, a*b ] );; 
gap> SetName( d12b, "d12b" ); 
gap> c6 := Intersection( d12a, d12b );; 
gap> SetName( c6, "c6" ); 
gap> xs24 := CrossedSquareByNormalSubgroups( c6, d12a, d12b, d24 ); 
[   c6 -> d12a ]
[   |      |  ]
[ d12b -> d24 ]
gap> cxs24 := PreCat2GroupOfPreCrossedSquare( xs24 ); 
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [((d24 |X d12a) |X (d12b |X c6)) => (d24 |X d12a)]
2 : [((d24 |X d12a) |X (d12b |X c6)) => (d24 |X d12b)]
gap> IsCat2Group( cxs24 );
true
gap> diag := Diagonal2DimensionalGroup( cxs24 );
[((d24 |X d12a) |X (d12b |X c6)) => d24]
gap> ## another example of a cat2-group whose diagonal is only a pre-cat1-group 
gap> t := TailMap( diag );;  kert := Kernel( t );; 
gap> h := HeadMap( diag );;  kerh := Kernel( h );; 
gap> kerth := CommutatorSubgroup( kert, kerh );;
gap> StructureDescription( kerth );
"C6 x C6"
gap> ## a nontrivial kerth means that diag is not a cat1-group 
gap> IsCat1Group( diag );
false

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "d24.tst", 10000 );

#############################################################################
##
#E  d24.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
