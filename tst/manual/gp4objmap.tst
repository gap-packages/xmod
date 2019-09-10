#############################################################################
##
#W  gp4objmap.tst                 XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp4objmap.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## make independent of gp3objmap.tst   
gap> gen12 := [ (1,2,3,4,5,6), (2,6)(3,5) ];;
gap> d12 := Group( gen12 );; 
gap> SetName( d12, "d12" );                

## Chapter 9

## Section 9.1.1
gap> all1 := AllCat1Groups( d12 );; 
gap> C68 := Cat2Group( all1[6], all1[8] );; 
gap> C811 := Cat2Group( all1[8], all1[11] );;
gap> C3Ga := Cat3Group( C68, C811 );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,6)(2,5)(3,4) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (1,3)(4,6) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> C3Gb := Cat3Group( all1[6], all1[8], all1[11] );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,6)(2,5)(3,4) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (1,3)(4,6) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> C3Ga = C3Gb;
true

## Section 9.1.2
gap> C116 := Cat2Group( all1[11], all1[6] );;
gap> Up3DimensionalGroup( C3Ga ) = C116;
true

## Section 9.2.1
gap> triples := AllCat3GroupTriples( d12 );;
gap> CatnGroupNumbers( d12 ).cat3; 
94
gap> triples[46];
[ 5, 7, 11 ]
gap> all1 := AllCat1Groups( d12 );; 
gap> Cat3Group( all1[5], all1[7], all1[11] );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,4)(2,3)(5,6) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp4objmap.tst", 10000 );
