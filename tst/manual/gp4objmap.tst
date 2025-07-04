#############################################################################
##
#W  gp4objmap.tst                 XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gp4objmap.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## make independent of gp3objmap.tst   
gap> gen12 := [ (1,2,3,4,5,6), (2,6)(3,5) ];;
gap> d12 := Group( gen12 );; 
gap> SetName( d12, "d12" );                

## Chapter 9

## Section 9.1.1
gap> alld12 := AllCat1Groups( d12 );; 
gap> C68 := Cat2Group( alld12[6], alld12[8] );; 
gap> C811 := Cat2Group( alld12[8], alld12[11] );;
gap> C3Ga := Cat3Group( C68, C811 );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,6)(2,5)(3,4) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (1,3)(4,6) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> C3Gb := Cat3Group( alld12[6], alld12[8], alld12[11] );;
gap> C3Ga = C3Gb;
true

## Section 9.1.2
gap> C116 := Cat2Group( alld12[11], alld12[6] );;
gap> Up3DimensionalGroup( C3Ga ) = C116;
true

## Section 9.2.1
gap> triples := AllCat3GroupTriples( d12 );;
gap> CatnGroupNumbers( d12 ).cat3; 
94
gap> triples[46];
[ 5, 7, 11 ]
gap> alld12 := AllCat1Groups( d12 );; 
gap> Cat3Group( alld12[5], alld12[7], alld12[11] );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,4)(2,3)(5,6) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> GroupsOfHigherDimensionalGroup( last ); 
[ d12, Group([ (), (1,4)(2,3)(5,6) ]), Group([ (1,4)(2,5)(3,6), (2,6)
  (3,5) ]), Group([ (), (1,4)(2,3)(5,6) ]), Group([ (1,5,3)(2,6,4), (1,4)(2,3)
  (5,6) ]), Group([ (), (1,4)(2,3)(5,6) ]), Group([ (), (1,4)(2,3)(5,6) ]), 
  Group([ (), (1,4)(2,3)(5,6) ]) ]

## Section 9.3.1
gap> PC4 := PreCatnGroup( [ alld12[5], alld12[7], alld12[11], alld12[12] ] );
(pre-)cat4-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,4)(2,3)(5,6) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
4 : [d12 => Group( [ (1,2,3,4,5,6), (2,6)(3,5) ] )]
gap> IsCatnGroup( PC4 );                                             
true
gap> HigherDimension( PC4 );
5

gap> G := Group( (1,2), (3,4,5), (6,7,8,9,10), (11,12,13,14,15,16,17), (20,21,22,23,24,25,26,27,28,29,30) );;
gap> SetName( G, "C2310" );
gap> all1 := AllCat1Groups( G );;
gap> Print( "G has ", CatnGroupNumbers( G ).cat1, " cat1-groups\n" );
G has 32 cat1-groups
gap> PC5 := PreCatnGroup( [ all1[2], all1[5], all1[13], all1[25], all1[32] ] );
(pre-)cat5-group with generating (pre-)cat1-groups:
1 : [C2310 => Group( [ (), (), (), (), (1,2) ] )]
2 : [C2310 => Group( [ (), (), (), (3,4,5), (1,2) ] )]
3 : [C2310 => Group( [ (), (), ( 6, 7, 8, 9,10), (3,4,5), (1,2) ] )]
4 : [C2310 => Group( [ (), (11,12,13,14,15,16,17), ( 6, 7, 8, 9,10), (3,4,5), 
  (1,2) ] )]
5 : [C2310 => Group( [ (20,21,22,23,24,25,26,27,28,29,30), 
  (11,12,13,14,15,16,17), ( 6, 7, 8, 9,10), (3,4,5), (1,2) ] )]
gap> HigherDimension( PC5 );
6

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp4objmap.tst", 10000 );
