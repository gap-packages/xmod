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
gap> C116 := Cat2Group( all1[11], all1[6] );;
gap> C3Ga := PreCat3Group( C68, C116 );
cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,6)(2,5)(3,4) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (1,3)(4,6) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> C3Gb := Cat3Group( all1[6], all1[8], all1[11] );
cat3-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (), (1,6)(2,5)(3,4) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (1,3)(4,6) ] )]
3 : [d12 => Group( [ (1,5,3)(2,6,4), (1,4)(2,3)(5,6) ] )]
gap> C3Ga = C3Gb;
true

## Section 9.1.2
gap> C811 := Cat2Group( all1[8], all1[11] );;
gap> Left3DimensionalGroup( C3Ga ) = C811;
true

## Section 9.1.3

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp4objmap.tst", 10000 );
