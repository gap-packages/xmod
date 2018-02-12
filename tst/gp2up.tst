#############################################################################
##
#W  gp2up.tst                     XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp2up.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 5

## Section 5.1.1
gap> g18 := Group( (1,2,3), (4,5,6), (2,3)(5,6) );;
gap> SetName( g18, "g18" );
gap> gen18 := GeneratorsOfGroup( g18 );;
gap> g1 := gen18[1];;  g2 := gen18[2];;  g3 := gen18[3];;
gap> s3 := Subgroup( g18, gen18{[2..3]} );;
gap> SetName( s3, "s3" );;
gap> t := GroupHomomorphismByImages( g18, s3, gen18, [g2,g2,g3] );;
gap> h := GroupHomomorphismByImages( g18, s3, gen18, [(),g2,g3] );;
gap> e := GroupHomomorphismByImages( s3, g18, [g2,g3], [g2,g3] );;
gap> C3 := Cat1Group( t, h, e );
[g18=>s3]
gap> SetName( Kernel(t), "c3" );;
gap> X3 := XModOfCat1Group( C3 );;
gap> Display( X3 );

Crossed module [c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,6,5) ]
: Range group has generators:
  [ (4,5,6), (2,3)(5,6) ]
: Boundary homomorphism maps source generators to:
  [ (4,6,5) ]
: Action homomorphism maps range generators to automorphisms:
  (4,5,6) --> { source gens --> [ (1,2,3)(4,6,5) ] }
  (2,3)(5,6) --> { source gens --> [ (1,3,2)(4,5,6) ] }
  These 2 automorphisms generate the group of automorphisms.
: associated cat1-group is [g18=>s3]

gap> imchi1 := [ (1,2,3)(4,6,5), (1,2,3)(4,6,5) ];;
gap> chi1 := DerivationByImages( X3, imchi1 );
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], 
[ (1,2,3)(4,6,5), (1,2,3)(4,6,5) ] )
gap> [ IsUp2DimensionalMapping( chi1 ), IsDerivation( chi1 ) ];
[ true, true ]

## Section 5.1.2
gap> hom2 := GroupHomomorphismByImages( s3, g18, [ (4,5,6), (2,3)(5,6) ], 
> [ (1,3,2)(4,6,5), (1,2)(4,6) ] );;
gap> xi2 := SectionByImages( C3, hom2 );                                 
SectionByImages( s3, g18, [ (4,5,6), (2,3)(5,6) ], 
[ (1,3,2)(4,6,5), (1,2)(4,6) ] )
gap> [ IsUp2DimensionalMapping( xi2 ), IsSection( xi2 ) ];
[ true, true ]
gap> chi2 := DerivationBySection( xi2 );
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], 
[ (1,3,2)(4,5,6), (1,2,3)(4,6,5) ] )
gap> xi1 := SectionByDerivation( chi1 );
SectionByImages( s3, g18, [ (4,5,6), (2,3)(5,6) ], [ (1,2,3), (1,2)(4,6) ] )

## Section 5.1.3
gap> chi12 := chi1 * chi2;
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], [ (1,2,3)(4,6,5), () ] )
gap> xi12 := xi1 * xi2;
## Section 5.2.1
gap> all3 := AllDerivations( X3 );
monoid of derivations with images list:
[ [ (), () ],
  [ (), (1,3,2)(4,5,6) ],
  [ (), (1,2,3)(4,6,5) ],
  [ (1,3,2)(4,5,6), () ],
  [ (1,3,2)(4,5,6), (1,3,2)(4,5,6) ],
  [ (1,3,2)(4,5,6), (1,2,3)(4,6,5) ],
  [ (1,2,3)(4,6,5), () ],
  [ (1,2,3)(4,6,5), (1,3,2)(4,5,6) ],
  [ (1,2,3)(4,6,5), (1,2,3)(4,6,5) ]
  ]
gap> PrintOneItemPerLine( ImagesTable( all3 ) );
[ [ 1, 1, 1, 1, 1, 1 ],
  [ 1, 1, 1, 3, 3, 3 ],
  [ 1, 1, 1, 2, 2, 2 ],
  [ 1, 3, 2, 1, 3, 2 ],
  [ 1, 3, 2, 3, 2, 1 ],
  [ 1, 3, 2, 2, 1, 3 ],
  [ 1, 2, 3, 1, 2, 3 ],
  [ 1, 2, 3, 3, 1, 2 ],
  [ 1, 2, 3, 2, 3, 1 ]
  ]

## Section 5.2.2
gap> reg3 := RegularDerivations( X3 );;
gap> imder3 := ImagesList( reg3 );;  Length( imder3 ); 
6
gap> chi4 := DerivationByImages( X3, imder3[4] );
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], [ (1,3,2)(4,5,6), () ] )
gap> chi5 := DerivationByImages( X3, imder3[5] );
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], 
[ (1,3,2)(4,5,6), (1,3,2)(4,5,6) ] )
gap> im4 := UpImagePositions( chi4 );
[ 1, 3, 2, 1, 3, 2 ]
gap> im5 := UpImagePositions( chi5 );
[ 1, 3, 2, 3, 2, 1 ]
gap> chi45 := chi4 * chi5;
DerivationByImages( s3, c3, [ (4,5,6), (2,3)(5,6) ], [ (), (1,3,2)(4,5,6) ] )
gap> im45 := UpImagePositions( chi45 );
[ 1, 1, 1, 3, 3, 3 ]
gap> Position( imder3, UpGeneratorImages( chi45 ) );
2
gap> chi45 = PrincipalDerivation( X3, (1,2,3)(4,6,5) );
true
gap> pder3 := PrincipalDerivations( X3 );
monoid of derivations with images list:
[ [ (), () ],
  [ (), (1,3,2)(4,5,6) ],
  [ (), (1,2,3)(4,6,5) ]
  ]


## Section 5.2.3
gap> wgt3 := WhiteheadGroupTable( X3 );; 
gap> PrintOneItemPerLine( wgt3 );
[ [ 1, 2, 3, 4, 5, 6 ],
  [ 2, 3, 1, 5, 6, 4 ],
  [ 3, 1, 2, 6, 4, 5 ],
  [ 4, 6, 5, 1, 3, 2 ],
  [ 5, 4, 6, 2, 1, 3 ],
  [ 6, 5, 4, 3, 2, 1 ]
  ]
gap> wpg3 := WhiteheadPermGroup( X3 );
Group([ (1,2,3)(4,5,6), (1,4)(2,6)(3,5) ])
gap> wmt3 := WhiteheadMonoidTable( X3 );; 
gap> PrintOneItemPerLine( wmt3 );
[ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ],
  [ 2, 3, 1, 5, 6, 4, 8, 9, 7 ],
  [ 3, 1, 2, 6, 4, 5, 9, 7, 8 ],
  [ 4, 6, 5, 1, 3, 2, 7, 9, 8 ],
  [ 5, 4, 6, 2, 1, 3, 8, 7, 9 ],
  [ 6, 5, 4, 3, 2, 1, 9, 8, 7 ],
  [ 7, 7, 7, 7, 7, 7, 7, 7, 7 ],
  [ 8, 8, 8, 8, 8, 8, 8, 8, 8 ],
  [ 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
  ]
gap> wtm3 := WhiteheadTransMonoid( X3 );
<transformation monoid of degree 9 with 3 generators>
gap> GeneratorsOfMonoid( wtm3 ); 
[ Transformation( [ 2, 3, 1, 5, 6, 4, 8, 9, 7 ] ), 
  Transformation( [ 4, 6, 5, 1, 3, 2, 7, 9, 8 ] ), 
  Transformation( [ 7, 7, 7, 7, 7, 7, 7, 7, 7 ] ) ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp2up.tst", 10000 );

#############################################################################
##
#E  gp2up.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
