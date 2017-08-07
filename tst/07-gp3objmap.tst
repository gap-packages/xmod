#############################################################################
##
#W  gp3objmap.tst                 XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 8

## Section 8.2.1
gap> d20 := DihedralGroup( IsPermGroup, 20 );;
gap> gend20 := GeneratorsOfGroup( d20 ); 
[ (1,2,3,4,5,6,7,8,9,10), (2,10)(3,9)(4,8)(5,7) ]
gap> p1 := gend20[1];;  p2 := gend20[2];;  p12 := p1*p2; 
(1,10)(2,9)(3,8)(4,7)(5,6)
gap> d10a := Subgroup( d20, [ p1^2, p2 ] );;
gap> d10b := Subgroup( d20, [ p1^2, p12 ] );;
gap> c5d := Subgroup( d20, [ p1^2 ] );;
gap> SetName( d20, "d20" );  SetName( d10a, "d10a" ); 
gap> SetName( d10b, "d10b" );  SetName( c5d, "c5d" ); 
gap> XSconj := CrossedSquareByNormalSubgroups( d20, d10b, d10a, c5d );
[  c5d -> d10b ]
[   |      |   ]
[ d10a -> d20  ]

gap> Name( XSconj );
"[c5d->d10b,d10a->d20]"
gap> XStrans := Transpose3DimensionalGroup( XSconj ); 
[  c5d -> d10a ]
[   |      |   ]
[ d10b -> d20  ]

gap> X20 := XModByNormalSubgroup( d20, d10a );
[d10a->d20]
gap> XSact := ActorCrossedSquare( X20 );
crossed square with:
      up = Whitehead[d10a->d20]
    left = [d10a->d20]
    down = Norrie[d10a->d20]
   right = Actor[d10a->d20]

## Section 8.2.2
gap> pos7 := Position( ids, [ [12,2], [24,5] ] );;
gap> Xn7 := nsx[pos7]; 
[Group( [ f2, f3, f4 ] )->Group( [ f1, f2, f4, f5 ] )]
gap> IdGroup( CentreXMod(Xn7) );  
[ [ 4, 1 ], [ 4, 1 ] ]
gap> CQXn7 := CentralQuotient( Xn7 );
crossed square with:
      up = [Group( [ f2, f3, f4 ] )->Group( [ f1 ] )]
    left = [Group( [ f2, f3, f4 ] )->Group( [ f1, f2, f4, f5 ] )]
    down = [Group( [ f1, f2, f4, f5 ] )->Group( [ f1, f2 ] )]
   right = [Group( [ f1 ] )->Group( [ f1, f2 ] )]

gap> IdGroup( CQXn7 );
[ [ [ 12, 2 ], [ 3, 1 ] ], [ [ 24, 5 ], [ 6, 1 ] ] ]

## Section 8.2.4
gap> Up2DimensionalGroup( XSconj );
[c5d->d10b]
gap> Right2DimensionalGroup( XSact );
Actor[d10a->d20]
gap> xpconj := CrossedPairing( XSconj );;
gap> ImageElmCrossedPairing( xpconj, [ p2, p12 ] );
(1,9,7,5,3)(2,10,8,6,4)
gap> diag := DiagonalAction( XSact );
[ (1,3,5,2,4)(6,10,14,8,12)(7,11,15,9,13), (1,2,5,4)(6,8,14,12)(7,11,13,9) 
 ] -> [ ^(1,3,5,7,9)(2,4,6,8,10), ^(1,2,5,4)(3,8)(6,7,10,9) ]

## Section 8.3.2
gap> ad20 := GroupHomomorphismByImages( d20, d20, [p1,p2], [p1,p2^p1] );;
gap> ad10a := GroupHomomorphismByImages( d10a, d10a, [p1^2,p2], [p1^2,p2^p1] );;
gap> ad10b := GroupHomomorphismByImages( d10b, d10b, [p1^2,p12], [p1^2,p12^p1] );;
gap> idc5d := IdentityMapping( c5d );;
gap> upconj := Up2DimensionalGroup( XSconj );;
gap> leftconj := Left2DimensionalGroup( XSconj );; 
gap> downconj := Down2DimensionalGroup( XSconj );; 
gap> rightconj := Right2DimensionalGroup( XSconj );; 
gap> up := XModMorphismByHoms( upconj, upconj, idc5d, ad10b );
[[c5d->d10b] => [c5d->d10b]]
gap> left := XModMorphismByHoms( leftconj, leftconj, idc5d, ad10a );
[[c5d->d10a] => [c5d->d10a]]
gap> down := XModMorphismByHoms( downconj, downconj, ad10a, ad20 );
[[d10a->d20] => [d10a->d20]]
gap> right := XModMorphismByHoms( rightconj, rightconj, ad10b, ad20 );
[[d10b->d20] => [d10b->d20]]
gap> autoconj := CrossedSquareMorphism( XSconj, XSconj, [up,left,right,down] );; 
gap> ord := Order( autoconj );;
gap> Display( autoconj );
Morphism of crossed squares :- 
:    Source = [c5d->d10b,d10a->d20]
:     Range = [c5d->d10b,d10a->d20]
:     order = 5
:    up-left: [ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10) ] ]
:   up-right: 
[ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1,10)( 2, 9)( 3, 8)( 4, 7)( 5, 6) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1, 2)( 3,10)( 4, 9)( 5, 8)( 6, 7) ] ]
:  down-left: 
[ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 2,10)( 3, 9)( 4, 8)( 5, 7) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1, 3)( 4,10)( 5, 9)( 6, 8) ] ]
: down-right: 
[ [ ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10), ( 2,10)( 3, 9)( 4, 8)( 5, 7) ], 
  [ ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10), ( 1, 3)( 4,10)( 5, 9)( 6, 8) ] ]
gap> IsAutomorphismHigherDimensionalDomain( autoconj );
true
gap> KnownPropertiesOfObject( autoconj );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsTotal", 
  "IsSingleValued", "IsInjective", "IsSurjective", 
  "IsPreCrossedSquareMorphism", "IsCrossedSquareMorphism", 
  "IsEndomorphismHigherDimensionalDomain", 
  "IsAutomorphismHigherDimensionalDomain" ]
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp3objmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
