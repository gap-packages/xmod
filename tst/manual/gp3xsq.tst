#############################################################################
##
#W  gp3xsq.tst                 XMOD test file                   Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gp3xsq.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## make independent of isoclinic.tst and gp2obj.tst 
gap> d24 := DihedralGroup( IsPermGroup, 24 );; 
gap> SetName( d24, "d24" );
gap> Y24 := XModByAutomorphismGroup( d24 );; 
gap> X24i := Image( IsomorphismPerm2DimensionalGroup( Y24 ) );;
gap> R24i := Range( X24i );; 
gap> genR24 := [ (2,4), (1,2,3,4), (6,7), (5,6,7) ];; 
gap> rhom24 := GroupHomomorphismByImages( R24i, Group( genR24 ) );; 
gap> shom24 := IdentityMapping( d24 );; 
gap> iso24 := IsomorphismByIsomorphisms( X24i, [ shom24, rhom24 ] );; 
gap> X24 := Range( iso24 );;
gap> SetName( X24, Name( X24i ) );
gap> nsx := NormalSubXMods( X24 );; 
gap> ids := List( nsx, n -> IdGroup(n) );; 
gap> gen12 := [ (1,2,3,4,5,6), (2,6)(3,5) ];;
gap> d12 := Group( gen12 );;                  
gap> gen6 := [ (7,8,9), (8,9) ];;
gap> s3 := Group( gen6 );;
gap> pr12 := GroupHomomorphismByImages( d12, s3, gen12, gen6 );;
gap> X12 := XModByCentralExtension( pr12 );;

## Chapter 8

## Section 8.2.1
gap> b := (2,4);; c := (1,2)(3,4);; p := (1,2,3,4);; 
gap> d8 := Group( b, c );; 
gap> SetName( d8, "d8" );; 
gap> L := Subgroup( d8, [p^2] );; 
gap> M := Subgroup( d8, [b] );; 
gap> N := Subgroup( d8, [c] );; 
gap> P := TrivialSubgroup( d8 );; 
gap> kappa := GroupHomomorphismByImages( L, M, [p^2], [b] );; 
gap> lambda := GroupHomomorphismByImages( L, N, [p^2], [c] );; 
gap> delta := GroupHomomorphismByImages( L, P, [p^2], [()] );; 
gap> mu := GroupHomomorphismByImages( M, P, [b], [()] );; 
gap> nu := GroupHomomorphismByImages( N, P, [c], [()] );; 
gap> up := XModByTrivialAction( kappa );; 
gap> left := XModByTrivialAction( lambda );; 
gap> diag := XModByTrivialAction( delta );; 
gap> right := XModByTrivialAction( mu );; 
gap> down := XModByTrivialAction( nu );; 
gap> xp := CrossedPairingByCommutators( N, M, L );; 
gap> Print( "xp([c,b]) = ", xp( c, b ), "\n" ); 
xp([c,b]) = (1,3)(2,4)
gap> PXS := PreCrossedSquareByPreXMods( up, left, right, down, diag, xp );;
gap> Display( PXS ); 
(pre-)crossed square with (pre-)crossed modules:
      up = [Group( [ (1,3)(2,4) ] ) -> Group( [ (2,4) ] )]
    left = [Group( [ (1,3)(2,4) ] ) -> Group( [ (1,2)(3,4) ] )]
   right = [Group( [ (2,4) ] ) -> Group( () )]
    down = [Group( [ (1,2)(3,4) ] ) -> Group( () )]
gap> IsCrossedSquare( PXS ); 
false

## Section 8.2.2 
gap> Size3d( PXS );
[ 2, 2, 2, 1 ]

## Section 8.2.3
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
gap> XSconj := CrossedSquareByNormalSubgroups( c5d, d10a, d10b, d20 );
[  c5d -> d10a ]
[   |      |   ]
[ d10b -> d20  ]
gap> xpc := CrossedPairing( XSconj );;
gap> xpc( p12, p2 );
(1,3,5,7,9)(2,4,6,8,10)

## Section 8.2.4
gap> X20 := XModByNormalSubgroup( d20, d10a );; 
gap> X10 := XModByNormalSubgroup( d10b, c5d );; 
gap> ok := IsNormalSub2DimensionalDomain( X20, X10 ); 
true 
gap> XS20 := CrossedSquareByNormalSubXMod( X20, X10 ); 
[  c5d -> d10a ]
[   |      |   ]
[ d10b -> d20  ]
gap> xp20 := CrossedPairing( XS20 );; 
gap> xp20( p1^2, p2 );
(1,7,3,9,5)(2,8,4,10,6)

## Section 8.2.5
gap> XSact := ActorCrossedSquare( X20 );
crossed square with crossed modules:
      up = Whitehead[d10a->d20]
    left = [d10a->d20]
   right = Actor[d10a->d20]
    down = Norrie[d10a->d20]
gap> W := Range( Up2DimensionalGroup( XSact ) );
Group([ (2,5)(3,4), (2,3,5,4), (1,4,2,5,3) ])
gap> StructureDescription( W );
"C5 : C4"
gap> xpa := CrossedPairing( XSact );;
gap> xpa( p1, (2,3,5,4) );
(1,7,3,9,5)(2,8,4,10,6)

## Section 8.2.6
gap> AXS20 := CrossedSquareByAutomorphismGroup( d20 );
[      d20 -> Inn(d20) ]
[     |          |     ]
[ Inn(d20) -> Aut(d20) ]

gap> StructureDescription( AXS20 );
[ "D20", "D10", "D10", "C2 x (C5 : C4)" ]
gap> I20 := Range( Up2DimensionalGroup( AXS20 ) );;
gap> genI20 := GeneratorsOfGroup( I20 );           
[ ^(1,2,3,4,5,6,7,8,9,10), ^(2,10)(3,9)(4,8)(5,7) ]
gap> xpi := CrossedPairing( AXS20 );;
gap> xpi( genI20[1], genI20[2] );
(1,9,7,5,3)(2,10,8,6,4)

## Section 8.2.7
gap> dn := Down2DimensionalGroup( XSconj );;
gap> rt := Right2DimensionalGroup( XSconj );;
gap> XSP := CrossedSquareByPullback( dn, rt ); 
[ (d10b x_d20 d10a) -> d10a ]
[         |             |   ]
[              d10b -> d20  ]
gap> StructureDescription( XSP );                  
[ "C5", "D10", "D10", "D20" ]
gap> XS12 := CrossedSquareByPullback( X12, X12 );; 
gap> StructureDescription( XS12 );                  
[ "C2 x C2 x S3", "D12", "D12", "S3" ]
gap> xp12 := CrossedPairing( XS12 );; 
gap> xp12( (1,2,3,4,5,6), (2,6)(3,5) );
(1,5,3)(2,6,4)(7,11,9)(8,12,10)

## Section 8.2.8
gap> k4 := Group( (1,2), (3,4) );;
gap> AX4 := XModByAutomorphismGroup( k4 );;
gap> X4 := Image( IsomorphismPermObject( AX4 ) );;
gap> XSS4 := CrossedSquareByXModSplitting( X4 );;
gap> StructureDescription( XSS4 );
[ "C2 x C2", "1", "1", "S3" ]
gap> XSS20 := CrossedSquareByXModSplitting( X20 );;
gap> up20 := Up2DimensionalGroup( XSS20 );; 
gap> Range( up20 ) = d10a; 
true
gap> SetName( Range( up20 ), "d10a" ); 
gap> Name( XSS20 );;
gap> XSS20;
[d10a->d10a,d10a->d20]
gap> xps := CrossedPairing( XSS20 );;
gap> xps( p1^2, p2 );
(1,7,3,9,5)(2,8,4,10,6)

## Section 8.2.9
gap> diag := Diagonal2DimensionalGroup( AXS20 );
[d20->Aut(d20)]
gap> XSdiag := CrossedSquare( diag );;      
gap> StructureDescription( XSdiag );  
[ "D20", "D10", "D10", "C2 x (C5 : C4)" ]

## Section 8.2.10
gap> XStrans := Transpose3DimensionalGroup( XSconj ); 
[  c5d -> d10b ]
[   |      |   ]
[ d10a -> d20  ]

## Section 8.2.11
gap> pos7 := Position( ids, [ [12,2], [24,5] ] );;
gap> Xn7 := nsx[pos7];;
gap> IdGroup( Xn7 );
[ [ 12, 2 ], [ 24, 5 ] ]
gap> IdGroup( CentreXMod( Xn7 ) );
[ [ 4, 1 ], [ 4, 1 ] ]
gap> CQXn7 := CentralQuotient( Xn7 );;
gap> StructureDescription( CQXn7 );
[ "C12", "C3", "C4 x S3", "S3" ]

## Section 8.2.13
gap> Up2DimensionalGroup( XSconj );
[c5d->d10a]
gap> Right2DimensionalGroup( XSact );
Actor[d10a->d20]
gap> Name( XSconj ); 
"[c5d->d10a,d10b->d20]"
gap> cross1 := CrossDiagonalActions( XSconj )[1];; 
gap> gensa := GeneratorsOfGroup( d10a );;
gap> gensb := GeneratorsOfGroup( d10b );;
gap> act1 := ImageElm( cross1, gensb[1] );;
gap> gensa[2]; ImageElm( act1, gensa[2] );
(2,10)(3,9)(4,8)(5,7)
(1,5)(2,4)(6,10)(7,9)

## Section 8.2.15
gap> xp := CrossedPairing( XSconj );;
gap> xp( (1,6)(2,5)(3,4)(7,10)(8,9), (1,5)(2,4)(6,9)(7,8) );
(1,7,8,5,3)(2,9,10,6,4)
gap> F := FreeGroup(1);;
gap> x := GeneratorsOfGroup(F)[1];;
gap> z := GroupHomomorphismByImages( F, F, [x], [x^0] );;
gap> id := GroupHomomorphismByImages( F, F, [x], [x] );;
gap> h := function(n,m)
>             return x^(ExponentSumWord(n,x)*ExponentSumWord(m,x));
>         end;;
gap> h( x^3, x^4 );
f1^12
gap> A := AutomorphismGroup( F );;
gap> a := GeneratorsOfGroup(A)[1];;
gap> act := GroupHomomorphismByImages( F, A, [x], [a^2] );;
gap> X0 := XModByBoundaryAndAction( z, act );;
gap> X1 := XModByBoundaryAndAction( id, act );;
gap> XSF := PreCrossedSquareByPreXMods( X0, X0, X1, X1, X0, h );; 
gap> IsCrossedSquare( XSF ); 
true

## Section 8.3.1
gap> Display( XS20 ); 
[  c5d -> d10a ]
[   |      |   ]
[ d10b -> d20  ]
gap> p5 := p1^5*p2;;  [p2,p12,p5];
[ (2,10)(3,9)(4,8)(5,7), (1,10)(2,9)(3,8)(4,7)(5,6), 
  (1,6)(2,5)(3,4)(7,10)(8,9) ]
gap> L1 := TrivialSubgroup( c5d );;
gap> M1 := Subgroup( d10a, [ p2 ] );;
gap> N1 := Subgroup( d10b, [ p5 ] );;
gap> P1 := Subgroup( d20, [ p2, p5 ] );;
gap> sub20 := SubCrossedSquare( XS20, L1, M1, N1, P1 );
crossed square with crossed modules:
      up = [Group( () ) -> Group( [ ( 2,10)( 3, 9)( 4, 8)( 5, 7) ] )]
    left = [Group( () ) -> Group( [ ( 1, 6)( 2, 5)( 3, 4)( 7,10)( 8, 9) ] )]
   right = [Group( [ ( 2,10)( 3, 9)( 4, 8)( 5, 7) ] ) -> Group( 
[ ( 2,10)( 3, 9)( 4, 8)( 5, 7), ( 1, 6)( 2, 5)( 3, 4)( 7,10)( 8, 9) ] )]
    down = [Group( [ ( 1, 6)( 2, 5)( 3, 4)( 7,10)( 8, 9) ] ) -> Group( 
[ ( 2,10)( 3, 9)( 4, 8)( 5, 7), ( 1, 6)( 2, 5)( 3, 4)( 7,10)( 8, 9) ] )]

gap> sxp := CrossedPairing( sub20 );;
gap> sxp( p5, p2 );
()

## Section 8.3.2
gap> TXS20 := TrivialSubCrossedSquare( XS20 );
crossed square with crossed modules:
      up = [Group( () ) -> Group( () )]
    left = [Group( () ) -> Group( () )]
   right = [Group( () ) -> Group( () )]
    down = [Group( () ) -> Group( () )]

## Section 8.4.3
gap> ad20 := GroupHomomorphismByImages( d20, d20, [p1,p2], [p1,p2^p1] );;
gap> ad10a := GroupHomomorphismByImages( d10a, d10a, [p1^2,p2], [p1^2,p2^p1] );;
gap> ad10b := GroupHomomorphismByImages( d10b, d10b, [p1^2,p12], [p1^2,p12^p1] );;
gap> idc5d := IdentityMapping( c5d );;
gap> up := Up2DimensionalGroup( XSconj );;
gap> lt := Left2DimensionalGroup( XSconj );; 
gap> rt := Right2DimensionalGroup( XSconj );; 
gap> dn := Down2DimensionalGroup( XSconj );; 
gap> mup := XModMorphismByGroupHomomorphisms( up, up, idc5d, ad10a );
[[c5d->d10a] => [c5d->d10a]]
gap> mlt := XModMorphismByGroupHomomorphisms( lt, lt, idc5d, ad10b );
[[c5d->d10b] => [c5d->d10b]]
gap> mrt := XModMorphismByGroupHomomorphisms( rt, rt, ad10a, ad20 );
[[d10a->d20] => [d10a->d20]]
gap> mdn := XModMorphismByGroupHomomorphisms( dn, dn, ad10b, ad20 );
[[d10b->d20] => [d10b->d20]]
gap> autoconj := CrossedSquareMorphism( XSconj, XSconj, [mup,mlt,mrt,mdn] );; 
gap> ord := Order( autoconj );;
gap> Display( autoconj );
Morphism of crossed squares :- 
: Source = [c5d->d10a,d10b->d20]
: Range = [c5d->d10a,d10b->d20]
:     order = 5
:    up-left: [ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10) ] ]
:   up-right: 
[ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 2,10)( 3, 9)( 4, 8)( 5, 7) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1, 3)( 4,10)( 5, 9)( 6, 8) ] ]
:  down-left: 
[ [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1,10)( 2, 9)( 3, 8)( 4, 7)( 5, 6) ], 
  [ ( 1, 3, 5, 7, 9)( 2, 4, 6, 8,10), ( 1, 2)( 3,10)( 4, 9)( 5, 8)( 6, 7) ] ]
: down-right: 
[ [ ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10), ( 2,10)( 3, 9)( 4, 8)( 5, 7) ], 
  [ ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10), ( 1, 3)( 4,10)( 5, 9)( 6, 8) ] ]
gap> IsAutomorphismHigherDimensionalDomain( autoconj );
true
gap> kpo := KnownPropertiesOfObject( autoconj );;
gap> Set( kpo );
[ "CanEasilyCompareElements", "CanEasilySortElements", 
  "IsAutomorphismHigherDimensionalDomain", "IsCrossedSquareMorphism", 
  "IsEndomorphismHigherDimensionalDomain", "IsInjective", 
  "IsPreCrossedSquareMorphism", "IsSingleValued", "IsSurjective", "IsTotal" ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp3xsq.tst", 10000 );
