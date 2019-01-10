#############################################################################
##
#W  gp3objmap.tst                 XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp3objmap.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## make independent of isoclinic.tst and gp2obj.tst 
gap> d24 := DihedralGroup( IsPermGroup, 24 );; 
gap> SetName( d24, "d24" );
gap> Y24 := XModByAutomorphismGroup( d24 );; 
gap> X24 := Image( IsomorphismPerm2DimensionalGroup( Y24 ) );;
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

## Section 8.2.2
gap> X20 := XModByNormalSubgroup( d20, d10a );; 
gap> X10 := XModByNormalSubgroup( d10b, c5d );; 
gap> ok := IsNormalSub2DimensionalDomain( X20, X10 ); 
true 
gap> XS20 := CrossedSquareByNormalSubXMod( X20, X10 ); 
[  c5d -> d10a ]
[   |      |   ]
[ d10b -> d20  ]

## Section 8.2.3
gap> XSact := ActorCrossedSquare( X20 );
crossed square with crossed modules:
      up = [d10a->d20]
    left = Whitehead[d10a->d20]
   right = Norrie[d10a->d20]
    down = Actor[d10a->d20]

## Section 8.2.4
gap> AXS20 := CrossedSquareByAutomorphismGroup( d20 );
[      d20 -> Inn(d20) ]
[     |          |     ]
[ Inn(d20) -> Aut(d20) ]

gap> StructureDescription( AXS20 );
[ "D20", "D10", "D10", "C2 x (C5 : C4)" ]

## Section 8.2.5
gap> XS12 := CrossedSquareByPullback( X12, X12 );; 
gap> StructureDescription( XS12 );                  
[ "C2 x C2 x S3", "D12", "D12", "S3" ]

## Section 8.2.6
gap> k4 := Group( (1,2), (3,4) );;
gap> AX4 := XModByAutomorphismGroup( k4 );;
gap> X4 := Image( IsomorphismPermObject( AX4 ) );;
gap> XS4 := CrossedSquareByXModSplitting( X4 );;
gap> StructureDescription( XS4 );
[ "C2 x C2", "1", "1", "S3" ]

## Section 8.2.7 
gap> diag := Diagonal2DimensionalGroup( AXS20 );
[d20->Aut(d20)]
gap> XSdiag := CrossedSquare( diag );;      
gap> StructureDescription( XSdiag );  
[ "D20", "D10", "D10", "C2 x (C5 : C4)" ]

## Section 8.2.8
gap> XStrans := Transpose3DimensionalGroup( XSconj ); 
[  c5d -> d10b ]
[   |      |   ]
[ d10a -> d20  ]

## Section 8.2.9
gap> Up2DimensionalGroup( XSconj );
[c5d->d10a]
gap> Down2DimensionalGroup( XSact );
Actor[d10a->d20]
gap> diact := DiagonalAction( XSact );;
gap> ImageElm( diact, (1,4)(2,3)(6,9)(7,8) );  
^(1,5,7,3)(2,8,6,10)
gap> diag := Diagonal2DimensionalGroup( XSconj );
[c5d->d20]
gap> Name( XSconj ); 
"[c5d->d10a,d10b->d20]"

## Section 8.3.2
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
gap> KnownPropertiesOfObject( autoconj );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsTotal", 
  "IsSingleValued", "IsInjective", "IsSurjective", 
  "IsPreCrossedSquareMorphism", "IsCrossedSquareMorphism", 
  "IsEndomorphismHigherDimensionalDomain", 
  "IsAutomorphismHigherDimensionalDomain" ]

# Section 8.4.1 
gap> a := (1,2,3,4)(5,6,7,8);;
gap> b := (1,5)(2,6)(3,7)(4,8);; 
gap> c := (2,6)(4,8);;
gap> G16 := Group( a, b, c );; 
gap> SetName( G16, "c4c2:c2" );
gap> t1a := GroupHomomorphismByImages( G16, G16, [a,b,c], [(),(),c] );; 
gap> C1a := PreCat1GroupByEndomorphisms( t1a, t1a );;
gap> t1b := GroupHomomorphismByImages( G16, G16, [a,b,c], [a,(),()] );; 
gap> C1b := PreCat1GroupByEndomorphisms( t1b, t1b );;
gap> C16 := Cat2Group( C1a, C1b );
cat2-group with generating (pre-)cat1-groups:
1 : [c4c2:c2 => Group( [ (), (), (2,6)(4,8) ] )]
2 : [c4c2:c2 => Group( [ (1,2,3,4)(5,6,7,8), (), () ] )]
gap> IsCat2Group( C16 );
true
gap> IsCat1Group( Diagonal2DimensionalGroup( C16 ) );
false

# Section 8.4.2 
gap> xsC16 := CrossedSquareOfCat2Group( C16 );
crossed square with crossed modules:
      up = [Group( [ (1,5)(2,6)(3,7)(4,8) ] ) -> Group( [ ( 2, 6)( 4, 8) ] )]
    left = [Group( [ (1,5)(2,6)(3,7)(4,8) ] ) -> Group( 
[ (1,2,3,4)(5,6,7,8), (), () ] )]
   right = [Group( [ ( 2, 6)( 4, 8) ] ) -> Group( () )]
    down = [Group( [ (1,2,3,4)(5,6,7,8), (), () ] ) -> Group( () )]

gap> SetName( Source( Down2DimensionalGroup( XSact ) ), "c5:c4" );
gap> SetName( Range( Down2DimensionalGroup( XSact ) ), "c5:c4" );
gap> Name( XSact );
"[d10a->d20,c5:c4->c5:c4]"

gap> C2act := Cat2GroupOfCrossedSquare( XSact );             
cat2-group with generating (pre-)cat1-groups:
1 : [((c5:c4 |X c5:c4) |X (d20 |X d10a))=>(c5:c4 |X c5:c4)]
2 : [((c5:c4 |X c5:c4) |X (d20 |X d10a))=>(c5:c4 |X d20)]
gap> Size( C2act );
[ 80000, 400, 400, 20 ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp3objmap.tst", 10000 );

#############################################################################
##
#E  gp3objmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
