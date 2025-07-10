#############################################################################
##
#W  gpnobjmap.tst                 XMOD test file                Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gpnobjmap.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

gap> SetAssertionLevel(3); 

## make independent of gp3xsq.tst 
gap> d20 := DihedralGroup( IsPermGroup, 20 );;
gap> gend20 := GeneratorsOfGroup( d20 );; 
gap> p1 := gend20[1];;  p2 := gend20[2];;  p12 := p1*p2;; 
gap> d10a := Subgroup( d20, [ p1^2, p2 ] );;
gap> d10b := Subgroup( d20, [ p1^2, p12 ] );;
gap> c5d := Subgroup( d20, [ p1^2 ] );;
gap> SetName( d20, "d20" );  SetName( d10a, "d10a" ); 
gap> SetName( d10b, "d10b" );  SetName( c5d, "c5d" ); 
gap> XSconj := CrossedSquareByNormalSubgroups( c5d, d10a, d10b, d20 );;

## 
gap> s3 := SmallGroup( 6, 1 );;
gap> homs := AllHomomorphisms( s3, s3 );;
gap> idem := Filtered( homs, i -> CompositionMapping(i,i) = i );
[ [ f1, f2 ] -> [ <identity> of ..., <identity> of ... ],
  [ f1, f2 ] -> [ f1, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2^2, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2, <identity> of ... ], [ f1, f2 ] -> [ f1, f2 ] ]
gap> pc1 := PreCat1GroupWithIdentityEmbedding( idem[1], idem[1] );
[Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
gap> pc2 := PreCat1GroupWithIdentityEmbedding( idem[2], idem[2] );
[Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> CC12 := CatnGroup( [ pc1, pc2 ] );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> HigherDimension( CC12 );
3

gap> pc3 := PreCat1GroupWithIdentityEmbedding( idem[5], idem[5] );
[Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
gap> CC233 := CatnGroup( [pc2, pc3, pc3] );
(pre-)cat3-group with generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

gap> IsPreCatnGroup( CC233 );
true
gap> IsCatnGroup( CC233 );
true

gap> CC5 := CatnGroup( [ Cat1Select(8,2,4), Cat1Select(8,2,3),
>          Cat1Select(8,2,2), Cat1Select(8,2,1), Cat1Select(8,2,1) ] );
(pre-)cat5-group with generating (pre-)cat1-groups:
1 : [C4 x C2=>Group( [ <identity> of ..., <identity> of ..., <identity> of ... ] )]
2 : [C4 x C2=>Group( [ <identity> of ..., f2 ] )]
3 : [C4 x C2=>Group( [ f1, <identity> of ... ] )]
4 : [C4 x C2=>C4 x C2]
5 : [C4 x C2=>C4 x C2]

gap> Display( CC5 );
(pre-)cat5-group with generating (pre-)cat1-groups:
1 : 
Cat1-group :- 
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: head homomorphism maps source generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: range embedding maps range generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: kernel has generators:
  [ f1, f2, f3 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f1, f2, f3 ]
: associated crossed module is [Group( [ f1, f2, f3 ] ) -> Group( 
[ <identity> of ..., <identity> of ..., <identity> of ... ] )]

2 : 
Cat1-group :- 
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ <identity> of ..., f2 ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., f2, <identity> of ... ]
: head homomorphism maps source generators to:
  [ <identity> of ..., f2, <identity> of ... ]
: range embedding maps range generators to:
  [ <identity> of ..., f2 ]
: kernel has generators:
  [ f1, f3 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ..., <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f1, f3 ]
: associated crossed module is [Group( [ f1, f3 ] ) -> Group( 
[ <identity> of ..., f2 ] )]

3 : 
Cat1-group :- 
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ f1, <identity> of ... ]
: tail homomorphism maps source generators to:
  [ f1, <identity> of ..., f3 ]
: head homomorphism maps source generators to:
  [ f1, <identity> of ..., f3 ]
: range embedding maps range generators to:
  [ f1, <identity> of ... ]
: kernel has generators:
  [ f2 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f2 ]
: associated crossed module is [Group( [ f2 ] ) -> Group( 
[ f1, <identity> of ... ] )]

4 : 
Cat1-group [C4 x C2=>C4 x C2] :- 
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group C4 x C2 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ f1, f2, f3 ]
: head homomorphism maps source generators to:
  [ f1, f2, f3 ]
: range embedding maps range generators to:
  [ f1, f2, f3 ]
: the kernel is trivial.
: associated crossed module is [triv->C4 x C2]

5 : 
Cat1-group [C4 x C2=>C4 x C2] :- 
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group C4 x C2 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ f1, f2, f3 ]
: head homomorphism maps source generators to:
  [ f1, f2, f3 ]
: range embedding maps range generators to:
  [ f1, f2, f3 ]
: the kernel is trivial.
: associated crossed module is [triv->C4 x C2]

gap> CC6 := Cat2Group( Cat1Select(6,2,2), Cat1Select(6,2,3) );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [C6=>Group( [ f2 ] )]
2 : [C6=>Group( [ f1 ] )]

gap> IsCat2Group( CC6 );
true

## now producing an error (13/01/20)
gap> xsCC6 := CrossedSquareOfCat2Group( CC6 );
crossed square with crossed modules:
      up = [Group( () ) -> Group( [ (3,4,5) ] )]
    left = [Group( () ) -> Group( [ (), (1,2) ] )]
   right = [Group( [ (3,4,5) ] ) -> Group( () )]
    down = [Group( [ (), (1,2) ] ) -> Group( () )]

gap> IsCrossedSquare( xsCC6 );
true

gap> CCconj := Cat2GroupOfCrossedSquare( XSconj );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [((d20 |X d10a) |X (d10b |X c5d)) => (d20 |X d10a)]
2 : [((d20 |X d10a) |X (d10b |X c5d)) => (d20 |X d10b)]
gap> IsCat2Group( CCconj );
true

gap> idCC233 := IdentityMapping( CC233 );;
WARNING: further checks are needed here
gap> Display( idCC233 );
Morphism of pre-cat3-groups :- 
: Source has (pre-)cat3-group with generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] ) => Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] ) => Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] ) => Group( [ f1, f2 ] )]
: Range has (pre-)cat3-group with generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] ) => Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] ) => Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] ) => Group( [ f1, f2 ] )]
: MappingGeneratorsImages for the source homomorphisms:
1 : [ [ f1, f2 ], [ f1, f2 ] ]
2 : [ [ f1, f2 ], [ f1, f2 ] ]
3 : [ [ f1, f2 ], [ f1, f2 ] ]
gap> IsBijective( idCC233 );
true

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gpnobjmap.tst", 10000 );
