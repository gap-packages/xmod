#############################################################################
##
#W  gp2ind.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gp2ind.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

##  make independent of gp2obj.tst  
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> SetName( s4, "s4" );  SetName( a4, "a4" ); 
gap> b1 := (11,12,13,14,15,16,17,18);; 
gap> b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> SetName( d16, "d16" ); 

## Chapter 7

## Section 7.1.1
gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );;
gap> XAq8 := XModByAutomorphismGroup( q8 );;
gap> s4b := Range( XAq8 );; 
gap> SetName( q8, "q8" );  SetName( s4b, "s4b" ); 
gap> a := q8.1;;  b := q8.2;; 
gap> alpha := GroupHomomorphismByImages( q8, q8, [a,b], [a^-1,b] );;
gap> beta := GroupHomomorphismByImages( q8, q8, [a,b], [a,b^-1] );;
gap> k4b := Subgroup( s4b, [ alpha, beta ] );;  SetName( k4b, "k4b" );
gap> Z8 := XModByNormalSubgroup( s4b, k4b );;
gap> SetName( XAq8, "XAq8" );  SetName( Z8, "Z8" );  
gap> SetInfoLevel( InfoXMod, 1 ); 
gap> XZ8 := CoproductXMod( XAq8, Z8 );
#I  prexmod is [ [ 32, 47 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2, [ 2, 1 ]
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ], [ [ 16, 14 ], [ 24, 12 ] ]
[Group( [ f1, f2, f3, f4 ] )->s4b]
gap> SetName( XZ8, "XZ8" ); 
gap> info := CoproductInfo( XZ8 );
rec( embeddings := [ [XAq8 => XZ8], [Z8 => XZ8] ], xmods := [ XAq8, Z8 ] )
gap> SetInfoLevel( InfoXMod, 0 ); 
gap> Y := CoproductXMod( [ XAq8, XAq8, Z8, Z8 ] );
[Group( [ f1, f2, f3, f4, f5, f6, f7, f8 ] )->s4b]
gap> StructureDescription( Y );          
[ "C2 x C2 x C2 x C2 x C2 x C2 x C2 x C2", "S4" ]
gap> CoproductInfo( Y );
rec( 
  embeddings := 
    [ [XAq8 => [Group( [ f1, f2, f3, f4, f5, f6, f7, f8 ] ) -> s4b]], 
      [XAq8 => [Group( [ f1, f2, f3, f4, f5, f6, f7, f8 ] ) -> s4b]], 
      [Z8 => [Group( [ f1, f2, f3, f4, f5, f6, f7, f8 ] ) -> s4b]], 
      [Z8 => [Group( [ f1, f2, f3, f4, f5, f6, f7, f8 ] ) -> s4b]] ], 
  xmods := [ XAq8, XAq8, Z8, Z8 ] )

## Section 7.2.1 : Example 1
gap> a := (6,7,8,9)(10,11,12);;  b := (7,9)(11,12);;
gap> d24 := Group( [ a, b ] );;
gap> SetName( d24, "d24" );
gap> c := (1,2)(3,4,5);;  d := (4,5);;
gap> d12 := Group( [ c, d ] );;
gap> SetName( d12, "d12" );
gap> bdy := GroupHomomorphismByImages( d24, d12, [a,b], [c,d] );;
gap> X24 := XModByCentralExtension( bdy );
[d24->d12]
gap> e := (13,14,15);;  f := (14,15);;
gap> s3 := Group( [ e, f ] );;
gap> SetName( s3, "s3" );;
gap> epi := GroupHomomorphismByImages( d12, s3, [c,d], [e,f] );;
gap> iX24 := InducedXModBySurjection( X24, epi );
[d24/ker->s3]
gap> Display( iX24 );               
Crossed module [d24/ker->s3] :- 
: Source group d24/ker has generators:
  [ ( 1,11, 5, 4,10, 8)( 2,12, 6, 3, 9, 7), 
  ( 1, 2)( 3, 4)( 5, 9)( 6,10)( 7,11)( 8,12) ]
: Range group s3 has generators:
  [ (13,14,15), (14,15) ]
: Boundary homomorphism maps source generators to:
  [ (13,14,15), (14,15) ]
: Action homomorphism maps range generators to automorphisms:
  (13,14,15) --> { source gens --> [ ( 1,11, 5, 4,10, 8)( 2,12, 6, 3, 9, 7), 
  ( 1, 6)( 2, 5)( 3, 8)( 4, 7)( 9,10)(11,12) ] }
  (14,15) --> { source gens --> [ ( 1, 8,10, 4, 5,11)( 2, 7, 9, 3, 6,12), 
  ( 1, 2)( 3, 4)( 5, 9)( 6,10)( 7,11)( 8,12) ] }
  These 2 automorphisms generate the group of automorphisms.
gap> morX24 := MorphismOfInducedXMod( iX24 );
[[d24->d12] => [d24/ker->s3]]

## Section 7.2.1 : Example 2 
gap> g := (16,17,18);;  h := (16,17,18,19);;
gap> s4 := Group( [ g, h ] );;
gap> SetName( s4, "s4" );;
gap> iota := GroupHomomorphismByImages( s3, s4, [e,f], [g^2*h^2,g*h^-1] );
[ (13,14,15), (14,15) ] -> [ (17,18,19), (18,19) ]
gap> iiX24 := InducedXModByCopower( iX24, iota, [ ] );
i*([d24/ker->s3])
gap> Size2d( iiX24 );               
[ 96, 24 ]
gap> StructureDescription( iiX24 );
[ "C2 x GL(2,3)", "S4" ]

## Section 7.2.1 : Example 3 
gap> alpha := CompositionMapping( iota, epi );
[ (1,2)(3,4,5), (4,5) ] -> [ (17,18,19), (18,19) ]
gap> jX24 := InducedXMod( X24, alpha );;
gap> StructureDescription( jX24 );
[ "C2 x GL(2,3)", "S4" ]

## Section 7.2.1 : Example 4 
gap> d12b := Subgroup( d24, [ a^2, b ] );;
gap> SetName( d12b, "d12b" ); 
gap> c6b := Subgroup( d12b, [ a^2 ] );; 
gap> SetName( c6b, "c6b" );  
gap> X12 := InducedXMod( d24, d12b, c6b );
i*([c6b->d12b])
gap> StructureDescription( X12 );
[ "C6 x C6", "D24" ]

## this command produces different results in stable-4.13 so removing here:
## gap> Display( MorphismOfInducedXMod( X12 ) );
## Morphism of crossed modules :- 
## : Source = [c6b->d12b] with generating sets:
##   [ ( 6, 8)( 7, 9)(10,12,11) ]
##   [ ( 6, 8)( 7, 9)(10,12,11), ( 7, 9)(11,12) ]
## :  Range = i*([c6b->d12b]) with generating sets:
##   [ ( 4, 5)( 6, 7)( 8, 9)(10,11)(12,13)(14,15), 
##   ( 4, 6, 8)( 5, 7, 9)(10,12,14)(11,13,15), 
##   ( 4,10)( 5,11)( 6,12)( 7,13)( 8,14)( 9,15), (1,2,3) ]
##   [ ( 6, 7, 8, 9)(10,11,12), ( 7, 9)(11,12) ]
## : Source Homomorphism maps source generators to:
##   [ ( 4, 9, 6, 5, 8, 7)(10,15,12,11,14,13) ]
## : Range Homomorphism maps range generators to:
##   [ ( 6, 8)( 7, 9)(10,12,11), ( 7, 9)(11,12) ]

## Section 7.2.2
gap> all := AllInducedXMods( q8 );;
gap> L := List( all, x -> Source( x ) );;
gap> Sort( L, function(g,h) return Size(g) < Size(h); end );;
gap> List( L, x -> StructureDescription( x ) );
[ "1", "1", "1", "1", "C2 x C2", "C2 x C2", "C2 x C2", "C4 x C4", "C4 x C4", 
  "C4 x C4", "C2 x C2 x C2 x C2" ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp2ind.tst", 10000 );
