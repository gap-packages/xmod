##############################################################################
##
#W  induced.tst                 GAP4 package `XMod'              Chris Wensley
##  
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 

gap> START_TEST( "XMod package: induced.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> 
gap> c4 := Group( (5,6,7,8) );; 
gap> SetName( c4, "c4" );
gap> AX4 := XModByAutomorphismGroup( c4 );; 
gap> isoX4 := IsomorphismPerm2DimensionalGroup( AX4 );; 
gap> X4 := Range( isoX4 );; 
gap> Display( X4 ); 

Crossed module [c4->PAut(c4)] :- 
: Source group c4 has generators:
  [ (5,6,7,8) ]
: Range group PAut(c4) has generators:
  [ (1,2) ]
: Boundary homomorphism maps source generators to:
  [ () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2) --> { source gens --> [ (5,8,7,6) ] }
  This automorphism generates the group of automorphisms.

gap> c6 := Group( (11,12,13,14,15,16) );; 
gap> SetName( c6, "c6" ); 
gap> c2 := Range( X4 );; 
gap> SetName( c2, "c2" );
gap> iota := GroupHomomorphismByImages( c2, c6, 
>                [ (1,2) ], [ (11,14)(12,15)(13,16) ] );; 
gap> indc4c2c6 := InclusionInducedXModByCopower( X4, iota, [ ] );; 
gap> StructureDescription( indc4c2c6 ); 
[ "C4 x C4 x C4", "C6" ]
gap> Size( indc4c2c6 );
[ 64, 6 ]
gap> Display( indc4c2c6 );

Crossed module i*([c4->PAut(c4)]) :- 
: Source group has generators:
  [ ( 1, 2, 3, 4), ( 5, 6, 7, 8), ( 9,10,11,12) ]
: Range group c6 has generators:
  [ (11,12,13,14,15,16) ]
: Boundary homomorphism maps source generators to:
  [ (), (), () ]
: Action homomorphism maps range generators to automorphisms:
  (11,12,13,14,15,16) --> { source gens --> [ ( 9,10,11,12), ( 1, 4, 3, 2), 
  ( 5, 6, 7, 8) ] }
  This automorphism generates the group of automorphisms.

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );; 
gap> AX8 := XModByAutomorphismGroup( q8 );; 
gap> s4a := Range( AX8 );; 
gap> gens4b := [ (11,12), (12,13), (13,14) ];; 
gap> s4b := Group( gens4b );; 
gap> SetName( s4b, "s4b" ); 
gap> iso8 := IsomorphismGroups( s4a, s4b );; 
gap> s3b := Group( (5,6), (6,7) );; 
gap> SetName( s3b, "s3b" ); 
gap> surj4 := GroupHomomorphismByImages( s4b, s3b, gens4b, 
>                 [ (5,6), (6,7), (5,6) ] );; 
gap> iota8 := iso8 * surj4;; 
gap> ind8 := SurjectiveInducedXMod( AX8, iota8 );; 
gap> StructureDescription( ind8 ); 
[ "C2 x C2", "S3" ]
gap> Size( ind8 );
[ 4, 6 ]
gap> n := 5;; 
gap> c2n := CyclicGroup( 2*n );; 
gap> SetName( c2n, "c2n" );
gap> X2n := XModByNormalSubgroup( c2n, c2n );; 
gap> g := GeneratorsOfGroup( c2n )[1];; 
gap> cn := Subgroup( c2n, [g^2] );; 
gap> SetName( cn, "cn" ); 
gap> surj := GroupHomomorphismByImages( c2n, cn, [g], [g^2] );; 
gap> ind1 := SurjectiveInducedXMod( X2n, surj );; 
gap> ok := IsCentralExtension2DimensionalGroup( ind1 );
true
gap> StructureDescription( ind1 );
[ "C10", "C5" ]
gap> inc := GroupHomomorphismByImages( cn, c2n, [g^2], [g^2] );; 
gap> ind2 := InclusionInducedXModByCopower( ind1, inc, [ ] );; 
gap> StructureDescription( ind2 );
[ "C10 x C10", "C10" ]
gap> Display( ind2 );

Crossed module i*([c2n/ker->cn]) :- 
: Source group has generators:
  [ ( 1, 2)( 5, 6, 7, 8, 9), ( 3, 4)(10,11,12,13,14) ]
: Range group c2n has generators:
  [ f1, f2 ]
: Boundary homomorphism maps source generators to:
  [ f2, f2 ]
: Action homomorphism maps range generators to automorphisms:
  f1 --> { source gens --> [ ( 3, 4)(10,11,12,13,14), ( 1, 2)( 5, 6, 7, 8, 9) 
 ] }
  f2 --> { source gens --> [ ( 1, 2)( 5, 6, 7, 8, 9), ( 3, 4)(10,11,12,13,14) 
 ] }
  These 2 automorphisms generate the group of automorphisms.

gap> c5 := Group( (1,2,3,4,5) );; 
gap> SetName( c5, "c5" );
gap> c4 := Group( (4,5,6,7) );; 
gap> SetName( c4, "c4" ); 
gap> bdy4 := GroupHomomorphismByImages( c5, c4, [ (1,2,3,4,5) ], [ () ] );; 
gap> X4 := XModByTrivialAction( bdy4 );; 
gap> Display( X4 ); 

Crossed module [c5->c4] :- 
: Source group c5 has generators:
  [ (1,2,3,4,5) ]
: Range group c4 has generators:
  [ (4,5,6,7) ]
: Boundary homomorphism maps source generators to:
  [ () ]
  The automorphism group is trivial

gap> c2 := Group( (8,9) );; 
gap> SetName( c2, "c2" ); 
gap> surj := GroupHomomorphismByImages( c4, c2, [ (4,5,6,7) ], [ (8,9) ] );; 
gap> K := Kernel( surj );; 
gap> SetName( K, "ker(surj)" ); 
gap> D := DisplacementGroup( X4, K, c5 );; 
gap> SetName( D, "D" ); 
gap> Size( D ) = 1; 
true
gap> ind2 := InducedXMod( X4, surj );; 
gap> Display( ind2 );

Crossed module i*([c5->c4]) :- 
: Source group c5/ker has generators:
  [ (1,5,4,3,2) ]
: Range group c2 has generators:
  [ (8,9) ]
: Boundary homomorphism maps source generators to:
  [ () ]
  The automorphism group is trivial

gap> c := (1,2,3,4,5,6);; 
gap> c6 := Group( c );; 
gap> SetName( c6, "c6" );
gap> monoc6 := GroupHomomorphismByImages( c2, c6, [(8,9)], [(1,4)(2,5)(3,6)] );; 
gap> indc6 := InducedXMod( ind2, monoc6 );; 
gap> Display( indc6 ); 

Crossed module i*(i*([c5->c4])) :- 
: Source group has generators:
  [ ( 1, 2, 3, 4, 5), ( 6, 7, 8, 9,10), (11,12,13,14,15) ]
: Range group c6 has generators:
  [ (1,2,3,4,5,6) ]
: Boundary homomorphism maps source generators to:
  [ (), (), () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3,4,5,6) --> { source gens --> [ (11,12,13,14,15), ( 1, 2, 3, 4, 5), 
  ( 6, 7, 8, 9,10) ] }
  This automorphism generates the group of automorphisms.

gap> s3 := Group( (11,12),(12,13) );; 
gap> SetName( s3, "s3" ); 
gap> monos3 := GroupHomomorphismByImages( c2, s3, [ (8,9) ], [ (11,13) ] );; 
gap> inds3 := InducedXMod( ind2, monos3 );; 
gap> Display( inds3 ); 

Crossed module i*(i*([c5->c4])) :- 
: Source group has generators:
  [ ( 1, 2, 3, 4, 5), ( 6, 7, 8, 9,10), (11,12,13,14,15) ]
: Range group s3 has generators:
  [ (11,12), (12,13) ]
: Boundary homomorphism maps source generators to:
  [ (), (), () ]
: Action homomorphism maps range generators to automorphisms:
  (11,12) --> { source gens --> [ ( 6, 7, 8, 9,10), ( 1, 2, 3, 4, 5), 
  (11,12,13,14,15) ] }
  (12,13) --> { source gens --> [ (11,12,13,14,15), ( 6, 7, 8, 9,10), 
  ( 1, 2, 3, 4, 5) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> c3 := Subgroup( c6, [ c^2 ] );; 
gap> SetName( c3, "c3" ); 
gap> surj6 := GroupHomomorphismByImages( c6, c3, [c], [c^2] );;  
gap> X6 := XModByCentralExtension( surj6 ); 
[c6->c3]
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> SetName( a4, "a4" ); 
gap> monoa4 := GroupHomomorphismByImages( c3, a4, [c^2], [(2,3,4)] );;  
gap> ind4 := InducedXMod( X6, monoa4, [ ] );; 
gap> Size( ind4 );
[ 48, 12 ]
gap> StructureDescription( ind4 );
[ "C2 x SL(2,3)", "A4" ]
gap> triv := Group( () );; 
gap> SetName( triv, "triv" );
gap> zero := GroupHomomorphismByImages( c6, triv, [c], [()] );; 
gap> X0 := XModByTrivialAction( zero );; 
gap> mono0 := GroupHomomorphismByImages( triv, s3, [()], [()] );; 
gap> ind0 := InducedXModFromTrivialRange( X0, mono0 );; 
gap> Display( ind0 );

Crossed module [c6^6->s3] :- 
: Source group c6^6 has generators:
  [ (1,2,3,4,5,6), ( 7, 8, 9,10,11,12), (13,14,15,16,17,18), 
  (19,20,21,22,23,24), (25,26,27,28,29,30), (31,32,33,34,35,36) ]
: Range group s3 has generators:
  [ (11,12), (12,13) ]
: Boundary homomorphism maps source generators to:
  [ (), (), (), (), (), () ]
: Action homomorphism maps range generators to automorphisms:
  (11,12) --> { source gens --> [ (13,14,15,16,17,18), (19,20,21,22,23,24), 
  ( 1, 2, 3, 4, 5, 6), ( 7, 8, 9,10,11,12), (31,32,33,34,35,36), 
  (25,26,27,28,29,30) ] }
  (12,13) --> { source gens --> [ ( 7, 8, 9,10,11,12), ( 1, 2, 3, 4, 5, 6), 
  (25,26,27,28,29,30), (31,32,33,34,35,36), (13,14,15,16,17,18), 
  (19,20,21,22,23,24) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "induced.tst", 10000 );

##############################################################################
##
#E  induced.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
