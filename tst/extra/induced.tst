############################################################################
##
#W  induced.tst                 GAP4 package `XMod'            Chris Wensley
##  
#Y  Copyright (C) 2001-2023, Chris Wensley et al, 

gap> START_TEST( "XMod package: induced.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> 
gap> c4a := Group( (5,6,7,8) );; 
gap> SetName( c4a, "c4a" );
gap> AX4 := XModByAutomorphismGroup( c4a );; 
gap> isoX4 := IsomorphismPerm2DimensionalGroup( AX4 );; 
gap> X4 := Range( isoX4 );; 
gap> Display( X4 ); 

Crossed module [c4a->PAut(c4a)] :- 
: Source group c4a has generators:
  [ (5,6,7,8) ]
: Range group PAut(c4a) has generators:
  [ (1,2) ]
: Boundary homomorphism maps source generators to:
  [ () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2) --> { source gens --> [ (5,8,7,6) ] }
  This automorphism generates the group of automorphisms.

gap> c2 := Range( X4 );; 
gap> SetName( c2, "c2" );
gap> c2dash := Group( (3,4) );;
gap> dash := GroupHomomorphismByImages( c2, c2dash, [(1,2)], [(3,4)] );;
gap> X4dash := InducedXMod( X4, dash );
i*([c4a->PAut(c4a)])

gap> c6 := Group( (11,12,13,14,15,16) );; 
gap> SetName( c6, "c6" ); 
gap> iota := GroupHomomorphismByImages( c2, c6, 
>                [ (1,2) ], [ (11,14)(12,15)(13,16) ] );; 
gap> indc4c2c6 := InducedXModByCopower( X4, iota, [ ] );; 
gap> StructureDescription( indc4c2c6 ); 
[ "C4 x C4 x C4", "C6" ]
gap> Size2d( indc4c2c6 );
[ 64, 6 ]
gap> indsrc := Source( indc4c2c6 );; 
gap> gensrc := GeneratorsOfGroup( indsrc );; 
gap> genc4c4c4 := [ (1,2,3,4), (5,6,7,8), (9,10,11,12) ];; 
gap> c4c4c4 := Group( genc4c4c4 );; 
gap> isosrc := GroupHomomorphismByImages( indsrc, c4c4c4, gensrc, genc4c4c4 );; 
gap> idindrng := IdentityMapping( Range( indc4c2c6 ) );; 
gap> isoind := IsomorphismByIsomorphisms( indc4c2c6, [ isosrc, idindrng ] );; 
gap> indc4c2c6i := Range( isoind );;
gap> SetName( indc4c2c6i, Name( indc4c2c6 ) );
gap> Display( indc4c2c6i );

Crossed module i*([c4a->PAut(c4a)]) :- 
: Source group has generators:
  [ (1,2,3,4), (5,6,7,8), ( 9,10,11,12) ]
: Range group c6 has generators:
  [ (11,12,13,14,15,16) ]
: Boundary homomorphism maps source generators to:
  [ (), (), () ]
: Action homomorphism maps range generators to automorphisms:
  (11,12,13,14,15,16) --> { source gens --> 
[ ( 9,10,11,12), (1,4,3,2), (5,6,7,8) ] }
  This automorphism generates the group of automorphisms.

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );; 
gap> XAq8 := XModByAutomorphismGroup( q8 );; 
gap> s4a := Range( XAq8 );; 
gap> gens4b := [ (11,12), (12,13), (13,14) ];; 
gap> s4b := Group( gens4b );; 
gap> SetName( s4b, "s4b" ); 
gap> iso8 := IsomorphismGroups( s4a, s4b );; 
gap> s3b := Group( (5,6), (6,7) );; 
gap> SetName( s3b, "s3b" ); 
gap> surj4 := GroupHomomorphismByImages( s4b, s3b, gens4b, 
>                 [ (5,6), (6,7), (5,6) ] );; 
gap> iota8 := iso8 * surj4;; 
gap> ind8 := InducedXModBySurjection( XAq8, iota8 );; 
gap> StructureDescription( ind8 ); 
[ "C2 x C2", "S3" ]
gap> Size2d( ind8 );
[ 4, 6 ]
gap> n := 5;; 
gap> c2n := CyclicGroup( 2*n );; 
gap> SetName( c2n, "c2n" );
gap> X2n := XModByNormalSubgroup( c2n, c2n );; 
gap> g := GeneratorsOfGroup( c2n )[1];; 
gap> cn := Subgroup( c2n, [g^2] );; 
gap> SetName( cn, "cn" ); 
gap> surj := GroupHomomorphismByImages( c2n, cn, [g], [g^2] );; 
gap> ind1 := InducedXModBySurjection( X2n, surj );; 
gap> ok := IsCentralExtension2DimensionalGroup( ind1 );
true
gap> StructureDescription( ind1 );
[ "C10", "C5" ]
gap> inc := GroupHomomorphismByImages( cn, c2n, [g^2], [g^2] );; 
gap> ind2 := InducedXModByCopower( ind1, inc, [ ] );; 
gap> StructureDescription( ind2 );
[ "C10 x C10", "C10" ]
gap> indsrc2 := Source( ind2 );; 
gap> gensrc2 := GeneratorsOfGroup( indsrc2 );; 
gap> genc10c10 := [ (1,2)(5,6,7,8,9), (3,4)(10,11,12,13,14) ];; 
gap> c10c10 := Group( genc10c10 );; 
gap> isosrc2 := GroupHomomorphismByImages( indsrc2,c10c10,gensrc2,genc10c10 );; 
gap> idindrng2 := IdentityMapping( Range( ind2 ) );; 
gap> isoind2 := IsomorphismByIsomorphisms( ind2, [ isosrc2, idindrng2 ] );; 
gap> ind2i := Range( isoind2 );;
gap> SetName( ind2i, Name( ind2 ) );
gap> Display( ind2i );

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
gap> c4b := Group( (4,5,6,7) );; 
gap> SetName( c4b, "c4b" ); 
gap> bdy54 := GroupHomomorphismByImages( c5, c4b, [ (1,2,3,4,5) ], [ () ] );; 
gap> X54 := XModByTrivialAction( bdy54 );; 
gap> Display( X54 ); 

Crossed module [c5->c4b] :- 
: Source group c5 has generators:
  [ (1,2,3,4,5) ]
: Range group c4b has generators:
  [ (4,5,6,7) ]
: Boundary homomorphism maps source generators to:
  [ () ]
  The automorphism group is trivial

gap> c2 := Group( (8,9) );; 
gap> SetName( c2, "c2" ); 
gap> surj := GroupHomomorphismByImages( c4b, c2, [ (4,5,6,7) ], [ (8,9) ] );; 
gap> K := Kernel( surj );; 
gap> SetName( K, "ker(surj)" ); 
gap> D := DisplacementGroup( X54, K, c5 );; 
gap> SetName( D, "D" ); 
gap> Size( D ) = 1; 
true
gap> ind2 := InducedXMod( X54, surj );; 
gap> Display( ind2 );

Crossed module i*([c5->c4b]) :- 
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
gap> StructureDescription( indc6 );
[ "C5 x C5 x C5", "C6" ]
gap> indsrc6 := Source( indc6 );; 
gap> gensrc6 := GeneratorsOfGroup( indsrc6 );; 
gap> genc5c5c5 := [ (1,2,3,4,5), (6,7,8,9,10), (11,12,13,14,15) ];; 
gap> c5c5c5 := Group( genc5c5c5 );; 
gap> isosrc6 := GroupHomomorphismByImages( indsrc6,c5c5c5,gensrc6,genc5c5c5 );; 
gap> idindrng6 := IdentityMapping( Range( indc6 ) );; 
gap> isoind6 := IsomorphismByIsomorphisms( indc6, [ isosrc6, idindrng6 ] );; 
gap> indc6i := Range( isoind6 );;
gap> SetName( indc6i, Name( indc6 ) );
gap> Display( indc6i ); 

Crossed module i*(i*([c5->c4b])) :- 
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
gap> StructureDescription( inds3 );
[ "C5 x C5 x C5", "S3" ]
gap> indsrc3 := Source( inds3 );; 
gap> gensrc3 := GeneratorsOfGroup( indsrc3 );; 
gap> isosrc3 := GroupHomomorphismByImages( indsrc3,c5c5c5,gensrc3,genc5c5c5 );; 
gap> idindrng3 := IdentityMapping( Range( inds3 ) );; 
gap> isoind3 := IsomorphismByIsomorphisms( inds3, [ isosrc3, idindrng3 ] );; 
gap> inds3i := Range( isoind3 );;
gap> SetName( inds3i, Name( inds3 ) );
gap> Display( inds3i ); 

Crossed module i*(i*([c5->c4b])) :- 
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
gap> Size2d( ind4 );
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

gap> ## this example used to be in tst/gp2ind.tst 
gap> b1 := (11,12,13,14,15,16,17,18);; 
gap> b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> SetName( d16, "d16" ); 
gap> d8 := Subgroup( d16, [ b1^2, b2 ] );  SetName( d8, "d8" ); 
Group([ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ])
gap> c4c := Subgroup( d8, [ b1^2 ] );  SetName( c4c, "c4c" ); 
Group([ (11,13,15,17)(12,14,16,18) ])
gap> Y16 := XModByNormalSubgroup( d16, d8 );                   
[d8->d16]
gap> Y8 := SubXMod( Y16, c4c, d8 );            
[c4c->d8]
gap> inc8 := InclusionMorphism2DimensionalDomains( Y16, Y8 ); 
[[c4c->d8] => [d8->d16]]
gap> incd8 := RangeHom( inc8 );;
gap> indY8 := InducedXMod( Y8, incd8 );
i*([c4c->d8])
gap> StructureDescription( indY8 );
[ "C4 x C4", "D16" ]
gap> morY8 := MorphismOfInducedXMod( indY8 );
[[c4c->d8] => i*([c4c->d8])]
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> s3c := Subgroup( s4, [ (2,3), (3,4) ] );;  
gap> SetName( s3c, "s3c" );
gap> indXs3c := InducedXMod( s4, s3c, s3c );
i*([s3c->s3c])
gap> StructureDescription( indXs3c );
[ "GL(2,3)", "S4" ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "induced.tst", 10000 );

#############################################################################
##
#E  induced.tst . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
