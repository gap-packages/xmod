#############################################################################
##
#W  gp2obj.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 2,  Section 2.1.1
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> SetName(s4,"s4");  SetName(a4,"a4"); 
gap> X4 := XModByNormalSubgroup( s4, a4 );
[a4->s4]

## Chapter 2,  Section 2.1.2 
gap> c5 := Group( (5,6,7,8,9) );;
gap> SetName( c5, "c5" );
gap> X5 := XModByAutomorphismGroup( c5 );
[c5 -> Aut(c5)] 
gap> Display( X5 );
Crossed module [c5->Aut(c5)] :- 
: Source group c5 has generators:
  [ (5,6,7,8,9) ]
: Range group Aut(c5) has generators:
  [ GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], [ (5,7,9,6,8) ] ) ]
: Boundary homomorphism maps source generators to:
  [ IdentityMapping( c5 ) ]
: Action homomorphism maps range generators to automorphisms:
  GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], 
[ (5,7,9,6,8) ] ) --> { source gens --> [ (5,7,9,6,8) ] }
  This automorphism generates the group of automorphisms.

## Chapter 2,  Section 2.1.3
gap> c4 := Group( (1,2,3,4) );; 
gap> SetName( c4, "c4" ); 
gap> bdy54 := GroupHomomorphismByImages( c5, c4, [ (5,6,7,8,9) ], [ () ] );; 
gap> X45 := XModByTrivialAction( bdy54 );; 
gap> Display( X45 );

Crossed module [c5->c4] :- 
: Source group c5 has generators:
  [ (5,6,7,8,9) ]
: Range group c4 has generators:
  [ (1,2,3,4) ]
: Boundary homomorphism maps source generators to:
  [ () ]
  The automorphism group is trivial

## Chapter 2,  Section 2.1.4
gap> gen23 := [ (4,5,6)(7,9,8), (2,7,3,4)(5,8,9,6) ];; 
gap> sl23 := Group( gen23 );; 
gap> SetName( sl23, "sl23" ); 
gap> im23 := [ (1,2,3), (1,2)(3,4) ];;
gap> surj23 := GroupHomomorphismByImages( sl23, a4, gen23, im23 );; 
gap> X23 := XModByCentralExtension( surj23 );; 
gap> Display(X23);

Crossed module [sl23->a4] :- 
: Source group sl23 has generators:
  [ (4,5,6)(7,9,8), (2,7,3,4)(5,8,9,6) ]
: Range group has generators:
  [ (1,2,3), (2,3,4) ]
: Boundary homomorphism maps source generators to:
  [ (1,2,3), (1,2)(3,4) ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3) --> { source gens --> [ (4,5,6)(7,9,8), (2,9,3,5)(4,6,7,8) ] }
  (2,3,4) --> { source gens --> [ (2,9,4)(3,5,7), (2,8,3,6)(4,9,7,5) ] }
  These 2 automorphisms generate the group of automorphisms.

## Chapter 2,  Section 2.1.5
gap> n := 12;;
gap> cn := CyclicGroup( n );; 
gap> c := cn.1;;
gap> cnn := DirectProduct( cn, cn );; 
gap> a := cnn.1;;  b := cnn.4;; 
gap> bdy := GroupHomomorphismByImages( cnn, cn, [a,b], [c^2,c^2] );; 
gap> twist := GroupHomomorphismByImages( cnn, cnn, [a,b], [b,a] );; 
gap> act := GroupHomomorphismByImages( cn, Group( twist ), [c], [twist] );; 
gap> Xn := XModByBoundaryAndAction( bdy, act );;
gap> Display( Xn );  

Crossed module :- 
: Source group has generators:
  [ f1, f2, f3, f4, f5, f6 ]
: Range group has generators:
  [ f1, f2, f3 ]
: Boundary homomorphism maps source generators to:
  [ f2, f3, f3^2, f2, f3, f3^2 ]
: Action homomorphism maps range generators to automorphisms:
  f1 --> { source gens --> [ f4, f5, f6, f1, f2, f3 ] }
  f2 --> { source gens --> [ f1, f2, f3, f4, f5, f6 ] }
  f3 --> { source gens --> [ f1, f2, f3, f4, f5, f6 ] }
  These 3 automorphisms generate the group of automorphisms.


## Chapter 2,  Section 2.1.7
gap> DirectProductOp( [ X4, X5 ], X4 );     
[a4xc5->s4xAut(c5)]

## Chapter 2,  Section 2.1.8
gap> XMod( [ Xn, Xn ], Xn );
[Group( [ f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12 ] )->Group( 
[ f1, f2, f3, f4, f5, f6 ] )]


## Chapter 2,  Section 2.2
gap> KnownAttributesOfObject( X5 );
[ "Name", "Range", "Source", "Boundary", "XModAction", 
  "IsomorphismPerm2DimensionalGroup" ]

## Chapter 2,  Section 2.2.1
gap> Boundary( X23 );
[ (4,5,6)(7,9,8), (2,7,3,4)(5,8,9,6) ] -> [ (1,2,3), (1,2)(3,4) ]

## Chapter 2,  Section 2.2.2
gap> a := GeneratorsOfGroup( Range( X5 ) )[1]^2; 
[ (5,6,7,8,9) ] -> [ (5,9,8,7,6) ]
gap> ImageElmXModAction( X5, (5,9,8,7,6), a );
(5,6,7,8,9)

## Chapter 2,  Section 2.2.3
gap> Size( X4 );
[ 12, 24 ]
gap> Name( X4 );
"[a4->s4]"
gap> IdGroup( X4 );
[ [ 12, 3 ], [ 24, 12 ] ]
gap> StructureDescription( X4 );
[ "A4", "S4" ]

## Chapter 2,  Section 2.2.4
gap> ext := ExternalSetXMod( X5 ); 
<xset:[ (), (5,6,7,8,9), (5,7,9,6,8), (5,8,6,9,7), (5,9,8,7,6) ]>
gap> Orbits( ext );
[ [ () ], [ (5,6,7,8,9), (5,7,9,6,8), (5,9,8,7,6), (5,8,6,9,7) ] ]


## Section 2.3
gap> KnownPropertiesOfObject( X4 );
[ "IsEmpty", "IsTrivial", "IsNonTrivial", "IsFinite", 
  "CanEasilyCompareElements", "CanEasilySortElements", "IsDuplicateFree", 
  "IsGeneratorsOfSemigroup", "IsPreXModDomain", "IsPerm2DimensionalGroup", 
  "IsPreXMod", "IsXMod", "IsNormalSubgroup2DimensionalGroup" ]
gap> RepresentationsOfObject( X4 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPreXModObj" ]

## Section 2.3.2
gap> IsPerm2DimensionalGroup( X4 );
true
gap> IsPc2DimensionalGroup( Xn );  
true
gap> IsFp2DimensionalGroup( Xn );
false

## Section 2.3.3
gap> IsPerm2DimensionalGroup( X4 );
true
gap> IsPc2DimensionalGroup( Xn );  
true
gap> IsFp2DimensionalGroup( Xn );
false


## Section 2.4.1 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );; 
gap> k4 := Subgroup( d8, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> SetName( d8, "d8" );  SetName( k4, "k4" ); 
gap> Y4 := SubXMod( X4, k4, d8 ); 
[k4->d8]
gap> TrivialSubXMod( X5 ); 
[Group( () )->Group( IdentityMapping( c5 ) )]


## Section 2.4.2
gap> Z4 := SubXMod( X4, k4, a4 ); 
[k4->a4]
gap> [ IsNormal( X4, Y4 ), IsNormal( X4, Z4 ) ];  
[ false, true ]
gap> NX4 := NormalSubXMods( X4 );;
gap> List( NX4, X -> StructureDescription(X) );
[ [ "1", "1" ], [ "A4", "A4" ], [ "C2 x C2", "A4" ], [ "C2 x C2", "C2 x C2" ],
  [ "A4", "S4" ] ]

## Section 2.5.1
gap> b1 := (11,12,13,14,15,16,17,18);;  b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> sk4 := Subgroup( d16, [ b1^4, b2 ] );;
gap> SetName( d16, "d16" );  SetName( sk4, "sk4" );
gap> bdy16 := GroupHomomorphismByImages( d16, sk4, [b1,b2], [b1^4*b2,b2] );;
gap> aut1 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1^5,b2] );;
gap> aut2 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1,b1^4*b2] );;
gap> aut16 := Group( [ aut1, aut2 ] );;
gap> act16 := GroupHomomorphismByImages( sk4, aut16, [b1^4,b2], [aut1,aut2] );;
gap> P16 := PreXModByBoundaryAndAction( bdy16, act16 );
[d16->sk4]
gap> IsXMod( P16 ); 
false
gap> S16 := SubPreXMod( P16, sk4, sk4 ); 
[sk4->sk4]

## Section 2.5.2
gap> P := PeifferSubgroup( P16 );
Group([ (11,15)(12,16)(13,17)(14,18), (11,13,15,17)(12,14,16,18) ])
gap> PeifferSubgroup( S16 );
Group([ (11,15)(12,16)(13,17)(14,18) ])
gap> X16 := XModByPeifferQuotient( P16 );
Peiffer([d16->sk4])
gap> Display( X16 );

Crossed module Peiffer([d16->sk4]) :- 
: Source group has generators:
  [ f1, f2 ]
: Range group has generators:
  [ (11,15)(12,16)(13,17)(14,18), (12,18)(13,17)(14,16) ]
: Boundary homomorphism maps source generators to:
  [ (12,18)(13,17)(14,16), (11,15)(12,14)(16,18) ]
  The automorphism group is trivial

gap> StructureDescription(X16);
[ "C2 x C2", "C2 x C2" ]

## Section 2.6.1
gap> g18gens := [ (1,2,3), (4,5,6), (2,3)(5,6) ];;     
gap> s3agens := [ (7,8,9), (8,9) ];;                
gap> g18 := Group( g18gens );;  SetName( g18, "g18" ); 
gap> s3a := Group( s3agens );;  SetName( s3a, "s3a" );
gap> t1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(),(8,9)]);     
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (), (8,9) ]
gap> h1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(7,8,9),(8,9)]);
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (7,8,9), (8,9) ]
gap> e1 := GroupHomomorphismByImages(s3a,g18,s3agens,[(1,2,3),(2,3)(5,6)]);   
[ (7,8,9), (8,9) ] -> [ (1,2,3), (2,3)(5,6) ]
gap> C18 := Cat1Group( t1, h1, e1 );
[g18=>s3a]

## Section 2.7.1
gap> Source( C18 );
g18
gap> Range( C18 );
s3a
gap> TailMap( C18 );
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (), (8,9) ]
gap> HeadMap( C18 );
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (7,8,9), (8,9) ]
gap> RangeEmbedding( C18 );
[ (7,8,9), (8,9) ] -> [ (1,2,3), (2,3)(5,6) ]
gap> Kernel( C18 );
Group([ (4,5,6) ])
gap> KernelEmbedding( C18 );
[ (4,5,6) ] -> [ (4,5,6) ]
gap> Name( C18 );
"[g18=>s3a]"
gap> Size( C18 );
[ 18, 6 ]
gap> StructureDescription( C18 );
[ "(C3 x C3) : C2", "S3" ]


## Section 2.8.1
gap> G2 := SmallGroup( 288, 956 );  SetName( G2, "G2" );
<pc group of size 288 with 7 generators>
gap> d12 := DihedralGroup( 12 );  SetName( d12, "d12" );
<pc group of size 12 with 3 generators>
gap> a1 := d12.1;;  a2 := d12.2;;  a3 := d12.3;;  a0 := One( d12 );;
gap> gensG2 := GeneratorsOfGroup( G2 );;
gap> t2 := GroupHomomorphismByImages( G2, d12, gensG2,
>           [ a0, a1*a3, a2*a3, a0, a0, a3, a0 ] );;
gap> h2 := GroupHomomorphismByImages( G2, d12, gensG2,
>           [ a1*a2*a3, a0, a0, a2*a3, a0, a0, a3^2 ] );;                   
gap> e2 := GroupHomomorphismByImages( d12, G2, [a1,a2,a3],
>        [ G2.1*G2.2*G2.4*G2.6^2, G2.3*G2.4*G2.6^2*G2.7, G2.6*G2.7^2 ] );;
gap> C2 := PreCat1GroupByTailHeadEmbedding( t2, h2, e2 );
[G2=>d12]
gap> IsCat1Group( C2 );
true
gap> Display(C2);

Cat1-group [G2=>d12] :- 
: Source group G2 has generators:
  [ f1, f2, f3, f4, f5, f6, f7 ]
: Range group d12 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., f1*f3, f2*f3, <identity> of ..., <identity> of ..., 
  f3, <identity> of ... ]
: head homomorphism maps source generators to:
  [ f1*f2*f3, <identity> of ..., <identity> of ..., f2*f3, <identity> of ..., 
  <identity> of ..., f3^2 ]
: range embedding maps range generators to:
  [ f1*f2*f4*f6^2, f3*f4*f6^2*f7, f6*f7^2 ]
: kernel has generators:
  [ f1, f4, f5, f7 ]
: boundary homomorphism maps generators of kernel to:
  [ f1*f2*f3, f2*f3, <identity> of ..., f3^2 ]
: kernel embedding maps generators of kernel to:
  [ f1, f4, f5, f7 ]

gap> KnownPropertiesOfObject( C2 );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsDuplicateFree", 
  "IsGeneratorsOfSemigroup", "IsPreCat1Domain", "IsPc2DimensionalGroup", 
  "IsPreCat1Group", "IsCat1Group", "IsEndomorphismPreCat1Group" ]

## Section 2.8.2
gap> IsEndomorphismPreCat1Group( C18 );
false
gap> X18 := EndomorphismPreCat1Group( C18 );
[g18=>Group( [ (1,2,3), (), (2,3)(5,6) ] )]


## Section 2.9.1
gap> SubCat1Group( C18, Subgroup( g18, [ (1,2,3), (4,5,6) ] ), 
>        Subgroup( s3a, [ (7,8,9) ] ) ); 
[Group( [ (1,2,3), (4,5,6) ] )=>Group( [ (7,8,9) ] )]

## Section 2.9.2
gap> C64 := DiagonalCat1Group( [(1,2,3,4), (3,4)] );
[Group( [ (1,2,3,4), (3,4), (5,6,7,8), (7,8) ] )=>Group( 
[ (1,2,3,4)(5,6,7,8), (3,4)(7,8) ] )]
gap> Display( C64 );

Cat1-group :- 
: Source group has generators:
  [ (1,2,3,4), (3,4), (5,6,7,8), (7,8) ]
: Range group has generators:
  [ (1,2,3,4)(5,6,7,8), (3,4)(7,8) ]
: tail homomorphism maps source generators to:
  [ (1,2,3,4)(5,6,7,8), (3,4)(7,8), (), () ]
: head homomorphism maps source generators to:
  [ (), (), (1,2,3,4)(5,6,7,8), (3,4)(7,8) ]
: range embedding maps range generators to:
  [ (1,2,3,4)(5,6,7,8), (3,4)(7,8) ]
: kernel has generators:
  [ (5,6,7,8), (7,8) ]
: boundary homomorphism maps generators of kernel to:
  [ (1,2,3,4)(5,6,7,8), (3,4)(7,8) ]
: kernel embedding maps generators of kernel to:
  [ (5,6,7,8), (7,8) ]

## Section 2.10.1
gap> X2 := XModOfCat1Group( C2 );;
gap> Display( X2 );

Crossed module xmod([G2=>d12]) :- 
: Source group has generators:
  [ f1, f4, f5, f7 ]
: Range group d12 has generators:
  [ f1, f2, f3 ]
: Boundary homomorphism maps source generators to:
  [ f1*f2*f3, f2*f3, <identity> of ..., f3^2 ]
: Action homomorphism maps range generators to automorphisms:
  f1 --> { source gens --> [ f1*f5, f4*f5, f5, f7^2 ] }
  f2 --> { source gens --> [ f1*f5*f7^2, f4, f5, f7 ] }
  f3 --> { source gens --> [ f1*f7, f4, f5, f7 ] }
  These 3 automorphisms generate the group of automorphisms.
: associated cat1-group is [G2=>d12]

gap> StructureDescription( X2 );
[ "D24", "D12" ]

## Section 2.11.1
gap> L18 := Cat1Select( 18 ); 
Usage:  Cat1Select( size, gpnum, num );
[ "D18", "C18", "C3 x S3", "(C3 x C3) : C2", "C6 x C3" ]
gap> L18_4 := Cat1Select( 18, 4 ); 
Usage:  Cat1Select( size, gpnum, num );
There are 4 cat1-structures for the group (C3 x C3) : C2.
Using small generating set [ f1, f2, f2*f3 ] for source of homs.
[ [range gens], [tail genimages], [head genimages] ] :-
(1)  [ [ f1 ], [ f1, <identity> of ..., <identity> of ... ], 
  [ f1, <identity> of ..., <identity> of ... ] ]
(2)  [ [ f1, f3 ], [ f1, <identity> of ..., f3 ], 
  [ f1, <identity> of ..., f3 ] ]
(3)  [ [ f1, f3 ], [ f1, <identity> of ..., f3 ], 
  [ f1, f3^2, <identity> of ... ] ]
(4)  [ [ f1, f2, f2*f3 ],  tail = head = identity mapping ]
4
gap> C18a := Cat1Select( 18, 4, 3 );
[(C3 x C3) : C2=>Group( [ f1, <identity> of ..., f3 ] )]
gap> iso18a := IsomorphismPermObject( C18a );;
gap> PC18a := Image( iso18a ); 
[Group( [ (2,3)(5,6), (4,5,6), (1,2,3) ] )=>Group( [ (2,3)(5,6), (), (1,2,3) 
 ] )]
gap> X18a := XModOfCat1Group( PC18a ); 
[Group( [ (4,5,6) ] )->Group( [ (2,3)(5,6), (), (1,2,3) ] )]

## Section 2.11.2 
gap> gp := SmallGroup( 102, 2 ); 
<pc group of size 102 with 3 generators>
gap> StructureDescription( gp ); 
"C3 x D34"
gap> all := AllCat1DataGroupsBasic( gp );
#I Edit last line of .../xmod/lib/nn.kk.out to end with ] ] ] ] ]
[ [Group( [ f1, f2, f3 ] )=>Group( [ f1, <identity> of ..., <identity> of ... 
     ] )], [Group( [ f1, f2, f3 ] )=>Group( [ f1, f2, <identity> of ... ] )], 
  [Group( [ f1, f2, f3 ] )=>Group( [ f1, <identity> of ..., f3 ] )], 
  [Group( [ f1, f2, f3 ] )=>Group( [ f1, f2, f3 ] )] ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp2obj.tst", 10000 );

#############################################################################
##
#E  gp2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
