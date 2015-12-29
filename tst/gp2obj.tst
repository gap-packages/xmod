#############################################################################
##
#W  gp2obj.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.45, 29/12/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

## Chapter 2,  Section 2.1.3
gap> c5 := Group( (5,6,7,8,9) );;
gap> SetName( c5, "c5" );;
gap> X1 := XModByAutomorphismGroup( c5 );
[c5->PAut(c5)]
gap> Display(X1);

Crossed module [c5->PAut(c5)] :- 
: Source group c5 has generators:
  [ (5,6,7,8,9) ]
: Range group PAut(c5) has generators:
  [ (1,2,4,3) ]
: Boundary homomorphism maps source generators to:
  [ () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,4,3) --> { source gens --> [ (5,7,9,6,8) ] }
  This automorphism generates the group of automorphisms.

gap> Size( X1 );
[ 5, 4 ]
gap> IdGroup( X1 ); 
[ [ 5, 1 ], [ 4, 1 ] ]
gap> ext := ExternalSetXMod( X1 ); 
<xset:[ (), (5,6,7,8,9), (5,7,9,6,8), (5,8,6,9,7), (5,9,8,7,6) ]>
gap> Orbits( ext );
[ [ () ], [ (5,6,7,8,9), (5,7,9,6,8), (5,9,8,7,6), (5,8,6,9,7) ] ]
gap> Print( RepresentationsOfObject(X1), "\n" );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPreXModObj" ]
gap> Print( KnownAttributesOfObject(X1), "\n" );
[ "Name", "Size", "Range", "Source", "IdGroup", "Boundary", "AutoGroup", 
  "XModAction", "ExternalSetXMod" ]

## Section 2.1.4
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> SetName(s4,"s4");  SetName(a4,"a4");  SetName(k4,"k4"); 
gap> X4 := XModByNormalSubgroup( s4, a4 );
[a4->s4]
gap> Y4 := SubXMod( X4, k4, a4 ); 
[k4->a4]
gap> IsNormal( X4, Y4 ); 
true
gap> NX4 := NormalSubXMods( X4 );;
gap> Length( NX4 ); 
5

## Section 2.2.1
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

## Section 2.2.2
gap> P := PeifferSubgroup( P16 );
Group([ (11,15)(12,16)(13,17)(14,18), (11,13,15,17)(12,14,16,18) ])
gap> X16 := XModByPeifferQuotient( P16 );
[d16/P->sk4]
gap> Display( X16 );

Crossed module [d16/P->sk4] :- 
: Source group has generators:
  [ f1, f2 ]
: Range group has generators:
  [ (11,15)(12,16)(13,17)(14,18), (12,18)(13,17)(14,16) ]
: Boundary homomorphism maps source generators to:
  [ (12,18)(13,17)(14,16), (11,15)(12,14)(16,18) ]
  The automorphism group is trivial

gap> iso16 := IsomorphismPermGroup( Source( X16 ) );;
gap> S16 := Image( iso16 );
Group([ (1,2), (3,4) ])

## Section 2.3.2
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
>        [ G2.1*G2.2*G2.4*G2.6^2, G2.3*G2.4*G2.6^2*G2.7, G2.6*G2.7^2 ] );
[ f1, f2, f3 ] -> [ f1*f2*f4*f6^2, f3*f4*f6^2*f7, f6*f7^2 ]
gap> C2 := PreCat1ByTailHeadEmbedding( t2, h2, e2 );
[G2=>d12]
gap> IsCat1( C2 );
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

## Section 2.3.3
gap> X2 := XModOfCat1( C2 );;
gap> Display( X2 );

Crossed module X([G2=>d12]) :- 
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

gap> StructureDescription(X2);
[ "D24", "D12" ]

## Section 2.4.1
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
gap> C18 := Cat1Select( 18, 4, 3 );
[(C3 x C3) : C2=>Group( [ f1, <identity> of ..., f3 ] )]
gap> Display( C18 );

Cat1-group :- 
: Source group (C3 x C3) : C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ f1, <identity> of ..., f3 ]
: tail homomorphism maps source generators to:
  [ f1, <identity> of ..., f3 ]
: head homomorphism maps source generators to:
  [ f1, f3^2, f3 ]
: range embedding maps range generators to:
  [ f1, <identity> of ..., f3 ]
: kernel has generators:
  [ f2 ]
: boundary homomorphism maps generators of kernel to:
  [ f3^2 ]
: kernel embedding maps generators of kernel to:
  [ f2 ]
: associated crossed module is [Group( [ f2 ] )->Group( 
[ f1, <identity> of ..., f3 ] )]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
