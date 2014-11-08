#############################################################################
##
#W  gp2obj.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
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
  [ (1,2,3,4) ]
: Boundary homomorphism maps source generators to:
  [ () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3,4) --> { source gens --> [ (5,7,9,6,8) ] }
  This automorphism generates the group of automorphisms.

gap> Size(X1);
[ 5, 4 ]
gap> Print( RepresentationsOfObject(X1), "\n" );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPreXModObj" ]
gap> Print( KnownAttributesOfObject(X1), "\n" );
[ "Name", "Size", "Range", "Source", "Boundary", "AutoGroup", "XModAction" ]

## Section 2.2.1
gap> c := (11,12,13,14,15,16,17,18);;  d := (12,18)(13,17)(14,16);;
gap> d16 := Group( c, d );;
gap> sk4 := Subgroup( d16, [ c^4, d ] );;
gap> SetName( d16, "d16" );  SetName( sk4, "sk4" );
gap> bdy16 := GroupHomomorphismByImages( d16, sk4, [c,d], [c^4*d,d] );;
gap> h1 := GroupHomomorphismByImages( d16, d16, [c,d], [c^5,d] );;
gap> h2 := GroupHomomorphismByImages( d16, d16, [c,d], [c,c^4*d] );;
gap> aut16 := Group( [ h1, h2 ] );;
gap> act16 := GroupHomomorphismByImages( sk4, aut16, [c^4,d], [h1,h2] );;
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
gap> s3 := SymmetricGroup(IsPcGroup,3);;   SetName(s3,"s3");
gap> gens3 := GeneratorsOfGroup(s3);;
gap> pc4 := CyclicGroup(4);;  SetName( pc4, "pc4" );
gap> s3c4 := DirectProduct( s3, pc4 );;  SetName( s3c4, "s3c4" );  
gap> gens3c4 := GeneratorsOfGroup( s3c4 );;
gap> a := gens3[1];;  b := gens3[2];;  one := One(s3);;
gap> t2 := GroupHomomorphismByImages( s3c4, s3, gens3c4, [a,b,one,one] );
[ f1, f2, f3, f4 ] -> [ f1, f2, <identity> of ..., <identity> of ... ]
gap> e2 := Embedding( s3c4, 1 );
Pcgs([ f1, f2 ]) -> [ f1, f2 ]
gap> C2 := Cat1( t2, t2, e2 ); 
[s3c4=>s3]
gap> Display( C2 );

Cat1-group [s3c4=>s3] :- 
: Source group s3c4 has generators:
  [ f1, f2, f3, f4 ]
: Range group s3 has generators:
  [ f1, f2 ]
: tail homomorphism maps source generators to:
  [ f1, f2, <identity> of ..., <identity> of ... ]
: head homomorphism maps source generators to:
  [ f1, f2, <identity> of ..., <identity> of ... ]
: range embedding maps range generators to:
  [ f1, f2 ]
: kernel has generators:
  [ f3, f4 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ..., <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f3, f4 ]

gap> IsPcCat1(C2);
true
gap> Size(C2);
[ 24, 6 ]

## Section 2.3.3
gap> SetName( Kernel( t2 ), "ker(t2)" );
gap> X2 := XModOfCat1( C2 );;
gap> Display( X2 );

Crossed module X([s3c4=>s3]) :- 
: Source group has generators:
  [ f3, f4 ]
: Range group s3 has generators:
  [ f1, f2 ]
: Boundary homomorphism maps source generators to:
  [ <identity> of ..., <identity> of ... ]
  The automorphism group is trivial
: associated cat1-group is [s3c4=>s3]


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

Cat1-group [(C3 x C3) : C2=>..] :- 
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
