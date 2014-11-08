#############################################################################
##
#W  gp2act.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 5

## Section 5.1.1
gap> X3;
[c3->s3]
gap> WGX3 := WhiteheadPermGroup( X3 );
Group([ (1,2,3)(4,5,6), (1,4)(2,6)(3,5) ])
gap> APX3 := AutomorphismPermGroup( X3 );
Group([ (5,7,6), (1,2)(3,4)(6,7) ])
gap> WX3 := WhiteheadXMod( X3 );; 
gap> Display( WX3 );

Crossed module Whitehead[c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,6,5) ]
: Range group has generators:
  [ (1,2,3)(4,5,6), (1,4)(2,6)(3,5) ]
: Boundary homomorphism maps source generators to:
  [ (1,3,2)(4,6,5) ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3)(4,5,6) --> { source gens --> [ (1,2,3)(4,6,5) ] }
  (1,4)(2,6)(3,5) --> { source gens --> [ (1,3,2)(4,5,6) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> LX3 := LueXMod( X3 );;
gap> Display( LX3 );

Crossed module Lue[c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,6,5) ]
: Range group has generators:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,7,6) --> { source gens --> [ (1,2,3)(4,6,5) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2)(4,5,6) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> NX3 := NorrieXMod( X3 );; 
gap> Display( NX3 );

Crossed module Norrie[c3->s3] :- 
: Source group has generators:
  [ (4,5,6), (2,3)(5,6) ]
: Range group has generators:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
: Action homomorphism maps range generators to automorphisms:
  (5,7,6) --> { source gens --> [ (4,5,6), (2,3)(4,5) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (4,6,5), (2,3)(5,6) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> AX3 := ActorXMod( X3 );; 
gap> Display( AX3);

Crossed module Actor[c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,5,6), (1,4)(2,6)(3,5) ]
: Range group has generators:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
: Action homomorphism maps range generators to automorphisms:
  (5,7,6) --> { source gens --> [ (1,2,3)(4,5,6), (1,5)(2,4)(3,6) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2)(4,6,5), (1,4)(2,6)(3,5) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> IAX3 := InnerActorXMod( X3 );;  
gap> Display( IAX3 );

Crossed module InnerActor[c3->s3] :- 
: Source group has generators:
  [ (1,3,2)(4,6,5) ]
: Range group has generators:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,6,7) --> { source gens --> [ (1,3,2)(4,6,5) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,2,3)(4,5,6) ] }
  These 2 automorphisms generate the group of automorphisms.


## Section 5.1.2
gap> IMX3 := InnerMorphism( X3 );; 
gap> Display( IMX3 );
Morphism of crossed modules :- 
: Source = [c3->s3] with generating sets:
  [ (1,2,3)(4,6,5) ]
  [ (4,5,6), (2,3)(5,6) ]
:  Range = Actor[c3->s3] with generating sets:
  [ (1,2,3)(4,5,6), (1,4)(2,6)(3,5) ]
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Source Homomorphism maps source generators to:
  [ (1,3,2)(4,6,5) ]
: Range Homomorphism maps range generators to:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
gap> IsInjective( IMX3 );
true
gap> ZX3 := XModCentre( X3 ); 
[Group( () )->Group( () )]
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2act.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
