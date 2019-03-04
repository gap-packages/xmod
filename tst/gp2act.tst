#############################################################################
##
#W  gp2act.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2019, Chris Wensley, et al
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp2act.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## make independent of gp2up.tst 
gap> g18 := Group( (1,2,3), (4,5,6), (2,3)(5,6) );;
gap> SetName( g18, "g18" );
gap> gen18 := GeneratorsOfGroup( g18 );;
gap> g1 := gen18[1];;  g2 := gen18[2];;  g3 := gen18[3];;
gap> s3 := Subgroup( g18, gen18{[2..3]} );;
gap> SetName( s3, "s3" );
gap> t := GroupHomomorphismByImages( g18, s3, gen18, [g2,g2,g3] );;
gap> h := GroupHomomorphismByImages( g18, s3, gen18, [(),g2,g3] );;
gap> e := GroupHomomorphismByImages( s3, g18, [g2,g3], [g2,g3] );;
gap> C3 := Cat1Group( t, h, e );;
gap> SetName( Kernel(t), "c3" );;
gap> X3 := XModOfCat1Group( C3 );;


## Chapter 6

## Section 6.1.1
gap> X3;
[c3->s3]
gap> APX3 := AutomorphismPermGroup( X3 );
Group([ (5,7,6), (1,2)(3,4)(6,7) ])
gap> Size( APX3 );
6
gap> genX3 := GeneratingAutomorphisms( X3 );    
[ [[c3->s3] => [c3->s3]], [[c3->s3] => [c3->s3]] ]
gap> e6 := Elements( APX3 )[6];
(1,2)(3,4)(5,7)
gap> m6 := PermAutomorphismAsXModMorphism( X3, e6 );;
gap> Display( m6 );
Morphism of crossed modules :- 
: Source = [c3->s3] with generating sets:
  [ (1,2,3)(4,6,5) ]
  [ (4,5,6), (2,3)(5,6) ]
: Range = Source
: Source Homomorphism maps source generators to:
  [ (1,3,2)(4,5,6) ]
: Range Homomorphism maps range generators to:
  [ (4,6,5), (2,3)(4,5) ]

## Section 6.1.2
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
  [ (1,2,3)(4,5,6) ]
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
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Action homomorphism maps range generators to automorphisms:
  (5,7,6) --> { source gens --> [ (1,2,3)(4,5,6), (1,6)(2,5)(3,4) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2)(4,6,5), (1,4)(2,6)(3,5) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> IAX3 := InnerActorXMod( X3 );;  
gap> Display( IAX3 );

Crossed module InnerActor[c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,5,6) ]
: Range group has generators:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,6,7) --> { source gens --> [ (1,2,3)(4,5,6) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2)(4,6,5) ] }
  These 2 automorphisms generate the group of automorphisms.


## Section 6.1.3
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
  [ (1,2,3)(4,5,6) ]
: Range Homomorphism maps range generators to:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
gap> IsInjective( IMX3 );
true
gap> ZX3 := XModCentre( X3 ); 
[Group( () )->Group( () )]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp2act.tst", 10000 );

#############################################################################
##
#E  gp2act.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
