#############################################################################
##
#W  gp2act.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2024, Chris Wensley, et al
##
gap> START_TEST( "XMod package: gp2act.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

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
gap> m6 := PermAutomorphismAs2dGroupMorphism( X3, e6 );;
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

gap> APC3 := AutomorphismPermGroup( C3 );
Group([ (1,3,2)(4,6,5)(7,9,8)(12,13,14), (2,3)(4,7)(5,9)(6,8)(10,11)(13,14) ])
gap> IdGroup( APC3 );
[ 6, 1 ]
gap> a := GeneratorsOfGroup( APC3 )[1];;
gap> m := PermAutomorphismAs2dGroupMorphism( C3, a );
[[g18 => s3] => [g18 => s3]]

## Section 6.1.2
gap> X3;
[c3->s3]
gap> WGX3 := WhiteheadPermGroup( X3 );
Group([ (1,2,3), (1,2) ])
gap> APX3 := AutomorphismPermGroup( X3 );
Group([ (5,7,6), (1,2)(3,4)(6,7) ])
gap> WX3 := WhiteheadXMod( X3 );; 
gap> Display( WX3 );
Crossed module Whitehead[c3->s3] :- 
: Source group has generators:
  [ (1,2,3)(4,6,5) ]
: Range group has generators:
  [ (1,2,3), (1,2) ]
: Boundary homomorphism maps source generators to:
  [ (1,2,3) ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3) --> { source gens --> [ (1,2,3)(4,6,5) ] }
  (1,2) --> { source gens --> [ (1,3,2)(4,5,6) ] }
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
  [ (1,2,3), (1,2) ]
: Range group has generators:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Action homomorphism maps range generators to automorphisms:
  (5,7,6) --> { source gens --> [ (1,2,3), (2,3) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2), (1,2) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );;
gap> SetName( q8, "q8" );
gap> XAq8 := XModByAutomorphismGroup( q8 );; 
gap> StructureDescription( WhiteheadXMod( XAq8 ) ); 
[ "Q8", "C2 x C2 x C2" ]
gap> StructureDescription( LueXMod( XAq8 ) );      
[ "Q8", "S4" ]
gap> StructureDescription( NorrieXMod( XAq8 ) );
[ "S4", "S4" ]
gap> StructureDescription( ActorXMod( XAq8 ) ); 
[ "C2 x C2 x C2", "S4" ]

## Section 6.1.3
gap> IMX3 := InnerMorphism( X3 );; 
gap> Display( IMX3 );
Morphism of crossed modules :- 
: Source = [c3->s3] with generating sets:
  [ (1,2,3)(4,6,5) ]
  [ (4,5,6), (2,3)(5,6) ]
:  Range = Actor[c3->s3] with generating sets:
  [ (1,2,3), (1,2) ]
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: Source Homomorphism maps source generators to:
  [ (1,2,3) ]
: Range Homomorphism maps range generators to:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
gap> IsInjective( IMX3 );
true
gap> ZX3 := XModCentre( X3 ); 
[Group( () )->Group( () )]
gap> IAX3 := InnerActorXMod( X3 );;  
gap> Display( IAX3 );
Crossed module InnerActor[c3->s3] :- 
: Source group has generators:
  [ (1,2,3) ]
: Range group has generators:
  [ (5,6,7), (1,2)(3,4)(6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,6,7) --> { source gens --> [ (1,2,3) ] }
  (1,2)(3,4)(6,7) --> { source gens --> [ (1,3,2) ] }
  These 2 automorphisms generate the group of automorphisms.

## Section 6.2.1
gap> C3;
[g18 => s3]
gap> AC3 := ActorCat1Group( C3 );
cat1(Actor[c3->s3])
gap> Display( AC3 );             
Cat1-group cat1(Actor[c3->s3]) :- 
: Source group has generators:
  [ ( 9,10), ( 8, 9,10), ( 5, 7, 6)( 8, 9,10), (1,2)(3,4)(6,7)(8,9) ]
: Range group has generators:
  [ (5,7,6), (1,2)(3,4)(6,7) ]
: tail homomorphism maps source generators to:
  [ (), (), (5,7,6), (1,2)(3,4)(6,7) ]
: head homomorphism maps source generators to:
  [ (1,2)(3,4)(5,6), (5,7,6), (5,7,6), (1,2)(3,4)(6,7) ]
: range embedding maps range generators to:
  [ ( 5, 7, 6)( 8, 9,10), (1,2)(3,4)(6,7)(8,9) ]
: kernel has generators:
  [ ( 9,10), ( 8, 9,10) ]
: boundary homomorphism maps generators of kernel to:
  [ (1,2)(3,4)(5,6), (5,7,6) ]
: kernel embedding maps generators of kernel to:
  [ ( 9,10), ( 8, 9,10) ]
: associated crossed module is Actor[c3->s3]
gap> StructureDescription( AC3 );
[ "S3 x S3", "S3" ]
gap> IAC3 := InnerActorCat1Group( C3 );
cat1(InnerActor[c3->s3])
gap> StructureDescription( IAC3 );
[ "(C3 x C3) : C2", "S3" ]


## Section 6.2.2
gap> c4q := Subgroup( q8, [ (1,2,3,4)(5,8,7,6) ] );;
gap> SetName( c4q, "c4q" );                         
gap> Xc4q := XModByNormalSubgroup( q8, c4q );;      
gap> AXc4q := Actor( Xc4q );
Actor[c4q->q8]
gap> StructureDescription( AXc4q );
[ "D8", "D8" ]
gap> IAXc4q := InnerActor( Xc4q );
InnerActor[c4q->q8]
gap> StructureDescription( IAXc4q );
[ "C2", "C2 x C2" ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp2act.tst", 10000 );
