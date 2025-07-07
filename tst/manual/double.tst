##############################################################################
##
#W  double.tst               XMod Package                 Chris Wensley
##
#Y  Copyright (C) 2023, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "xmod package: double.tst" );
gap> xmod_infolevel_saved := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );; 

##  make this test independent of gp2obj.tst
gap> d16 := Group( (11,12,13,14,15,16,17,18), (12,18)(13,17)(14,16) );; 
gap> SetName( d16, "d16" ); 

## SubSection 11.1.1 
gap> g := (11,12,13,14,15,16);;  h := (12,16)(13,15);;
gap> gend12 := [ g, h ];;
gap> d12 := Group( gend12 );;
gap> SetName( d12, "d12" ); 
gap> gens3 := [ (7,8,9), (8,9) ];;
gap> s3 := Group( gens3 );;
gap> SetName( s3, "s3" ); 
gap> pr12 := GroupHomomorphismByImages( d12, s3, gend12, gens3 );;
gap> X12 := XModByCentralExtension( pr12 );; 
gap> SetName( X12, "X12" ); 
gap> Display( X12 ); 

Crossed module X12 :- 
: Source group d12 has generators:
  [ (11,12,13,14,15,16), (12,16)(13,15) ]
: Range group s3 has generators:
  [ (7,8,9), (8,9) ]
: Boundary homomorphism maps source generators to:
  [ (7,8,9), (8,9) ]
: Action homomorphism maps range generators to automorphisms:
  (7,8,9) --> { source gens --> [ (11,12,13,14,15,16), (11,13)(14,16) ] }
  (8,9) --> { source gens --> [ (11,16,15,14,13,12), (12,16)(13,15) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> Gs3 := Groupoid( s3, [-6..-1] );;
gap> SetName( Gs3, "Gs3" ); 
gap> D1 := SinglePieceDoubleGroupoid( Gs3, X12 );; 
gap> D1!.groupoid;
Gs3
gap> D1!.prexmod;
X12
gap> a1 := Arrow(Gs3,(7,8),-6,-5);;    a2 := Arrow(Gs3,(8,9),-5,-4);;
gap> b1 := Arrow(Gs3,(7,8,9),-1,-3);;  b2 := Arrow(Gs3,(7,9),-3,-4 );; 
gap> c1 := Arrow(Gs3,(7,9),-2,-2);;    c2 := Arrow(Gs3,(7,8),-2,-3);; 
gap> d1 := Arrow(Gs3,(7,9),-6,-1);;    d2 := Arrow(Gs3,(8,9),-1,-2);; 
gap> e1 := Arrow(Gs3,(8,9),-5,-3);;    e2 := Arrow(Gs3,(7,9,8),-3,-2);; 
gap> f1 := Arrow(Gs3,(7,8),-4,-4);;    f2 := Arrow(Gs3,(8,9),-4,-3);; 

gap> ## the rest of this test temporarily deleted
gap> ## while waiting for version 1.77 of groupoids to be accepted

gap> #
gap> SetInfoLevel( InfoXMod, xmod_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
