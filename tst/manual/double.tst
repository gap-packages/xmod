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


gap> sq1 := SquareOfArrows( D1, g*h, a1, d1, e1, b1 ); 
[-6] ---- (7,8) ---> [-5]
  |                         |
(7,9)    (11,16)(12,15)(13,14)    (8,9)
  V                         V
[-1] ---- (7,8,9) ---> [-3]
gap> ElementOfSquare( sq1 ); 
(11,16)(12,15)(13,14)
gap> UpArrow( sq1 );
[(7,8) : -6 -> -5]
gap> LeftArrow( sq1 );
[(7,9) : -6 -> -1]
gap> RightArrow( sq1 );
[(8,9) : -5 -> -3]
gap> DownArrow( sq1 );
[(7,8,9) : -1 -> -3]

gap> sq2 := SquareOfArrows( D1, g^2, a2, e1, f1, b2 );;  
gap> ElementOfSquare( sq2 ); 
(11,13,15)(12,14,16)
gap> LeftArrow( sq2 ) = RightArrow( sq1 ); 
true
gap> sq3 := SquareOfArrows( D1, g, b1, d2, e2, c1 );;
gap> UpArrow( sq3 ) = DownArrow( sq1 ); 
true
gap> sq4 := SquareOfArrows( D1, h, b2, e2, f2, c2 );;
gap> UpArrow(sq4)=DownArrow(sq2) and LeftArrow(sq4)=RightArrow(sq3); 
true

gap> sq12 := LeftRightProduct( D1, sq1, sq2 );
[-6] ---- (7,9,8) ---> [-4]
  |                         |
(7,9)    (11,12)(13,16)(14,15)    (7,8)
  V                         V
[-1] ---- (7,8) ---> [-4]

gap> sq34 := LeftRightProduct( D1, sq3, sq4 );
[-1] ---- (7,8) ---> [-4]
  |                         |
(8,9)    (11,12)(13,16)(14,15)    (8,9)
  V                         V
[-2] ---- (7,9,8) ---> [-3]

gap> sq13 := UpDownProduct( D1, sq1, sq3 ); 
[-6] ---- (7,8) ---> [-5]
  |                         |
(7,8,9)    (11,13)(14,16)    (7,9)
  V                         V
[-2] ---- (7,9) ---> [-2]

gap> sq24 := UpDownProduct( D1, sq2, sq4 ); 
[-5] ---- (8,9) ---> [-4]
  |                         |
(7,9)    (11,15)(12,14)    (7,9,8)
  V                         V
[-2] ---- (7,8) ---> [-3]

gap> sq1324 := LeftRightProduct( D1, sq13, sq24 );
[-6] ---- (7,9,8) ---> [-4]
  |                         |
(7,8,9)    (11,15,13)(12,16,14)    (7,9,8)
  V                         V
[-2] ---- (7,9,8) ---> [-3]

gap> sq1234 := UpDownProduct( D1, sq12, sq34 );;
gap> sq1324 = sq1234;
true


## Subsection 11.2.1 
gap> g := (1,2,3,4);;  h := (1,3);;
gap> gend8 := [ g, h ];;
gap> d8 := Group( gend8 );;
gap> SetName( d8, "d8" ); 
gap> Gd8 := Groupoid( d8, [-9..-7] );;
gap> SetName( Gd8, "Gd8" ); 
gap> B0 := SinglePieceBasicDoubleGroupoid( Gd8 );; 
gap> B0!.groupoid;
Gd8
gap> B0!.objects;
[ -9 .. -7 ]
gap> a0 := Arrow(Gd8,(),-9,-7);;         b0 := Arrow(Gd8,(2,4),-9,-8);;  
gap> d0 := Arrow(Gd8,g,-9,-9);;          e0 := Arrow(Gd8,(1,3),-7,-8);;      
gap> bdy0 := b0![1]^-1 * d0![1]^-1 * a0![1] * e0![1];; 
gap> bsq0 := SquareOfArrows( B0, bdy0, a0, d0, e0, b0 ); 
[-9] ---- () ---> [-7]
  |                         |
(1,2,3,4)    (1,4,3,2)    (1,3)
  V                         V
[-9] ---- (2,4) ---> [-8]

gap> D0 := EnhancedBasicDoubleGroupoid( B0 );;
gap> D0!.prexmod;
[d8->d8]
gap> bsq0 = SquareOfArrows( D0, bdy0, a0, d0, e0, b0 ); 
true

## Subsection 11.3.1 
gap> D16 := DoubleGroupoidWithZeroBoundary( Gs3, d16 );;
gap> D16!.prexmod;
[d16->Group( [ () ] )]
gap> e16 := Arrow( Gs3, (7,9,8), -5, -3 );;
gap> sq16 := SquareOfArrows( D16, (), a1, d1, e16, b1 );
[-6] ---- (7,8) ---> [-5]
  |                         |
(7,9)    ()    (7,9,8)
  V                         V
[-1] ---- (7,8,9) ---> [-3]

gap> #
gap> SetInfoLevel( InfoXMod, xmod_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
