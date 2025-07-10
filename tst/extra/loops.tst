##############################################################################
##
#W  loops.tst                   GAP4 package `XMod'              Chris Wensley
##  
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 

gap> START_TEST( "XMod package: loops.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 1 );;

gap> M0 := SmallGroup( 16, 4 );;
gap> a := M0.1;;  b := M0.2;;  c := M0.3;;  d := M0.4;; 
gap> [ a^2=d, b^2=c, b^a=b^-1 ]; 
[ true, true, true ]
gap> SetName( M0, "c4|Xc4" ); 

gap> X0 := XModByAutomorphismGroup( M0 );; 
gap> Display( X0 );
Crossed module [c4|Xc4->Aut(c4|Xc4)] :- 
: Source group c4|Xc4 has generators:
  [ f1, f2, f3, f4 ]
: Range group Aut(c4|Xc4) has generators:
  [ Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f4, f3, f4 ] ]
: Boundary homomorphism maps source generators to:
  [ Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2, f3, f4 ] ]
: Action homomorphism maps range generators to automorphisms:
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2, f3, f4 ] --> { source gens --> 
[ f1*f2, f2, f3, f4 ] }
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2, f3, f4 ] --> { source gens --> 
[ f1*f3, f2, f3, f4 ] }
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3, f3, f4 ] --> { source gens --> 
[ f1, f2*f3, f3, f4 ] }
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2, f3, f4 ] --> { source gens --> 
[ f1*f4, f2, f3, f4 ] }
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f4, f3, f4 ] --> { source gens --> 
[ f1, f2*f4, f3, f4 ] }
  These 5 automorphisms generate the group of automorphisms.

gap> IdGroup( X0 );
[ [ 16, 4 ], [ 32, 27 ] ]
gap> bdy0 := Boundary( X0 );; 
gap> idM0 := IdentityMapping( M0 );;
gap> AM0 := Range( X0 );
Aut(c4|Xc4)
gap> SetName( AM0, "Aut(c4|Xc4)" );
gap> GeneratorsOfGroup( AM0 );
[ Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2, f3, f4 ], 
  Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f4, f3, f4 ] ]
gap> isoAM0 := IsomorphismPcGroup( AM0 );; 
gap> PAM0 := Range( isoAM0 );;
gap> SetName( PAM0, Concatenation( "P", Name(AM0) ) );
gap> iso := IsomorphismByIsomorphisms( X0, [idM0, isoAM0] );
[[c4|Xc4->Aut(c4|Xc4)] => [c4|Xc4->PAut(c4|Xc4)]]
gap> PX0 := Range( iso );                                   
[c4|Xc4->PAut(c4|Xc4)]
gap> P0 := Range( PX0 );;
gap> genP0 := GeneratorsOfGroup( P0 );; 
gap> g1 := genP0[1];;  g2 := genP0[2];;  g3 := genP0[3];; 
gap> g4 := genP0[4];;  g5 := genP0[5];; 
gap> act := XModAction( PX0 );; 
gap> igenP0 := List( genP0, g -> ImageElm( act, g ) );; 
gap> ima := List( igenP0, g -> ImageElm( g, a ) ); 
[ f1, f1, f1*f2*f4, f1*f2, f1*f3 ]
gap> imc := List( igenP0, g -> ImageElm( g, c ) ); 
[ f3, f3, f3, f3, f3 ]
gap> for p in P0 do 
>        Print( "-------------------------------------------------\n" );;
>        ip := Image( act, p );; 
>        Print( [p,ip], "\n" ); 
>        if ( ( ImageElm(ip,a) = a*b^2 ) and ( ImageElm(ip,b) = b ) ) then 
>            Print( "alpha = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a ) and ( ImageElm(ip,b) = b^-1 ) ) then 
>            Print( "beta = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a^-1 ) and ( ImageElm(ip,b) = b ) ) then 
>            Print( "gamma = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a ) and ( ImageElm(ip,b) = a^2*b ) ) then 
>            Print( "delta = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a*b ) and ( ImageElm(ip,b) = b ) ) then 
>            Print( "tau = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a ) and ( ImageElm(ip,b) = b^a ) ) then 
>            Print( "conjugation by a = ", p, "\n" );; 
>        fi;;
>        if ( ( ImageElm(ip,a) = a^b ) and ( ImageElm(ip,b) = b ) ) then 
>            Print( "conjugation by b = ", p, "\n" );; 
>        fi;;
>    od; 
-------------------------------------------------
[ <identity> of ..., IdentityMapping( c4|Xc4 ) ]
-------------------------------------------------
[ f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2, f3, f4 ] ]
alpha = f5
conjugation by b = f5
-------------------------------------------------
[ f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2, f3, f4 ] ]
tau = f4
-------------------------------------------------
[ f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3, f2, f3, f4 ] ]
-------------------------------------------------
[ f3, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f4, f2, f3, f4 ] ]
-------------------------------------------------
[ f3*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3*f4, f2, f3, f4 ] ]
-------------------------------------------------
[ f3*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3*f4, f2, f3, f4 ] ]
-------------------------------------------------
[ f3*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2, f3, f4 ] ]
gamma = f3*f4*f5
-------------------------------------------------
[ f2, Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3, f3, f4 ] ]
beta = f2
conjugation by a = f2
-------------------------------------------------
[ f2*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f3, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f4, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f3*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3*f4, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f3*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3*f4, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f2*f3*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2*f3, f3, f4 ] ]
-------------------------------------------------
[ f1, Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f4, f3, f4 ] ]
delta = f1
-------------------------------------------------
[ f1*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f3, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f4, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f3*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3*f4, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f3*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3*f4, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f3*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2, Pcgs([ f1, f2, f3, f4 ]) -> [ f1, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f3, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f4, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f3*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f2*f3*f4, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f3*f4, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f3*f4, f2*f3*f4, f3, f4 ] ]
-------------------------------------------------
[ f1*f2*f3*f4*f5, Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2*f3*f4, f3, f4 ] ]

gap> alpha := ImageElm( act, g5 ); 
[ f1*f3, f2, f3, f4 ] -> [ f1, f2, f3, f4 ]
gap> beta := ImageElm( act, g2 ); 
[ f1, f2*f3, f3, f4 ] -> [ f1, f2, f3, f4 ]
gap> gamma := ImageElm( act, g3*g4*g5 ); 
Pcgs([ f1, f2, f3, f4 ]) -> [ f1*f4, f2, f3, f4 ]
gap> delta := ImageElm( act, g1 ); 
[ f1, f2*f4, f3, f4 ] -> [ f1, f2, f3, f4 ]
gap> tau := ImageElm( act, g4 ); 
[ f1*f2*f3, f2, f3, f4 ] -> Pcgs([ f1, f2, f3, f4 ])

gap> all := AllLoopsXMod( PX0 ); 
#I  LoopsXMod with a = <identity> of ...,  IdGroup = [ [ 16, 4 ], [ 128, 2163 \
] ]
#I  LoopsXMod with a = f1,  IdGroup = [ [ 16, 4 ], [ 64, 267 ] ]
#I  LoopsXMod with a = f3,  IdGroup = [ [ 16, 4 ], [ 64, 261 ] ]
#I  LoopsXMod with a = f1*f3,  IdGroup = [ [ 16, 4 ], [ 64, 193 ] ]
#I  LoopsXMod with a = f3*f4,  IdGroup = [ [ 16, 4 ], [ 128, 2163 ] ]
[ [c4|Xc4->Group( [ f9, f8, f5, f4, f3, f2, f1 ] )], 
  [c4|Xc4->Group( [ f9, f8, f5, f3*f4, f2, f1 ] )], 
  [c4|Xc4->Group( [ f9, f8, f5, f4, f3, f2*f7 ] )], 
  [c4|Xc4->Group( [ f9, f8, f5, f3*f4, f2*f7, f1*f4 ] )], 
  [c4|Xc4->Group( [ f9, f8, f5, f4, f3, f2, f1 ] )] ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "loops.tst", 10000 );

##############################################################################
##
#E  loops.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
