#############################################################################
##
#W  isoclinic.tst               XMOD test file                   Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 16/09/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> G := SmallGroup(8,5);
<pc group of size 8 with 3 generators>
gap> N := NormalSubgroups(G);
[ <pc group of size 8 with 3 generators>, Group([ f1, f3 ]), Group([ f1, f2*f3 ]), 
  Group([ f1, f2 ]), Group([ f2, f3 ]), Group([ f1*f2, f3 ]), Group([ f1*f3, f2 ]), 
  Group([ f1*f2, f2*f3 ]), Group([ f3 ]), Group([ f1*f3 ]), Group([ f2*f3 ]), 
  Group([ f1*f2*f3 ]), Group([ f2 ]), Group([ f1*f2 ]), Group([ f1 ]), Group([  ]) ]
gap> T := N[3];
Group([ f1, f2*f3 ])
gap> X1 := XMod(G,T);
[Group( [ f1, f2*f3 ] )->Group( [ f1, f2, f3 ] )]
gap> Size(X1);
[ 4, 8 ]
gap> TG := PreXModFixedPointSubgroup(X1);
Group([ f1, f2*f3 ])
gap> IsSubgroup(T,TG);
true
gap> stGT := PreXModStabilizer(X1);
<pc group of size 8 with 3 generators>
gap> IsSubgroup(G,stGT);
true
gap> C_X1 := CentreXMod(X1);
[Group( [ f1, f2*f3 ] )->Group( [ f1, f2, f3 ] )]
gap> G30 := SmallGroup(30,2);
<pc group of size 30 with 3 generators>
gap> X30 := XModByAutomorphismGroup( G30 ); 
[Group( [ f1, f2, f3 ] )->Group( [ f1, f2, f3, f4 ] )]
gap> Size(X30);
[ 30, 40 ]
gap> norm := NormalSubgroups( G30 );;
gap> N2 := norm[2];  N3 := norm[3];             
Group([ f1, f3 ])
Group([ f2, f3 ])
gap> bdy := Boundary( X30 ); 
[ f1, f2, f3 ] -> [ f3, <identity> of ..., f4^2 ]
gap> R2 := Image( bdy, N2 );  R3 := Image( bdy, N3 ); 
Group([ f3, f4^2 ])
Group([ <identity> of ..., f4^2 ])
gap> X2 := SubXMod( X30, N2, R2 );
[Group( [ f1, f3 ] )->Group( [ f3, f4^2 ] )]
gap> X3 := SubXMod( X30, N3, R3 );
[Group( [ f2, f3 ] )->Group( [ <identity> of ..., f4^2 ] )]
gap> X5 := IntersectionSubXMod( X30, X2, X3 );
[Group( [ f3, f3^2, f3^3, f3^4 ] )->Group( [ <identity> of ..., f4^2 ] )]
gap> Size(last);
[ 5, 5 ]
gap> IsAsphericalXMod(X1);
true
gap> IsSimplyConnectedXMod(X1);
true
gap> IsFaithfulXMod(X1);
false
gap> DGT := DisplacementSubgroup(X1);
Group([  ])
gap> IsSubgroup(T,DGT);
true
gap> DX1 := DerivedSubXMod(X1);
[Group( <identity> of ... )->Group( <identity> of ... )]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
