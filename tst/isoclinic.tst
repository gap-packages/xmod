#############################################################################
##
#W  isoclinic.tst               XMOD test file                   Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 17/09/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> D24 := DihedralGroup(24);
<pc group of size 24 with 4 generators>
gap> SetName( D24, "D24" );
gap> norm := NormalSubgroups( D24 );;
gap> List( norm, n->Size(n) );
[ 24, 12, 12, 12, 6, 4, 2, 3, 1 ]
gap> N5 := norm[5];;
gap> X5 := XModByNormalSubgroup( D24, N5 );
[Group( [ f3, f4 ] )->D24]
gap> Size( X5 );
[ 6, 24 ]
gap> fix := PreXModFixedPointSubgroup( X5 );
<pc group of size 2 with 1 generators>
gap> stab := PreXModStabilizer( X5 );
<pc group of size 12 with 1 generators>
gap> DisplacementSubgroup( X5 );
<pc group of size 3 with 1 generators>
gap> DX5 := DerivedSubXMod( X5 );  
[Group( [ f4 ] )->Group( <identity> of ... )]

gap> X24 := XModByAutomorphismGroup( D24 ); 
[D24->PAut(D24)]
gap> Size(X24);
[ 24, 48 ]
gap> N3 := norm[3];  N4 := norm[4];             
Group([ f1*f2*f3*f4^2, f3, f4 ])
Group([ f1, f3, f4 ])
gap> bdy := Boundary( X24 ); 
[ f1, f2, f3, f4 ] -> [ f1*f3, f2*f5, f2^2, f2 ]
gap> R3 := Image( bdy, N3 );  R4 := Image( bdy, N4 ); 
Group([ f1*f2^2*f3*f5, f2^2, f2 ])
Group([ f1*f3, f2^2, f2 ])
gap> X3 := SubXMod( X24, N3, R3 );
[Group( [ f1*f2*f3*f4^2, f3, f4 ] )->Group( [ f1*f2^2*f3*f5, f2^2, f2 ] )]
gap> X4 := SubXMod( X24, N4, R4 );
[Group( [ f1, f3, f4 ] )->Group( [ f1*f3, f2^2, f2 ] )]
gap> X6 := IntersectionSubXMod( X24, X3, X4 );
[Group( [ f3, f4, f3*f4, f4^2, f3*f4^2 ] )->Group( [ f2, f2^2 ] )]
gap> Size( X6 );
[ 6, 3 ]
gap> Y6 := CommutatorSubXMod( X24, X3, X4 );
[Group( [ f3 ] )->Group( [ f2 ] )]
gap> X6 = Y6;
true

gap> CentreXMod( X5 );      
[Group( [ f3*f4 ] )->Group( [ f3*f4 ] )]

gap> [ IsAsphericalXMod(X5), IsAsphericalXMod(X24) ];
[ true, false ]
gap> [ IsSimplyConnectedXMod(X5), IsSimplyConnectedXMod(X24) ];
[ true, true ]
gap> [ IsFaithfulXMod(X5), IsFaithfulXMod(X24) ];              
[ false, true ]

gap> ## DX24 := DerivedSubXMod( X24 ); fails! 

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
