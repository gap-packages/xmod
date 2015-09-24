#############################################################################
##
#W  isoclinic.tst               XMOD test file                   Alper Odabas
#W                                                               & Enver Uslu
##  version 2.44, 23/09/2015 
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
Group([ f4^2, f4 ])
gap> DX5 := DerivedSubXMod( X5 );  
[Group( [ f4^2, f4 ] )->Group( [ f3, f4 ] )]

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
[Group( [ f3, f4^2, f4, f3*f4^2 ] )->Group( [ f2 ] )]
gap> Size(Y6);
[ 6, 3 ]
gap> X6 = Y6;
false

gap> nsx := NormalSubXMods( X24 );; 
gap> Length( nsx );  Size( nsx[30] );
40
[ 4, 8 ]
gap> Q := FactorXMod( X24, nsx[30] ); 
[Group( [ f1, f2, f2^2, f2 ] )->Group( 
[ f1, f2, <identity> of ..., <identity> of ..., <identity> of ... ] )]
gap> Size( Q );
[ 6, 6 ]

gap> CentreXMod( X5 );      
[Group( [ f3*f4 ] )->Group( [ f3*f4 ] )]

gap> LX1 := LowerCentralSeriesOfXMod( X1 );
[ [c5->PAut(c5)], [Group( [ (5,6,7,8,9) ] )->Group( () )] ]
gap> List( LX1, x -> Size(x) );            
[ [ 5, 4 ], [ 5, 1 ] ]
gap> IsNilpotent2dGroup( X1 );             
false
gap> NilpotencyClass2dGroup( X1 );         
0

## Note that the following example (taken from the nq manual) FAILS 
## gap> F := FreeGroup(2);
## <free group on the generators [ f1, f2 ]>
## gap> G := F/[F.1^2,F.2^2];
## <fp group on the generators [ f1, f2 ]>
## gap> H := NilpotentQuotient( G, 4 );
## Pcp-group with orders [ 2, 2, 2, 2, 2 ]
## gap> SetName(H,"H");
## gap> XH := XModByNormalSubgroup(H,H);
## [H->H]
## gap> LowerCentralSeriesOfXMod(XH);
## Error, cannot compute this
##  called from
## MappedVector( ExponentsByIgs( gens, a ), imgs ) called from
## CoKernelOfMultiplicativeGeneralMapping( map ) called from
## IsMapping( hom ) called from
## GroupHomomorphismByImages( H, G, genH, genH ) called from
## InclusionMappingGroups( Psrc, Ssrc ) called from
## ...  at line 35 of *stdin*
## you can 'quit;' to quit to outer loop, or
## you can 'return;' to continue 


gap> [ IsAbelian2dGroup(X5), IsAbelian2dGroup(X24) ];
[ false, false ]
gap> [ IsAspherical2dGroup(X5), IsAspherical2dGroup(X24) ];
[ true, false ]
gap> [ IsSimplyConnected2dGroup(X5), IsSimplyConnected2dGroup(X24) ];
[ true, true ]
gap> [ IsFaithful2dGroup(X5), IsFaithful2dGroup(X24) ];              
[ false, true ]

gap> DX24 := DerivedSubXMod( X24 ); 
[Group( [ f4, f4^2, f3*f4, f2*f4^2 ] )->Group( [ f2, f5 ] )]

gap> G := SmallGroup( 64, 6 );
<pc group of size 64 with 6 generators>
gap> Q := CentralQuotient( G );
Group([ f1, f2, f3, <identity> of ..., <identity> of ..., <identity> of ... ])
gap> H := SmallGroup( 32, 41 );
<pc group of size 32 with 5 generators>
gap> iso := Isoclinism( G, H );
[ [ f1, f2, f3, <identity> of ..., <identity> of ..., <identity> of ... ] -> 
    [ f1, f2*f3, f3, <identity> of ..., <identity> of ..., <identity> of ... ]
    , [ f3, f5 ] -> [ f4*f5, f5 ] ]
gap> K := SmallGroup(32,43);
<pc group of size 32 with 5 generators>
gap> AreIsoclinicGroups(G,K);
false

gap> CommutatorSubgroup( G, G );
Group([ f3, f5 ])
gap> IsStemGroup( G );
false
gap> IsoclinicStemGroups( G );
[ [ 16, 7 ], [ 16, 8 ], [ 16, 9 ] ]
gap> IsoclinicStemGroups( SmallGroup(32,6) );
[ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ] ]
gap> AllStemGroupIds( 32 );
[ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ], [ 32, 18 ], [ 32, 19 ], [ 32, 20 ], 
  [ 32, 27 ], [ 32, 28 ], [ 32, 29 ], [ 32, 30 ], [ 32, 31 ], [ 32, 32 ], 
  [ 32, 33 ], [ 32, 34 ], [ 32, 35 ], [ 32, 43 ], [ 32, 44 ], [ 32, 49 ], 
  [ 32, 50 ] ]

gap> MiddleLength(G);
1.

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
