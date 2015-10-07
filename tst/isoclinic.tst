#############################################################################
##
#W  isoclinic.tst               XMOD test file                   Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 07/10/2015 
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
gap> SetName( X5, "X5" );
gap> Size( X5 );
[ 6, 24 ]
gap> d := Displacement( XModAction(X5), N5.1, D24.1 );
f4^2
gap> Image( Boundary(X5), d ) = Comm( N5.1, D24.1 );  
true
gap> DisplacementSubgroup( X5 );
Group([ f4^2, f4 ])

gap> fix := FixedPointSubgroupXMod( X5, N5, D24 );
Group([ f3*f4 ])
gap> stab := StabilizerSubgroupXMod( X5, N5, D24 );
<pc group of size 12 with 1 generators>
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
[Group( [ f3, f4^2, f4 ] )->Group( [ f2 ] )]
gap> Size(Y6);
[ 6, 3 ]
gap> X6 = Y6;
false


gap> G24:= SmallGroup( 24, 14 );
<pc group of size 24 with 4 generators>
gap> norm := NormalSubgroups( G24 );;
gap> List( norm, n -> Size(n));   
[ 24, 12, 12, 12, 12, 12, 12, 12, 6, 6, 6, 6, 6, 6, 6, 4, 2, 2, 2, 3, 1 ]
gap> N2 := norm[2];
Group([ f1, f3, f4 ])
gap> X2 := XModByNormalSubgroup( G24, N2 );
[Group( [ f1, f3, f4 ] )->Group( [ f1, f2, f3, f4 ] )]
gap> stab := StabilizerSubgroupXMod( X2, N2, G24 );
<pc group of size 4 with 2 generators>
gap> fix := FixedPointSubgroupXMod( X2, N2, G24 );
Group([ f3 ])
gap> im := Image(Boundary(X2),fix);
Group([ f3 ])
gap> N9 := norm[9];
Group([ f3, f4 ])
gap> C9 := Centralizer( G24, N9 );
Group([ f2, f3, f4 ])
gap> inter := Intersection( stab, C9 );
<pc group of size 4 with 2 generators>
gap> stab=Intersection( stab, C9 );
true
gap> Y9 := SubXMod( X2, fix, inter );
[Group( [ f3 ] )->Group( [ f2, f3 ] )]
gap> Display(Y9);

Crossed module [..->..] :- 
: Source group has generators:
  [ f3 ]
: Range group has generators:
  [ f2, f3 ]
: Boundary homomorphism maps source generators to:
  [ f3 ]
  The automorphism group is trivial

gap> Size(Y9);
[ 2, 4 ]



gap> nsx := NormalSubXMods( X24 );; 
gap> Length( nsx );  Size( nsx[30] );
40
[ 4, 8 ]
gap> Q := FactorXMod( X24, nsx[30] ); 
[Group( [ f1, f2, f2^2, f2 ] )->Group( 
[ f1, f2, <identity> of ..., <identity> of ..., <identity> of ... ] )]
gap> Size( Q );
[ 6, 6 ]
gap> nat := NaturalMorphismByNormalSubXMod( X24, nsx[30] ); 
[[D24->PAut(D24)] => [..]]



gap> ZX5 := CentreXMod( X5 );      
[Group( [ f3*f4 ] )->Group( [ f3*f4 ] )]
gap> IdGroup( ZX5 );
[ [ 2, 1 ], [ 2, 1 ] ]
gap> CDX5 := Centralizer( X5, DX5 );
[Group( [ f3*f4 ] )->Group( [ f3 ] )]
gap> IdGroup( CDX5 );    
[ [ 2, 1 ], [ 6, 2 ] ]
gap> NDX5 := Normalizer( X5, DX5 ); 
[Group( <identity> of ... )->Group( [ f2 ] )]
gap> IdGroup( NDX5 );
[ [ 1, 1 ], [ 12, 2 ] ]



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
gap> IsoclinicStemGroup( G );
<pc group of size 16 with 4 generators>
gap> IdGroup( last );
[ 16, 7 ]
gap> AllStemGroupIds( 32 );
[ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ], [ 32, 18 ], [ 32, 19 ], [ 32, 20 ], 
  [ 32, 27 ], [ 32, 28 ], [ 32, 29 ], [ 32, 30 ], [ 32, 31 ], [ 32, 32 ], 
  [ 32, 33 ], [ 32, 34 ], [ 32, 35 ], [ 32, 43 ], [ 32, 44 ], [ 32, 49 ], 
  [ 32, 50 ] ]
gap> AllStemGroupFamilies( 32 );
[ [ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ] ], [ [ 32, 18 ], [ 32, 19 ], [ 32, 20 ] ],
  [ [ 32, 27 ], [ 32, 28 ], [ 32, 29 ], [ 32, 30 ], [ 32, 31 ], [ 32, 32 ], 
      [ 32, 33 ], [ 32, 34 ], [ 32, 35 ] ], [ [ 32, 43 ], [ 32, 44 ] ], 
  [ [ 32, 49 ], [ 32, 50 ] ] ]

gap> MiddleLength(G);
1.
gap> RankXMod( X1 );
[ 2.32193, 2. ]

gap> CQX5 := CentralQuotient(X5);            
X5/Z(X5)
gap> MappingGeneratorsImages( Boundary( CQX5 ) );
[ [ f1^2, f1 ], [ f3^2, f3 ] ]
gap> CentralQuotientHomomorphism(X5);    
[[..] => [..]]

gap> xc6s3 := AllXMods( SmallGroup(6,2), SmallGroup(6,1) );;   
gap> Length( xc6s3 );           
4
gap> x66 := AllXMods( [6,6] );;   
gap> Length( x66 );
17
gap> x36 := AllXMods( 36 );; 
gap> Length( x36 ); 
205
gap> px66 := AllPreXMods( [6,6] );;   
gap> Length( px66 );
40

gap> IsomorphismXMods( x66[1], x66[2] );
[[Group( [ f1, f2 ] )->Group( [ f1, f2 ] )] => [Group( [ f1, f2 ] )->Group( 
[ f1, f2 ] )]]
gap> L := ListWithIdenticalEntries( 17, 1 );;
gap> for i in [1..16] do 
>      if ( L[i] = 1 ) then 
>        X1 := x66[i]; 
>        for j in [i+1..17] do 
>          if ( L[j] = 1 ) then 
>            X2 := x66[j];            
>            mor := IsomorphismXMods( X1, X2 ); 
>            if ( mor <> fail ) then L[j]:=0; fi; 
>          fi;
>        od;
>      fi;
>    od;
gap> L;
[ 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0 ]
gap> Sum( L );
9

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
