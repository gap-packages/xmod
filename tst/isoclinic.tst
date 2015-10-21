#############################################################################
##
#W  isoclinic.tst               XMOD test file           Chris Wensley & Alper Odabas
#W                                                                     & Enver Uslu
##  version 2.43, 21/10/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> D24 := DihedralGroup(24);;  SetName( D24, "D24" );
gap> X24 := XModByAutomorphismGroup( D24 ); 
[D24->PAut(D24)]
gap> Size(X24);
[ 24, 48 ]
gap> nsx := NormalSubXMods( X24 );; 
gap> Length( nsx ); 
40
gap> X30 := nsx[30]; 
[Group( [ f2*f4^2, f3*f4 ] )->Group( [ f3, f4, f5 ] )]
gap> Size( X30 ); 
[ 4, 8 ]
gap> nat30 := NaturalMorphismByNormalSubXMod( X24, X30 ); 
[[D24->PAut(D24)] => [..]]
gap> Q30 := FactorXMod( X24, X30 ); 
[Group( [ f1, f2, f2^2, f2 ] )->Group( 
[ f1, f2, <identity> of ..., <identity> of ..., <identity> of ... ] )]
gap> Size( Q30 );
[ 6, 6 ]
gap> nat := NaturalMorphismByNormalSubXMod( X24, X30 ); 
[[D24->PAut(D24)] => [..]]

gap> X5 := nsx[5];
[D24->Group( [ f1*f3, f2, f5 ] )]
gap> X7 := nsx[7];
[Group( [ f2, f3, f4 ] )->Group( [ f1, f2, f4, f5 ] )]
gap> X57 := IntersectionSubXMods( X24, X5, X7 );
[Group( [ f2, f3, f4 ] )->Group( [ f2, f5, f2^2, f2*f5, f2^2*f5 ] )]
gap> Position( nsx, X57 );                              
21
gap> [ Size(X5), Size(X7), Size(X57) ];
[ [ 24, 12 ], [ 12, 24 ], [ 12, 6 ] ]

gap> X22 := nsx[22];
[Group( [ f3, f4 ] )->Group( [ f1, f2, f3, f5 ] )]
gap> Display(X22);

Crossed module :- 
: Source group has generators:
  [ f3, f4 ]
: Range group has generators:
  [ f1, f2, f3, f5 ]
: Boundary homomorphism maps source generators to:
  [ f2^2, f2 ]
: Action homomorphism maps range generators to automorphisms:
  f1 --> { source gens --> [ f3*f4^2, f4^2 ] }
  f2 --> { source gens --> [ f3, f4 ] }
  f3 --> { source gens --> [ f3, f4 ] }
  f5 --> { source gens --> [ f3, f4 ] }
  These 4 automorphisms generate the group of automorphisms.

gap> S22:=Source(X22);;  SetName( S22, "c6" ); 
gap> R22 := Range(X22);;  SetName( R22, "c2c2s3" );
gap> Name( X22 );
"[c6->c2c2s3]"
gap> r := R22.1;;  s := S22.1;; 
gap> d := Displacement( XModAction(X22), r, s );
f4
gap> b22 := Boundary( X22 );;
gap> Image( b22, d ) = Comm( r, Image( b22, s ) );  
true
gap> DisplacementSubgroup( X22 );
Group([ f4 ])

gap> fix := FixedPointSubgroupXMod( X22, S22, R22 );
Group([ f3*f4 ])
gap> stab := StabilizerSubgroupXMod( X22, S22, R22 );
Group([ f5, f2*f3 ])
gap> DX22 := DerivedSubXMod( X22 );  
[Group( [ f4 ] )->Group( [ f2 ] )]

gap> C57 := CommutatorSubXMod( X24, X5, X7 );
[Group( [ f2 ] )->Group( [ f2, f5 ] )]
gap> Size(C57);
[ 12, 6 ]
gap> X57 = C57;
true
gap> Q24 := CentralQuotient( D24) ;                     
[D24->Group( [ f1, f2, f3 ] )]


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
Group([ f2, f3 ])
gap> fix := FixedPointSubgroupXMod( X2, N2, G24 );
Group([ f3 ])
gap> im := Image(Boundary(X2),fix);
Group([ f3 ])
gap> N9 := norm[9];
Group([ f3, f4 ])
gap> C9 := Centralizer( G24, N9 );
Group([ f2, f3, f4 ])
gap> inter := Intersection( stab, C9 );
Group([ f2, f3 ])
gap> stab=Intersection( stab, C9 );
true
gap> Y9 := SubXMod( X2, fix, inter );
[Group( [ f3 ] )->Group( [ f2, f3 ] )]
gap> Display(Y9);

Crossed module :- 
: Source group has generators:
  [ f3 ]
: Range group has generators:
  [ f2, f3 ]
: Boundary homomorphism maps source generators to:
  [ f3 ]
  The automorphism group is trivial

gap> Size(Y9);
[ 2, 4 ]

gap> ZX22 := CentreXMod( X22 );      
[Group( [ f3*f4 ] )->Group( [ f3, f5 ] )]
gap> IdGroup( ZX22 );
[ [ 2, 1 ], [ 4, 2 ] ]
gap> CDX22 := Centralizer( X22, DX22 );
[Group( [ f3*f4 ] )->Group( [ f2 ] )]
gap> IdGroup( CDX22 );    
[ [ 2, 1 ], [ 3, 1 ] ]
gap> NDX22 := Normalizer( X22, DX22 ); 
[Group( <identity> of ... )->Group( [ f5, f2*f3 ] )]
gap> IdGroup( NDX22 );
[ [ 1, 1 ], [ 12, 5 ] ]

gap> LX1 := LowerCentralSeries( X1 );
[ [c5->PAut(c5)], [Group( [ (5,9,8,7,6) ] )->Group( () )] ]
gap> List( LX1, x -> Size(x) );            
[ [ 5, 4 ], [ 5, 1 ] ]
gap> IsNilpotent2dGroup( X1 );             
false
gap> NilpotencyClassOf2dGroup( X1 );         
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


gap> [ IsAbelian2dGroup(X22), IsAbelian2dGroup(X24) ];
[ false, false ]
gap> [ IsAspherical2dGroup(nsx[38]), IsAspherical2dGroup(X24) ];
[ true, false ]
gap> [ IsSimplyConnected2dGroup(X22), IsSimplyConnected2dGroup(X24) ];
[ true, true ]
gap> [ IsFaithful2dGroup(X22), IsFaithful2dGroup(X24) ];              
[ false, true ] 



#### testing isoclinism of groups #### 

gap> G := SmallGroup( 64, 6 );;  StructureDescription( G ); 
"(C8 x C4) : C2"
gap> Q := CentralQuotient( G );;  IdGroup( Q );
[ [ 64, 6 ], [ 8, 3 ] ]
gap> Boundary( Q );
[ f1, f2, f3, f4, f5, f6 ] -> [ f1, f2, f3, <identity> of ..., 
  <identity> of ..., <identity> of ... ]

gap> H := SmallGroup( 32, 41 );;  StructureDescription( H );
"C2 x Q16"
gap> IdGroup( CentralQuotient( H ) );
[ [ 32, 41 ], [ 8, 3 ] ]
gap> Isoclinism( G, H );
[ [ f1, f2, f3 ] -> [ f1, f2*f3, f3 ], [ f3, f5 ] -> [ f4*f5, f5 ] ]
gap> K := SmallGroup( 32, 43 );;  StructureDescription( K );
"(C2 x D8) : C2"
gap> IdGroup( CentralQuotient( K ) );                       
[ [ 32, 43 ], [ 16, 11 ] ]
gap> AreIsoclinicDomains( G, K );
false

gap> DerivedSubgroup(G);     
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

gap> CQX24 := CentralQuotient(X24);            
crossed square with:
      up = [D24->Group( [ f1, f2, f3 ] )]
    left = [D24->PAut(D24)]
    down = [PAut(D24)->PAut(D24)]
   right = [D24->PAut(D24)]/Z([D24->PAut(D24)])

gap> MappingGeneratorsImages( Boundary( Right2dGroup( CQX24 ) ) );
[ [ f1, f2, f3^2, f3 ], [ f1*f3, f2*f5, f2^2, f2 ] ]

gap> xc6s3 := AllXMods( SmallGroup(6,2), SmallGroup(6,1) );;   
gap> Length( xc6s3 );           
4
gap> x66 := AllXMods( [6,6] );;   
gap> Length( x66 );
17
gap> x36 := AllXMods( 36 );; 
gap> Length( x36 ); 
205

gap> IsomorphismXMods( x66[1], x66[2] );
[[Group( [ f1, f2 ] )->Group( [ f1, f2 ] )] => [Group( [ f1, f2 ] )->Group( 
[ f1, f2 ] )]]
gap> L := ListWithIdenticalEntries( 17, 1 );;
gap> for i in [1..16] do 
>      if ( L[i] = 1 ) then 
>        Y1 := x66[i]; 
>        for j in [i+1..17] do 
>          if ( L[j] = 1 ) then 
>            Y2 := x66[j];            
>            mor := IsomorphismXMods( Y1, Y2 ); 
>            if ( mor <> fail ) then L[j]:=0; fi; 
>          fi;
>        od;
>      fi;
>    od;
gap> L;
[ 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0 ]
gap> Sum( L );
9

############################################################################# 
gap> all44 := AllXMods( [4,4] );; 
gap> Length( last );
60
gap> iso44 := AllXModsUpToIsomorphism( all44 );;
gap> Length( last );
18 
gap> L := ListWithIdenticalEntries( 60, 1 );;
gap> for i in [1..59] do 
>      if ( L[i] = 1 ) then 
>        L[i] := [i]; 
>        Y1 := all44[i]; 
>        for j in [i+1..60] do 
>          if ( L[j] = 1 ) then 
>            Y2 := all44[j];            
>            mor := IsomorphismXMods( Y1, Y2 ); 
>            if ( mor <> fail ) then 
>              L[j]:=0; 
>              Add( L[i], j );
>            fi; 
>          fi;
>        od;
>      fi;
>    od;
gap> L := Filtered( L, k -> k<>0 );
[ [ 1 ], [ 2 ], [ 3, 6 ], [ 4 ], [ 5 ], [ 7 ], [ 8, 9, 10 ], [ 11, 13, 15 ], 
  [ 12, 14, 16 ], [ 17 ], [ 18, 19, 20 ], [ 21, 23, 25 ], [ 22, 24, 26 ], 
  [ 27 ], [ 28, 29, 30, 31, 32, 33, 34, 35, 36 ], 
  [ 37, 39, 41, 43, 45, 49, 52, 55, 59 ], 
  [ 38, 40, 42, 44, 46, 50, 53, 56, 60 ], [ 47, 48, 51, 54, 57, 58 ] ]



#### testing isoclinism of groups #### 

gap> IdGroup( X24 );
[ [ 24, 6 ], [ 48, 38 ] ]
gap> IdGroup( CentreXMod(X24) );     
[ [ 2, 1 ], [ 1, 1 ] ]
gap> CQX24 := CentralQuotient( X24 );
crossed square with:
      up = [D24->Group( [ f1, f2, f3 ] )]
    left = [D24->PAut(D24)]
    down = [PAut(D24)->PAut(D24)]
   right = [D24->PAut(D24)]/Z([D24->PAut(D24)])

gap> IdGroup( CQX24 );
[ [ [ 24, 6 ], [ 12, 4 ] ], [ [ 48, 38 ], [ 48, 38 ] ] ]

############################################################################# 
##  Example 6 from the paper 

gap> C8 := Cat1(16,8,1);
[QD16=>Group( [ f2, f2 ] )]
gap> X8 := XMod(C8); 
[Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )]
gap> IdGroup( X8 );
[ [ 8, 1 ], [ 2, 1 ] ]
gap> ZX8 := CentreXMod( X8 );
[Group( [ f4 ] )->Group( <identity> of ... )]
gap> FX8 := FactorXMod( X8, ZX8 );
[Group( [ f1, f2, <identity> of ... ] )->Group( [ f2, f2 ] )]
gap> DX8 := DerivedSubXMod( X8 );
[Group( [ f3 ] )->Group( <identity> of ... )]
gap> RankXMod( X8 );  
[ 3., 1. ]
gap> MiddleLength( X8 );    
[ 1., 0. ]
gap> LowerCentralSeries(X8);
[ [Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )], 
  [Group( [ f3 ] )->Group( <identity> of ... )], 
  [Group( [ f4 ] )->Group( <identity> of ... )], 
  [Group( <identity> of ... )->Group( <identity> of ... )] ]

gap> C9 := Cat1(32,9,1);
[(C8 x C2) : C2=>Group( [ f2, f2 ] )]
gap> X9 := XMod( C9 );
[Group( [ f1*f2*f3, f3, f4, f5 ] )->Group( [ f2, f2 ] )]
gap> IdGroup( X9 );
[ [ 16, 5 ], [ 2, 1 ] ]
gap> ZX9 := CentreXMod( X9 );
[Group( [ f4, f5 ] )->Group( <identity> of ... )]
gap> FX9 := FactorXMod( X9, ZX9 );
[Group( [ f1, f2, <identity> of ..., <identity> of ... ] )->Group( 
[ f2, f2 ] )]
gap> DX9 := DerivedSubXMod( X9 );
[Group( [ f3 ] )->Group( <identity> of ... )]
gap> morF := IsomorphismXMods( FX8, FX9 );
[[Group( [ f1, f2, <identity> of ... ] )->Group( [ f2, f2 ] )] => [Group( 
[ f1, f2, <identity> of ..., <identity> of ... ] )->Group( [ f2, f2 ] )]]
gap> morD := IsomorphismXMods( DX8, DX9 );
[[Group( [ f3 ] )->Group( <identity> of ... )] => [Group( 
[ f3 ] )->Group( <identity> of ... )]]
gap> IsStemXMod(X8);
true
gap> IsStemXMod(X9);
false
gap> iso89 := Isoclinism( X8, X9 );;
gap> MappingGeneratorsImages( iso89[1] );
[ [ [ f1, f2, <identity> of ... ], [ f1*f2, f2, <identity> of ... ] ], 
  [ [ f2, f2 ], [ f2, f2 ] ] ]
gap> MappingGeneratorsImages( iso89[2] );
[ [ [ f3 ], [ f3*f5 ] ], [ [  ], [  ] ] ]
gap> XG := XMod(G);; 
gap> XH := XMod(H);;
gap> AreIsoclinicDomains( XG, XH );
true


gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
