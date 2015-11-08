#############################################################################
##
#W  isoclinic.tst                 XMOD test file                 Alper Odabas
#W                                                               & Enver Uslu
##  version 2.43, 07/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

#### 7.1.1 
gap> D24 := DihedralGroup(24);;  SetName( D24, "D24" );
gap> X24 := XModByAutomorphismGroup( D24 );; 
gap> Size(X24);
[ 24, 48 ]
gap> nsx := NormalSubXMods( X24 );; 
gap> ids := List( nsx, n -> IdGroup(n) );; 
gap> pos1 := Position( ids, [ [4,1], [8,3] ] );;
gap> Xn1 := nsx[pos1]; 
[Group( [ f2*f4^2, f3*f4 ] )->Group( [ f3, f4, f5 ] )]
gap> Size( Xn1 );
[ 4, 8 ]
gap> natn := NaturalMorphismByNormalSubXMod( X24, Xn1 ); 
[[D24->PAut(D24)] => [..]]
gap> Qn1 := FactorXMod( X24, Xn1 ); 
[Group( [ f1, f2 ] )->Group( [ f1, f2 ] )]
gap> Size( Qn1 );
[ 6, 6 ]

#### 7.1.2
gap> pos2 := Position( ids, [ [24,6], [12,4] ] );;
gap> Xn2 := nsx[pos2]; 
[D24->Group( [ f1*f3, f2, f5 ] )]
gap> pos3 := Position( ids, [ [12,2], [24,5] ] );;
gap> Xn3 := nsx[pos3]; 
[Group( [ f2, f3, f4 ] )->Group( [ f1, f2, f4, f5 ] )]
gap> Xn23 := IntersectionSubXMods( X24, Xn2, Xn3 );
[Group( [ f2, f3, f4 ] )->Group( [ f2, f5, f2^2, f2*f5, f2^2*f5 ] )]
gap> [ Size(Xn2), Size(Xn3), Size(Xn23) ];
[ [ 24, 12 ], [ 12, 24 ], [ 12, 6 ] ]

#### 7.1.3
gap> pos4 := Position( ids, [ [6,2], [24,14] ] );;
gap> Xn4 := nsx[pos4];; 
gap> Sn4 := Source(Xn4);; 
gap> Rn4 := Range(Xn4);; 
gap> r := Rn4.1;;  s := Sn4.1;; 
gap> d := Displacement( XModAction(Xn4), r, s );
f4
gap> bn4 := Boundary( Xn4 );;
gap> Image( bn4, d ) = Comm( r, Image( bn4, s ) );  
true
gap> DisplacementSubgroup( Xn4 );
Group([ f4 ])

#### 7.1.4
gap> Cn23 := CommutatorSubXMod( X24, Xn2, Xn3 );
[Group( [ f2 ] )->Group( [ f2, f5 ] )]
gap> Size(Cn23);
[ 12, 6 ]
gap> Xn23 = Cn23;
true
gap> Q24 := CentralQuotient( D24) ;                     
[D24->Group( [ f1, f2, f3 ] )]

#### 7.1.5
gap> DXn4 := DerivedSubXMod( Xn4 );  
[Group( [ f4 ] )->Group( [ f2 ] )]

#### 7.1.6
gap> fix := FixedPointSubgroupXMod( Xn4, Sn4, Rn4 );
Group([ f3*f4 ])
gap> stab := StabilizerSubgroupXMod( Xn4, Sn4, Rn4 );
Group([ f5, f2*f3 ])

#### 7.1.7
gap> ZXn4 := CentreXMod( Xn4 );      
[Group( [ f3*f4 ] )->Group( [ f3, f5 ] )]
gap> IdGroup( ZXn4 );
[ [ 2, 1 ], [ 4, 2 ] ]
gap> CDXn4 := Centralizer( Xn4, DXn4 );
[Group( [ f3*f4 ] )->Group( [ f2 ] )]
gap> IdGroup( CDXn4 );    
[ [ 2, 1 ], [ 3, 1 ] ]
gap> NDXn4 := Normalizer( Xn4, DXn4 ); 
[Group( <identity> of ... )->Group( [ f5, f2*f3 ] )]
gap> IdGroup( NDXn4 );
[ [ 1, 1 ], [ 12, 5 ] ]

#### 7.1.8
gap> Q24 := CentralQuotient( D24);  Size( Q24 );                     
[D24->Group( [ f1, f2, f3 ] )]
[ 24, 12 ]

#### 7.1.9
gap> [ IsAbelian2dGroup(Xn4), IsAbelian2dGroup(X24) ];
[ false, false ]
gap> pos7 := Position( ids, [ [3,1], [6,1] ] );;
gap> [ IsAspherical2dGroup(nsx[pos7]), IsAspherical2dGroup(X24) ];
[ true, false ]
gap> [ IsSimplyConnected2dGroup(Xn4), IsSimplyConnected2dGroup(X24) ];
[ true, true ]
gap> [ IsFaithful2dGroup(Xn4), IsFaithful2dGroup(X24) ];              
[ false, true ] 

#### 7.1.10
gap> LowerCentralSeries(X24);      
[ [D24->PAut(D24)], [Group( [ f2 ] )->Group( [ f2, f5 ] )], 
  [Group( [ f3*f4^2 ] )->Group( [ f2 ] )], [Group( [ f4 ] )->Group( [ f2 ] )] 
 ]
gap> IsNilpotent2dGroup(X24);      
false
gap> NilpotencyClassOf2dGroup(X24);
0

#### 7.1.11
gap> xc6s3 := AllXMods( SmallGroup(6,2), SmallGroup(6,1) );;   
gap> Length( xc6s3 );           
4
gap> x66 := AllXMods( [6,6] );;   
gap> Length( x66 );
17
gap> x36 := AllXMods( 36 );; 
gap> Length( x36 ); 
205
gap> size36 := List( x36, x -> [ Size(Source(x)), Size(Range(x)) ] );;
gap> Collected( size36 );
[ [ [ 1, 36 ], 14 ], [ [ 2, 18 ], 7 ], [ [ 3, 12 ], 21 ], [ [ 4, 9 ], 14 ], 
  [ [ 6, 6 ], 17 ], [ [ 9, 4 ], 102 ], [ [ 12, 3 ], 8 ], [ [ 18, 2 ], 18 ], 
  [ [ 36, 1 ], 4 ] ]

#### 7.1.12
gap> IsomorphismXMods( x66[1], x66[2] );
[[Group( [ f1, f2 ] )->Group( [ f1, f2 ] )] => [Group( [ f1, f2 ] )->Group( 
[ f1, f2 ] )]]
gap> iso66 := AllXModsUpToIsomorphism( x66 );;  Length( iso66 ); 
9 

#### testing isoclinism of groups #### 

#### 7.2.1
gap> G := SmallGroup( 64, 6 );;  StructureDescription( G ); 
"(C8 x C4) : C2"
gap> QG := CentralQuotient( G );;  IdGroup( QG );
[ [ 64, 6 ], [ 8, 3 ] ]
gap> H := SmallGroup( 32, 41 );;  StructureDescription( H );
"C2 x Q16"
gap> QH := CentralQuotient( H );;  IdGroup( QH );
[ [ 32, 41 ], [ 8, 3 ] ]
gap> Isoclinism( G, H );
[ [ f1, f2, f3 ] -> [ f1, f2*f3, f3 ], [ f3, f5 ] -> [ f4*f5, f5 ] ]
gap> K := SmallGroup( 32, 43 );;  StructureDescription( K );
"(C2 x D8) : C2"
gap> QK := CentralQuotient( K );;  IdGroup( QK );                       
[ [ 32, 43 ], [ 16, 11 ] ]
gap> AreIsoclinicDomains( G, K );
false

#### 7.2.2
gap> DerivedSubgroup(G);     
Group([ f3, f5 ])
gap> IsStemDomain( G );
false
gap> IsoclinicStemDomain( G );
<pc group of size 16 with 4 generators>
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

#### 7.2.3
gap> MiddleLength(G);
1.
gap> RankXMod(X1);
[ 2.32193, 2. ]


#### testing isoclinism of groups #### 

#### 7.3.1
gap> C8 := Cat1(16,8,1);;
gap> X8 := XMod(C8);  IdGroup( X8 );
[Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )]
[ [ 8, 1 ], [ 2, 1 ] ]
gap> C9 := Cat1(32,9,1);
[(C8 x C2) : C2=>Group( [ f2, f2 ] )]
gap> X9 := XMod( C9 );  IdGroup( X9 );
[Group( [ f1*f2*f3, f3, f4, f5 ] )->Group( [ f2, f2 ] )]
[ [ 16, 5 ], [ 2, 1 ] ]
gap> AreIsoclinicDomains( X8, X9 );
true
gap> ism89 := Isoclinism( X8, X9 );;
gap> Display( ism89 );
[ [[Group( [ f1 ] )->Group( [ f2 ] )] => [Group( [ f1 ] )->Group( [ f2 ] )]], 
  [[Group( [ f3 ] )->Group( <identity> of ... )] => [Group( 
    [ f3 ] )->Group( <identity> of ... )]] ]

#### 7.3.2
gap> IsStemDomain(X8);
true
gap> IsStemDomain(X9);
false

#### 7.3.3
gap> MiddleLength(X24);
[ 2.58496, 2.58496 ]



gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
