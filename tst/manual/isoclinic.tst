#############################################################################
##
#W  isoclinic.tst                 XMOD test file                 Alper Odabas
#W                                                               & Enver Uslu
#Y  Copyright (C) 2001-2022, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: isoclinic.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

## Chapter 4

#### 4.1.1 
gap> d24 := DihedralGroup( IsPermGroup, 24 );; 
gap> SetName( d24, "d24" );
gap> Y24 := XModByAutomorphismGroup( d24 ); 
[d24->Aut(d24)]
gap> Size2d( Y24 );
[ 24, 48 ]
gap> X24i := Image( IsomorphismPerm2DimensionalGroup( Y24 ) );;
gap> R24i := Range( X24i );; 
gap> genR24 := [ (2,4), (1,2,3,4), (6,7), (5,6,7) ];; 
gap> rhom24 := GroupHomomorphismByImages( R24i, Group( genR24 ) );; 
gap> shom24 := IdentityMapping( d24 );; 
gap> iso24 := IsomorphismByIsomorphisms( X24i, [ shom24, rhom24 ] );; 
gap> X24 := Range( iso24 );;
gap> SetName( X24, Name( X24i ) );
gap> nsx := NormalSubXMods( X24 );; 
gap> Length( nsx );
40
gap> ids := List( nsx, n -> IdGroup(n) );; 
gap> pos1 := Position( ids, [ [4,1], [8,3] ] );;
gap> Xn1 := nsx[pos1];; 
gap> IdGroup( Xn1 );
[ [ 4, 1 ], [ 8, 3 ] ]
gap> nat1 := NaturalMorphismByNormalSubPreXMod( X24, Xn1 );; 
gap> Qn1 := FactorPreXMod( X24, Xn1 );; 
gap> [ Size2d( Xn1 ), Size2d( Qn1 ) ];
[ [ 4, 8 ], [ 6, 6 ] ]

#### 4.1.2
gap> pos2 := Position( ids, [ [24,6], [12,4] ] );;
gap> Xn2 := nsx[pos2];; 
gap> IdGroup( Xn2 );         
[ [ 24, 6 ], [ 12, 4 ] ]
gap> pos3 := Position( ids, [ [12,2], [24,5] ] );;
gap> Xn3 := nsx[pos3];; 
gap> IdGroup( Xn3 );
[ [ 12, 2 ], [ 24, 5 ] ]
gap> Xn23 := IntersectionSubXMods( X24, Xn2, Xn3 );;
gap> IdGroup( Xn23 );
[ [ 12, 2 ], [ 6, 2 ] ]

#### 4.1.3
gap> pos4 := Position( ids, [ [6,2], [24,14] ] );;
gap> Xn4 := nsx[pos4];; 
gap> bn4 := Boundary( Xn4 );;
gap> Sn4 := Source(Xn4);; 
gap> Rn4 := Range(Xn4);; 
gap> genRn4 := GeneratorsOfGroup( Rn4 );;
gap> L := List( genRn4, g -> ( Order(g) = 2 ) and 
>                 not ( IsNormal( Rn4, Subgroup( Rn4, [g] ) ) ) );;
gap> pos := Position( L, true );;
gap> s := Sn4.1;  r := genRn4[pos]; 
(1,3,5,7,9,11)(2,4,6,8,10,12)
(6,7)
gap> act := XModAction( Xn4 );; 
gap> d := Displacement( act, r, s );
(1,5,9)(2,6,10)(3,7,11)(4,8,12)
gap> Image( bn4, d ) = Comm( r, Image( bn4, s ) );  
true
gap> Qn4 := Subgroup( Rn4, [ (6,7), (1,3), (2,4) ] );;   
gap> Tn4 := Subgroup( Sn4, [ (1,3,5,7,9,11)(2,4,6,8,10,12) ] );;
gap> DisplacementGroup( Xn4, Qn4, Tn4 );                        
Group([ (1,5,9)(2,6,10)(3,7,11)(4,8,12) ])
gap> DisplacementSubgroup( Xn4 );
Group([ (1,5,9)(2,6,10)(3,7,11)(4,8,12) ])

#### 4.1.4
gap> CAn23 := CrossActionSubgroup( X24, Xn2, Xn3 );;
gap> IdGroup( CAn23 );
[ 12, 2 ]
gap> Cn23 := CommutatorSubXMod( X24, Xn2, Xn3 );;
gap> IdGroup( Cn23 );
[ [ 12, 2 ], [ 6, 2 ] ]
gap> Xn23 = Cn23;
true

#### 4.1.5
gap> DXn4 := DerivedSubXMod( Xn4 );;
gap> IdGroup( DXn4 );
[ [ 3, 1 ], [ 3, 1 ] ]

#### 4.1.6
gap> fix := FixedPointSubgroupXMod( Xn4, Sn4, Rn4 );
Group([ (1,7)(2,8)(3,9)(4,10)(5,11)(6,12) ])
gap> stab := StabilizerSubgroupXMod( Xn4, Sn4, Rn4 );;
gap> IdGroup( stab );
[ 12, 5 ]

#### 4.1.7
gap> ZXn4 := CentreXMod( Xn4 );; 
gap> IdGroup( ZXn4 );
[ [ 2, 1 ], [ 4, 2 ] ]
gap> CDXn4 := Centralizer( Xn4, DXn4 );;
gap> IdGroup( CDXn4 );    
[ [ 2, 1 ], [ 3, 1 ] ]
gap> NDXn4 := Normalizer( Xn4, DXn4 );; 
gap> IdGroup( NDXn4 );
[ [ 1, 1 ], [ 12, 5 ] ]

#### 4.1.8
gap> Q24 := CentralQuotient( d24);  IdGroup( Q24 );                     
[d24->d24/Z(d24)]
[ [ 24, 6 ], [ 12, 4 ] ]

#### 4.1.9
gap> [ IsAbelian2DimensionalGroup(Xn4), IsAbelian2DimensionalGroup(X24) ];
[ false, false ]
gap> pos7 := Position( ids, [ [3,1], [6,1] ] );;
gap> IsAspherical2DimensionalGroup( nsx[ pos7 ] );
true
gap> IsAspherical2DimensionalGroup( X24 );
false
gap> IsSimplyConnected2DimensionalGroup( Xn4 ); 
true
gap> IsSimplyConnected2DimensionalGroup( X24 );
true
gap> IsFaithful2DimensionalGroup( Xn4 ); 
false
gap> IsFaithful2DimensionalGroup( X24 );
true

#### 4.1.10
gap> lcs := LowerCentralSeries( X24 );;      
gap> List( lcs, g -> IdGroup(g) );
[ [ [ 24, 6 ], [ 48, 38 ] ], [ [ 12, 2 ], [ 6, 2 ] ], [ [ 6, 2 ], [ 3, 1 ] ], 
  [ [ 3, 1 ], [ 3, 1 ] ] ]
gap> IsNilpotent2DimensionalGroup( X24 );      
false
gap> NilpotencyClassOf2DimensionalGroup( X24 );
0

#### 4.1.11
gap> gend24 := GeneratorsOfGroup( d24 );;             
gap> a := gend24[1];; b:= gend24[2];; 
gap> J := Subgroup( d24, [a^2,b] ); 
Group([ (1,3,5,7,9,11)(2,4,6,8,10,12), (2,12)(3,11)(4,10)(5,9)(6,8) ])
gap> K := Subgroup( d24, [a^2,a*b] );
Group([ (1,3,5,7,9,11)(2,4,6,8,10,12), (1,12)(2,11)(3,10)(4,9)(5,8)(6,7) ])
gap> XJ := XModByNormalSubgroup( d24, J );;
gap> XK := XModByNormalSubgroup( d24, K );;
gap> iso := IsomorphismXMods( XJ, XK );;
gap> SourceHom( iso );
[ (1,3,5,7,9,11)(2,4,6,8,10,12), (2,12)(3,11)(4,10)(5,9)(6,8) ] -> 
[ (1,3,5,7,9,11)(2,4,6,8,10,12), (1,12)(2,11)(3,10)(4,9)(5,8)(6,7) ]
gap> RangeHom( iso ); 
[ (1,2,3,4,5,6,7,8,9,10,11,12), (2,12)(3,11)(4,10)(5,9)(6,8) ] -> 
[ (1,2,3,4,5,6,7,8,9,10,11,12), (1,12)(2,11)(3,10)(4,9)(5,8)(6,7) ]

#### 4.1.12
gap> c6 := SmallGroup( 6, 2 );; 
gap> s3 := SmallGroup( 6, 1 );; 
gap> Ac6s3 := AllXMods( c6, s3 );;   
gap> Length( Ac6s3 );           
4
gap> Ic6s3 := AllXModsUpToIsomorphism( c6, s3 );; 
gap> List( Ic6s3, obj -> IsTrivialAction2DimensionalGroup( obj ) ); 
[ true, false, false ]
gap> Kc6s3 := List( Ic6s3, obj -> KernelCokernelXMod( obj ) );; 
gap> List( Kc6s3, obj -> IdGroup( obj ) );
[ [ [ 6, 2 ], [ 6, 1 ] ], [ [ 6, 2 ], [ 6, 1 ] ], [ [ 2, 1 ], [ 2, 1 ] ] ]
gap> A66 := AllXMods( [6,6] );;   
gap> Length( A66 );
17
gap> IA66 := IsomorphismClassRepresentatives2dGroups( A66 );;
gap> Length( IA66 );
9
gap> x36 := AllXMods( 36 );; 
gap> Length( x36 ); 
205
gap> size36 := List( x36, x -> Size2d( x ) );;
gap> Collected( size36 );
[ [ [ 1, 36 ], 14 ], [ [ 2, 18 ], 7 ], [ [ 3, 12 ], 21 ], [ [ 4, 9 ], 14 ], 
  [ [ 6, 6 ], 17 ], [ [ 9, 4 ], 102 ], [ [ 12, 3 ], 8 ], [ [ 18, 2 ], 18 ], 
  [ [ 36, 1 ], 4 ] ]

#### testing isoclinism of groups #### 

#### 4.2.1
gap> G := SmallGroup( 64, 6 );; 
gap> QG := CentralQuotient( G );;  IdGroup( QG );
[ [ 64, 6 ], [ 8, 3 ] ]
gap> H := SmallGroup( 32, 41 );;  
gap> QH := CentralQuotient( H );;  IdGroup( QH );
[ [ 32, 41 ], [ 8, 3 ] ]
gap> Isoclinism( G, H );
[ [ f1, f2, f3 ] -> [ f1, f2*f3, f3 ], [ f3, f5 ] -> [ f4*f5, f5 ] ]
gap> K := SmallGroup( 32, 43 );;  
gap> QK := CentralQuotient( K );;  IdGroup( QK );                       
[ [ 32, 43 ], [ 16, 11 ] ]
gap> AreIsoclinicDomains( G, K );
false

#### 4.2.2
gap> DerivedSubgroup(G);     
Group([ f3, f5 ])
gap> IsStemDomain( G );
false
gap> IsoclinicStemDomain( G );
<pc group of size 16 with 4 generators>
gap> AllStemGroupIds( 16 );     
[ [ 16, 7 ], [ 16, 8 ], [ 16, 9 ] ]
gap> AllStemGroupFamilies( 16 );
[ [ [ 16, 7 ], [ 16, 8 ], [ 16, 9 ] ] ]

#### 4.2.3
gap> IsoclinicMiddleLength( G );
1
gap> IsoclinicRank( G );
4

#### testing isoclinism of crossed modules #### 

#### 4.3.1
gap> C8 := Cat1Group( 16, 8, 2 );;
gap> X8 := XMod(C8);  IdGroup( X8 );
[Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )]
[ [ 8, 1 ], [ 2, 1 ] ]
gap> C9 := Cat1Group( 32, 9, 2 );
[(C8 x C2) : C2 => Group( [ f2, f2 ] )]
gap> X9 := XMod( C9 );  IdGroup( X9 );
[Group( [ f1*f2*f3, f3, f4, f5 ] )->Group( [ f2, f2 ] )]
[ [ 16, 5 ], [ 2, 1 ] ]
gap> AreIsoclinicDomains( X8, X9 );
true
gap> ism89 := Isoclinism( X8, X9 );;
gap> Display( ism89 );
[ [[Group( [ f1, f2, <identity> of ... ] ) -> Group( [ f2, f2 ] )] => [Group( 
    [ f1, f2, <identity> of ..., <identity> of ... ] ) -> Group( 
    [ f2, f2 ] )]], 
  [[Group( [ f3 ] ) -> Group( <identity> of ... )] => [Group( 
    [ f3 ] ) -> Group( <identity> of ... )]] ]

#### 4.3.2
gap> IsStemDomain(X8);
true
gap> IsStemDomain(X9);
false

#### 4.3.3
gap> IsoclinicMiddleLength(X8);
[ 1, 0 ]
gap> IsoclinicRank(X8);        
[ 3, 1 ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "isoclinic.tst", 10000 );
