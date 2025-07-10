#############################################################################
##
#W  gp3cat2.tst                   XMOD test file               Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gp3cat2.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## make independent of isoclinic.tst, gp2obj.tst, gp3xsq.tst 

gap> d20 := DihedralGroup( IsPermGroup, 20 );;
gap> gend20 := GeneratorsOfGroup( d20 ); 
[ (1,2,3,4,5,6,7,8,9,10), (2,10)(3,9)(4,8)(5,7) ]
gap> p1 := gend20[1];;  p2 := gend20[2];; 
gap> d10a := Subgroup( d20, [ p1^2, p2 ] );;
gap> SetName( d20, "d20" );  SetName( d10a, "d10a" ); 
gap> X20 := XModByNormalSubgroup( d20, d10a );; 
gap> XSact := ActorCrossedSquare( X20 );;

## Chapter 8

# Section 8.5.1 
gap> a := (1,2,3,4,5,6);;  b := (2,6)(3,5);; 
gap> G := Group( a, b );;  SetName( G, "d12" );
gap> t1 := GroupHomomorphismByImages( G, G, [a,b], [a^3,b] );; 
gap> up := PreCat1GroupWithIdentityEmbedding( t1, t1 );;
gap> t2 := GroupHomomorphismByImages( G, G, [a,b], [a^4,b] );; 
gap> left := PreCat1GroupWithIdentityEmbedding( t2, t2 );;
gap> C2a := Cat2Group( up, left );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )]
gap> IsCat2Group( C2a );
true
gap> genR := [ (1,4)(2,5)(3,6), (2,6)(3,5) ];;
gap> R := Subgroup( G, genR );; 
gap> genQ := [ (1,3,5)(2,4,6), (2,6)(3,5) ];; 
gap> Q := Subgroup( G, genQ );; 
gap> Pa := Group( b );;  SetName( Pa, "c2a" ); 
gap> Pb := Group( (7,8) );; ## SetName( Pb, "c2b" ); 
gap> t3 := GroupHomomorphismByImages( R, Pb, genR, [(),(7,8)] );; 
gap> e3 := GroupHomomorphismByImages( Pb, R, [(7,8)], [(2,6)(3,5)] );; 
gap> right := PreCat1GroupByTailHeadEmbedding( t3, t3, e3 );;
gap> t4 := GroupHomomorphismByImages( Q, Pb, genQ, [(),(7,8)] );; 
gap> e4 := GroupHomomorphismByImages( Pb, Q, [(7,8)], [(2,6)(3,5)] );; 
gap> down := PreCat1GroupByTailHeadEmbedding( t4, t4, e4 );;
gap> t0 := t1 * t3;; 
gap> e0 := GroupHomomorphismByImages( Pb, G, [(7,8)], [(2,6)(3,5)] );; 
gap> diag := PreCat1GroupByTailHeadEmbedding( t0, t0, e0 );;
gap> C2b := PreCat2GroupByPreCat1Groups( up, left, right, down, diag ); 
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )]
gap> C2a = C2b;
false
gap> GroupsOfHigherDimensionalGroup( C2a )[4];
Group([ (), (2,6)(3,5) ])
gap> GroupsOfHigherDimensionalGroup( C2b )[4];
Group([ (7,8) ])

# Section 8.5.2
gap> Diagonal2DimensionalGroup( C2a );
[d12 => Group( [ (), (2,6)(3,5) ] )]
gap> Diagonal2DimensionalGroup( C2b );
[d12 => Group( [ (7,8) ] )]

# Section 8.5.3
gap> C2ab := DirectProductOp( [ C2a, C2b ], C2a ); 
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [Group( [ (1,2,3,4,5,6), (2,6)(3,5), ( 7, 8, 9,10,11,12), ( 8,12)( 9,11) 
 ] ) => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5), ( 7,10)( 8,11)( 9,12), 
  ( 8,12)( 9,11) ] )]
2 : [Group( [ (1,2,3,4,5,6), (2,6)(3,5), ( 7, 8, 9,10,11,12), ( 8,12)( 9,11) 
 ] ) => Group( [ (1,5,3)(2,6,4), (2,6)(3,5), ( 7, 9,11)( 8,10,12), 
  ( 8,12)( 9,11) ] )]
gap> StructureDescription( C2ab );  
[ "C2 x C2 x S3 x S3", "C2 x C2 x C2 x C2", "S3 x S3", "C2 x C2" ]
gap> SetName( C2ab, "C2ab" );
gap> Embedding( C2ab, 1 );   
<mapping: (pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )] -> C2ab >
gap> Projection( C2ab, 2 );
<mapping: C2ab -> (pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )] >

# Section 8.5.4
gap> DisplayLeadMaps( C2b );
(pre-)cat2-group with up-left group: [ (1,2,3,4,5,6), (2,6)(3,5) ]
   up tail=head images: [ (1,4)(2,5)(3,6), (2,6)(3,5) ]
 left tail=head images: [ (1,5,3)(2,6,4), (2,6)(3,5) ]

# Section 8.5.5
gap> TC2a := Transpose3DimensionalGroup( C2a );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]

# Section 8.5.6
gap> gamma := GroupHomomorphismByImages( G, G, [a,b], [a^-1,b] );;
gap> rho := IdentityMapping( R );;
gap> xi := GroupHomomorphismByImages( Q, Q, [a^2,b], [a^-2,b] );;
gap> pi := IdentityMapping( Pa );;
gap> homs := [ gamma, rho, xi, pi ];;
gap> mor1 := Cat2GroupMorphismByGroupHomomorphisms( C2a, C2a, homs );   
<mapping: (pre-)cat2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )] -> (pre-)cat
2-group with generating (pre-)cat1-groups:
1 : [d12 => Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )]
2 : [d12 => Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )] >
gap> upmor := Cat1GroupMorphism( up, up, gamma, rho );; 
gap> ltmor := Cat1GroupMorphism( left, left, gamma, xi );; 
gap> mor2 := Cat2GroupMorphismByCat1GroupMorphisms( C2a, C2a, upmor, ltmor );; 
gap> mor1 = mor2; 
true

# Section 8.5.7
gap> xsC2a := CrossedSquareOfCat2Group( C2a );;
gap> IdGroup( xsC2a );
[ [ 1, 1 ], [ 2, 1 ], [ 3, 1 ], [ 2, 1 ] ]

gap> SetName( Source( Right2DimensionalGroup( XSact ) ), "c5:c4" );
gap> SetName( Range( Right2DimensionalGroup( XSact ) ), "c5:c4" );
gap> Name( XSact );
"[d10a->c5:c4,d20->c5:c4]"

gap> C2act := Cat2GroupOfCrossedSquare( XSact );             
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [((c5:c4 |X c5:c4) |X (d20 |X d10a))=>(c5:c4 |X c5:c4)]
2 : [((c5:c4 |X c5:c4) |X (d20 |X d10a))=>(c5:c4 |X d20)]
gap> Size3d( C2act );
[ 80000, 400, 400, 20 ]

# Section 8.5.8
gap> G24 := SmallGroup( 24, 10 );; 
gap> w := G24.1;; x := G24.2;; y := G24.3;; z := G24.4;; o := One(G24);; 
gap> R24 := Subgroup( G24, [x,y] );; 
gap> txy := GroupHomomorphismByImages( G24, R24, [w,x,y,z], [o,x,y,o] );; 
gap> exy := GroupHomomorphismByImages( R24, G24, [x,y], [x,y] );; 
gap> C1xy := PreCat1GroupByTailHeadEmbedding( txy, txy, exy );; 
gap> Q24 := Subgroup( G24, [w,y] );; 
gap> twy := GroupHomomorphismByImages( G24, Q24, [w,x,y,z], [w,o,y,o] );; 
gap> ewy := GroupHomomorphismByImages( Q24, G24, [w,y], [w,y] );; 
gap> C1wy := PreCat1GroupByTailHeadEmbedding( twy, twy, ewy );; 
gap> C2wxy := PreCat2Group( C1xy, C1wy );; 
gap> dg := Diagonal2DimensionalGroup( C2wxy );;
gap> C1sub := Subdiagonal2DimensionalGroup( C2wxy );; 
gap> [ IsCat1Group(dg), IsCat1Group(C1sub), IsSub2DimensionalGroup(dg,C1sub) ];
[ false, true, true ]

# Section 8.5.9
gap> gps := GroupsOfHigherDimensionalGroup( C2ab );;
gap> c6c2 := Subgroup( gps[1], [ (1,2,3,4,5,6), (8,12)(9,11) ] );;
gap> c2c2 := Subgroup( gps[2], [ (1,4)(2,5)(3,6), (8,12)(9,11) ] );;
gap> c3c2 := Subgroup( gps[3], [ (1,5,3)(2,6,4), (8,12)(9,11) ] );;
gap> SC2ab := SubCat2Group( C2ab, c6c2, c2c2, c3c2 );;
gap> Display( SC2ab );              
(pre-)cat2-group with groups: [ Group( [ (1,2,3,4,5,6), ( 8,12)( 9,11) ] ), 
  Group( [ (1,4)(2,5)(3,6), ( 8,12)( 9,11) ] ), 
  Group( [ (1,5,3)(2,6,4), ( 8,12)( 9,11) ] ), 
  Group( [ (), ( 8,12)( 9,11) ] ) ]
   up tail=head: [ [ (1,2,3,4,5,6), ( 8,12)( 9,11) ], 
  [ (1,4)(2,5)(3,6), ( 8,12)( 9,11) ] ]
 left tail=head: [ [ (1,2,3,4,5,6), ( 8,12)( 9,11) ], 
  [ (1,5,3)(2,6,4), ( 8,12)( 9,11) ] ]
right tail=head: [ [ (1,4)(2,5)(3,6), ( 8,12)( 9,11) ], 
  [ (), ( 8,12)( 9,11) ] ]
 down tail=head: [ [ (1,5,3)(2,6,4), ( 8,12)( 9,11) ], [ (), ( 8,12)( 9,11) ] 
 ]

# Section 8.5.10
gap> TC2ab := TrivialSubCat2Group( C2ab );
(pre-)cat2-group with generating (pre-)cat1-groups:
1 : [Group( () ) => Group( () )]
2 : [Group( () ) => Group( () )]

# Section 8.6.1
gap> G8 := Group( (1,2), (3,4), (5,6) );;  SetName( G8, "G8" ); 
gap> A := Subgroup( G8, [ (1,2) ] );; 
gap> B := Subgroup( G8, [ (3,4) ] );;
gap> AllCat2GroupsWithImagesNumber( G8, A, A );
4
gap> all := AllCat2GroupsWithImages( G8, A, A );;     
gap> ## for C2 in all do DisplayLeadMaps( C2 ); od;
gap> AllCat2GroupsWithImagesNumber( G8, A, B );
16
gap> iso := AllCat2GroupsWithImagesUpToIsomorphism( G8, A, B );;
gap> for C2 in iso do DisplayLeadMaps( C2 ); od;
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail=head images: [ (), (3,4), () ]
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail/head images: [ (), (3,4), () ], [ (), (3,4), (3,4) ]
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail/head images: [ (1,2), (), () ], [ (1,2), (), (1,2) ]
 left tail/head images: [ (), (3,4), () ], [ (), (3,4), (3,4) ]

# Section 8.6.2
gap> up := Up2DimensionalGroup( iso[1] );;                
gap> AllCat2GroupsWithFixedUp( up );;                    
gap> Length(last);                                       
28
gap> L := AllCat2GroupsWithFixedUpAndLeftRange( up, B );;
gap> for C in L do DisplayLeadMaps( C ); od;             
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail=head images: [ (), (3,4), () ]
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail/head images: [ (), (3,4), () ], [ (), (3,4), (3,4) ]
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail/head images: [ (), (3,4), (3,4) ], [ (), (3,4), () ]
(pre-)cat2-group with up-left group: [ (1,2), (3,4), (5,6) ]
   up tail=head images: [ (1,2), (), () ]
 left tail=head images: [ (), (3,4), (3,4) ]

# Section 8.6.3
gap> AllCat2GroupsNumber( G );
41
gap> reps2 := AllCat2GroupsUpToIsomorphism( G );;
gap> Length( reps2 );
10
gap> List( reps2, C -> StructureDescription( C ) );
[ [ "D12", "C2", "C2", "C2" ], [ "D12", "C2", "C2 x C2", "C2" ], 
  [ "D12", "C2", "S3", "C2" ], [ "D12", "C2", "D12", "C2" ], 
  [ "D12", "C2 x C2", "C2 x C2", "C2 x C2" ], [ "D12", "C2 x C2", "S3", "C2" ]
    , [ "D12", "C2 x C2", "D12", "C2 x C2" ], [ "D12", "S3", "S3", "S3" ], 
  [ "D12", "S3", "D12", "S3" ], [ "D12", "D12", "D12", "D12" ] ]
gap> fams := AllCat2GroupFamilies( G );
[ [ 1, 2, 3, 4, 5, 6 ], [ 7, 8, 10, 11, 13, 14 ], [ 16, 17, 18, 23, 24, 25 ], 
  [ 30, 31, 32, 33, 34, 35 ], [ 9, 12, 15 ], [ 19, 20, 21, 26, 27, 28 ], 
  [ 36, 37, 38 ], [ 22, 29 ], [ 39, 40 ], [ 41 ] ]
gap> CatnGroupNumbers( G );
rec( cat1 := 12, cat2 := 41, idem := 21, iso1 := 4, iso2 := 10, 
  isopredg := 0, predg := 0, siso := 4, symm := 12 )
gap> CatnGroupLists( G );
rec( allcat2pos := [ 1, 7, 9, 16, 19, 22, 30, 36, 39, 41 ],
  cat2classes := 
    [ [ [ 1, 1 ], [ 2, 2 ], [ 3, 3 ], [ 4, 4 ], [ 5, 5 ], [ 6, 6 ] ], 
      [ [ 1, 7 ], [ 5, 7 ], [ 2, 8 ], [ 6, 8 ], [ 3, 9 ], [ 4, 9 ] ], 
      [ [ 1, 10 ], [ 2, 10 ], [ 3, 10 ], [ 4, 11 ], [ 5, 11 ], [ 6, 11 ] ], 
      [ [ 1, 12 ], [ 2, 12 ], [ 3, 12 ], [ 4, 12 ], [ 5, 12 ], [ 6, 12 ] ], 
      [ [ 7, 7 ], [ 8, 8 ], [ 9, 9 ] ], 
      [ [ 7, 10 ], [ 8, 10 ], [ 9, 10 ], [ 7, 11 ], [ 8, 11 ], [ 9, 11 ] ], 
      [ [ 7, 12 ], [ 8, 12 ], [ 9, 12 ] ], [ [ 10, 10 ], [ 11, 11 ] ], 
      [ [ 10, 12 ], [ 11, 12 ] ], [ [ 12, 12 ] ] ], 
  cat2pairs := [ [ 1, 1 ], [ 1, 7 ], [ 1, 10 ], [ 1, 12 ], [ 2, 2 ], 
      [ 2, 8 ], [ 2, 10 ], [ 2, 12 ], [ 3, 3 ], [ 3, 9 ], [ 3, 10 ], 
      [ 3, 12 ], [ 4, 4 ], [ 4, 9 ], [ 4, 11 ], [ 4, 12 ], [ 5, 5 ], 
      [ 5, 7 ], [ 5, 11 ], [ 5, 12 ], [ 6, 6 ], [ 6, 8 ], [ 6, 11 ], 
      [ 6, 12 ], [ 7, 7 ], [ 7, 10 ], [ 7, 11 ], [ 7, 12 ], [ 8, 8 ], 
      [ 8, 10 ], [ 8, 11 ], [ 8, 12 ], [ 9, 9 ], [ 9, 10 ], [ 9, 11 ], 
      [ 9, 12 ], [ 10, 10 ], [ 10, 12 ], [ 11, 11 ], [ 11, 12 ], [ 12, 12 ] ],
  omit := false, pisopos := [  ], sisopos := [ 1, 5, 8, 10 ] )

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp3cat2.tst", 10000 );
