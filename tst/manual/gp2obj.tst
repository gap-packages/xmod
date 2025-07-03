#############################################################################
##
#W  gp2obj.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gp2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

## Chapter 2,  Section 2.1.1
gap> c5 := Group( (5,6,7,8,9) );;
gap> SetName( c5, "c5" );
gap> id5 := IdentityMapping( c5 );;
gap> ac5 := AutomorphismGroup( c5 );; 
gap> act := MappingToOne( c5, ac5 );;
gap> XMod( id5, act ) = XModByBoundaryAndAction( id5, act );
true

## Section 2.1.3
gap> q8 := QuaternionGroup( IsPermGroup, 8 );
Group([ (1,5,3,7)(2,8,4,6), (1,2,3,4)(5,6,7,8) ])
gap> SetName( q8, "q8" );
gap> c2 := Centre( q8 );                     
Group([ (1,3)(2,4)(5,7)(6,8) ])
gap> SetName( c2, "<-1>" );
gap> bdy := InclusionMappingGroups( q8, c2 );;
gap> X8a := XModByTrivialAction( bdy );
[<-1>->q8]
gap> c4 := Subgroup( q8, [q8.1] );;
gap> SetName( c4, "<i>" );
gap> X8b := XModByNormalSubgroup( q8, c4 );
[<i>->q8]
gap> Display(X8b);        
Crossed module [<i>->q8] :- 
: Source group has generators:
  [ (1,5,3,7)(2,8,4,6) ]
: Range group q8 has generators:
  [ (1,5,3,7)(2,8,4,6), (1,2,3,4)(5,6,7,8) ]
: Boundary homomorphism maps source generators to:
  [ (1,5,3,7)(2,8,4,6) ]
: Action homomorphism maps range generators to automorphisms:
  (1,5,3,7)(2,8,4,6) --> { source gens --> [ (1,5,3,7)(2,8,4,6) ] }
  (1,2,3,4)(5,6,7,8) --> { source gens --> [ (1,7,3,5)(2,6,4,8) ] }
  These 2 automorphisms generate the group of automorphisms.

## Section 2.1.4
gap> X5 := XModByAutomorphismGroup( c5 );
[c5->Aut(c5)]
gap> Display( X5 );
Crossed module [c5->Aut(c5)] :- 
: Source group c5 has generators:
  [ (5,6,7,8,9) ]
: Range group Aut(c5) has generators:
  [ GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], [ (5,7,9,6,8) ] ) ]
: Boundary homomorphism maps source generators to:
  [ IdentityMapping( c5 ) ]
: Action homomorphism maps range generators to automorphisms:
  GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], 
[ (5,7,9,6,8) ] ) --> { source gens --> [ (5,7,9,6,8) ] }
  This automorphism generates the group of automorphisms.

## Section 2.1.5
gap> gen12 := [ (1,2,3,4,5,6), (2,6)(3,5) ];;
gap> d12 := Group( gen12 );;                  
gap> gen6 := [ (7,8,9), (8,9) ];;
gap> s3 := Group( gen6 );;
gap> SetName( d12, "d12" );  SetName( s3, "s3" ); 
gap> pr12 := GroupHomomorphismByImages( d12, s3, gen12, gen6 );;
gap> Kernel( pr12 ) = Centre( d12 );
true
gap> X12 := XModByCentralExtension( pr12 );;
gap> Display( X12 );                         
Crossed module [d12->s3] :- 
: Source group d12 has generators:
  [ (1,2,3,4,5,6), (2,6)(3,5) ]
: Range group s3 has generators:
  [ (7,8,9), (8,9) ]
: Boundary homomorphism maps source generators to:
  [ (7,8,9), (8,9) ]
: Action homomorphism maps range generators to automorphisms:
  (7,8,9) --> { source gens --> [ (1,2,3,4,5,6), (1,3)(4,6) ] }
  (8,9) --> { source gens --> [ (1,6,5,4,3,2), (2,6)(3,5) ] }
  These 2 automorphisms generate the group of automorphisms.

## Section 2.1.6
gap> gens4 := [ (11,12), (12,13), (13,14) ];; 
gap> s4 := Group( gens4 );; 
gap> theta := GroupHomomorphismByImages( s4, s3, gens4, [(7,8),(8,9),(7,8)] );;
gap> X1 := XModByPullback( X12, theta );; 
gap> StructureDescription( Source( X1 ) );
"C2 x S4"
gap> SetName( s4, "s4" );  SetName( Source( X1 ), "c2s4" ); 
gap> infoX1 := PullbackInfo( Source( X1 ) );;
gap> infoX1!.directProduct;
Group([ (1,2,3,4,5,6), (2,6)(3,5), (7,8), (8,9), (9,10) ])
gap> infoX1!.projections[1];
[ (7,8)(9,10), (7,9)(8,10), (2,6)(3,5)(8,9), (1,5,3)(2,6,4)(8,10,9), 
  (1,6,5,4,3,2)(8,9,10) ] -> [ (), (), (2,6)(3,5), (1,5,3)(2,6,4), 
  (1,6,5,4,3,2) ]
gap> infoX1!.projections[2];
[ (7,8)(9,10), (7,9)(8,10), (2,6)(3,5)(8,9), (1,5,3)(2,6,4)(8,10,9), 
  (1,6,5,4,3,2)(8,9,10) ] -> [ (11,12)(13,14), (11,13)(12,14), (12,13), 
  (12,14,13), (12,13,14) ]

## Section 2.1.8
gap> X8ab := DirectProduct( X8a, X8b );
[[<-1>->q8]x[<i>->q8]]
gap> infoX8ab := DirectProductInfo( X8ab );
rec( embeddings := [ [[<-1>->q8] => [..]], [[<i>->q8] => [..]] ], 
  objects := [ [<-1>->q8], [<i>->q8] ], 
  projections := [ [[..] => [<-1>->q8]], [[..] => [<i>->q8]] ] )
gap> DirectProduct( X8a, X8b, X12 );
[[[<-1>->q8]x[<i>->q8]]x[d12->s3]]

## Section 2.1.9
gap> [ Source( X12 ), Range( X12 ) ];    
[ d12, s3 ]
gap> Boundary( X12 ); 
[ (1,2,3,4,5,6), (2,6)(3,5) ] -> [ (7,8,9), (8,9) ]
gap> XModAction( X12 );
[ (7,8,9), (8,9) ] -> 
[ [ (1,2,3,4,5,6), (2,6)(3,5) ] -> [ (1,2,3,4,5,6), (1,3)(4,6) ], 
  [ (1,2,3,4,5,6), (2,6)(3,5) ] -> [ (1,6,5,4,3,2), (2,6)(3,5) ] ]

## Section 2.1.10
gap> ImageElmXModAction( X12, (1,2,3,4,5,6), (8,9) );
(1,6,5,4,3,2)

## Section 2.1.11
gap> Size2d( X5 );
[ 5, 4 ]

## Section 2.1.12
gap> IdGroup( X5 ); 
[ [ 5, 1 ], [ 4, 1 ] ]
gap> ext := ExternalSetXMod( X5 ); 
<xset:[ (), (5,6,7,8,9), (5,7,9,6,8), (5,8,6,9,7), (5,9,8,7,6) ]>
gap> Orbits( ext );
[ [ () ], [ (5,6,7,8,9), (5,7,9,6,8), (5,9,8,7,6), (5,8,6,9,7) ] ]
gap> a := GeneratorsOfGroup( Range( X5 ) )[1]^2; 
[ (5,6,7,8,9) ] -> [ (5,9,8,7,6) ]
gap> ImageElmXModAction( X5, (5,7,9,6,8), a );
(5,8,6,9,7)
gap> Print( RepresentationsOfObject( X5 ), "\n" );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPreXModObj" ]

gap> kpa := KnownAttributesOfObject( X5 );; 
gap> Set( kpa ); 
[ "Boundary", "ExternalSetXMod", "HigherDimension", "IdGroup", "Name", 
  "Range", "Size2d", "Source", "XModAction" ]

## Section 2.2.1 
gap> [ IsTrivial( X5 ),  IsNonTrivial( X5 ),  IsFinite( X5 ) ];
[ false, true, true ]
gap> IsAssociative( X5 ); 
true 
gap> kpo := KnownPropertiesOfObject( X5 );;
gap> Set( kpo );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsAssociative", 
  "IsAutomorphismGroup2DimensionalGroup", "IsDuplicateFree", "IsFinite", 
  "IsGeneratorsOfSemigroup", "IsNonTrivial", "IsPreXMod", "IsPreXModDomain", 
  "IsTrivial", "IsXMod" ]
gap> Is2DimensionalGroup( X5 );
true
gap> IsAutomorphismGroup2DimensionalGroup( X5 );
true

## Section 2.2.2
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> SetName(s4,"s4");  SetName(a4,"a4");  SetName(k4,"k4"); 
gap> X4 := XModByNormalSubgroup( s4, a4 );
[a4->s4]
gap> Y4 := SubXMod( X4, k4, a4 ); 
[k4->a4]
gap> IsNormal( X4, Y4 ); 
true
gap> NX4 := NormalSubXMods( X4 );;
gap> Length( NX4 ); 
5

## Section 2.2.3
gap> d8d8 := Group( (1,2,3,4), (1,3), (5,6,7,8), (5,7) );;
gap> X88 := XModByAutomorphismGroup( d8d8 );;
gap> Size2d( X88 );
[ 64, 2048 ]
gap> Y88 := KernelCokernelXMod( X88 );;
gap> IdGroup(Y88);
[ [ 4, 2 ], [ 128, 928 ] ]

## gap> StructureDescription( Y88 );
## [ "C2 x C2", "(D8 x D8) : C2" ] or [ "C2 x C2", "(C2 x D8) : D8" ] 

## Section 2.3.1
gap> b1 := (11,12,13,14,15,16,17,18);;  b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> sk4 := Subgroup( d16, [ b1^4, b2 ] );;
gap> SetName( d16, "d16" );  SetName( sk4, "sk4" );
gap> bdy16 := GroupHomomorphismByImages( d16, sk4, [b1,b2], [b1^4*b2,b2] );;
gap> aut1 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1^5,b2] );;
gap> aut2 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1,b1^4*b2] );;
gap> aut16 := Group( [ aut1, aut2 ] );;
gap> act16 := GroupHomomorphismByImages( sk4, aut16, [b1^4,b2], [aut1,aut2] );;
gap> P16 := PreXModByBoundaryAndAction( bdy16, act16 );
[d16->sk4]
gap> IsXMod( P16 );
false
gap> Q16 := PreXModWithTrivialRange( d16, d16 ); 
[d16->Group( [ () ] )]
gap> SQ16 := SubPreXMod( Q16, sk4, Group( [()] ) ); 
[sk4->Group( [ () ] )]

## Section 2.3.2
gap> P := PeifferSubgroup( P16 );
Group([ (11,15)(12,16)(13,17)(14,18), (11,13,15,17)(12,14,16,18) ])
gap> X16 := XModByPeifferQuotient( P16 );
Peiffer([d16->sk4])
gap> Display( X16 );
Crossed module Peiffer([d16->sk4]) :- 
: Source group has generators:
  [ f1, f2 ]
: Range group has generators:
  [ (11,15)(12,16)(13,17)(14,18), (12,18)(13,17)(14,16) ]
: Boundary homomorphism maps source generators to:
  [ (12,18)(13,17)(14,16), (11,15)(12,14)(16,18) ]
  The automorphism group is trivial

gap> iso16 := IsomorphismPermGroup( Source( X16 ) );;
gap> S16 := Image( iso16 );
Group([ (1,2), (3,4) ])

## Section 2.4.1
gap> g18gens := [ (1,2,3), (4,5,6), (2,3)(5,6) ];;     
gap> s3agens := [ (7,8,9), (8,9) ];;                
gap> g18 := Group( g18gens );;  SetName( g18, "g18" ); 
gap> s3a := Group( s3agens );;  SetName( s3a, "s3a" );
gap> t1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(),(8,9)]);     
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (), (8,9) ]
gap> h1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(7,8,9),(8,9)]);
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (7,8,9), (8,9) ]
gap> e1 := GroupHomomorphismByImages(s3a,g18,s3agens,[(1,2,3),(2,3)(5,6)]);   
[ (7,8,9), (8,9) ] -> [ (1,2,3), (2,3)(5,6) ]
gap> C18 := Cat1Group( t1, h1, e1 );
[g18=>s3a]

## Section 2.4.2
gap> [ Source( C18 ), Range( C18 ) ];
[ g18, s3a ]
gap> TailMap( C18 );
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (), (8,9) ]
gap> HeadMap( C18 );
[ (1,2,3), (4,5,6), (2,3)(5,6) ] -> [ (7,8,9), (7,8,9), (8,9) ]
gap> RangeEmbedding( C18 );
[ (7,8,9), (8,9) ] -> [ (1,2,3), (2,3)(5,6) ]
gap> Kernel( C18 );
Group([ (4,5,6) ])
gap> KernelEmbedding( C18 );
[ (4,5,6) ] -> [ (4,5,6) ]
gap> Name( C18 );
"[g18=>s3a]"
gap> Size2d( C18 );
[ 18, 6 ]
gap> StructureDescription( C18 );
[ "(C3 x C3) : C2", "S3" ]

## Section 2.4.3
gap> C4 := DiagonalCat1Group( [ (1,2,3), (2,3,4) ] );;
gap> SetName( Source(C4), "a4a4" );  SetName( Range(C4), "a4d" );
gap> Display( C4 );
Cat1-group [a4a4=>a4d] :- 
: Source group a4a4 has generators:
  [ (1,2,3), (2,3,4), (5,6,7), (6,7,8) ]
: Range group a4d has generators:
  [ ( 9,10,11), (10,11,12) ]
: tail homomorphism maps source generators to:
  [ ( 9,10,11), (10,11,12), (), () ]
: head homomorphism maps source generators to:
  [ (), (), ( 9,10,11), (10,11,12) ]
: range embedding maps range generators to:
  [ (1,2,3)(5,6,7), (2,3,4)(6,7,8) ]
: kernel has generators:
  [ (5,6,7), (6,7,8) ]
: boundary homomorphism maps generators of kernel to:
  [ ( 9,10,11), (10,11,12) ]
: kernel embedding maps generators of kernel to:
  [ (5,6,7), (6,7,8) ]

## Section 2.4.4
gap> R4 := TransposeCat1Group( C4 );
[a4a4=>a4d]
gap> Boundary( R4 );
[ (2,3,4), (1,2,3) ] -> [ (10,11,12), (9,10,11) ]
gap> TailMap( R4 ) = HeadMap( R4 ); 
false
gap> TailMap( R4 ) = HeadMap( C4 ); 
true
gap> MappingGeneratorsImages( TransposeIsomorphism(C4) );
[ [ [ (1,2,3), (2,3,4), (5,6,7), (6,7,8) ], 
      [ (5,6,7), (6,7,8), (1,2,3), (2,3,4) ] ], 
  [ [ (9,10,11), (10,11,12) ], [ (9,10,11), (10,11,12) ] ] ]

## Section 2.4.5
gap> s4 := Group( (1,2,3), (3,4) );;  SetName( s4, "s4" ); 
gap> k4 := Subgroup( s4, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> h := GroupHomomorphismByImages( s4, s4, [(1,2,3),(3,4)], [(),(3,4)] );;
gap> c2 := Image( h );;  SetName( c2, "c2" );
gap> C := PreCat1Group( h, h );
[s4=>c2]
gap> P := PeifferSubgroupPreCat1Group( C );;
gap> P = k4;
true
gap> C2 := Cat1GroupByPeifferQuotient( C );
[Group( [ f1, f2 ] )=>c2]
gap> StructureDescription( C2 );
[ "S3", "C2" ]
gap> rec2 := PreXModRecordOfPreCat1Group( C );;
gap> XC := rec2.prexmod;;
gap> StructureDescription( XC );  
[ "A4", "C2" ]
gap> XC2 := XModByPeifferQuotient( XC );;
gap> StructureDescription( XC2 );
[ "C3", "C2" ]
gap> CXC2 := Cat1GroupOfXMod( XC2 );;
gap> StructureDescription( CXC2 );
[ "S3", "C2" ]
gap> IsomorphismCat1Groups( C2, CXC2 );
[[Group( [ f1, f2 ] ) => c2] => [(..|X..) => c2]]

## Section 2.4.6
gap> s3 := Subgroup( s4, [(2,3),(3,4)] );;
gap> res := GeneralRestrictedMapping( h, s3, s3 );;
gap> S := PreCat1Group( res, res );
[Group( [ (2,3), (3,4) ] )=>Group( [ (3,4), (3,4) ] )]

## Section 2.4.7
gap> C418 := DirectProduct( C4, C18 );
[(a4a4xg18)=>(a4d x s3a)]
gap> infoC418 := DirectProductInfo( C418 );
rec( 
  embeddings := [ [[a4a4=>a4d] => [(a4a4xg18)=>(a4d x s3a)]], 
      [[g18=>s3a] => [(a4a4xg18)=>(a4d x s3a)]] ], 
  objects := [ [a4a4=>a4d], [g18=>s3a] ], 
  projections := [ [[(a4a4xg18)=>(a4d x s3a)] => [a4a4=>a4d]], 
      [[(a4a4xg18)=>(a4d x s3a)] => [g18=>s3a]] ] )
gap> t418 := TailMap( C418 );
[ (1,2,3), (2,3,4), (5,6,7), (6,7,8), (9,10,11), (12,13,14), (10,11)(13,14) 
 ] -> [ (1,2,3), (2,3,4), (), (), (5,6,7), (), (6,7) ]
gap> h418 := HeadMap( C418 );
[ (1,2,3), (2,3,4), (5,6,7), (6,7,8), (9,10,11), (12,13,14), (10,11)(13,14) 
 ] -> [ (), (), (1,2,3), (2,3,4), (5,6,7), (5,6,7), (6,7) ]
gap> e418 := RangeEmbedding( C418 );
[ (1,2,3), (2,3,4), (5,6,7), (6,7) ] -> [ (1,2,3)(5,6,7), (2,3,4)(6,7,8), 
  (9,10,11), (10,11)(13,14) ]

## Section 2.5.1
gap> G8 := SmallGroup( 288, 956 );  SetName( G8, "G8" );
<pc group of size 288 with 7 generators>
gap> d12 := DihedralGroup( 12 );  SetName( d12, "d12" );
<pc group of size 12 with 3 generators>
gap> a1 := d12.1;;  a2 := d12.2;;  a3 := d12.3;;  a0 := One( d12 );;
gap> gensG8 := GeneratorsOfGroup( G8 );;
gap> t8 := GroupHomomorphismByImages( G8, d12, gensG8,
>           [ a0, a1*a3, a2*a3, a0, a0, a3, a0 ] );;
gap> h8 := GroupHomomorphismByImages( G8, d12, gensG8,
>           [ a1*a2*a3, a0, a0, a2*a3, a0, a0, a3^2 ] );;                   
gap> e8 := GroupHomomorphismByImages( d12, G8, [a1,a2,a3],
>        [ G8.1*G8.2*G8.4*G8.6^2, G8.3*G8.4*G8.6^2*G8.7, G8.6*G8.7^2 ] );;
gap> C8 := PreCat1GroupByTailHeadEmbedding( t8, h8, e8 );
[G8=>d12]
gap> IsCat1Group( C8 );
true
gap> Display(C8);
Cat1-group [G8=>d12] :- 
: Source group G8 has generators:
  [ f1, f2, f3, f4, f5, f6, f7 ]
: Range group d12 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., f1*f3, f2*f3, <identity> of ..., <identity> of ..., 
  f3, <identity> of ... ]
: head homomorphism maps source generators to:
  [ f1*f2*f3, <identity> of ..., <identity> of ..., f2*f3, <identity> of ..., 
  <identity> of ..., f3^2 ]
: range embedding maps range generators to:
  [ f1*f2*f4*f6^2, f3*f4*f6^2*f7, f6*f7^2 ]
: kernel has generators:
  [ f1, f4, f5, f7 ]
: boundary homomorphism maps generators of kernel to:
  [ f1*f2*f3, f2*f3, <identity> of ..., f3^2 ]
: kernel embedding maps generators of kernel to:
  [ f1, f4, f5, f7 ]

gap> IsCat1Group( C8 );
   true

## Section 2.5.2
gap> G5 := Group( (1,2,3,4,5) );;                                             
gap> t := GroupHomomorphismByImages( G5, G5, [(1,2,3,4,5)], [(1,5,4,3,2)] );;
gap> PC5 := PreCat1GroupByTailHeadEmbedding( t, t, t );
[Group( [ (1,2,3,4,5) ] )=>Group( [ (1,2,3,4,5) ] )]
gap> IsPreCat1GroupWithIdentityEmbedding( PC5 );
false
gap> IPC5 := IsomorphicPreCat1GroupWithIdentityEmbedding( PC5 );
[Group( [ (1,2,3,4,5) ] )=>Group( [ (1,2,3,4,5) ] )]
gap> TailMap( IPC5 ); RangeEmbedding( IPC5 );
[ (1,2,3,4,5) ] -> [ (1,2,3,4,5) ]
[ (1,2,3,4,5) ] -> [ (1,2,3,4,5) ]

## Section 2.5.3
gap> X8 := XModOfCat1Group( C8 );;
gap> Display( X8 );
Crossed module xmod([G8=>d12]) :- 
: Source group has generators:
  [ f1, f4, f5, f7 ]
: Range group d12 has generators:
  [ f1, f2, f3 ]
: Boundary homomorphism maps source generators to:
  [ f1*f2*f3, f2*f3, <identity> of ..., f3^2 ]
: Action homomorphism maps range generators to automorphisms:
  f1 --> { source gens --> [ f1*f5, f4*f5, f5, f7^2 ] }
  f2 --> { source gens --> [ f1*f5*f7^2, f4, f5, f7 ] }
  f3 --> { source gens --> [ f1*f7, f4, f5, f7 ] }
  These 3 automorphisms generate the group of automorphisms.
: associated cat1-group is [G8=>d12]

gap> StructureDescription( X8 );
[ "D24", "D12" ]

## Section 2.6.1
gap> d12 := DihedralGroup( IsPermGroup, 12 );  SetName( d12, "d12" );
Group([ (1,2,3,4,5,6), (2,6)(3,5) ])
gap> c2 := Subgroup( d12, [ (1,6)(2,5)(3,4) ] );; 
gap> AllCat1GroupsWithImageNumber( d12, c2 );
1
gap> L12 := AllCat1GroupsWithImage( d12, c2 );
[ [d12=>Group( [ (), (1,6)(2,5)(3,4) ] )] ]

## Section 2.6.2
gap> qd16 := SmallGroup( 16, 8 );; 
gap> AllCat1GroupsMatrix( qd16 );;                 
number of idempotent endomorphisms found = 10
number of cat1-groups found = 5
number of additional pre-cat1-groups found = 9
1.........
.21.......
.11.......
...21.....
...11.....
.....21...
.....11...
.......21.
.......11.
.........2

## Section 2.6.3
gap> iter := AllCat1GroupsIterator( d12 );;
gap> AllCat1GroupsNumber( d12 );
12
gap> iso12 := AllCat1GroupsUpToIsomorphism( d12 );
[ [d12=>Group( [ (), (2,6)(3,5) ] )], 
  [d12=>Group( [ (1,4)(2,5)(3,6), (2,6)(3,5) ] )], 
  [d12=>Group( [ (1,5,3)(2,6,4), (2,6)(3,5) ] )], 
  [d12=>Group( [ (1,2,3,4,5,6), (2,6)(3,5) ] )] ]

## Section 2.6.4
gap> CatnGroupNumbers( d12 );
rec( cat1 := 12, idem := 21, iso1 := 4, siso := 4, symm := 12 )

## Section 2.7.1
gap> SetInfoLevel( InfoXMod, 1 );
gap> L18 := Cat1Select( 18 ); 
#I  Usage:  Cat1Select( size, gpnum, num );  where gpnum <= 5
fail
gap> L18_4 := Cat1Select( 18, 4 ); 
#I  Usage:  Cat1Select( size, gpnum, num );  where num <= 4
fail
gap> SetInfoLevel( InfoXMod, 0 );
gap> B18 := Cat1Select( 18, 4, 2 );
[(C3 x C3) : C2=>Group( [ f1, <identity> of ..., f3 ] )]
gap> iso18 := IsomorphismPermObject( B18 );;
gap> PB18 := Image( iso18 );;
gap> Display( PB18 );
Cat1-group :- 
: Source group has generators:
  [ (4,5,6), (1,2,3), (2,3)(5,6) ]
: Range group has generators:
  [ (4,5,6), (2,3)(5,6) ]
: tail homomorphism maps source generators to:
  [ (4,5,6), (), (2,3)(5,6) ]
: head homomorphism maps source generators to:
  [ (4,5,6), (), (2,3)(5,6) ]
: range embedding maps range generators to:
  [ (4,5,6), (2,3)(5,6) ]
: kernel has generators:
  [ (1,2,3) ]
: boundary homomorphism maps generators of kernel to:
  [ () ]
: kernel embedding maps generators of kernel to:
  [ (1,2,3) ]

gap> Y18 := XModOfCat1Group( PB18 );;
gap> Display( Y18 );
Crossed module :- 
: Source group has generators:
  [ (1,2,3) ]
: Range group has generators:
  [ (4,5,6), (2,3)(5,6) ]
: Boundary homomorphism maps source generators to:
  [ () ]
: Action homomorphism maps range generators to automorphisms:
  (4,5,6) --> { source gens --> [ (1,2,3) ] }
  (2,3)(5,6) --> { source gens --> [ (1,3,2) ] }
  These 2 automorphisms generate the group of automorphisms.
: associated cat1-group is [Group( [ (4,5,6), (1,2,3), (2,3)(5,6) 
 ] ) => Group( [ (4,5,6), (2,3)(5,6) ] )]

## Section 2.8.1
gap> IdGroup( X8 );
[ [ 24, 6 ], [ 12, 4 ] ]
gap> IdGroup( C8 );
[ [ 288, 956 ], [ 12, 4 ] ]

## Section 2.8.2
gap> IsSubXMod( X4, Y4 );
true
gap> IsSubPreCat1Group( C, S );
true

## Section 2.9.1
gap> s3 := Group( (11,12), (12,13) );; 
gap> c3c3 := Group( [ (14,15,16), (17,18,19) ] );; 
gap> bdy := GroupHomomorphismByImages( c3c3, s3, 
>        [(14,15,16),(17,18,19)], [(11,12,13),(11,12,13)] );;
gap> a := GroupHomomorphismByImages( c3c3, c3c3, 
>        [(14,15,16),(17,18,19)], [(14,16,15),(17,19,18)] );; 
gap> aut := Group( [a] );; 
gap> act := GroupHomomorphismByImages( s3, aut, [(11,12),(12,13)], [a,a] );;
gap> X33 := XModByBoundaryAndAction( bdy, act );; 
gap> C33 := Cat1GroupOfXMod( X33 );; 
gap> G33 := Source( C33 );; 
gap> gpd33 := GroupGroupoid( C33 );;
gap> ObjectList( gpd33 );
[ (), (12,13), (11,12), (11,12,13), (11,13,12), (11,13) ]
gap> p1 := Pieces( gpd33 )[1];; 
gap> Set( RaysOfGroupoid( p1 ) );  
[ ()>-()->(), ()>-(7,8,9)->(11,12,13), ()>-(7,9,8)->(11,13,12) ]
gap> p2 := Pieces( gpd33 )[2];; 
gap> Set( RaysOfGroupoid( p2 ) );  
[ (12,13)>-(2,3)(5,6)(8,9)->(12,13), (12,13)>-(2,3)(5,6)(7,8)->(11,12), 
  (12,13)>-(2,3)(5,6)(7,9)->(11,13) ]

## Section 2.9.2 
gap> piece2 := Pieces( gpd33 )[2];;
gap> obs2 := piece2!.objects; 
[ (12,13), (11,12), (11,13) ]
gap> RaysOfGroupoid( piece2 );
[ (12,13)>-(2,3)(5,6)(8,9)->(12,13), (12,13)>-(2,3)(5,6)(7,9)->(11,13), 
  (12,13)>-(2,3)(5,6)(7,8)->(11,12) ]
gap> g1 := (1,2)(5,6)(7,9);; 
gap> g2 := (2,3)(4,5)(7,8);;                         
gap> g1 * g2;
(1,3,2)(4,5,6)(7,9,8)
gap> e1 := GroupGroupoidElement( C33, (12,13), g1 ); 
(11,12)>-(1,2)(5,6)(7,9)->(12,13)
gap> e2 := GroupGroupoidElement( C33, (12,13), g2 );
(12,13)>-(2,3)(4,5)(7,8)->(11,13)
gap> e1*e2;
(11,12)>-(1,2)(4,5)(8,9)->(11,13)
gap> e2^-1;
(11,13)>-(1,3)(4,6)(7,9)->(12,13)
gap> ## obgp := ObjectGroup( gpd33, (11,12) );;
gap> ## GeneratorsOfGroup( obgp )[1];
gap> ## Homset( gpd33, (11,12), (11,13) );

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp2obj.tst", 10000 );
