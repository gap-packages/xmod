#############################################################################
##
#W  gp2map.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2023, Chris Wensley et al, 
##
##
gap> START_TEST( "XMod package: gp2map.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

##  make this test independent of gp2obj.tst
gap> c5 := Group( (5,6,7,8,9) );;
gap> SetName( c5, "c5" );
gap> X5 := XModByAutomorphismGroup( c5 );;
gap> G8 := SmallGroup( 288, 956 );;  
gap> SetName( G8, "G8" );
gap> d12 := DihedralGroup( 12 );;  
gap> SetName( d12, "d12" );
gap> a1 := d12.1;;  a2 := d12.2;;  a3 := d12.3;;  a0 := One( d12 );;
gap> gensG8 := GeneratorsOfGroup( G8 );;
gap> g18gens := [ (1,2,3), (4,5,6), (2,3)(5,6) ];;
gap> g18 := Group( g18gens );; 
gap> SetName( g18, "g18" );
gap> s3agens := [ (7,8,9), (8,9) ];;
gap> s3a := Group( s3agens );; 
gap> SetName( s3a, "s3a" );
gap> t1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(),(8,9)]);;
gap> h1 := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(7,8,9),(8,9)]);;
gap> e1 := GroupHomomorphismByImages(s3a,g18,s3agens,[(1,2,3),(2,3)(5,6)]);;
gap> C18 := Cat1Group( t1, h1, e1 );;
gap> t8 := GroupHomomorphismByImages( G8, d12, gensG8,
>           [ a0, a1*a3, a2*a3, a0, a0, a3, a0 ] );;
gap> h8 := GroupHomomorphismByImages( G8, d12, gensG8,
>           [ a1*a2*a3, a0, a0, a2*a3, a0, a0, a3^2 ] );;                   
gap> e8 := GroupHomomorphismByImages( d12, G8, [a1,a2,a3],
>        [ G8.1*G8.2*G8.4*G8.6^2, G8.3*G8.4*G8.6^2*G8.7, G8.6*G8.7^2 ] );;
gap> C8 := PreCat1GroupByTailHeadEmbedding( t8, h8, e8 );;
gap> X8 := XModOfCat1Group( C8 );;


## Chapter 3

## Section 3.2.3
gap> sigma5 := GroupHomomorphismByImages(c5,c5,[(5,6,7,8,9)],[(5,9,8,7,6)] );;
gap> rho5 := IdentityMapping( Range(X5) );;
gap> mor5 := XModMorphism( X5, X5, sigma5, rho5 );
[[c5->Aut(c5)] => [c5->Aut(c5)]]
gap> Display( mor5 );
Morphism of crossed modules :- 
: Source = [c5->Aut(c5)] with generating sets:
  [ (5,6,7,8,9) ]
  [ GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], [ (5,7,9,6,8) ] ) ]
: Range = Source
: Source Homomorphism maps source generators to:
  [ (5,9,8,7,6) ]
: Range Homomorphism maps range generators to:
  [ GroupHomomorphismByImages( c5, c5, [ (5,6,7,8,9) ], [ (5,7,9,6,8) ] ) ]
gap> IsAutomorphism2DimensionalDomain( mor5 );
true
gap> Order( mor5 );
2
gap> RepresentationsOfObject( mor5 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "Is2DimensionalMappingRep" ]
gap> kpo := KnownPropertiesOfObject( mor5 );; 
gap> Set( kpo ); 
[ "CanEasilyCompareElements", "CanEasilySortElements", 
  "IsAutomorphism2DimensionalDomain", "IsEndomorphism2DimensionalDomain", 
  "IsInjective", "IsPreXModMorphism", "IsSingleValued", "IsSurjective", 
  "IsTotal", "IsXModMorphism", "RespectsMultiplication" ]
gap> kao := KnownAttributesOfObject( mor5 );;
gap> Set( kao ); 
[ "Name", "Order", "Range", "RangeHom", "Source", "SourceHom" ]

## Section 3.2.4
gap> q8 := SmallGroup(8,4);;   ## quaternion group 
gap> XAq8 := XModByAutomorphismGroup( q8 );
[Group( [ f1, f2, f3 ] )->Group( [ Pcgs([ f1, f2, f3 ]) -> [ f1*f2, f2, f3 ], 
  Pcgs([ f1, f2, f3 ]) -> [ f2, f1*f2, f3 ], 
  Pcgs([ f1, f2, f3 ]) -> [ f1*f3, f2, f3 ], 
  Pcgs([ f1, f2, f3 ]) -> [ f1, f2*f3, f3 ] ] )]
gap> iso := IsomorphismPerm2DimensionalGroup( XAq8 );;
gap> YAq8 := Image( iso );;
gap> s4 := SymmetricGroup(4);; 
gap> isos4 := IsomorphismGroups( Range(YAq8), s4 );;
gap> id := IdentityMapping( Source( YAq8 ) );; 
gap> IsBijective( id );;  IsBijective( isos4 );;
gap> mor := IsomorphismByIsomorphisms( YAq8, [id,isos4] );;
gap> ZAq8 := Image( mor );;

## Section 3.3.1
gap> t3 := GroupHomomorphismByImages(g18,s3a,g18gens,[(),(7,8,9),(8,9)]);;     
gap> e3 := GroupHomomorphismByImages(s3a,g18,s3agens,[(4,5,6),(2,3)(5,6)]);;   
gap> C3 := Cat1Group( t3, h1, e3 );; 
gap> imgamma := [ (4,5,6), (1,2,3), (2,3)(5,6) ];; 
gap> gamma := GroupHomomorphismByImages( g18, g18, g18gens, imgamma );;
gap> rho := IdentityMapping( s3a );; 
gap> phi3 := Cat1GroupMorphism( C18, C3, gamma, rho );;
gap> Display( phi3 );;
Morphism of cat1-groups :- 
: Source = [g18=>s3a] with generating sets:
  [ (1,2,3), (4,5,6), (2,3)(5,6) ]
  [ (7,8,9), (8,9) ]
:  Range = [g18=>s3a] with generating sets:
  [ (1,2,3), (4,5,6), (2,3)(5,6) ]
  [ (7,8,9), (8,9) ]
: Source Homomorphism maps source generators to:
  [ (4,5,6), (1,2,3), (2,3)(5,6) ]
: Range Homomorphism maps range generators to:
  [ (7,8,9), (8,9) ]

## Section 3.3.2
gap> phi5 := Cat1GroupMorphismOfXModMorphism( mor5 );
[[(Aut(c5) |X c5)=>Aut(c5)] => [(Aut(c5) |X c5)=>Aut(c5)]]
gap> mor3 := XModMorphismOfCat1GroupMorphism( phi3 );;
gap> Display( mor3 );
Morphism of crossed modules :- 
: Source = xmod([g18=>s3a]) with generating sets:
  [ (4,5,6) ]
  [ (7,8,9), (8,9) ]
:  Range = xmod([g18=>s3a]) with generating sets:
  [ (1,2,3) ]
  [ (7,8,9), (8,9) ]
: Source Homomorphism maps source generators to:
  [ (1,2,3) ]
: Range Homomorphism maps range generators to:
  [ (7,8,9), (8,9) ]

## Section 3.3.3
gap> iso8 := IsomorphismPerm2DimensionalGroup( C8 );
   [[G8=>d12] => [..]]

## Section 3.3.4
gap> G := Group( (1,2,3,4)(5,6,7,8) );; 
gap> H := Subgroup( G, [ (1,3)(2,4)(5,7)(6,8) ] );;
gap> XG := XModByNormalSubgroup( G, H );
[Group( [ (1,3)(2,4)(5,7)(6,8) ] )->Group( [ (1,2,3,4)(5,6,7,8) ] )]
gap> sdpr := SmallerDegreePermutationRepresentation2DimensionalGroup( XG );; 
gap> Range( sdpr );
[Group( [ (1,2) ] )->Group( [ (1,2,3,4) ] )]

## Section 3.4.1
gap> H8 := Subgroup(G8, [G8.3,G8.4,G8.6,G8.7] );  SetName( H8, "H8" );
Group([ f3, f4, f6, f7 ])
gap> c6 := Subgroup( d12, [a2,a3] );  SetName( c6, "c6" );
Group([ f2, f3 ])
gap> SC8 := Sub2DimensionalGroup( C8, H8, c6 );
[H8=>c6]
gap> IsCat1Group( SC8 );
true
gap> inc8 := InclusionMorphism2DimensionalDomains( C8, SC8 );
[[H8=>c6] => [G8=>d12]]
gap> CompositionMorphism( iso8, inc8 ); 
[[H8=>c6] => Pc[G8=>d12]]

## Section 3.4.2
gap> c2 := Group( (19,20) );                                    
Group([ (19,20) ])
gap> X0 := XModByNormalSubgroup( c2, c2 );  SetName( X0, "X0" );
[Group( [ (19,20) ] )->Group( [ (19,20) ] )]
gap>  SX8 := Source( X8 );;
gap> genSX8 := GeneratorsOfGroup( SX8 ); 
[ f1, f4, f5, f7 ]
gap> sigma0 := GroupHomomorphismByImages(SX8,c2,genSX8,[(19,20),(),(),()]);
[ f1, f4, f5, f7 ] -> [ (19,20), (), (), () ]
gap> rho0 := GroupHomomorphismByImages(d12,c2,[a1,a2,a3],[(19,20),(),()]);
[ f1, f2, f3 ] -> [ (19,20), (), () ]
gap> mor0 := XModMorphism( X8, X0, sigma0, rho0 );;           
gap> K0 := Kernel( mor0 );;
gap> StructureDescription( K0 );
[ "C12", "C6" ]

## Section 3.5 
gap> C18a := Cat1Select( 18, 4, 4 );;          
gap> StructureDescription( C18a );             
[ "(C3 x C3) : C2", "S3" ]
gap> QuotientQuasiIsomorphism( C18a, true );   
kernel-cokernel: [ "1", "C2" ]
(quo:) normal subxmod: [ "C3", "C3" ]
[ [ 2, 1 ], [ 2, 1 ] ], [ 2, 1, 1 ], [ "C2", "C2" ]
bdy2: [ [ f2 ], [ f3^2 ] ]
[ [ 2, 1, 1 ] ]
gap> SubQuasiIsomorphism( C18a, false );
[ [ 2, 1, 1 ], [ 2, 1, 1 ], [ 2, 1, 1 ] ]
gap> L18a := QuasiIsomorphism( C18a, [18,4,4], false );
[ [ 2, 1, 1 ], [ 18, 4, 4 ] ]


gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gp2map.tst", 10000 );
