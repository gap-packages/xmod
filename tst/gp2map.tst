#############################################################################
##
#W  gp2map.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 3

## Section 3.2.3
gap> sigma1 := GroupHomomorphismByImages(c5,c5,[(5,6,7,8,9)],[(5,9,8,7,6)] );;
gap> rho1 := IdentityMapping( Range(X1) );;
gap> mor1 := XModMorphism( X1, X1, sigma1, rho1 );
[[c5->PAut(c5)] => [c5->PAut(c5)]]
gap> Display( mor1 );
Morphism of crossed modules :- 
: Source = [c5->PAut(c5)] with generating sets:
  [ (5,6,7,8,9) ]
  [ (1,2,3,4) ]
: Range = Source
: Source Homomorphism maps source generators to:
  [ (5,9,8,7,6) ]
: Range Homomorphism maps range generators to:
  [ (1,2,3,4) ]
gap> IsAutomorphism2DimensionalDomain(mor1);
true
gap> Order(mor1);
2
gap> RepresentationsOfObject(mor1);
[ "IsComponentObjectRep", "IsAttributeStoringRep", "Is2DimensionalMappingRep" ]
gap> KnownPropertiesOfObject(mor1);
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsTotal", 
  "IsSingleValued", "IsInjective", "IsSurjective", "RespectsMultiplication", 
  "IsPreXModMorphism", "IsXModMorphism", "IsEndomorphism2DimensionalDomain", 
  "IsAutomorphism2DimensionalDomain" ]
gap> KnownAttributesOfObject(mor1);
[ "Name", "Order", "Range", "Source", "SourceHom", "RangeHom" ]

## Section 3.3.1
gap> iso2 := IsomorphismPerm2DimensionalGroup( C2 );
[[G2=>d12] => [..]]

## Section 3.4.1
gap> H2 := Subgroup(G2, [G2.3,G2.4,G2.6,G2.7] );  SetName( H2, "H2" );
Group([ f3, f4, f6, f7 ])
gap> c6 := Subgroup( d12, [a2,a3] );  SetName( c6, "c6" );
Group([ f2, f3 ])
gap> SC2 := Sub2DimensionalGroup( C2, H2, c6 );
[H2=>c6]
gap> IsCat1Group( SC2 );
true
gap> inc2 := InclusionMorphism2DimensionalDomains( C2, SC2 );
[[H2=>c6] => [G2=>d12]]
gap> CompositionMorphism( iso2, inc2 );                  
[[H2=>c6] => Pc[G2=>d12]]

## Section 3.4.2
gap> c2 := Group( (19,20) );                                    
Group([ (19,20) ])
gap> X0 := XModByNormalSubgroup( c2, c2 );  SetName( X0, "X0" );
[Group( [ (19,20) ] )->Group( [ (19,20) ] )]
gap>  SX2 := Source( X2 );;
gap> genSX2 := GeneratorsOfGroup( SX2 ); 
[ f1, f4, f5, f7 ]
gap> sigma0 := GroupHomomorphismByImages(SX2,c2,genSX2,[(19,20),(),(),()]);
[ f1, f4, f5, f7 ] -> [ (19,20), (), (), () ]
gap> rho0 := GroupHomomorphismByImages(d12,c2,[a1,a2,a3],[(19,20),(),()]);
[ f1, f2, f3 ] -> [ (19,20), (), () ]
gap> mor0 := XModMorphism( X2, X0, sigma0, rho0 );;           
gap> K0 := Kernel( mor0 );;
gap> StructureDescription( K0 );
[ "C12", "C6" ]
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2map.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
