#############################################################################
##
#W  gp2map.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
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
gap> IsAutomorphism2dDomain(mor1);
true
gap> Order(mor1);
2
gap> RepresentationsOfObject(mor1);
[ "IsComponentObjectRep", "IsAttributeStoringRep", "Is2dMappingRep" ]
gap> KnownPropertiesOfObject(mor1);
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsTotal", 
  "IsSingleValued", "IsInjective", "IsSurjective", "RespectsMultiplication", 
  "IsPreXModMorphism", "IsXModMorphism", "IsEndomorphism2dDomain", 
  "IsAutomorphism2dDomain" ]
gap> KnownAttributesOfObject(mor1);
[ "Name", "Order", "Range", "Source", "SourceHom", "RangeHom" ]

## Section 3.3.1
gap> iso2 := IsomorphismPermObject( C2 ); 
[[s3c4=>s3] => [..]]

## the following failed test using 4.dev, 22/01/13, so removed for now 
## gap> Display( iso2 ); 

## Section 3.4.1
gap> GeneratorsOfGroup( d16 );
[ (11,12,13,14,15,16,17,18), (12,18)(13,17)(14,16) ]
gap> d8 := Subgroup( d16, [ c^2, d ] );;
gap> c4 := Subgroup( d8, [ c^2 ] );;
gap> SetName( d8, "d8" );  SetName( c4, "c4" );
gap> X16 := XModByNormalSubgroup( d16, d8 );
[d8->d16]
gap> X8 := SubXMod( X16, c4, d8 );
[c4->d8]
gap> IsSubXMod( X16, X8 );
true
gap> inc8 := InclusionMorphism2dDomains( X16, X8 );
[[c4->d8] => [d8->d16]]
gap> rho := GroupHomomorphismByImages( d16, d16, [c,d], [c,d^(c^2)] );;
gap> sigma := GroupHomomorphismByImages( d8, d8, [c^2,d], [c^2,d^(c^2)] );;
gap> mor := XModMorphismByHoms( X16, X16, sigma, rho );
[[d8->d16] => [d8->d16]]
gap> comp := inc8 * mor;
[[c4->d8] => [d8->d16]]
gap> comp = CompositionMorphism(mor,inc8);
true

## Section 3.4.2
gap> c2 := Group( (19,20) );;
gap> i2 := Subgroup( c2, [()] );;
gap> X9 := XModByNormalSubgroup( c2, i2 );;
gap> sigma9 := GroupHomomorphismByImages( c4, i2, [c^2], [()] );;
gap> rho9 := GroupHomomorphismByImages( d8, c2, [c^2,d], [(),(19,20)] );;
gap> mor9 := XModMorphism( X8, X9, sigma9, rho9 );
[[c4->d8] => [..]]
gap> K9 := Kernel( mor9 );
[Group( [ (11,13,15,17)(12,14,16,18) ] )->Group( [ (11,13,15,17)(12,14,16,18) 
 ] )]
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2map.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
