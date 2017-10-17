#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gpd2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 9

gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> SetName( a4, "a4" );
gap> Ga4 := SinglePieceGroupoid( a4, [-9,-8,-7] );;
gap> Display( Ga4 );
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> SetName( Ga4, "Ga4" );; 
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );; 
gap> SetName( k4, "k4" );
gap> PX0 := DiscreteNormalPreXModWithObjects( Ga4, k4 );;
gap> Print( PX0, "\n" );
[perm homogeneous, discrete groupoid: < k4, [ -9, -8, -7 ] >
->  Ga4]
gap> IsXMod( PX0 ); 
true
gap> SX0 := Source( PX0 );;
gap> SetName( SX0, "3(k4)" );; 
gap> Name( PX0 );
"[3(k4)->Ga4]"
gap> CategoriesOfObject( PX0 );
[ "IsListOrCollection", "IsCollection", "IsExtLElement", 
  "CategoryCollections(IsExtLElement)", "IsExtRElement", 
  "CategoryCollections(IsExtRElement)", 
  "CategoryCollections(IsMultiplicativeElement)", 
  "CategoryCollections(IsAssociativeElement)", "IsGeneralizedDomain", 
  "IsDomainWithObjects", 
  "CategoryCollections(IsMultiplicativeElementWithObjects)", 
  "CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)", 
  "CategoryCollections(IsMultiplicativeElementWithObjectsAndInverses)", 
  "IsMagmaWithObjects", "IsHigherDimensionalDomain", "Is2DimensionalDomain", 
  "Is2DimensionalDomainWithObjects", "Is2DimensionalMagmaWithObjects", 
  "Is2DimensionalMagmaWithObjectsAndOne", 
  "Is2DimensionalMagmaWithObjectsAndInverses", 
  "Is2DimensionalGroupWithObjects" ]
gap> KnownPropertiesOfObject( PX0 );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsDuplicateFree", 
  "IsGeneratorsOfSemigroup", "IsPreXModDomain", "IsPerm2DimensionalGroup", 
  "IsPreXMod", "IsXMod", "IsPreXModWithObjects" ]
gap> KnownAttributesOfObject( PX0 ); 
[ "Name", "Range", "Source", "Boundary", "ObjectList", "AutoGroup", 
  "XModAction" ]
gap> Size(PX0);
[ 12, 108 ]
gap> IsPermXMod(PX0);
true
gap> r := Arrow( Ga4, (1,3,4), -7, -8 ); 
[(1,3,4) : -7 -> -8]
gap> s := Arrow( SX0, (1,3)(2,4), -8, -8 ); 
[(1,3)(2,4) : -8 -> -8]
gap> ims := ImageElmXModAction( PX0, s, r );
[(1,4)(2,3) : -7 -> -7]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gpd2obj.tst", 10000 );

#############################################################################
##
#E  gpd2obj.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
