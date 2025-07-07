#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2022, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gpd2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 10

## Subsection 10.1.1 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> SetName( s4, "s4" );
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );;
gap> SetName( a4, "a4" );
gap> X4 := XModByNormalSubgroup( s4, a4 );; 

gap> ## the majority of this test has been removed while waiting 
gap> ## for version 1.77 of groupoids to be accepted

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gpd2obj.tst", 10000 );
