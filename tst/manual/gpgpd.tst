#############################################################################
##
#W  gpgpd.tst                     XMOD test file               Chris Wensley
## 
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gpgpd.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;
gap> saved_infolevel_warning := InfoLevel( InfoWarning );; 
gap> SetInfoLevel( InfoWarning, 0 );;

## Subsection 2.8.1 
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
gap> gpd33 := GroupGroupoid( C33 ); 
groupoid with 2 pieces:
1:  single piece groupoid with rays: < Group( [ ()>-( 4, 5, 6)( 7, 9, 8)->() 
 ] ), [ (), (11,12,13), (11,13,12) ], [ ()>-()->(), ()>-(7,8,9)->(11,12,13), 
  ()>-(7,9,8)->(11,13,12) ] >
2:  single piece groupoid with rays: < Group( 
[ (12,13)>-( 2, 3)( 4, 6)( 7, 8)->(12,13) ] ), [ (12,13), (11,12), (11,13) ], 
[ (12,13)>-(2,3)(5,6)(8,9)->(12,13), (12,13)>-(2,3)(5,6)(7,9)->(11,13), 
  (12,13)>-(2,3)(5,6)(7,8)->(11,12) ] >

## Subsection 2.8.2 
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
gap> obgp := ObjectGroup( gpd33, (11,12) );
<group with 1 generators>
gap> GeneratorsOfGroup( obgp )[1];
(11,13)>-( 1, 3)( 4, 6)( 7, 8)->(11,13)
gap> Homset( gpd33, (11,12), (11,13) );
<homset (11,12) -> (11,13) with head group Group( 
[ (11,12)>-( 1, 2)( 4, 6)( 7, 8)->(11,12) ] )>

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> SetInfoLevel( InfoWarning, saved_infolevel_warning );; 
gap> STOP_TEST( "gpgpd.tst", 10000 );
