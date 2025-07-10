##############################################################################
##
#W  issue9.tst                   GAP4 package `XMod'             Chris Wensley
#W             
#Y  Copyright (C) 2018, Chris Wensley et al,  
##  
gap> START_TEST( "XMod package: issue9.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

gap> G := Group( (1,2,3), (2,3), (4,5,6), (5,6) );;
gap> Ggens := GeneratorsOfGroup( G );;
gap> R := Subgroup( G, [ (1,2,3)(4,5,6), (2,3)(5,6) ] );;
gap> Rgens := GeneratorsOfGroup( R );;
gap> t := GroupHomomorphismByImages( G, R, Ggens,
>      [ (1,2,3)(4,5,6), (2,3)(5,6), (), () ] );
[ (1,2,3), (2,3), (4,5,6), (5,6) ] -> [ (1,2,3)(4,5,6), (2,3)(5,6), (), () ]
gap> h := GroupHomomorphismByImages( G, R, Ggens,
>      [ (), (),  (1,2,3)(4,5,6), (2,3)(5,6) ] );
[ (1,2,3), (2,3), (4,5,6), (5,6) ] -> [ (), (), (1,2,3)(4,5,6), (2,3)(5,6) ]
gap> e := GroupHomomorphismByImages( R, G, Rgens, Rgens );;
gap> C2 := PreCat1GroupByTailHeadEmbedding( t, h, e );
[Group( [ (1,2,3), (2,3), (4,5,6), (5,6) ] )=>Group(
[ (1,2,3)(4,5,6), (2,3)(5,6) ] )]
gap> SetName( G, "s3s3" );; SetName( R, "s3d" );;
gap> C2 := PreCat1GroupByTailHeadEmbedding( t, h, e );
[s3s3=>s3d]
gap> X2 := XModOfCat1Group(C2);
xmod([s3s3=>s3d])
gap> Display( X2 );

Crossed module xmod([s3s3=>s3d]) :- 
: Source group has generators:
  [ (4,5,6), (5,6) ]
: Range group has generators:
  [ (1,2,3)(4,5,6), (2,3)(5,6) ]
: Boundary homomorphism maps source generators to:
  [ (1,2,3)(4,5,6), (2,3)(5,6) ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3)(4,5,6) --> { source gens --> [ (4,5,6), (4,6) ] }
  (2,3)(5,6) --> { source gens --> [ (4,6,5), (5,6) ] }
  These 2 automorphisms generate the group of automorphisms.
: associated cat1-group is [s3s3=>s3d]

gap> A2 := AutomorphismPermGroup( X2 );
Group([ (1,3,2)(4,6,5)(7,9,8), (2,3)(5,6)(8,9) ])

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "issue9.tst", 10000 );

#############################################################################
##
#E  issue9.tst  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
