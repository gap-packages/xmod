##############################################################################
##
#W  cat1data.tst                GAP4 package `XMod'              Chris Wensley
##  
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 

gap> START_TEST( "XMod package: cat1data.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 

gap> d36 := SmallGroup( 36, 4 );
<pc group of size 36 with 4 generators>
gap> StructureDescription( d36 );
"D36"
gap> SetName( d36, "d36"); 
gap> classes := EndomorphismClasses( d36, 2 );;
gap> Length( classes );
15
gap> c10 := classes[10];
endomorphism class for group d36
gap> Display( c10 );    
class of group endomorphisms with
natural hom: [ f1, f2, f3, f4 ] -> 
[ f1, <identity> of ..., f2, <identity> of ... ]
isomorphism: [ f1, <identity> of ..., f2, <identity> of ... ] -> 
[ f1, <identity> of ..., f4, <identity> of ... ]
autogp gens: [ Pcgs([ f1, f2 ]) -> [ f1, f2^2 ], 
  Pcgs([ f1, f2 ]) -> [ f1*f2, f2 ] ]
conjugators: [ <identity> of ..., f3, f3^2 ]
gap> classes := EndomorphismClasses( d36, 1 );;
gap> Length( classes );
17
gap> classes := EndomorphismClasses( d36, 3 );;
gap> Length( classes );                        
9

gap> SetInfoLevel( InfoXMod, 2 );
gap> gens := GeneratorsOfGroup( d36 );
[ f1, f2, f3, f4 ]
gap> ngens := Length( gens );;
gap> f1 := gens[1];;  f2 := gens[2];;  f3 := gens[3];;  f4 := gens[4];;
gap> id := One( d36 );;
gap> Cat1RepresentativesToFile( d36 );
nreps = 15
#I   rep number: 1
#I   rep number: 2
#I   rep number: 3
#I   rep number: 5
#I   rep number: 6
#I   rep number: 7
#I   rep number: 8
#I   rep number: 10
#I   rep number: 11
#I   rep number: 12
#I   rep number: 13
#I   rep number: 15
15 reps reduced to 12
Group( <identity> of ... ) <--> 1
Group( [ f1 ] ) <--> C2
Group( [ f2 ] ) <--> C2
Group( [ f4 ] ) <--> C3
Group( [ f1, f2 ] ) <--> C2 x C2
Group( [ f4, f1 ] ) <--> S3
Group( [ f4, f2 ] ) <--> C6
Group( [ f4, f3 ] ) <--> C9
Group( [ f4, f1, f2 ] ) <--> D12
Group( [ f4, f3, f1 ] ) <--> D18
Group( [ f4, f3, f2 ] ) <--> C18
Group( [ f4, f3, f1, f2 ] ) <--> D36
Remember to edit the file 36.4.rep in /lib,
changing the first entry in ireps to TrivialSubgroup(gp).
12

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "cat1data.tst", 10000 );

##############################################################################
##
#E  cat1data.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
