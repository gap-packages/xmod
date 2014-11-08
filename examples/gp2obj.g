#############################################################################
##
#W  gp2obj.g                 XMOD example files                 Chris Wensley
#W                                                                & Murat Alp
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod test file gp2obj.g (version 08/11/14) :-");
Print("\ntesting constructions of crossed modules\n\n");

c5 := Group( (5,6,7,8,9) );
SetName( c5, "c5" );
X1 := XModByAutomorphismGroup( c5 );
Print("X1 = ",X1," is a crossed module :-\n");
Display( X1 );
Print("Properties; Attributes; Representations of X1 :-\n");
Print( KnownPropertiesOfObject(X1), "\n" );
Print( KnownAttributesOfObject(X1), "\n" );
Print( RepresentationsOfObject(X1), "\n" );
Print("X1 has size ", Size(X1), "\n\n" );

s3c4 := Group( (1,2), (2,3), (4,5,6,7) );
SetName( s3c4, "s3c4" );
s3 := Subgroup( s3c4, [ (1,2), (2,3) ] );
SetName( s3, "s3" );
gens3c4 := GeneratorsOfGroup( s3c4 );
imb := [ (1,2), (2,3), () ];
bX2 := GroupHomomorphismByImages( s3c4, s3, gens3c4, imb );
im1 := List( gens3c4, g -> g^(1,2) );
a1 := GroupHomomorphismByImages( s3c4, s3c4, gens3c4, im1 );
im2 := List( gens3c4, g -> g^(2,3) );
a2 := GroupHomomorphismByImages( s3c4, s3c4, gens3c4, im2 );
A := Group( a1, a2 );
aX2 := GroupHomomorphismByImages( s3, A, [(1,2),(2,3)], [a1,a2] );
X2 := XMod( bX2, aX2 );
Print("crossed module X2 = ",X2,"\n");
Display(X2);

s4 := Group( (1,2,3,4), (1,2) );
SetName( s4, "s4");
a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );
SetName( a4, "a4" );
k4 := Subgroup( s4, [ (1,2)(3,4), (1,3)(2,4) ] );
SetName( k4, "k4" );
X4 := XModByNormalSubgroup( a4, k4 );
Print("crossed module X4 = ",X4,"\n");
Display(X4);

d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );
SetName( d8, "d8" );
gend8 := GeneratorsOfGroup( d8 );
genk4 := GeneratorsOfGroup( k4 );
f8 := GroupHomomorphismByImages( d8, k4, gend8, genk4 );
X8 := XModByCentralExtension( f8 );
Print("crossed module X8 = ",X8,"\n");
Display(X8);

q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );
SetName( q8, "q8" );
genq8 := GeneratorsOfGroup( q8 );
iaq8 := InnerAutomorphismsByNormalSubgroup( q8, q8 );
geniaq8 := GeneratorsOfGroup( iaq8 );
a := GroupHomomorphismByImages( q8, q8, genq8,
        [(1,5,3,7)(2,6,4,8),(1,4,3,2)(5,6,7,8)] );
genA8 := Concatenation( geniaq8, [a] );
idq8 := IdentityMapping( q8 );
A8 := Group( genA8, idq8 );
ok := IsGroupOfAutomorphisms( A8 );
AX8 := XModByGroupOfAutomorphisms( q8, A8 );
Print("crossed module AX8 = ",AX8,"\n");
Display(AX8);

imf := [ (1,3)(2,4), (1,3)(2,4) ];
f := GroupHomomorphismByImages( k4, d8, genk4, imf );
T8 := XModByTrivialAction( f );
Print("crossed module T8 = ",T8,"\n");
Display(T8);

imk4 := [ (1,3)(2,4), (1,4)(2,3) ];
c3 := Group( (11,12,13) );
SetName(c3,"c3");
a3 := GroupHomomorphismByImages( k4, k4, genk4, imk4 );
A3 := Group( a3 );
ic3 := GroupHomomorphismByImages( c3, A3, [(11,12,13)], [a3] );
ok := IsGroupOfAutomorphisms( A3 );
R3 := AbelianModuleObject( k4, ic3 );
X3 := XModByAbelianModule( R3 );
Print("crossed module X3 = ",X3,"\n");
Display(X3);

X14 := DirectProduct( X1, X4 );
Print("Direct product of X1 and X4 = ", X14, "\n" );
Display( X14 );
e1 := Embedding( X14, 1 );
e2 := Embedding( X14, 2 );
Print("Direct Product Information for X14:\n", DirectProductInfo(X14), "\n");

Print("\nPre-XMods:\n");
c := (11,12,13,14,15,16,17,18); 
d := (12,18)(13,17)(14,16);
d16 := Group( c, d );
SetName( d16, "d16" );
sk4 := Subgroup( d16, [ c^4, d ] );
SetName( sk4, "sk4" );
bdy16 := GroupHomomorphismByImages( d16, sk4, [c,d], [c^4,d] );
h1 := GroupHomomorphismByImages( d16, d16, [c,d], [c^5,d] );
h2 := GroupHomomorphismByImages( d16, d16, [c,d], [c,c^4*d] );
aut16 := Group( [h1,h2] );
act16 := GroupHomomorphismByImages( sk4, aut16, [c^4,d], [h1,h2] );
P16 := PreXModByBoundaryAndAction( bdy16, act16 );
Print("pre-crossed module P16 = ",P16,"\n");
Display(P16);
P := PeifferSubgroup( P16 );
Print( "P16 has Peiffer subgroup:\n", P, "\n" );
X16 := XModByPeifferQuotient( P16 );
Print("Peiffer quotient xmod X16 = ", X16, "\n" );
Display( X16 );
iso16 := IsomorphismPermGroup( Source( X16 ) );
S16 := Image( iso16 );
Print("S16 = ", S16, "\n\n" );

Print("\n=========================================================\n");
Print("\ntesting constructions of cat1-groups\n");
Print("\nFirst, a permutation group example:\n\n");

t2 := GroupHomomorphismByImages( s3c4, s3, gens3c4, [(1,2),(2,3),()] );
e2 := InclusionMappingGroups( s3c4, s3 );
C2 := Cat1( t2, t2, e2 );
Print("cat1-group C2 = ",C2,"\n" );
Display( C2 );
XC2 := XModOfCat1( C2 );
Print("associated crossed module is XC2 = ", XC2, "\n" );
Display( XC2 );

Print("\nSecondly, the PcGroup version:\n\n");
ps3 := SymmetricGroup(IsPcGroup,3);;   SetName(ps3,"ps3");
genps3 := GeneratorsOfGroup(ps3);
pc4 := CyclicGroup(4);;  SetName( pc4, "pc4" );
ps3c4 := DirectProduct( ps3, pc4 );;  SetName( ps3c4, "ps3c4" );  
genps3c4 := GeneratorsOfGroup( ps3c4 );
a := genps3[1];;  b := genps3[2];;  one := One(ps3);;
pt2 := GroupHomomorphismByImages( ps3c4, ps3, genps3c4, [a,b,one,one] );
Print("\npt2 = ", pt2, "\n" );
SetName( Kernel(pt2), "ker(pt2)" );
pe2 := Embedding( ps3c4, 1 );
Print("\npe2 = ", pe2, "\n" );
pC2 := Cat1( pt2, pt2, pe2 );
Display( pC2 );
Print("IsPcCat1( pC2 ) ? ", IsPcCat1(pC2), "\n" );
Print("\npC2 has size ", Size(pC2), "\n" );
pX2 := XModOfCat1( pC2 );
Display( pX2 );

Print("==============================================================\n\n");

CX1 := Cat1OfXMod( X1 );
Print("cat1-group associated to X1 is CX1 = \n", CX1, "\n" );
Display( CX1 );

hol20 := Group( (5,6,7,8,9), (6,7,9,8) );
SetName( hol20, "hol20" );
gen20 := GeneratorsOfGroup( hol20 );
c4 := Subgroup( hol20, [ (6,7,9,8) ] );
SetName( c4, "c4" );
im20 := [ (), (6,7,9,8) ];
h20 := GroupHomomorphismByImages( hol20, c4, gen20, im20 );
t20 := h20;
e20 := InclusionMappingGroups( hol20, c4 );
C20 := Cat1( t20, h20, e20 );
Print("cat1-group C20 = ",C20,"\n" );
Display( C20 );

Print("Properties; Attributes; Representations of C20 :-\n");
Print( KnownPropertiesOfObject(C20), "\n" );
Print( KnownAttributesOfObject(C20), "\n" );
Print( RepresentationsOfObject(C20), "\n" );
Print("C20 has size ", Size(C20), "\n\n" );

c5 := Subgroup( hol20, [ (5,6,7,8,9) ] );
SetName( c5, "c5" );
X20 := XModByNormalSubgroup( hol20, c5 );
Print("normal inclusion crossed module X20 = ",X20,"\n" );
Display( X20 );
CX20 := Cat1OfXMod( X20 );
Print("associated cat1-group CX20 = ",CX20,"\n" );
Display( CX20 );
G20 := Source( CX20 );
Print("CX20 has source ",G20,", with Properties, Attributes:\n");
Print(KnownPropertiesOfObject(G20),"\n");
Print(KnownAttributesOfObject(G20),"\n");
sdp20 := SemidirectProductInfo(G20);
Print("The SemidirectProductInfo of G20 is:\n",sdp20,"\n");

Print("\nSelecting from data file cat1data.g :-\n" );
Print("(1) Listing groups of given order:\n");
L18 := Cat1Select( 18 );
Print("\n(2) Listing cat1-structures for a given group:\n");
L18_4 := Cat1Select( 18, 4 );
Print("\n(3) Selecting one structure in particular:");
C18 := Cat1Select( 18, 4, 3 );
Display( C18 );
iso18 := IsomorphismPermObject( C18 ); 
PC18 := Image( iso18 ); 
Display( PC18 ); 
X18 := XModByCat1( PC18 );
Display( X18 ); 

#############################################################################
##
#E  gp2obj.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
