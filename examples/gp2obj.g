#############################################################################
##
#W  gp2obj.g                 XMOD example files                 Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2016, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod test file gp2obj.g (version 10/11/15) :-");
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
ext := ExternalSetXMod( X1 ); 
Print( "X1 has external set:\n", ext, "\n" ); 
Print( "and this has orbits:\n", Orbits(ext), "\n" );

s4 := Group( (1,2), (2,3), (3,4) );
SetName( s4, "s4");
a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );
SetName( a4, "a4" );
k4 := Subgroup( s4, [ (1,2)(3,4), (1,3)(2,4) ] );
SetName( k4, "k4" );
X4 := XModByNormalSubgroup( a4, k4 );
Print("crossed module X4 = ",X4,"\n");
Display(X4);
Y4 := SubXMod( X4, k4, a4 ); 
Print("subcrossed module Y4 = ",Y4,"\n");
Display(Y4);
Print( "Y4 is normal in X4? ", IsNormal(X4,Y4), "\n" );
NX4 := NormalSubXMods( X4 ); 
Print( "X4 has ", Length(NX4), " normal subcrossed modules\n\n" ); 

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
Print("Direct Product Information for X14:\n", DirectProductInfo(X14), "\n\n");

Print("\nPre-XMods:\n");
b1 := (11,12,13,14,15,16,17,18); 
b2 := (12,18)(13,17)(14,16);
d16 := Group( b1, b2 );
SetName( d16, "d16" );
sk4 := Subgroup( d16, [ b1^4, b2 ] );
SetName( sk4, "sk4" );
bdy16 := GroupHomomorphismByImages( d16, sk4, [b1,b2], [b1^4,b2] );
aut1 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1^5,b2] );
aut2 := GroupHomomorphismByImages( d16, d16, [b1,b2], [b1,b1^4*b2] );
aut16 := Group( [aut1, aut2] );
act16 := GroupHomomorphismByImages( sk4, aut16, [b1^4,b2], [aut1,aut2] );
P16 := PreXModByBoundaryAndAction( bdy16, act16 );
Print("pre-crossed module P16 = ",P16,"\n");
Display(P16);
Print( "IsXMod( P16 ) ? ", IsXMod(P16), "\n\n" ); 

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
Print("\nFirst, a pc-group example:\n\n");

G2 := SmallGroup( 288, 956 );  SetName( G2, "G2" );
Print( "G2 = ", G2, "\n" );
d12 := DihedralGroup( 12 );  SetName( d12, "d12" );
Print( "d12 = ", d12, "\n" );
a1 := d12.1;;  a2 := d12.2;;  a3 := d12.3;;  a0 := One( d12 );;
gensG2 := GeneratorsOfGroup( G2 );;
t2 := GroupHomomorphismByImages( G2, d12, gensG2,
          [ a0, a1*a3, a2*a3, a0, a0, a3, a0 ] );;
h2 := GroupHomomorphismByImages( G2, d12, gensG2,
          [ a1*a2*a3, a0, a0, a2*a3, a0, a0, a3^2 ] );;                   
e2 := GroupHomomorphismByImages( d12, G2, [a1,a2,a3],
          [ G2.1*G2.2*G2.4*G2.6^2, G2.3*G2.4*G2.6^2*G2.7, G2.6*G2.7^2 ] );
C2 := PreCat1ByTailHeadEmbedding( t2, h2, e2 );
Print( "C2 is the pre-cat-group ", C2, "\n" );
Print( "C2 is a cat1-group? ", IsCat1(C2), "\n" );
Display(C2);



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

X2 := XModOfCat1( C2 ); 
Print( "the crossed module obtained from C2 is X2 =\n" ); 
Display( X2 );
Print( "X2 has structure ", StructureDescription(X2), "\n\n" ); 


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
