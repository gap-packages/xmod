#############################################################################
##
#W  gp2map.g                 XMOD example files                 Chris Wensley
#W                                                                & Murat Alp
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod example file gp2map.g (version 10/11/15) :-");
Print("\ntesting constructions of crossed module morphisms\n\n");

msl23 := SpecialLinearGroup( 2, 3 );
isl23 := IsomorphismPermGroup( msl23 );
Print( "isl23 =\n", isl23, "\n" );
sl23 := Image( isl23 );
Print( "sl23 = ", sl23, "\n" );
SetName( sl23, "sl23" ); 
norm := List( NormalSubgroups( sl23 ), n -> Size(n) ); 
pos := Position( norm, 8 ); 
nq8 := NormalSubgroups( sl23 )[ pos ];
if not ( Size( nq8 ) = 8 ) then
    Error( "expecting a subgroup of order 8" );
fi;
Print("normal subgroup nq8 of sl23 = ",nq8,"\n");
SetName( nq8, "nq8" );
X23 := XModByNormalSubgroup( sl23, nq8 );
Print("Normal subgroup xmod X23 = ", X23, "\n" );
Display( X23 );

c5 := Group( (5,6,7,8,9) );
SetName( c5, "c5" );
X1 := XModByAutomorphismGroup( c5 );

sigma1 := GroupHomomorphismByImages( c5, c5, [(5,6,7,8,9)], [(5,9,8,7,6)] );
rho1 := InclusionMappingGroups( Range(X1), Range(X1) );
#ok := IsGroupHomomorphism(sigma1) and IsGroupHomomorphism( rho1 );
mor1 := XModMorphism( X1, X1, sigma1, rho1 );
Print("mor1 = ",mor1, " is a morphism X1 -> X1 :-\n");
Display( mor1 );
Print("Is mor1 an automorphism? ", 
  ( IsEndomorphism2dDomain(mor1) and IsBijective(mor1) ), "\n");
Print("The order of mor1 is ", Order(mor1), "\n\n");

gensl23 := GeneratorsOfGroup(sl23);
gennq8 := GeneratorsOfGroup(nq8);
rmor := GroupHomomorphismByImages( sl23, a4, gensl23, [(1,2,3),(1,2)(3,4)] );
smor := GroupHomomorphismByImages(nq8, k4, gennq8, [(1,3)(2,4),(1,4)(2,3),()]);
mor23 := XModMorphism( X23, X4, smor, rmor );
Print("mor23 = ",mor23, " is a morphism X23 -> X4 :-\n");
Display( mor23 );
Print("Is mor23 single valued? ", IsSingleValued(mor23), "\n");
Print("Is mor23 surjective? ", IsSurjective(mor23), "\n");
Print("Is mor23 total? ", IsTotal(mor23), "\n");
Print("Is mor23 injective? ", IsInjective(mor23), "\n\n");
Print("KnownProperties; Attributes; Representations of m23 :-\n");
Print(KnownPropertiesOfObject(mor23),"\n");
Print(KnownAttributesOfObject(mor23),"\n");
Print(RepresentationsOfObject(mor23),"\n\n");

c4 := Subgroup( nq8, [ gennq8[1] ] );
SetName(c4,"c4");
S23 := SubXMod( X23, c4, nq8 );
Print("subxmod S23 of X23 = ", S23, "\n" );
Display( S23 );
Print("S23 is a sub-xmod of X23 ? ", IsSubXMod( X23, S23 ), "\n" );
Print("S23 is a normal sub-xmod of X23 ? ", IsNormal( X23, S23 ), "\n\n" );
inc23 := InclusionMorphism2dDomains( X23, S23 );
Print("inc23 = inclusion morphism\n", mor23, "\n");
Display( inc23 );

imor4 := IdentityMapping( X4 );
Print("\nimor4 = identity mapping on X4\n", imor4, "\n");
Display( imor4 );

N23 := NormalSubXMods( X23 );
Print("X23 has ", Length(N23), " sub-crossed modules :-\n",N23,"\n" );
Print("with sizes ",List(N23,n->Size(n)),"\n\n");

x := gennq8[1]*gennq8[2];
ia23 := InnerAutomorphismXMod( S23, x );
Print("\nia23 = inner automorphism of S23\n", ia23, "\n");
Display( ia23 );

comp1 := ia23*inc23;
Print("ia23 * inc23 = ", comp1, "\n" );
comp2 := CompositionMorphism( inc23, ia23 );
Print("CompositionMorphism( inc23, ia23 ) = ", comp2, "\n");
Print("comp1 = comp2 ? ",comp1 = comp2,"\n\n");

c2 := Subgroup( nq8, [ gennq8[1]^2 ] );
SetName( c2, "c2" );
SS23 := SubXMod( S23, c2, nq8 );
Print("subxmod SS23 of S23 = ", SS23, "\n" );
Display( SS23 );
incSS := InclusionMorphism2dDomains( S23, SS23 );
Print("inclusion morphism SS23 -> S23 = ", incSS, "\n");
Display( incSS );
T23 := PreXModBySourceHom( incSS );
Print("\nTop crossed module for incSS = ", T23, "\n" );
Display( T23 );

Print("\n\n========================================================\n");
Print("\ntesting constructions of cat1-group morphisms\n\n");

CX4 := Cat1OfXMod( X4 );
Print("cat1-group associated to X4 is CX4 = ", CX4, "\n" );
Display( CX4 );

sdp := Group( (11,12,13), (14,15,16), (12,13)(15,16) );
SetName( sdp, "sdp" );
s3b := Group( (24,25,26), (22,23)(25,26) );
SetName( s3b, "s3b" );
gensdp := GeneratorsOfGroup( sdp );
gens3b := GeneratorsOfGroup( s3b );
t18 := GroupHomomorphismByImages(sdp, s3b, gensdp,
  [ (24,25,26), (24,25,26), (22,23)(25,26) ] );
h18 :=  GroupHomomorphismByImages(sdp, s3b, gensdp,
  [ (), (24,25,26), (22,23)(25,26) ] );
e18 := GroupHomomorphismByImages(s3b, sdp, gens3b,
  [ (14,15,16), (12,13)(15,16) ] );
C18 := Cat1( t18, h18, e18 );
Print("cat1-group C18 = ", C18, "\n" );
Display( C18 );
XC18 := XModOfCat1( C18 );
Print("associated crossed module XC18 = ", XC18, "\n" );
Display( XC18 );

Cmor23 := Cat1MorphismOfXModMorphism( mor23 );
Print("the cat1-morphism corresponding to mor23 is :-\n");
Display(Cmor23);
Print("\nmor23 and Cmor23 have attributes:\n");
Print(KnownAttributesOfObject(mor23),"\n");
Print(KnownAttributesOfObject(Cmor23),"\n\n");

#############################################################################
##
#E  gp2map.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
