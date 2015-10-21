#############################################################################
##
#W  gp2ind.g                  XMOD example files                Chris Wensley
##
##  version 2.43, 21/10/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod test file gp2ind.g (version 18/09/15) :-");

Print("\nSurjective example :-\n" ); 

s4gens := [ (1,2), (2,3), (3,4) ];
s4 := Group( s4gens ); SetName(s4,"s4");
a4gens := [ (1,2,3), (2,3,4) ];
a4 := Subgroup( s4, a4gens );  SetName( a4, "a4" );
s3 := Group( (5,6),(6,7) );  SetName( s3, "s3" );
epi := GroupHomomorphismByImages( s4, s3, s4gens, [(5,6),(6,7),(5,6)] );
X4 := XModByNormalSubgroup( s4, a4 );
indX4 := SurjectiveInducedXMod( X4, epi );
Print( "\n", indX4, "\n" );
Display( indX4 );
morX4 := MorphismOfInducedXMod( indX4 );
Print( "\n", morX4, "\n" );

Print("\nInjective example :-\n" ); 

c := (11,12,13,14,15,16,17,18);;  d := (12,18)(13,17)(14,16);;
d16 := Group( c, d );
gend16 := GeneratorsOfGroup( d16 );
sk4 := Subgroup( d16, [ c^4, d ] );
gensk4 := GeneratorsOfGroup( sk4 );
SetName( d16, "d16" );  SetName( sk4, "sk4" );

Print( "\n", GeneratorsOfGroup( d16 ), "\n" );
d8 := Subgroup( d16, [ c^2, d ] );
c4 := Subgroup( d8, [ c^2 ] );
SetName( d8, "d8" );  SetName( c4, "c4" );
X16 := XModByNormalSubgroup( d16, d8 );
Print( X16, "\n" ); 
X8 := SubXMod( X16, c4, d8 );
Print( X8, "\n" );
Print( IsSubXMod( X16, X8 ), "\n" );
inc8 := InclusionMorphism2dDomains( X16, X8 );
Print( inc8, "\n" );
rho := GroupHomomorphismByImages( d16, d16, [c,d], [c,d^(c^2)] );
sigma := GroupHomomorphismByImages( d8, d8, [c^2,d], [c^2,d^(c^2)] );
mor := XModMorphismByHoms( X16, X16, sigma, rho );
Print( mor, "\n" );
comp := inc8 * mor;
Print( "comp = ", comp, "\n" );
Print( "comp = CompositionMorphism(mor,inc8) ?\n" );
Print( comp = CompositionMorphism(mor,inc8), "\n" );

incd8 := RangeHom( inc8 );
Print( [ Source(incd8), Range(incd8), IsInjective(incd8) ], "\n" );
indX8 := InducedXMod( X8, incd8 );
Print( "\n", indX8, "\n" );
Display( indX8 );
morX8 := MorphismOfInducedXMod( indX8 );
Print( "\n", morX8, "\n" );
Display( morX8 );

Print("HERE\n");
s3b := Subgroup( s4, [ (2,3), (3,4) ] );;  SetName( s3b, "s3b" );
indX3 := InducedXMod( s4, s3b, s3b );
Print( "\n", indX3, "\n" );
isoX3 := IsomorphismGroups( Source( indX3 ), GeneralLinearGroup(2,3) );
Print( "\n", isoX3, "\n" );

Print("\n\nUsing function BookExample :-\n" ); 
Print(    "=============================\n\n" ); 

BookExample := function(M,P,Q) 
    local ok, gensP, inc, X1, X2; 
    ok := IsNormal(P,M) and IsSubgroup(Q,P); 
    if ok then 
        X1 := XModByNormalSubgroup(P,M); 
        gensP := GeneratorsOfGroup(P); 
        inc := GroupHomomorphismByImages(P,Q,gensP,gensP); 
        X2 := InducedXMod( X1, inc ); 
    fi;
    Display(X2);
    return X2;
end;

s4 := Group( (1,2),(2,3),(3,4) ); 
SetName(s4,"s4"); 
a4 := Subgroup(s4,[(1,2,3),(2,3,4)]);
SetName(a4,"a4");
d8a := Subgroup(s4,[(1,2,3,4),(1,3)]); 
SetName(d8a,"d8a");
c4a := Subgroup(s4,[(1,2,3,4)]); 
SetName(c4a,"c4a"); 

indC4D8S4 := BookExample( c4a, d8a, s4 ); 
G := Source( indC4D8S4 ); 
iso := IsomorphismGroups( G, s4 ); 
Print( "source of induced xmod is isomorphic to s4 :-\n", iso, "\n\n" );

s3a := Subgroup( s4, [(1,2),(2,3)] );
SetName(s3a,"s3a");
c3a := Subgroup(s4,[(1,2,3)]); 
SetName(c3a,"c3a"); 

indC3C3S4 := BookExample( c3a, c3a, s4 ); 
c3sl23 := Source( indC3C3S4 ); 
indC3S3S4 := BookExample( c3a, s3a, s4 ); 
sl23 := Source( indC3S3S4 ); 
indS3S3S4 := BookExample( s3a, s3a, s4 ); 
gl23 := Source( indS3S3S4 ); 

indC4C4S4 := BookExample( c4a, c4a, s4 ); 
G96 := Source( indC4C4S4 ); 
Print( "G96: ", StructureDescription(G96), "\n" ); 
A96 := AutomorphismGroup(G96); 
Print( "A96: ", StructureDescription(A96), "\n" ); 

c2b := Subgroup( s4, [(1,2)(3,4)] );
SetName( c2b, "c2b" ); 
indC2C2S4 := BookExample( c2b, c2b, s4 ); 
G128 := Source( indC2C2S4 ); 
## Print( "G128: ", StructureDescription(G128), "\n" ); 

ccgl23 := ConjugacyClassesSubgroups( gl23 ); 
Print( "length: ", List( ccgl23, c -> Size(c) ), "\n" ); 
Print( "orders: ", List( ccgl23, c -> Size(Representative(c)) ), "\n" ); 

sl23b := Representative( ccgl23[15] ); 
indSSG23 := BookExample( sl23b, sl23b, gl23 );
H72 := Source( indSSG23 );
Print( "H72: ", StructureDescription(H72), "\n" ); 

ccsl23 := ConjugacyClassesSubgroups( sl23b ); 
Print( "length: ", List( ccsl23, c -> Size(c) ), "\n" ); 
Print( "orders: ", List( ccsl23, c -> Size(Representative(c)) ), "\n" ); 

c3b := Representative( ccsl23[3] ); 
indC3C3SL23 := BookExample( c3b, c3b, sl23b ); 
Print( "indC3C3SL23: ", StructureDescription( Source(indC3C3SL23) ), "\n" ); 

c4b := Representative( ccsl23[4] ); 
indC4C4SL23 := BookExample( c4b, c4b, sl23b ); 
Print( "indC4C4SL23: ", StructureDescription( Source(indC4C4SL23) ), "\n" ); 

Print( "\nWorking with GL(3,2) = Aut(C_2^3), of order 168 :-\n\n" ); 
gl32a := GeneralLinearGroup(3,2); 
isogl32 := IsomorphismPermGroup( gl32a ); 
gl32 := Image( isogl32 ); 
Print( GeneratorsOfGroup( gl32 ), "\n\n" ); 
ccgl32 := ConjugacyClassesSubgroups( gl32 ); 
Print( "length: ", List( ccgl32, c -> Size(c) ), "\n" ); 
Print( "orders: ", List( ccgl32, c -> Size(Representative(c)) ), "\n" ); 

Print( "\nWorking with SL(2,7), of order 336 :-\n\n" ); 
sl27a := SpecialLinearGroup(2,7); 
isosl27 := IsomorphismPermGroup( sl27a ); 
sl27 := Image( isosl27 ); 
Print( "\nSL(2,7) has size ", Size(sl27), " and generators:\n" );
Print( GeneratorsOfGroup( sl27 ), "\n" ); 
ccsl27 := ConjugacyClassesSubgroups( sl27 ); 
Print( "length: ", List( ccsl27, c -> Size(c) ), "\n" ); 
Print( "orders: ", List( ccsl27, c -> Size(Representative(c)) ), "\n" ); 

#############################################################################
##
#E  gp2ind.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
