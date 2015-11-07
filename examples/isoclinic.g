#############################################################################
##
#W  isoclinic.g               XMOD example files   Chris Wensley & Alper Odabas
##                                                               & Enver Uslu
##  version 2.43, 05/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

SetInfoLevel( InfoXMod, 2 ); 
Print("\nXMod test file isoclinic.g (version 05/11/15) :-\n\n");

D24 := DihedralGroup(24);  
SetName( D24, "D24" );
X24 := XModByAutomorphismGroup( D24 );
Print( "X24 has size ", Size(X24), "\n" );
nsx := NormalSubXMods( X24 ); 
ids := List( nsx, n -> IdGroup(n) ); 
pos1 := Position( ids, [ [4,1], [8,3] ] ); 
Xn1 := nsx[pos1];  
Print( "normal subxmod Xn1 = ", Xn1, "\n" ); 
Print( "Xn1 has size ", Size( Xn1 ), "\n" );
natn := NaturalMorphismByNormalSubXMod( X24, Xn1 ); 
Print( "natn = ", natn, "\n" ); 
Qn1 := FactorXMod( X24, Xn1 );  
Print( "factor xmod Qn1 has size ", Size( Qn1 ), "\n\n" );


pos2 := Position( ids, [ [24,6], [12,4] ] );
Xn2 := nsx[pos2]; 
Print( "normal subxmod Xn2 = ", Xn2, "\n" ); 
Print( "Xn2 has size ", Size( Xn2 ), "\n" );
pos3 := Position( ids, [ [12,2], [24,5] ] );
Xn3 := nsx[pos3]; 
Print( "normal subxmod Xn3 = ", Xn3, "\n" ); 
Print( "Xn3 has size ", Size( Xn3 ), "\n" );
Xn23 := IntersectionSubXMods( X24, Xn2, Xn3 );
Print( "Xn23 is the intersection of crossed  modules Xn2, Xn3 :-\n" );
Print( Xn3, "\n" ); 
Print( "Xn2,Xn3,Xn23 have sizes: ", [Size(Xn2),Size(Xn3),Size(Xn23) ], "\n\n" );


pos4 := Position( ids, [ [6,2], [24,14] ] );;
Xn4 := nsx[pos4];; 
Print( "normal subxmod Xn4 = ", Xn4, "\n" ); 
Print( "Xn4 has size ", Size( Xn4 ), "\n" );
Sn4 := Source(Xn4);;  SetName( Sn4, "c6" ); 
Rn4 := Range(Xn4);;  SetName( Rn4, "c2c2s3" );
Display(Xn4);


r := Rn4.1;  s := Sn4.1; 
d := Displacement( XModAction(Xn4), r, s ); 
Print( "in Xn4 the displacement <r,s> = <", r, ",", s, "> is d = ", d, "\n" ); 
bn4 := Boundary( Xn4 );
imd := Image( bn4, d ); 
ims := Image( bn4, s ); 
Print( "the boundary of Xn4 maps s = ", s, " to ims = ", ims, "\n" ); 
Print( "the boundary of Xn4 maps d to imd = ", imd, "\n" );  
Print( "which should equal [r,ims] = [", r, ",", ims, "] = " ); 
Print( Comm( r, ims ), "\n" );  
Print( "the displacement subgroup of Xn4 is: " ); 
Print( DisplacementSubgroup( Xn4 ), "\n\n" );

Cn23 := CommutatorSubXMod( X24, Xn2, Xn3 );
Print( "The commutator subxmod  [Xn2,Xn3] in XN24 is Cn23 = ", Cn23, "\n" ); 
Print( "Cn23 has size ", Size(Cn23) );
Print( " and IdGroup ", IdGroup( Cn23 ), "\n" ); 
Print( "Xn23 = Cn23 ? ", Xn23 = Cn23, "\n" ); 
Q24 := CentralQuotient( D24);       
Print( "the central quotient of D24 is Q24 = ", Q24, "\n" ); 
Print( "Q24 has size ", Size( Q24 ) );
Print( " and structure ", StructureDescription( Q24 ), "\n\n" );  


DXn4 := DerivedSubXMod( Xn4 );  
Print( "Xn4 has derived subxmod DXn4 = ", DXn4, "\n" );
fix := FixedPointSubgroupXMod( Xn4, Sn4, Rn4 );
Print( "Xn4 has fixed point subgroup ", fix, "\n" );
stab := StabilizerSubgroupXMod( Xn4, Sn4, Rn4 );
Print( "Xn4 has stabilizer subgroup ", stab, "\n" ); 


ZXn4 := CentreXMod( Xn4 );      
Print( "Xn4 has centre ", ZXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( ZXn4 ), "\n" );
CDXn4 := Centralizer( Xn4, DXn4 );
Print( "the centralizer of DXn4 in Xn4 is ", CDXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( CDXn4 ), "\n" );
NDXn4 := Normalizer( Xn4, DXn4 ); 
Print( "the normalizer of DXn4 in Xn4 is ", NDXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( NDXn4 ), "\n" );

Print("\n"); zz := 3;  zz[2]:=4; 


Q24 := CentralQuotient( D24);  Size( Q24 );                     
[D24->Group( [ f1, f2, f3 ] )]
[ 24, 12 ]
N7 := nsx[7];;  IdGroup( N7 );
[ [ 12, 2 ], [ 24, 5 ] ]
IdGroup( CentreXMod(N7) );  
[ [ 4, 1 ], [ 4, 1 ] ]
CQN7 := CentralQuotient( N7 );
crossed square with:
      up = [Group( [ f2, f3, f4 ] )->Group( [ f1, f1^2, f1 ] )]
    left = [Group( [ f2, f3, f4 ] )->Group( [ f1, f2, f4, f5 ] )]
    down = [Group( [ f1, f2, f4, f5 ] )->Group( 
[ f1, f2, <identity> of ..., <identity> of ... ] )]
   right = [Group( [ f1, f1^2, f1 ] )->Group( 
[ f1, f2, <identity> of ..., <identity> of ... ] )]

IdGroup( CQN7 );
[ [ [ 12, 2 ], [ 3, 1 ] ], [ [ 24, 5 ], [ 6, 1 ] ] ]
]]>


[ IsAbelian2dGroup(Xn4), IsAbelian2dGroup(X24) ];
[ false, false ]
pos7 := Position( ids, [ [3,1], [6,1] ] );;
[ IsAspherical2dGroup(nsx[pos7]), IsAspherical2dGroup(X24) ];
[ true, false ]
[ IsSimplyConnected2dGroup(Xn4), IsSimplyConnected2dGroup(X24) ];
[ true, true ]
[ IsFaithful2dGroup(Xn4), IsFaithful2dGroup(X24) ];              
[ false, true ] 


C8 := Cat1(16,8,1);
[QD16=>Group( [ f2, f2 ] )]
X8 := XMod(C8); 
[Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )]
IdGroup( X8 );
[ [ 8, 1 ], [ 2, 1 ] ]
ZX8 := CentreXMod( X8 );
[Group( [ f4 ] )->Group( <identity> of ... )]
FX8 := FactorXMod( X8, ZX8 );
[Group( [ f1, f2, <identity> of ... ] )->Group( [ f2, f2 ] )]
DX8 := DerivedSubXMod( X8 );
[Group( [ f3 ] )->Group( <identity> of ... )]
RankXMod( X8 );  
[ 3., 1. ]
MiddleLength( X8 );    
[ 1., 0. ]
LowerCentralSeries(X8);
[ [Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )], 
  [Group( [ f3 ] )->Group( <identity> of ... )], 
  [Group( [ f4 ] )->Group( <identity> of ... )], 
  [Group( <identity> of ... )->Group( <identity> of ... )] ]


xc6s3 := AllXMods( SmallGroup(6,2), SmallGroup(6,1) );;   
Length( xc6s3 );           
4
x66 := AllXMods( [6,6] );;   
Length( x66 );
17
x36 := AllXMods( 36 );; 
Length( all36 ); 
205


IsomorphismXMods( x66[1], x66[2] );
[[Group( [ f1, f2 ] )->Group( [ f1, f2 ] )] => [Group( [ f1, f2 ] )->Group( 
[ f1, f2 ] )]]
L := ListWithIdenticalEntries( 17, 1 );;
for i in [1..16] do 
>      if ( L[i] = 1 ) then 
>        X1 := x66[i]; 
>        for j in [i+1..17] do 
>          if ( L[j] = 1 ) then 
>            X2 := x66[j];            
>            mor := IsomorphismXMods( X1, X2 ); 
>            if ( mor <> fail ) then L[j]:=0; fi; 
>          fi;
>        od;
>      fi;
>    od;
L;
[ 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0 ]
Sum( L );
9


all44 := AllXMods( [4,4] );; 
Length( last );
60
iso44 := AllXModsUpToIsomorphism( all44 );;
Length( last );
18 
L := ListWithIdenticalEntries( 60, 1 );;
for i in [1..59] do 
>      if ( L[i] = 1 ) then 
>        L[i] := [i]; 
>        Y1 := all44[i]; 
>        for j in [i+1..60] do 
>          if ( L[j] = 1 ) then 
>            Y2 := all44[j];            
>            mor := IsomorphismXMods( Y1, Y2 ); 
>            if ( mor <> fail ) then 
>              L[j]:=0; 
>              Add( L[i], j );
>            fi; 
>          fi;
>        od;
>      fi;
>    od;
L := Filtered( L, k -> k<>0 );
[ [ 1 ], [ 2 ], [ 3, 6 ], [ 4 ], [ 5 ], [ 7 ], [ 8, 9, 10 ], [ 11, 13, 15 ], 
  [ 12, 14, 16 ], [ 17 ], [ 18, 19, 20 ], [ 21, 23, 25 ], [ 22, 24, 26 ], 
  [ 27 ], [ 28, 29, 30, 31, 32, 33, 34, 35, 36 ], 
  [ 37, 39, 41, 43, 45, 49, 52, 55, 59 ], 
  [ 38, 40, 42, 44, 46, 50, 53, 56, 60 ], [ 47, 48, 51, 54, 57, 58 ] ]


G := SmallGroup( 64, 6 );;  StructureDescription( G ); 
"(C8 x C4) : C2"
Q := CentralQuotient( G );;  IdGroup( Q );
[ [ 64, 6 ], [ 8, 3 ] ]
H := SmallGroup( 32, 41 );;  StructureDescription( H );
"C2 x Q16"
IdGroup( CentralQuotient( H ) );
[ [ 32, 41 ], [ 8, 3 ] ]
Isoclinism( G, H );
[ [ f1, f2, f3 ] -> [ f1, f2*f3, f3 ], [ f3, f5 ] -> [ f4*f5, f5 ] ]
K := SmallGroup( 32, 43 );;  StructureDescription( K );
"(C2 x D8) : C2"
IdGroup( CentralQuotient( K ) );                       
[ [ 32, 43 ], [ 16, 11 ] ]
AreIsoclinicDomains( G, K );
false


DerivedSubgroup(G);     
Group([ f3, f5 ])
IsStemGroup( G );
false
IsoclinicStemGroup( G );
<pc group of size 16 with 4 generators>
IdGroup( last );
[ 16, 7 ]
AllStemGroupIds( 32 );
[ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ], [ 32, 18 ], [ 32, 19 ], [ 32, 20 ], 
  [ 32, 27 ], [ 32, 28 ], [ 32, 29 ], [ 32, 30 ], [ 32, 31 ], [ 32, 32 ], 
  [ 32, 33 ], [ 32, 34 ], [ 32, 35 ], [ 32, 43 ], [ 32, 44 ], [ 32, 49 ], 
  [ 32, 50 ] ]
AllStemGroupFamilies( 32 );
[ [ [ 32, 6 ], [ 32, 7 ], [ 32, 8 ] ], [ [ 32, 18 ], [ 32, 19 ], [ 32, 20 ] ],
  [ [ 32, 27 ], [ 32, 28 ], [ 32, 29 ], [ 32, 30 ], [ 32, 31 ], [ 32, 32 ], 
      [ 32, 33 ], [ 32, 34 ], [ 32, 35 ] ], [ [ 32, 43 ], [ 32, 44 ] ], 
  [ [ 32, 49 ], [ 32, 50 ] ] ]


MiddleLength(G);
1.
RankXMod(X1);
[ 2.32193, 2. ]


C8 := Cat1(16,8,1);
[QD16=>Group( [ f2, f2 ] )]
X8 := XMod(C8); 
[Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )]
IdGroup( X8 );
[ [ 8, 1 ], [ 2, 1 ] ]
ZX8 := CentreXMod( X8 );
[Group( [ f4 ] )->Group( <identity> of ... )]
FX8 := FactorXMod( X8, ZX8 );
[Group( [ f1, f2, <identity> of ... ] )->Group( [ f2, f2 ] )]
DX8 := DerivedSubXMod( X8 );
[Group( [ f3*f4, f4 ] )->Group( <identity> of ... )]
RankXMod( X8 );  
[ 3., 1. ]
MiddleLength( X8 );    
[ 1., 0. ]
LowerCentralSeries(X8);
[ [Group( [ f1*f2*f3, f3, f4 ] )->Group( [ f2, f2 ] )], 
  [Group( [ f3*f4, f4 ] )->Group( <identity> of ... )], 
  [Group( [ f4 ] )->Group( <identity> of ... )], 
  [Group( [ <identity> of ... ] )->Group( <identity> of ... )] ]

C9 := Cat1(32,9,1);
[(C8 x C2) : C2=>Group( [ f2, f2 ] )]
X9 := XMod( C9 );
[Group( [ f1*f2*f3, f3, f4, f5 ] )->Group( [ f2, f2 ] )]
IdGroup( X9 );
[ [ 16, 5 ], [ 2, 1 ] ]
ZX9 := CentreXMod( X9 );
[Group( [ f4, f5 ] )->Group( <identity> of ... )]
FX9 := FactorXMod( X9, ZX9 );
[Group( [ f1, f2, <identity> of ..., <identity> of ... ] )->Group( 
[ f2, f2 ] )]
DX9 := DerivedSubXMod( X9 );
[Group( [ f3*f5, f5 ] )->Group( <identity> of ... )]
morF := IsomorphismXMods( FX8, FX9 );
[[Group( [ f1, f2, <identity> of ... ] )->Group( [ f2, f2 ] )] => [Group( 
[ f1, f2, <identity> of ..., <identity> of ... ] )->Group( [ f2, f2 ] )]]
morD := IsomorphismXMods( DX8, DX9 );
[[Group( [ f3*f4, f4 ] )->Group( <identity> of ... )] => [Group( 
[ f3*f5, f5 ] )->Group( <identity> of ... )]]
IsStemXMod(X8);
true
IsStemXMod(X9);
false
iso89 := Isoclinism( X8, X9 );;
MappingGeneratorsImages( iso89[1] );
[ [ [ f1, f2, <identity> of ... ], [ f1*f2, f2, <identity> of ... ] ], 
  [ [ f2, f2 ], [ f2, f2 ] ] ]
MappingGeneratorsImages( iso89[2] );
[ [ [ f3*f4, f4 ], [ f3, f5 ] ], [ [  ], [  ] ] ]
XG := XMod(G);; 
XH := XMod(H);;
AreIsoclinicDomains( XG, XH );
true


all44 := AllXMods( [4,4] );;
Length( all44 );            
60
iso44 := AllXModsUpToIsomorphism( all44 );;
Length( iso44 );
18
IsoclinicXModFamily( iso44[1], iso44 );
[ 1, 3, 4, 6, 8, 10, 12, 14, 16, 18 ]
IsoclinicXModFamily( iso44[2], iso44 );
[ 2, 5, 7, 9, 11, 13, 15, 17 ]


IsStemXMod(X8);
true
IsStemXMod(X9);
false

