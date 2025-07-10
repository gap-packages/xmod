	#############################################################################
##
#W  gp4obj.gi                   GAP4 package `XMod'             Chris Wensley
##                                                               Alper Odabas
##  This file implements generic methods for (pre-)crossed cubes 
##  and (pre-)cat3-groups.
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al, 
    
#############################################################################
##
#M  PreCat3GroupObj( [<front>,<left>,<up>,<right>,<down>,<back>] ) 
##
InstallMethod( PreCat3GroupObj, "for a list of pre-cat2-groups", true,
    [ IsList ], 0,
function( L )

    local PC, ok;

    if not ( Length( L ) = 6 ) then 
        Error( "there should be 6 pre-cat2-groups in the list L" ); 
    fi; 
    PC := rec();
    ObjectifyWithAttributes( PC, PreCat3GroupObjType, 
        Front3DimensionalGroup, L[1], 
        Left3DimensionalGroup, L[2], 
        Up3DimensionalGroup, L[3],
        Right3DimensionalGroup, L[4],
        Down3DimensionalGroup, L[5],
        Back3DimensionalGroup, L[6], 
        GeneratingCat1Groups, [ Up2DimensionalGroup( L[1] ),
                                Left2DimensionalGroup( L[1] ), 
                                Left2DimensionalGroup( L[2] ) ],
        HigherDimension, 4, 
        IsHigherDimensionalGroup, true, 
        IsPreCat3Group, true, 
        IsPreCatnGroup, true );
    ok := IsCat3Group( PC );
    return PC;
end );

#############################################################################
##
#M  IsPreCat3Group . . . . . . . . . . .  check that this is a pre-cat3-group
#M  IsCat3Group . . . . . . . . . . . . check that the object is a cat3-group
##
InstallMethod( IsCat3Group, "generic method for a cat3-group",
    true, [ IsHigherDimensionalGroup ], 0,
function( P )
    return ( HigherDimension( P ) = 4 ) 
        and IsCat2Group( Front3DimensionalGroup( P ) )  
        and IsCat2Group( Left3DimensionalGroup( P ) )  
        and IsCat2Group( Up3DimensionalGroup( P ) )  
        and IsCat2Group( Right3DimensionalGroup( P ) )  
        and IsCat2Group( Down3DimensionalGroup( P ) )  
        and IsCat2Group( Back3DimensionalGroup( P ) );  
end ); 

#############################################################################
##
#M  DetermineRemainingFaces . . . . . . . . . . . for list of pre-catn-groups
## 
InstallGlobalFunction( DetermineRemainingFaces, function( arg )

    local nargs, C1;

    nargs := Length( arg );
    if ( nargs = 2 ) then
        if IsPreCat1Group(arg[1]) and IsPreCat1Group(arg[2]) then
            return DetermineRemainingCat1Groups( arg[1], arg[2] );
        elif IsPreCat2Group(arg[1]) and IsPreCat2Group(arg[2]) then
            return DetermineRemainingCat2Groups( arg[1], arg[2] );
        else
            return fail;
        fi;
    else
        return fail;
    fi;
end );

#############################################################################
##
#M  DetermineRemainingCat2Groups . . . . . . . . . . for two pre-cat2-groups
## 
InstallMethod( DetermineRemainingCat2Groups, "for front,left pre-cat2-gps", 
    true, [ IsPreCat2Group, IsPreCat2Group ], 0,
function( front, left )

    local Cfl, Cfu, Cll, up, Cud, Cfr, right, Cfd, Clr, down, 
          Cld, Cur, back, Crd, G, R, Q, H, N, P, M, L; 

    Cfl := Left2DimensionalGroup( front ); 
    if not ( Cfl = Up2DimensionalGroup( left ) ) then 
        Error( "front-left mis-match" ); 
    fi; 
    Cfu := Up2DimensionalGroup( front ); 
    Cll := Left2DimensionalGroup( left ); 
    up := Cat2Group( Cll, Cfu ); 
    if ( up = fail ) then 
        Info( InfoXMod, 1, "up fails to be a cat2-group" ); 
        return fail; 
    fi; 
    Cud := Down2DimensionalGroup( up ); 
    Cfr := Right2DimensionalGroup( front ); 
    right := Cat2Group( Cud, Cfr ); 
    if ( right = fail ) then 
        Info( InfoXMod, 1, "right fails to be a cat2-group" ); 
        return fail; 
    fi; 
    Cfd := Down2DimensionalGroup( front ); 
    Clr := Right2DimensionalGroup( left ); 
    down := Cat2Group( Cfd, Clr ); 
    if ( down = fail ) then 
        Info( InfoXMod, 1, "down fails to be a cat2-group" ); 
        return fail; 
    fi; 
    Cld := Down2DimensionalGroup( left ); 
    Cur := Right2DimensionalGroup( up ); 
    back := Cat2Group( Cld, Cur ); 
    if ( back = fail ) then 
        Info( InfoXMod, 1, "back fails to be a cat2-group" ); 
        return fail; 
    fi; 
    return [ up, right, down, back ]; 
end ); 

#############################################################################
##
#M  PreCat3GroupByPreCat2Groups( <front>,<left>,<up>,<right>,<down>,<back> ) 
##
InstallMethod( PreCat3GroupByPreCat2Groups, "for a pair of pre-cat2-groups", 
    true, [ IsPreCat2Group, IsPreCat2Group, IsPreCat2Group, 
            IsPreCat2Group, IsPreCat2Group, IsPreCat2Group ], 0,
function( front, left, up, right, down, back )

    local Cfu, Cfl, Cul, Cur, Cfr, Cfd, Cld, Clb, Cub, Crd, 
          G, R, Q, H, N, P, M, L; 

    Cfu := Up2DimensionalGroup( front ); 
    Cfl := Left2DimensionalGroup( front ); 
    Cul := Up2DimensionalGroup( up ); 
    Cur := Down2DimensionalGroup( up ); 
    Cfr := Right2DimensionalGroup( front ); 
    Cfd := Down2DimensionalGroup( front ); 
    Cld := Down2DimensionalGroup( left ); 
    Clb := Down2DimensionalGroup( left ); 
    Cub := Right2DimensionalGroup( up ); 
    Crd := Down2DimensionalGroup( right ); 
    G := Source( Cfu ); 
    R := Range( Cfu ); 
    Q := Range( Cfl ); 
    H := Range( Cub ); 
    N := Range( Cur ); 
    P := Range( Cfd ); 
    M := Range( Cld ); 
    L := Range( Crd ); 
    return PreCat3GroupObj( [ front, left, up, right, down, back ] ); 
end ); 

#############################################################################
##
#F  (Pre)Cat3Group( front, left )    cat3-group from two (pre)cat2-groups
#F  (Pre)Cat3Group( front, uplt )    cat3-group from cat2 and cat1-groups
#F  (Pre)Cat3Group( fup, flt, llt )  cat3-group from 3 cat1-groups
#F  (Pre)Cat3Group( XS )             cat3-group from (pre)crossed cube
##
InstallGlobalFunction( PreCat3Group, function( arg )

    local nargs, front, left, Cfl, Clu, urdb, C3G, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 3 ) ) then
        Print( "standard usage: (Pre)Cat3Group( front, left );\n" );
        Print( "            or: (Pre)Cat3Group( front, cat1 );\n" );
        Print( "            or: (Pre)Cat3Group( cat1, cat1, cat1 );\n" );
        Print( "            or: (Pre)Cat3Group( Xcube );\n" );
        return fail;
    fi; 
    if not ForAll( arg, HasHigherDimension ) then 
        Error( "each argument should have a higher dimension" ); 
    fi;
    if ( nargs = 1 ) then 
            Error( "PreCat3GroupOfPreCrossedcube not yet implemented" ); 
        ## C3G := PreCat3GroupOfPreCrossedcube( arg[1] );
    elif ( nargs = 3 ) then 
        front := PreCat2Group( arg[1], arg[2] ); 
        left := PreCat2Group( arg[2], arg[3] ); 
    elif ( nargs = 2 ) and ( HigherDimension( arg[2] ) = 2 ) then 
        front := arg[1]; 
        Cfl := Left2DimensionalGroup( arg[1] ); 
        left := PreCat2Group( Cfl, arg[2] ); 
    else 
        front := arg[1]; 
        left := arg[2]; 
    fi; 
    if ( ( front = fail ) or (left = fail ) ) then 
        return fail; 
    fi;
    if not IsPreCat2Group( front ) and IsPreCat2Group( left ) then 
        Error( "front and left are not both pre-cat2-groups" ); 
    fi; 
    Cfl := Left2DimensionalGroup( front ); 
    Clu := Up2DimensionalGroup( left ); 
    if not ( Cfl = Clu ) then 
        Info( InfoXMod, 1, "front-left <> left-up" ); 
        return fail; 
    fi; 
    urdb := DetermineRemainingCat2Groups( front, left ); 
    if ( urdb = fail ) then 
        Info( InfoXMod, 2, "failure with RemainingCat2Groups" ); 
        return fail; 
    fi;
    C3G := PreCat3GroupByPreCat2Groups( 
               front, left, urdb[1], urdb[2], urdb[3], urdb[4] ); 
    if ( C3G = fail ) then 
        return fail; 
    fi;
    ok := IsPreCat3Group( C3G );
    if ok then 
        ok := IsCat3Group( C3G ); 
        return C3G;
    else
        return fail;
    fi;
end );

InstallGlobalFunction( Cat3Group, function( arg )

    local nargs, C3G, ok; 

    nargs := Length( arg );
    if ( nargs = 3 ) then 
        C3G := PreCat3Group( arg[1], arg[2], arg[3] ); 
    elif ( nargs = 2 ) then 
        C3G := PreCat3Group( arg[1], arg[2] ); 
    elif ( nargs = 1 ) then 
        C3G := PreCat3Group( arg[1] ); 
    else 
        Print( "standard usage: (Pre)Cat3Group( cat2, cat2 );\n" );
        Print( "            or: (Pre)Cat3Group( cat2, cat1 );\n" );
        Print( "            or: (Pre)Cat3Group( cat1, cat1, cat1 );\n" );
        Print( "            or: (Pre)Cat3Group( XCube );\n" );
        return fail; 
    fi;
    ok := not ( C3G = fail ) and IsCat3Group( C3G ); 
    if ok then 
        return C3G; 
    else 
        return fail; 
    fi; 
end ); 

InstallMethod( AllCat3GroupTriples, "for a group", [ IsGroup ], 0,
function( G )

    local all1, all2, pairs, T, t, n, i, j, k, front, left, up, a, b, c, pos; 

    InitCatnGroupRecords( G ); 
    if IsBound( CatnGroupLists( G ).cat3triples ) then 
        return CatnGroupLists( G ).cat3triples; 
    fi; 
    all1 := AllCat1Groups( G ); 
    all2 := AllCat2Groups( G ); 
    pairs := CatnGroupLists( G ).cat2pairs; 
    T := [ ];
    t := 0; 
    n := Length( pairs ); 
    for i in [1..n] do 
        front := pairs[i]; 
        a := front[1]; 
        b := front[2]; 
        for j in [i..n] do 
            left := pairs[j]; 
            if ( left[1] = b ) then 
                c := left[2]; 
                up := [ c, a ]; 
                pos := Position( pairs, up ); 
                if ( pos = fail ) then 
                    pos := Position( pairs, [ a, c ] ); 
                fi; 
                if ( pos <> fail ) then 
                    Add( T, [ a, b, c ] ); 
                    t := t+1; 
                fi; 
            fi; 
        od; 
    od; 
    CatnGroupNumbers( G ).cat3 := Length( T ); 
    CatnGroupLists( G ).cat3triples := T; 
    return T; 
end ); 

InstallMethod( AllCat3GroupsNumber, "for a group", [ IsGroup ], 0, 
function( G ) 

    local n, C, triples; 

    InitCatnGroupRecords( G ); 
    if IsBound( CatnGroupNumbers( G ).cat3 ) then 
        return CatnGroupNumbers( G ).cat3; 
    fi; 
    ## not already known, so perform the calculation 
    triples := AllCat3GroupTriples( G ); 
    return Length( triples ); 
end ); 

InstallMethod( AllCat3Groups, "for a group", [ IsGroup ], 0, 
function( G ) 

    local all1, triples, n, L, i, t, C; 

    InitCatnGroupRecords( G ); 
    all1 := AllCat1Groups( G ); 
    triples := AllCat3GroupTriples( G ); 
    n := Length( triples ); 
    L := [ ]; 
    for i in [1..n] do
        t := triples[i]; 
        C := Cat3Group( all1[t[1]], all1[t[2]], all1[t[3]] );
        Add( L, C ); 
    od;
    return L; 
end ); 
