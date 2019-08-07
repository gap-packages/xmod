##############################################################################
##
#W  gp4obj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)crossed cubes 
##  and (pre-)cat3-groups.
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
    
##############################################################################
##
#M  PreCat3GroupObj( [<front>,<up>,<left>,<right>,<down>,<back>] ) 
##
InstallMethod( PreCat3GroupObj, "for a list of pre-cat1-groups", true,
    [ IsList ], 0,
function( L )

    local PC, ok;

    if not ( Length( L ) = 6 ) then 
        Error( "there should be 6 pre-cat2-groups in the list L" ); 
    fi; 
    PC := rec();
    ObjectifyWithAttributes( PC, PreCat2GroupObjType, 
      Front3DimensionalGroup, L[1], 
      Up3DimensionalGroup, L[2], 
      Left3DimensionalGroup, L[3],
      Right3DimensionalGroup, L[4],
      Down3DimensionalGroup, L[5],
      Back3DimensionalGroup, L[6], 
      GeneratingCat1Groups, [ Up2DimensionalGroup( L[1] ),
                              Left2DimensionalGroup( L[1] ), 
                              Up2DimensionalGroup( L[2] ) ],
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
        and IsCat2Group( Up3DimensionalGroup( P ) )  
        and IsCat2Group( Left3DimensionalGroup( P ) )  
        and IsCat2Group( Right3DimensionalGroup( P ) )  
        and IsCat2Group( Down3DimensionalGroup( P ) )  
        and IsCat2Group( Back3DimensionalGroup( P ) );  
end ); 

##############################################################################
##
#M  DetermineRemainingCat2Groups . . . . . . . . . . . for two pre-cat2-groups
## 
InstallMethod( DetermineRemainingCat2Groups, "for front, up pre-cat2-groups", 
    true, [ IsPreCat2Group, IsPreCat2Group ], 0,
function( front, up )

    local Cfu, Cfl, Cul, left, Cur, Cfr, right, Cfd, Clr, down, 
          Clb, Cub, back, Crd, G, R, Q, H, N, P, M, L; 

    Cfu := Up2DimensionalGroup( front ); 
    if not ( Cfu = Left2DimensionalGroup( up ) ) then 
        Error( "front-up mis-match" ); 
    fi; 
    Cfl := Left2DimensionalGroup( front ); 
    Cul := Up2DimensionalGroup( up ); 
    left := Cat2Group( Cfl, Cul ); 
    if ( left = fail ) then 
        Info( InfoXMod, 1, "left fails to be a cat2-group" ); 
        return fail; 
    fi; 
    Cur := Down2DimensionalGroup( up ); 
    Cfr := Right2DimensionalGroup( front ); 
    right := Cat2Group( Cur, Cfr ); 
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
    Clb := Down2DimensionalGroup( left ); 
    Cub := Right2DimensionalGroup( up ); 
    back := Cat2Group( Clb, Cub ); 
    if ( back = fail ) then 
        Info( InfoXMod, 1, "back fails to be a cat2-group" ); 
        return fail; 
    fi; 
    return [ left, right, down, back ]; 
end ); 

##############################################################################
##
#M  PreCat3GroupByPreCat2Groups( <front>,<up>,<left>,<right>,<down>,<back> ) 
##
InstallMethod( PreCat3GroupByPreCat2Groups, "for a pair of pre-cat2-groups", 
    true, [ IsPreCat2Group, IsPreCat2Group, IsPreCat2Group, 
            IsPreCat2Group, IsPreCat2Group, IsPreCat2Group ], 0,
function( front, up, left, right, down, back )

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
    return PreCat3GroupObj( [ front, up, left, right, down, back ] ); 
end ); 

#############################################################################
##
#F  (Pre)Cat3Group( C2front, C2up )      cat3-group from two (pre)cat2-groups
#F  (Pre)Cat3Group( XS )                 cat3-group from (pre)crossed cube
##
InstallGlobalFunction( PreCat3Group, function( arg )

    local nargs, C2f, C2u, C1uf, C1uf2, lrdb, C3G, ok;

    nargs := Length( arg );
    if ( ( nargs < 1 ) or ( nargs > 3 ) ) then
        Print( "standard usage: (Pre)Cat3Group( cat2, cat2 );\n" );
        Print( "            or: (Pre)Cat3Group( cat1, cat1, cat1 );\n" );
        Print( "            or: (Pre)Cat3Group( Xcube );\n" );
        return fail;
    fi; 
    if ( nargs = 1 ) then 
            Error( "argument is not a pre-crossed cube" ); 
        ## C3G := PreCat3GroupOfPreCrossedcube( arg[1] );
    elif ( nargs = 3 ) then 
        C2f := Cat2Group( arg[1], arg[2] ); 
        C2u := Cat2Group( arg[3], arg[1] ); 
    else 
        C2f := arg[1]; 
        C2u := arg[2]; 
    fi; 
    if not IsPreCat2Group( C2f ) and IsPreCat2Group( C2u ) then 
        Error( "C2f and C2u are not both pre-cat2-groups" ); 
    fi; 
    C1uf := Up2DimensionalGroup( C2f ); 
    C1uf2 := Left2DimensionalGroup( C2u ); 
    ## if the two cat1-groups are unequal but isomorphic then make 
    ## an isomorphic copy of .... 
    if not ( C1uf = C1uf2 ) then 
        ## iso := IsomorphismGroups( S2, S1 ); 
        Error( "C1uf <> C1uf2" ); 
    fi; 
    lrdb := DetermineRemainingCat2Groups( C2f, C2u ); 
    if ( lrdb = fail ) then 
        Info( InfoXMod, 2, "failure with RemainingCat2Groups" ); 
        return fail; 
    fi;
    C3G := PreCat3GroupByPreCat2Groups( 
               C2f, C2u, lrdb[1], lrdb[2], lrdb[3], lrdb[4] ); 
    if ( C3G = fail ) then 
        return fail;   ## Error( "C3G fails to be a PreCat3Group" ); 
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
    if ( nargs = 2 ) then 
        C3G := PreCat3Group( arg[1], arg[2] ); 
    elif ( nargs = 3 ) then 
        C3G := PreCat3Group( arg[1], arg[2], arg[3] ); 
    else 
        Print( "standard usage: (Pre)Cat3Group( cat2, cat2 );\n" );
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

