#############################################################################
##
#W  gpnobj.gi                   GAP4 package `XMod'             Chris Wensley
##                                                               Alper Odabas
##  This file implements generic methods for (pre-)catn-groups.
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  

#############################################################################
##
#M  IsHigherDimensionalGroup( <obj> )  . . . . . . . for 2-dimensional groups
#M  HigherDimension( <obj> )  . . . .  . . . . . . . for 2-dimensional groups
##
## InstallTrueMethod( IsHigherDimensionalGroup, Is2DimensionalGroup ); 
#? installing this method causes problems with printing!  

InstallImmediateMethod( HigherDimension, Is2DimensionalGroup, 0, 
    function( obj ) 
    return 2;
end );

#############################################################################
##
#M  HigherDimension . . . . . . .  for a higher dimensional domain or mapping
##
## #? this does not work because G is not a HigherDimensionalGroup 
##
## InstallMethod( HigherDimension, "generic method for a 2dim-group", true, 
##     [ Is2DimensionalDomain ], 0,
## function( G )
##     return 2;  
## end ); 

InstallMethod( HigherDimension, "generic method for an ndim-group", true, 
    [ IsHigherDimensionalGroup ], 0,
function( G )
    if ( HasIsPreCrossedSquare(G) and IsPreCrossedSquare(G) ) then 
        return 3; 
    elif ( HasIsPreCat2Group(G) and IsPreCat2Group(G) ) then 
        return 3; 
    elif HasGeneratingCat1Groups( G ) then 
        return Length( GeneratingCat1Groups(G) ) + 1; 
    else 
        Print( "HigherDimension not yet implemented\n" ); 
        return fail; 
    fi; 
end ); 

#############################################################################
##
#M  GroupsOfHigherDimensionalGroup . . . . . . for a higher-dimensional group
##
InstallOtherMethod( GroupsOfHigherDimensionalGroup, "method for an n-group", 
    true, [ IsHigherDimensionalGroup ], 0, 
function( G )

    local dim, u, d, f, b;

    dim := HigherDimension( G ); 
    if ( dim = 2 ) then 
        return [ Source(G), Range(G) ]; 
    elif ( dim = 3 ) then 
        u := Up2DimensionalGroup( G ); 
        d := Down2DimensionalGroup( G ); 
        return [ Source(u), Range(u), Source(d), Range(d) ]; 
    elif ( dim = 4 ) then 
        f := Front3DimensionalGroup( G ); 
        b := Back3DimensionalGroup( G ); 
        return Concatenation( GroupsOfHigherDimensionalGroup( f ), 
                              GroupsOfHigherDimensionalGroup( b ) );  
    else 
        Error( "only implemented for dim in [2,3,4] so far" ); 
    fi; 
end ); 

#############################################################################
##
#M  IsPreCatnGroup . . . . . . . .  check that the object is a pre-catn-group
##
InstallMethod( IsPreCatnGroup, "generic method for a pre-catn-group",
    true, [ IsHigherDimensionalGroup ], 0,
function( P )

    local G, C, L, n, i, j, endt, endh, ti, tj, hi, hj, PL;

    if not ( IsPreCatnObj( P )  ) then
        return false;
    fi;
    L := GeneratingCat1Groups( P );
    n := HigherDimension( P ) - 1;
    PL := [ ];
    for i in [1..n] do 
        if not ( IsPerm2DimensionalGroup( L[i] ) ) then
            C := Image( IsomorphismPermObject( L[i] ) );
        else
            C := L[i];
        fi;
        Add( PL, C, i );
    od;
    L := PL;
    G := Source( L[1] );
        if ForAny( L, C -> IsomorphismGroups(Source(C),G)=fail ) then
        Info( InfoXMod, 2, 
            "generating cat1-groups should have isomorphic sources" );
        return false;        
    fi;
    endt := ListWithIdenticalEntries( n, 0 ); 
    endh := ListWithIdenticalEntries( n, 0 ); 
    for i in [1..n] do 
        C := L[i]; 
        endt[i] := TailMap( C ) * RangeEmbedding( C ); 
        endh[i] := HeadMap( C ) * RangeEmbedding( C ); 
    od;
    # check conditions 1,2,3
    for i in [1..n-1] do 
        for j in [i+1..n] do 
            ti := endt[i]; 
            tj := endt[j];
            hi := endh[i];
            hj := endh[j];
            if not ( hi*hj = hj*hi ) then
                Info( InfoXMod, 2, "head maps do not commute at", [i,j] );
                return false;
            fi;        
            if not ( ti*tj = tj*ti ) then
                Info( InfoXMod, 2, "tail maps do not commute at", [i,j] );
                return false;
            fi;    
            if not ( ( hi*tj = tj*hi ) and ( hj*ti = ti*hj ) ) then
                Info( InfoXMod, 2, 
                    "head maps do not commute with tail maps at", [i,j] );
                return false;
            fi;        
        od;
    od;
    return true;
end );

#############################################################################
##
#M  IsCatnGroup . . . . . . . . . . . . check that the object is a catn-group
##
InstallMethod( IsCatnGroup, "generic method for a catn-group",
    true, [ IsHigherDimensionalGroup ], 0,
function( P )

    local L;

    if not ( IsPreCatnGroup( P ) ) then
        Info( InfoXMod, 2, "P is not a pre-catn-group" );
        return false;
    fi;
    L := GeneratingCat1Groups( P );
    if ForAny( L, x -> not IsCat1Group(x) ) then 
        Info( InfoXMod, 2, "each item in the list must be Cat1-Group" );
        return false;
    fi;
    return true;
end );

#############################################################################
##
#M  IsPreCatnGroupWithIdentityEmbeddings . . check that all e are identities
##
InstallMethod( IsPreCatnGroupWithIdentityEmbeddings, "test a pre-catn-group", 
    true, [ IsPreCatnGroup ], 0,
function( obj ) 

    local gps, G, dim, up, lt, rt, dn; 

    gps := GroupsOfHigherDimensionalGroup( obj ); 
    G := gps[1]; 
    if not ForAll( gps, H -> IsSubgroup( G, H ) ) then 
        return false; 
    fi; 
    dim := HigherDimension( obj ); 
    if ( dim = 2 ) then 
        return IsPreCat1GroupWithIdentityEmbedding( obj ); 
    elif ( dim = 3 ) then 
        up := Up2DimensionalGroup( obj ) ; 
        lt := Left2DimensionalGroup( obj ); 
        rt := Right2DimensionalGroup( obj ); 
        dn := Down2DimensionalGroup( obj ); 
        return IsPreCat1GroupWithIdentityEmbedding( up )
           and IsPreCat1GroupWithIdentityEmbedding( lt )  
           and IsPreCat1GroupWithIdentityEmbedding( rt )  
           and IsPreCat1GroupWithIdentityEmbedding( dn );  
    else 
        Error( "not implemented for dim >= 4" ); 
    fi;
end );

#############################################################################
##
#M  PreCatnObj ( <cat1-groups> ) . . . . . . . . . . . make a pre-catn object
##
InstallMethod( PreCatnObj, "for list of cat1-groups", true, [ IsList ], 0,
function( L )

    local G1, PC, ok, name, n;
    
    if ForAny( L, x -> not IsPreCat1Group(x) ) then 
        Print( "Each item in the list must be PreCat1-group \n" );
        return fail;
    fi;
    G1 := Source( L[1] ); 
    if ForAny( L, x -> ( IsomorphismGroups(Source(x),G1) = fail ) ) then 
        Error( "the cat1-groups fail to have isomorphic sources" ); 
    fi;
    n := Length( L );
    if ( n = 2 ) then 
        return PreCat2Group( L[1], L[2] ); 
    elif ( n = 3 ) then 
        return PreCat3Group( L[1], L[2], L[3] ); 
    else 
        PC := rec();
        ObjectifyWithAttributes( PC, PreCatnObjType, 
            GeneratingCat1Groups, L, 
            HigherDimension, n+1, 
            IsHigherDimensionalGroup, true );
        if not IsPreCatnGroup( PC ) then
            Info( InfoXMod, 1, "Warning: not a pre-catn-group" ); 
            PC := fail; 
        fi;
        return PC; 
    fi;
end );

#############################################################################
##
#F  CatnGroup( <size>, <gpnum>, <dim>, <num> )        from data in CATn_LIST
#F  CatnGroup( L )                       catn-group from list of cat1-groups
#F  PreCatnGroup( L )            pre-catn-group from list of pre-cat1-groups
##
InstallGlobalFunction( PreCatnGroup, function( arg )

    local nargs, PnG, ok, usage;

    nargs := Length( arg );
    usage := 
        "standard usage: PreCatnGroup( [pre-cat1-gp,pre-cat1-gp,...] );\n";
    if not ( ( nargs = 1 ) and IsList( arg[1] ) ) then 
        Print( usage );
        Error( "please input a list of cat1-groups" );
    fi; 
    PnG := PreCatnObj( arg[1] ); 
    if ( PnG = fail ) then 
        Print( usage ); 
    else 
        ok := IsPreCatnGroup( PnG );
        if ok then
            return PnG;
        fi;                
    fi;        
    return fail;
end );

InstallGlobalFunction( CatnGroup, function( arg )

    local nargs, CnG, ok, ok2, usage1, usage2, dim;

    nargs := Length( arg );    
    usage1 := 
        "standard usage: CatnGroup( [cat1-group,cat1-group,...] );\n"; 
    ##  usage2 will become relevant if CatnSelect is implemented 
    usage2 := 
        "            or: CatnGroup( size, gpnum, dimension, num );\n";
    if not ( ( nargs = 1 ) or ( nargs = 4 ) ) then 
        Print( usage1 );
    fi; 
    if ( nargs = 1 ) then
        CnG := PreCatnObj( arg[1] ); 
        if ( CnG = fail ) then 
            Print( usage1 ); 
        else 
            ok := IsPreCatnGroup( CnG ); 
            if ok then 
                dim := HigherDimension( CnG ); 
                if ( dim = 3 ) then 
                    SetIsPreCat2Group( CnG, true ); 
                    ok2 := IsCatnGroup( CnG );
                    if ok2 then
                        SetIsCat2Group( CnG, true ); 
                    fi;
                fi; 
                return CnG;
            fi;                
        fi;
    elif ( (nargs = 4) and IsInt( arg[1] ) and IsInt( arg[2] ) 
                   and  IsInt( arg[3] ) and IsInt( arg[4] )  ) then
        Print( "CatnSelect is not yet implemented\n" ); 
        # return CatnSelect( arg[1], arg[2], arg[3], arg[4] );
    else   
        Print( usage1 ); 
    fi;
    return fail;
end );

#############################################################################
##
#M  \=( <dom1>, <dom2> ) . . test if two higher dimensional objects are equal
##
InstallMethod( \=,
    "generic method for two higher dimensional domains",
    IsIdenticalObj, [ IsHigherDimensionalGroup, IsHigherDimensionalGroup ], 0,
function ( dom1, dom2 )
    
    local n1, n2, L1, L2;
    
    n1 := HigherDimension( dom1 )-1;
    n2 := HigherDimension( dom2 )-1;
    if ( n1  <>  n2 ) then
        return false;
    fi;
    L1 := GeneratingCat1Groups( dom1 );
    L2 := GeneratingCat1Groups( dom2 );
    return ForAll( [1..n1], i -> L1[i] = L2[i] );
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . . for n-dimensional groups 
##
InstallMethod( String, "for an nd-group", true, 
    [ IsHigherDimensionalGroup ], 0, 
function( gnd ) 
    return( STRINGIFY( "higher dimensional group with dimension ", 
                       String( HigherDimension(gnd)-1 ) ) ); 
end );

InstallMethod( ViewString, "for an nd-group", true, 
    [ IsHigherDimensionalGroup ], 0, String ); 

InstallMethod( PrintString, "for an nd-group", true, 
    [ IsHigherDimensionalGroup ], 0, String ); 

InstallMethod( PrintObj, "for an nd-group", true, 
    [ IsHigherDimensionalGroup ], 0,
function( gnd )

    local i, n, L;

    if HasName( gnd ) then
        Print( Name( gnd ), "\n" );
    else 
        n := HigherDimension(gnd)-1; 
        L := GeneratingCat1Groups( gnd ); 
        Print( "(pre-)cat", n, "-group with generating (pre-)cat1-groups:\n" );
        for i in [1..n] do 
            Print( i, " : ", L[i] ); 
            if ( i < n ) then 
                Print( "\n" );
            fi;
        od;   
    fi;
end );

InstallMethod( ViewObj, "method for a nd-group", true, 
    [ IsHigherDimensionalGroup ], 0, PrintObj ); 

#############################################################################
##
#M Display( <gnd> . . . . . . . . . . . . . . . . . . . . display a nd-group 
##
InstallMethod( Display, "method for a nd-group", true, 
    [ IsHigherDimensionalGroup ], 0,
function( gnd )

    local i, n, L;

    n := HigherDimension( gnd ) - 1;
    if IsPreCatnGroup( gnd ) then 
        L := GeneratingCat1Groups( gnd );
        Print( "(pre-)cat", n, "-group with generating (pre-)cat1-groups:\n" ); 
        for i in [1..n] do 
            Print( i, " : " ); 
            Display( L[i] );
        od; 
    else 
        Print( "Display not yet implemented for this object\n" ); 
    fi;
end );

#############################################################################
##
#M  IdGroup . . . . . . . . . . . . . . . . . for a higher-dimensional domain
##
InstallOtherMethod( IdGroup, "method for a nd-domain", true, 
    [ IsHigherDimensionalDomain ], 0, 
function( dom )

    local u, d;

    u := Up2DimensionalGroup( dom ); 
    d := Down2DimensionalGroup( dom ); 
    return [ [ IdGroup( Source(u) ), IdGroup( Range(u) ) ], 
             [ IdGroup( Source(d) ), IdGroup( Range(d) ) ] ]; 
end ); 

#############################################################################
##
#M  IsPermHigherDimensionalgroup  . . . . . . for a higher-dimensional domain
##
InstallOtherMethod( IsPermHigherDimensionalGroup, "method for a nd-domain", 
    true, [ IsHigherDimensionalDomain ], 0, 
function( dom )
    return IsHigherDimensionalGroup( dom ) 
           and ForAll( GeneratingCat1Groups( dom ),
                       V -> IsPerm2DimensionalGroup( V ) ); 
end ); 
