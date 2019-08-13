##############################################################################
##
#W  gpnmap.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for Higher Dimensional Mappings for 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  MakeHigherDimensionalGroupMorphism( [ <src>, <rng>, <list of maps> ) 
##
InstallMethod( MakeHigherDimensionalGroupMorphism,
    "for two higher dimensional objects and list of 2d-morphisms", true, 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup, IsList ], 0,
function( src, rng, mors )

    local mor, dim, n, i;
    
    dim := HigherDimension( src );
    if ( dim <> HigherDimension( rng ) ) then 
        Info( InfoXMod, 1, "Source and Range must have the same dimension" );
        return fail;
    fi;
    n := Length( mors ); 
    if ( HasIsPreCat2Group(src) and IsPreCat2Group(src) ) then 
        if ( 2^(dim-1) <> n ) then
            Info( InfoXMod, 1, "3rd argument should have length ", 2^(dim-1) );
            return fail;
        fi; 
    elif ( HasIsPreCrossedSquare(src) and IsPreCrossedSquare(src) ) then  
        ## morphism of (pre)crossed squares 
        if ( dim  <> n-1 ) then
            Info( InfoXMod, 1, "third argument should have length ", dim+1 );
            return fail; 
        fi;
    fi;
    if ForAny( mors, x -> not Is2DimensionalGroupMorphism(x) ) then 
        Info( InfoXMod, 1, 
              "Entries in list mors must be pre-cat1-group morphisms" );
        return fail;
    fi;
    mor := rec();
    ObjectifyWithAttributes( mor, TypeHigherDimensionalGroupMorphism,
        Source, src,
        Range, rng,
        HigherDimension, dim );
    if ( HasIsPreCrossedSquare( src ) and IsPreCrossedSquare( src ) ) 
        or ( HasIsPreCat2Group( src ) and IsPreCat2Group( src ) ) then 
        SetUp2DimensionalMorphism( mor, mors[1] );
        SetLeft2DimensionalMorphism( mor, mors[2] );
        SetRight2DimensionalMorphism( mor, mors[3] );
        SetDown2DimensionalMorphism( mor, mors[4] ); 
    else 
        SetListOf2DimensionalMappings( mor, mors ); 
    fi; 
    return mor; 
end );

#############################################################################
##
#M  IsPreCatnGroupMorphism . . . . . check the axioms for a pre-catn morphism
##
InstallMethod( IsPreCatnGroupMorphism,
    "generic method for pre-catn homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PC, QC, dim, upmor, dnmor, ok, 2dmor, genPC, genQC, rangePC, rangeQC, 
          x, rnghoms, srchoms, G1, G2, P1, P2, p, q, comp, perm2dmor, i, 
          gamma, rho, sigma, pi;

    PC := Source( mor );
    QC := Range( mor );
    dim := HigherDimension( PC ); 
    if not ( dim = HigherDimension( QC ) ) then
        Info( InfoXMod, 1, "PC, QC do not have the same dimension" );
        return false;
    fi; 
    if ( dim = 3 ) then 
        if ( HasIsPreCat2Group( PC ) and IsPreCat2Group( PC ) ) then 
            return IsPreCat2GroupMorphism( mor ); 
        else 
            return IsPreCrossedSquareMorphism( mor ); 
        fi; 
    fi; 
    Error( "should never get here - method not yet implemented" );
    2dmor := ListOf2DimensionalMappings( mor );
    if ForAny( 2dmor, x -> not Is2DimensionalGroupMorphism(x) ) then 
        Info( InfoXMod, 1, "Is2DimensionalGroupMorphism(x) fails for ", x );
        return false;
    fi;
    genPC := GeneratingCat1Groups( PC ); 
    genQC := GeneratingCat1Groups( QC ); 
    rangePC := List( genPC, x -> Range(x) );
    rangeQC := List( genQC, x -> Range(x) );
    rnghoms := List( 2dmor, x -> RangeHom(x) );
    srchoms := List( 2dmor, x -> SourceHom(x) ); 
    for x in [1..Length(genPC)] do 
        if ( Source( rnghoms[x] ) <> rangePC[x] ) then 
            Info( InfoXMod, 1, x, "a : ", Source(rnghoms[x]), 
                  " <> ", rangePC[x] );  
            return false;
        fi;    
        if ( Range( rnghoms[x] ) <> rangeQC[x] ) then 
            Info( InfoXMod, 1, x, "b : ", Source(rnghoms[x]), 
                  " <> ", rangePC[x] );  
            return false;
        fi;    
    od;
    
    # The cause of the wrong result : SmallGroup(n,m) <> SmallGroup(n,m) 
    # Print("Problem of equality of morphism \n");
    # construct perm 2d-morphisms
    
    if not ( IsPermHigherDimensionalGroup( PC ) and 
             IsPermHigherDimensionalGroup( QC ) ) then 
        Error( "only implemented for permutation groups so far" ); 
    fi; 
##    perm 2dmor := [ ]; 
##    for i in [1..Length(2dmor)] do
##        G1 := Source( SourceHom( 2dmor[i] ) );
##        G2 := Range( SourceHom( 2dmor[i] ) );
##        P1 := ImagesSource( IsomorphismPermGroup( G1 ) );
##        P2 := ImagesSource( IsomorphismPermGroup( G2 ) );
##        p := IsomorphismGroups( P1, G1 );
##        q := IsomorphismGroups( G2, P2 );
##        comp := p * SourceHom( 2dmor[i] ) * q;    
##        Add( perm2dmor, comp, i );
##    od;
    
    # check that equality of vertex homomorphisms 
    if ( Length( 2dmor ) > 4 ) then 
        Error( "not yet implemented for dimension >=3" ); 
    fi; 
    gamma := SourceHom( 2dmor[1] ); 
    if not ( gamma = SourceHom( 2dmor[2] ) ) then 
        Info( InfoXMod, 2, "unequal gammas" ); 
        return false; 
    fi; 
    rho := RangeHom( 2dmor[1] ); 
    if not ( rho = SourceHom( 2dmor[3] ) ) then 
        Info( InfoXMod, 2, "unequal rhos" ); 
        return false; 
    fi; 
    sigma := RangeHom( 2dmor[2] ); 
    if not ( sigma = SourceHom( 2dmor[4] ) ) then 
        Info( InfoXMod, 2, "unequal sigmas" ); 
        return false; 
    fi; 
    pi := RangeHom( 2dmor[3] ); 
    if not ( pi = RangeHom( 2dmor[4] ) ) then 
        Info( InfoXMod, 2, "unequal pis" ); 
        return false; 
    fi; 
    return true;
end );

#############################################################################
##
#M  IsCatnGroupMorphism
##
InstallMethod( IsCatnGroupMorphism, "generic method for higher dim mappings", 
    true, [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )
    local ispre;
    ispre := IsPreCatnGroupMorphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCatnGroup( Source(mor) ) and IsCatnGroup( Range(mor) ) );
    fi;
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . test if two higher dimensional morphisms are equal
##
InstallMethod( \=,
    "generic method for two higher dimensional morphisms", IsIdenticalObj, 
    [ IsHigherDimensionalMapping, IsHigherDimensionalMapping ], 0,
function ( mor, phi )
    
    local n1, n2, mors1, mors2;
    
    n1 := HigherDimension( mor );
    n2 := HigherDimension( phi );
    mors1 := ListOf2DimensionalMappings( mor );
    mors2 := ListOf2DimensionalMappings( phi );        
    
    if ( ( n1 <> n2 ) or ( mors1 <> mors2  ) ) then
        return false;
    elif ( IsPreCatnGroupMorphism( mor ) and IsPreCatnGroupMorphism( phi ) 
           and (Source(mor) = Source(phi)) and (Range(mor) = Range(phi)) ) then
        return true;
    fi;
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . for a higher dimensional mapping
##
InstallOtherMethod( MappingGeneratorsImages, "method for a higher dim mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )
    
    local mors, imors;
    
    mors := ListOf2DimensionalMappings( map );
    imors := List( mors, f -> MappingGeneratorsImages(f) );
    return imors;
end );

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . .  or a higher dimensional mapping
##
InstallMethod( Name, "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local nsrc, nrng, name;

    if HasName( Source( mor ) ) then
        nsrc := Name( Source( mor ) );
    else
        nsrc := "[..]";
    fi;
    if HasName( Range( mor ) ) then
        nrng := Name( Range( mor ) );
    else
        nrng := "[..]";
    fi;
    name := Concatenation( "[", nsrc, " => ", nrng, "]" );
    SetName( mor, name );
    return name;
end );

#############################################################################
##
#F  Display( <mor> ) . . . . .  print details of a higher dimensional mapping 
##
InstallMethod( Display, "display a higher dimensional mapping", true,
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local P, Q, n, mors, i;

    P := Source( mor );
    Q := Range( mor );
    n := HigherDimension( mor ); 
    if (( n = 3 ) and HasIsPreCrossedSquareMorphism( mor ) ) then 
        Display3DimensionalMorphism( mor ); 
    else 
        mors := ListOf2DimensionalMappings( mor );
        if ( HasIsCatnGroupMorphism( mor ) and IsCatnGroupMorphism( mor ) ) then
            Print( "Morphism of cat",n-1,"-groups :- \n" );
        else
            Print( "Morphism of pre-cat",n-1,"-groups :- \n" );
        fi; 
        if HasName( P ) then
            Print( ": Source = ", Name( P ), "\n" );
        else
            Print( ": Source has ", P, "\n" );
        fi;
        if HasName( Q ) then
            Print( ": Range = ", Name( Q ), "\n" );
        else
            Print( ": Range has ", Q, "\n" );
        fi;
        if HasOrder( mor ) then
            Print( ": order = ", Order( mor ), "\n" );
        fi; 
        Print( ": MappingGeneratorsImages for the source homomorphisms:\n" );
        for i in [1..Length(mors)] do
            Print( i, " : ", MappingGeneratorsImages(SourceHom(mors[i])),"\n");
        od;
    fi;
end ); 

##############################################################################
##
#M  IdentityMapping( <obj> )
##
InstallOtherMethod( IdentityMapping, "for higher dimensional object", true,
    [ IsHigherDimensionalGroup ], 0,
function( obj )

    local dim, idmaps;
    
    ## this works for catn-groups but should be extended to n-cubes 
    dim := HigherDimension( obj ); 
    if ( dim = 3 ) then 
        idmaps := [ IdentityMapping( Up2DimensionalGroup( obj ) ), 
                    IdentityMapping( Left2DimensionalGroup( obj ) ), 
                    IdentityMapping( Right2DimensionalGroup( obj ) ), 
                    IdentityMapping( Down2DimensionalGroup( obj ) ) ]; 
        if ( HasIsCrossedSquare( obj ) and IsCrossedSquare( obj ) ) then 
            return CrossedSquareMorphismByXModMorphisms( obj, obj, idmaps ); 
        elif ( HasIsCat2Group( obj ) and IsCat2Group( obj ) ) then 
            return PreCat2GroupMorphismByPreCat1GroupMorphisms(obj,obj,idmaps); 
        else 
            Error( "should never get here" ); 
        fi; 
    else 
        idmaps := List( GeneratingCat1Groups( obj ), C -> IdentityMapping(C) ); 
        return PreCatnGroupMorphismByMorphisms( obj, obj, idmaps ); 
    fi;
end );

##############################################################################
##
#F  CatnGroupMorphism( [list] ) . . catn morphism
##
InstallGlobalFunction( CatnGroupMorphism, function( arg )

    local nargs;
    nargs := Length( arg );

    if ( ( nargs = 3 ) and IsCatnGroup( arg[1]) 
         and IsCatnGroup( arg[2] ) and IsList( arg[3] ) ) then
        return  CatnGroupMorphismByMorphisms( arg[1], arg[2], arg[3] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: CatnGroupMorphism( src, rng, list of maps );" );
    return fail;
end );

###############################################################################
##
#M  PreCatnGroupMorphismByMorphisms( <src>, <rng>, <list of maps> ) 
##
InstallMethod( PreCatnGroupMorphismByMorphisms,
    "method for a higher dimensional mapping", true,
    [ IsPreCatnGroup, IsPreCatnGroup, IsList ], 0,
function( src, rng, L )

    local dim, filter, fam, mor, ok, nsrc, nrng, name, n;
    
    dim := HigherDimension( src )-1; 
    if not ( dim = HigherDimension(rng)-1 ) then 
        Error( "src and rng have different dimensions" ); 
    fi; 
    if ( Length(L) <> dim ) then 
        Info( InfoXMod, 1, "L should contain ", dim, " morphisms" );
        return fail;        
    fi;
    mor := MakeHigherDimensionalGroupMorphism( src, rng, L );
        if not IsPreCatnGroupMorphism( mor ) then
            Info( InfoXMod, 2, "not a morphism of pre-catn-groups" );
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CatnGroupMorphismByMorphisms . . . . . . for a higher dimensional mapping
##
InstallMethod( CatnGroupMorphismByMorphisms, "method for a nd-mapping", true, 
    [ IsCatnGroup, IsCatnGroup, IsList], 0,
function( src, rng, L )

    local mor, ok;
    mor := PreCatnGroupMorphismByMorphisms( src, rng, L );
    ok := IsCatnGroupMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CompositionMorphism  . . . . . . . . . . .for a higher dimensional mapping
##
InstallOtherMethod( CompositionMorphism, "generic method for nd-mappings",
    IsIdenticalObj, [ IsHigherDimensionalMapping, IsHigherDimensionalMapping ], 
    0,
function( m2, m1 )

    local up, lt, rt, dn, comp, ok, list, comps, n1, n2, mors1, mors2, i;
    
    n1 := HigherDimension( m1 );
    n2 := HigherDimension( m2 );
    mors1 := ListOf2DimensionalMappings( m1 );
    mors2 := ListOf2DimensionalMappings( m2 );    

    if not ( Range( m1 ) = Source( m2 ) ) then
        Info( InfoXMod, 2, "Range(m1) <> Source(m2)" );
        return fail;
    fi;
    if not ( n1 = n2 ) then
        Info( InfoXMod, 2, "Dimension - (m1) <> Dimension - (m2)" );
        return fail;
    fi;
    
    comps := List( [1..Length(mors1)], 
                   i -> CompositionMapping( mors2[i], mors1[i] ) );
    list := [ Source( m1 ), Range( m2 ) ];
    Append( list, comps );
    comp := MakeHigherDimensionalMapping( list );
    if ( IsPreCatnGroupMorphism( m1 ) and IsPreCatnGroupMorphism( m2 ) ) then
        SetIsPreCatnGroupMorphism( comp, true );
    fi;
    if ( IsCatnGroupMorphism( m1 ) and IsCatnGroupMorphism( m2 ) ) then
        SetIsCatnGroupMorphism( comp, true );
    fi;
    return comp;
end );

##############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( Order, "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local dim, ok, 2d_maps;

    dim := HigherDimension( mor );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(mor), Left2DimensionalMorphism(mor), 
              Right2DimensionalMorphism(mor), Down2DimensionalMorphism(mor) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(mor); 
    fi; 
    if not ( IsAutomorphismHigherDimensionalDomain(mor) ) then
       Info( InfoXMod, 2, "Parameter is not an automorphism" );
       return fail;
    fi;
       return  Lcm( List(2d_maps, f -> Order(f) ) );
end );

##############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsInjective, "method for a higher dimensional mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsInjective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsSurjective, "method for a higher dimensional mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsSurjective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsSingleValued, "method for a higher dimensional mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsSingleValued(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsTotal, "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsTotal(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsBijective, "method for a higher dimensional mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsBijective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsEndomorphismHigherDimensionalDomain( map ) . . for a higher dim mapping
#M  IsAutomorphismHigherDimensionalDomain( map ) . . for a higher dim mapping
##
InstallMethod( IsEndomorphismHigherDimensionalDomain, 
    "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( map )

    local dim, ok, 2d_maps;
    
    dim := HigherDimension( map );
    if ( dim = 3 ) then 
        2d_maps := 
            [ Up2DimensionalMorphism(map), Left2DimensionalMorphism(map), 
              Right2DimensionalMorphism(map), Down2DimensionalMorphism(map) ]; 
    else 
        2d_maps := ListOf2DimensionalMappings(map); 
    fi; 
    ok := ForAll( 2d_maps, f -> IsEndomorphism2DimensionalDomain( f ) );
    if not ok then
        return false;
    fi;    
    return true;
end );

InstallMethod( IsAutomorphismHigherDimensionalDomain, 
    "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
    map -> IsEndomorphismHigherDimensionalDomain(map) and IsBijective(map) );
