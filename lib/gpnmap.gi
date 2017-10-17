##############################################################################
##
#W  gpnmap.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for Higher Dimensional Mappings for 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  MakeHigherDimensionalGroupMorphism( [ <src>, <rng>, <list of maps> ) 
##
InstallMethod( MakeHigherDimensionalGroupMorphism,
    "for two higher dimensional objects and list of 2dim-morphisms", true, 
    [ IsHigherDimensionalGroup, IsHigherDimensionalGroup, IsList ], 0,
function( src, rng, mors )

    local mor, dim, n, i;
    
    dim := HigherDimension( src );
    if ( dim <> HigherDimension( rng ) ) then 
        Print( "Source and Range must have the same dimension\n" );
        return fail;
    fi;
    n := Length( mors ); 
    if ( HasIsPreCat2Group(src) and IsPreCat2Group(src) ) then 
        if ( dim-1  <> n ) then
            Print( "third argument should have length ", dim-1, "\n" );
            return fail;
        fi; 
    elif ( HasIsPreCrossedSquare(src) and IsPreCrossedSquare(src) ) then  
        ## morphism of (pre)crossed squares 
        if ( dim  <> n-1 ) then
            Print( "third argument should have length ", dim+1, "\n" );
            return fail; 
        fi;
    fi;
    if ForAny( mors, x -> not Is2DimensionalGroupMorphism(x) ) then 
        Print( "Entries in list mors must be pre-cat1-group morphisms\n" );
        return fail;
    fi;
    mor := rec();
    ObjectifyWithAttributes( mor, TypeHigherDimensionalGroupMorphism,
        Source, src,
        Range, rng,
        ListOfHomomorphisms, mors,
        HigherDimension, dim );
    if ( HasIsPreCrossedSquare( src ) and IsPreCrossedSquare( src ) ) then 
        SetUp2DimensionalMorphism( mor, mors[1] );
        SetLeft2DimensionalMorphism( mor, mors[2] );
        SetRight2DimensionalMorphism( mor, mors[3] );
        SetDown2DimensionalMorphism( mor, mors[4] ); 
    fi; 
    return mor; 
end );


#############################################################################
##
#M  IsPreCatnMorphism . . . . . . .  check the axioms for a pre-catn morphism
##
InstallMethod( IsPreCatnMorphism,
    "generic method for pre-catn homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PC, QC, upmor, dnmor, ok, 2dmor, genPC, genQC, rangePC, rangeQC, x,
           rnghoms, srchoms, G1, G2, P1, P2, p, q, comp, perm2dmor, i;

    PC := Source( mor );
    QC := Range( mor );
    if not ( IsHigherDimensionalGroup( PC ) 
             and IsHigherDimensionalGroup( QC ) ) then
        return false;
    fi;
    2dmor := ListOfHomomorphisms( mor );
    if ForAny( 2dmor, x -> not Is2DimensionalGroupMorphism(x) ) then 
        return false;
    fi;
    genPC := GeneratingCat1Groups( PC ); 
    genQC := GeneratingCat1Groups( QC ); 
    rangePC := List( genPC, x -> Range(x) );
    rangeQC := List( genQC, x -> Range(x) );
    rnghoms := List( 2dmor, x -> RangeHom(x) );
    srchoms := List( 2dmor, x -> SourceHom(x) ); 
    for x in [1..Length(2dmor)] do 
        if ( Source( rnghoms[x] ) <> rangePC[x] ) then 
            Print( x, "a : ", Source(rnghoms[x]), " <> ", rangePC[x], "\n" );  
            return false;
        fi;    
        if ( Range( rnghoms[x] ) <> rangeQC[x] ) then 
            Print( x, "b : ", Source(rnghoms[x]), " <> ", rangePC[x], "\n" );  
            return false;
        fi;    
    od;
    
    # The cause of the wrong result : SmallGroup(n,m) <> SmallGroup(n,m) 
    # Print("Problem of equality of morphism \n");
    # construct perm 2d-morphisms
    
    perm2dmor := [];
    
    for i in [1..Length(2dmor)] do
        G1 := Source(SourceHom( 2dmor[i] ));
        G2 := Range(SourceHom( 2dmor[i] ));
        P1 := Image(IsomorphismPermGroup(G1));
        P2 := Image(IsomorphismPermGroup(G2));
        p := IsomorphismGroups(P1,G1);
        q := IsomorphismGroups(G2,P2);
        comp := p * SourceHom( 2dmor[i] ) * q;    
        Add(perm2dmor,comp,i);
    od;
    
    # check that equality of srchoms
    if ForAny(perm2dmor, x ->  perm2dmor[1] <> x ) then 
        return false;
    fi; 
    return true;
end );

#############################################################################
##
#M  IsCatnMorphism
##
InstallMethod( IsCatnMorphism, "generic method for higher dimensional mappings", true,
   [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )
    local ispre;
    ispre := IsPreCatnMorphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCatnGroup( Source( mor ) ) and IsCatnGroup(  Range( mor ) ) );
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
    mors1 := ListOfHomomorphisms( mor );
    mors2 := ListOfHomomorphisms( phi );        
    
    if ( ( n1 <> n2 ) or ( mors1 <> mors2  ) ) then
        return false;
    elif ( IsPreCatnMorphism( mor ) and IsPreCatnMorphism( phi ) 
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
    
    mors := ListOfHomomorphisms( map );
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
        Display3dMorphism( mor ); 
    else 
        mors := ListOfHomomorphisms( mor );
        if ( HasIsCatnMorphism( mor ) and IsCatnMorphism( mor ) ) then
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

    local idmaps;
    
    ## this works for catn-groups but should be extended to n-cubes 
    if ( HasIsCrossedSquare( obj ) and IsCrossedSquare( obj ) ) then 
        idmaps := [ IdentityMapping( Up2DimensionalGroup( obj ) ), 
                    IdentityMapping( Left2DimensionalGroup( obj ) ), 
                    IdentityMapping( Right2DimensionalGroup( obj ) ), 
                    IdentityMapping( Down2DimensionalGroup( obj ) ) ]; 
        return CrossedSquareMorphismByMorphisms( obj, obj, idmaps ); 
    else 
        idmaps := List( GeneratingCat1Groups( obj ), C -> IdentityMapping(C) ); 
        return PreCatnMorphismByMorphisms( obj, obj, idmaps ); 
    fi;
end );

##############################################################################
##
#F  CatnMorphism( [list] ) . . catn morphism
##
InstallGlobalFunction( CatnMorphism, function( arg )

    local nargs;
    nargs := Length( arg );

    if ( ( nargs = 3 ) and IsCatnGroup( arg[1]) 
         and IsCatnGroup( arg[2] ) and IsList( arg[3] ) ) then
        return  CatnMorphismByMorphisms( arg[1], arg[2], arg[3] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: CatnMorphism( src, rng, list of maps );" );
    return fail;
end );

###############################################################################
##
#M  PreCatnMorphismByMorphisms( <src>, <rng>, <list of maps> ) 
##
InstallMethod( PreCatnMorphismByMorphisms,
    "method for a higher dimensional mapping", true,
    [ IsPreCatnGroup, IsPreCatnGroup, IsList ], 0,
function( src, rng, L )

    local dim, filter, fam, mor, ok, nsrc, nrng, name, n;
    
    dim := HigherDimension( src )-1; 
    if not ( dim = HigherDimension(rng)-1 ) then 
        Error( "src and rng have different dimensions" ); 
    fi; 
    if ( Length(L) <> dim ) then 
        Print( "L should contain ", dim, " morphisms\n" );
        return fail;        
    fi;
    mor := MakeHigherDimensionalGroupMorphism( src, rng, L );
        if not IsPreCatnMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-catn-groups" );
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CatnMorphismByMorphisms . . . . . . . . . for a higher dimensional mapping
##
InstallMethod( CatnMorphismByMorphisms, "method for a nd-mapping", true, 
    [ IsCatnGroup, IsCatnGroup, IsList], 0,
function( src, rng, L )

    local mor, ok;
    mor := PreCatnMorphismByMorphisms( src, rng, L );
    ok := IsCatnMorphism( mor );
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
    mors1 := ListOfHomomorphisms( m1 );
    mors2 := ListOfHomomorphisms( m2 );    

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
    if ( IsPreCatnMorphism( m1 ) and IsPreCatnMorphism( m2 ) ) then
        SetIsPreCatnMorphism( comp, true );
    fi;
    if ( IsCatnMorphism( m1 ) and IsCatnMorphism( m2 ) ) then
        SetIsCatnMorphism( comp, true );
    fi;
    return comp;
end );

##############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( Order, "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local ok, 2d_maps;

    2d_maps := ListOfHomomorphisms(mor);
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

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map ); 
    ok := ForAll( 2d_maps, f -> IsInjective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsSurjective, "method for a higher dimensional mapping", 
    true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map );    
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

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map );    
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

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map );    
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

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map );    
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

    local ok, 2d_maps;
    
    2d_maps := ListOfHomomorphisms( map );
    ok := ForAll( 2d_maps, f -> IsEndomorphism2DimensionalDomain(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

InstallMethod( IsAutomorphismHigherDimensionalDomain, 
    "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
    map -> IsEndomorphismHigherDimensionalDomain( map ) 
           and IsBijective( map ) );

##############################################################################
##
#E  gpnmap.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
