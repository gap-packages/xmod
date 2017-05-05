##############################################################################
##
#W  gp3map.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for Higher Dimensional Mappings for 
##  (pre-)catn-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  MakeHigherDimensionalGroupMorphism( [ <src>, <rng>, <2dMap>, <2dMap>, ... ] ) 
##
InstallMethod( MakeHigherDimensionalGroupMorphism,
    "for two higher dimensional objects and 2d-morphisms", true, 
    [ IsList ], 0,
function( L )

    local  filter, fam, mor, src, rng, n1, n2, m_list, n, i;
	
	src := L[1];
	rng := L[2];
	
	n := Length(L);
	
	if ( n < 4 ) then
	    Print( "Number of arguments must be at least four \n" );
		return fail;		
	elif ( not(IsHigherDimensionalGroup(src)) or not(IsHigherDimensionalGroup(rng)) ) then
	    Print( "Source and Range must be PreCatn-group \n" );
		return fail;
	fi;
	
	n1 := PreCatnDimension( src );
	n2 := PreCatnDimension( rng );
	
	if ( n1 <> n2 ) then
	    Print( "Source and Range must be same dimension		\n" );
		return fail;
	fi;
	
	if ( n1 <> n-2 ) then
	    Print( "Number of arguments are wrong		\n" );
		return fail;
	fi;
	
	 m_list := []; 
	            
	
	for i in [3..n] do 
            Add(m_list,L[i],i-2);
    od;
	
	if ForAny(m_list, x -> not Is2DimensionalGroupMorphism(x) ) then 
        Print( "Each item in the list must be PreCat1-group moprphism. (except first two)\n" );
        return fail;
    fi;
    
    fam := FamilyHigherDimensionalGroupMorphism;
    filter := IsHigherDimensionalMappingRep;
    mor := rec();
    ObjectifyWithAttributes( mor, 
      NewType( fam, filter ),
      Source, src,
      Range, rng,
      2DimensionalGroupMorphisms, m_list,
      PreCatnGroupMorphismDimension, n1  );
    return mor; 
end );


#############################################################################
##
#M  IsPreCatnMorphism      check the axioms for a pre-catn 
##
InstallMethod( IsPreCatnMorphism,
    "generic method for pre-catn homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local  PC, QC, upmor, dnmor, ok, 2dmor, list_2dPC, list_2dQC, list_range_of_2dPC, list_range_of_2dQC, x,
		list_range_hom_of_2dmor, list_source_hom_of_2dmor, G1, G2, P1, P2, p, q, comp, perm2dmor, i;

    PC := Source( mor );
    QC := Range( mor );
    if not ( IsHigherDimensionalGroup( PC ) and IsHigherDimensionalGroup( QC ) ) then
        return false;
    fi;
    
    2dmor := 2DimensionalGroupMorphisms( mor );
	if ForAny(2dmor, x -> not Is2DimensionalGroupMorphism(x) ) then 
        return false;
    fi;
	
	list_2dPC := 2DimensionalGroups( PC ); 
	list_2dQC := 2DimensionalGroups( QC ); 
	
	list_range_of_2dPC := List(list_2dPC, x -> Range(x));
	list_range_of_2dQC := List(list_2dQC, x -> Range(x));
	
	list_range_hom_of_2dmor := List(2dmor, x -> RangeHom(x));
	list_source_hom_of_2dmor := List(2dmor, x -> SourceHom(x));
	
	if ForAny([1..Length(2dmor)], x ->  Source( list_range_hom_of_2dmor[x] ) <> list_range_of_2dPC[x] ) then 
        return false;
    fi;	
	
	if ForAny([1..Length(2dmor)], x ->  Range( list_range_hom_of_2dmor[x] ) <> list_range_of_2dQC[x] ) then 
        return false;
    fi;	
	
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
	
	# check that equality of SourceHoms
	
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
    local  ispre;
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
    "generic method for two higher dimensional morphisms",
    IsIdenticalObj, [ IsHigherDimensionalMapping, IsHigherDimensionalMapping ], 0,
    function ( mor, phi )
	
	local  n1, n2, m_list1, m_list2;
	
	n1 := PreCatnGroupMorphismDimension( mor );
	n2 := PreCatnGroupMorphismDimension( phi );
	m_list1 := 2DimensionalGroupMorphisms( mor );
	m_list2 := 2DimensionalGroupMorphisms( phi );		
	
	if ( ( n1 <> n2 ) or ( m_list1 <> m_list2  ) ) then
		return false;
    elif ( IsPreCatnMorphism( mor ) and IsPreCatnMorphism( phi ) and ( Source( mor ) = Source( phi ) ) and ( Range( mor ) = Range( phi )  ) ) then
		return true;
    fi;
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . for a higher dimensional mapping
##
InstallOtherMethod( MappingGeneratorsImages, "method for a higher dimensional mapping", true,
    [ IsHigherDimensionalMapping ], 0,
    function( map )
	
	local  m_list, im_list;
	
	m_list := 2DimensionalGroupMorphisms( map );
	im_list := List( m_list, f -> MappingGeneratorsImages(f) );
	return im_list;
end );

#############################################################################
##
#M  Name . . . . . . . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallMethod( Name, "method for a higher dimensional mapping", true, 
    [ IsHigherDimensionalMapping ], 0,
function( mor )

    local  nsrc, nrng, name;

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
#F  Display( <mor> ) . . . . . . . print details of a higher dimensional mapping 
##
InstallMethod( Display, "display a higher dimensional mapping", true,
    [ IsHigherDimensionalMapping ], 0,
    function( mor )

    local  P, Q, n, m_list, i;

    P := Source( mor );
    Q := Range( mor );
	n := PreCatnGroupMorphismDimension( mor );
	m_list := 2DimensionalGroupMorphisms( mor );
	
    if ( HasIsCatnMorphism( mor ) and IsCatnMorphism( mor ) ) then
            Print( "Morphism of cat",n,"-groups :- \n" );
    else
            Print( "Morphism of pre-cat",n,"-groups :- \n" );
    fi; 
    if HasName( P ) then
        Print( ":    Source = ", Name( P ), "\n" );
    else
        Print( ":    Source = \n", P, "\n" );
    fi;
    if HasName( Q ) then
        Print( ":     Range = ", Name( Q ), "\n" );
    else
        Print( ":     Range = \n", Q, "\n" );
    fi;
    if HasOrder( mor ) then
        Print( ":     order = ", Order( mor ), "\n" );
    fi;
	for i in [1..Length(m_list)] do
	    Print( MappingGeneratorsImages(SourceHom(m_list[i])),"\n");
	od;
end ); 

##############################################################################
##
#M  IdentityMapping( <obj> )
##
InstallOtherMethod( IdentityMapping, "for higher dimensional object", true,
    [ IsHigherDimensionalGroup ], 0,
function( obj )

    local  id_list, n, i, list, L;
	
	n := PreCatnDimension(obj);
	L := 2DimensionalGroups(obj);
	id_list := [];
	for i in [1..n] do
		Add(id_list,IdentityMapping(L[i]),i);
	od;
    list := [obj, obj];
	Append(list, id_list);
    return PreCatnMorphismByMorphisms( list );
end );
    


##############################################################################
##
#F  CatnMorphism( [list] ) . . catn morphism
##
##
InstallGlobalFunction( CatnMorphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    if ( ( nargs = 1 ) and IsList( arg[1] ) ) then
        return  CatnMorphismByMorphisms( arg[1] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: CatnMorphism( [list] );" );
    return fail;
end );

###############################################################################
##
#M  PreCatnMorphismByMorphisms( [list] ) 
##
InstallMethod( PreCatnMorphismByMorphisms,
    "method for a higher dimensional mapping", true,
    [ IsList ], 0,
function( L )

    local  filter, fam, mor, ok, nsrc, nrng, name, n, src, rng;
	
	n := Length(L);
	src := L[1];
	rng := L[2];
	
	if ( n < 4 ) then
	    Print( "Number of arguments must be at least four \n" );
		return fail;		
	elif ( not(IsHigherDimensionalGroup(src)) or not(IsHigherDimensionalGroup(rng)) ) then
	    Print( "Source and Range must be PreCatn-group \n" );
		return fail;
	fi;

    mor := MakeHigherDimensionalGroupMorphism( L );
	
    if not IsPreCatnMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-catn.\n" );
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CatnMorphismByMorphisms( [list] ) . . . . . .for a higher dimensional mapping
##
InstallMethod( CatnMorphismByMorphisms, "method for a higher dimensional mapping", 
    true, [ IsList], 0,
function( L )

    local  mor, ok;
    mor := PreCatnMorphismByMorphisms( L );
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
InstallOtherMethod( CompositionMorphism, "generic method for higher dimensional mappings",
    IsIdenticalObj, [ IsHigherDimensionalMapping, IsHigherDimensionalMapping ], 0,
function( m2, m1 )

    local  up, lt, rt, dn, comp, ok, list, comp_list, n1, n2, m_list1, m_list2, i;
	
	n1 := PreCatnGroupMorphismDimension( m1 );
	n2 := PreCatnGroupMorphismDimension( m2 );
	m_list1 := 2DimensionalGroupMorphisms( m1 );
	m_list2 := 2DimensionalGroupMorphisms( m2 );	

    if not ( Range( m1 ) = Source( m2 ) ) then
        Info( InfoXMod, 2, "Range(m1) <> Source(m2)" );
        return fail;
    fi;
    if not ( n1 = n2 ) then
        Info( InfoXMod, 2, "Dimension - (m1) <> Dimension - (m2)" );
        return fail;
    fi;
	
    comp_list := List( [1..Length(m_list1)], i -> CompositionMapping( m_list2[i], m_list1[i] ) );
    list := [Source( m1 ), Range( m2 )];
	Append(list, comp_list);
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

	2d_maps := 2DimensionalGroupMorphisms(mor);
    if not ( IsAutomorphismHigherDimensionalDomain(mor) ) then
       Info( InfoXMod, 2, "Parameter is not an automorphism" );
       return fail;
    fi;
       return  Lcm( List(2d_maps, f -> Order(f) ) );
end );

##############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsInjective,
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
	
	2d_maps := 2DimensionalGroupMorphisms( map ); 
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
InstallOtherMethod( IsSurjective,
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
	
	2d_maps := 2DimensionalGroupMorphisms( map );    
	ok := ForAll( 2d_maps, f -> IsSurjective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . . .  . for a higher dimensional mapping
##
InstallOtherMethod( IsSingleValued,
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
	
	2d_maps := 2DimensionalGroupMorphisms( map );    
	ok := ForAll( 2d_maps, f -> IsSingleValued(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsTotal,
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
	
	2d_maps := 2DimensionalGroupMorphisms( map );    
	ok := ForAll( 2d_maps, f -> IsTotal(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . . . . . . for a higher dimensional mapping
##
InstallOtherMethod( IsBijective,
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
    
	2d_maps := 2DimensionalGroupMorphisms( map );    
	ok := ForAll( 2d_maps, f -> IsBijective(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

##############################################################################
##
#M  IsEndomorphismHigherDimensionalDomain( map ) . . . . for a higher dimensional mapping
#M  IsAutomorphismHigherDimensionalDomain( map ) . . . . for a higher dimensional mapping
##
InstallMethod( IsEndomorphismHigherDimensionalDomain, 
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
function( map )

    local ok, 2d_maps;
    
	2d_maps := 2DimensionalGroupMorphisms( map );
    ok := ForAll( 2d_maps, f -> IsEndomorphism2DimensionalDomain(f) );
    if not ok then
        return false;
    fi;    
    return true;
end );

InstallMethod( IsAutomorphismHigherDimensionalDomain, 
    "method for a higher dimensional mapping", true, [ IsHigherDimensionalMapping ], 0,
    map -> IsEndomorphismHigherDimensionalDomain( map ) and IsBijective( map ) );

##############################################################################
##
#E  gpnmap.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
