##############################################################################
##
#W  gp2map.gi                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##  This file installs methods for 2dMappings for crossed modules and 
##  cat1-groups. 
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  Make2dGroupMorphism( <src>, <rng>, <shom>, <rhom> ) . . . . . 2d-group map 
##
InstallMethod( Make2dGroupMorphism,
    "for 2d-group, 2d-group, homomorphism, homomorphism", true,
    [ Is2dGroup, Is2dGroup, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, shom, rhom )

    local  filter, fam, mor, ok;

    fam := Family2dGroupMorphism; 
    filter := Is2dMappingRep;
    #### 04/07/06 #### why are these checks here?
    #### surely they should have been performed already?
    ok := ( ( Source( src ) = Source( shom ) ) and
            (  Range( src ) = Source( rhom ) ) and
            ( Source( rng ) = Range( shom ) ) and
            (  Range( rng ) = Range( rhom ) ) );
    if not ok then
        Info( InfoXMod, 2, "sources and ranges do not match" );
        return fail;
    fi;
    mor := rec();
    ObjectifyWithAttributes( mor, 
        NewType( fam, filter ),
        Source, src,
        Range, rng,
        SourceHom, shom,
        RangeHom, rhom );
    return mor;
end );

#############################################################################
##
#M  IsPreXModMorphism       check diagram of group homs commutes
##
InstallMethod( IsPreXModMorphism, "generic method for morphisms of 2d-groups",
    true, [ Is2dGroupMorphism ], 0,
function( mor )

    local  PM, Pact, Pbdy, Prng, Psrc, QM, Qact, Qbdy, Qrng, Qsrc,
           morsrc, morrng, x2, x1, y2, z2, y1, z1, gensrc, genrng;

    PM := Source( mor );
    QM := Range( mor );
    if not ( IsPreXMod( PM ) and IsPreXMod( QM ) ) then
        return false;
    fi;
    Psrc := Source( PM );
    Prng := Range( PM );
    Pbdy := Boundary( PM );
    Pact := XModAction( PM );
    Qsrc := Source( QM );
    Qrng := Range( QM );
    Qbdy := Boundary( QM );
    Qact := XModAction( QM );
    morsrc := SourceHom( mor );
    morrng := RangeHom( mor );
    # now check that the homomorphisms commute
    gensrc := GeneratorsOfGroup( Psrc );
    genrng := GeneratorsOfGroup( Prng );
    Info( InfoXMod, 3, "Checking that the diagram commutes :- \n",
        "    boundary( morsrc( x ) ) = morrng( boundary( x ) )" );
    for x2 in gensrc do
        y1 := ( x2 ^ morsrc ) ^ Qbdy;
        z1 := ( x2 ^ Pbdy ) ^ morrng;
        if not ( y1 = z1 ) then
            Info( InfoXMod, 3, "Square does not commute! \n",
                "when x2= ", x2, " Qbdy( morsrc(x2) )= ", y1, "\n",
                "              and morrng( Pbdy(x2) )= ", z1 );
            return false;
        fi;
    od;
    # now check that the actions commute:
    Info( InfoXMod, 3,
          "Checking:  morsrc(x2^x1) = morsrc(x2)^(morrng(x1))" );
    for x2 in gensrc do
        for x1 in genrng do
            y2 := ( x2 ^ ( x1 ^ Pact) ) ^ morsrc;
            z2 := ( x2 ^ morsrc ) ^ ( ( x1 ^ morrng ) ^ Qact );
            if not ( y2 = z2 ) then
                Info( InfoXMod, 2, "Actions do not commute! \n",
                      "When  x2 = ", x2, "  and  x1 = ", x1, "\n",
                      "           morsrc(x2^x1) = ", y2, "\n",
                      "and morsrc(x2)^(morrng(x1) = ", z2 );
                return false;
            fi;
        od;
    od;
    return true;
end );

#############################################################################
##
#M  IsXModMorphism
##
InstallMethod( IsXModMorphism, "generic method for pre-xmod morphisms", true,
    [ IsPreXModMorphism ], 0,
function( mor )
    return ( IsXMod( Source( mor ) ) and IsXMod(  Range( mor ) ) );
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . . . . . . . .  for a 2dMapping
##
InstallOtherMethod( MappingGeneratorsImages, "for a 2dMapping", true,
    [ Is2dMapping ], 0,
    function( map )
    return [ MappingGeneratorsImages( SourceHom( map ) ),
             MappingGeneratorsImages( RangeHom( map ) ) ];
end );

#############################################################################
##
#F  Display( <mor> ) . . . . print details of a (pre-)crossed module morphism
##
InstallMethod( Display, "display a morphism of pre-crossed modules", true,
    [ IsPreXModMorphism ], 0,
    function( mor )

    local  morsrc, morrng, gensrc, genrng, P, Q, name, ok;

    name := Name( mor );
    P := Source( mor );
    Q := Range( mor );
    morsrc := SourceHom( mor );
    gensrc := GeneratorsOfGroup( Source( P ) );
    morrng := RangeHom( mor );
    genrng := GeneratorsOfGroup( Range( P ) );
    if IsXModMorphism( mor ) then
        Print( "Morphism of crossed modules :- \n" );
    else
        Print( "Morphism of pre-crossed modules :- \n" );
    fi;
    Print( ": Source = ", P, " with generating sets:\n  " );
    Print( gensrc, "\n  ", genrng, "\n" );
    if ( Q = P ) then
        Print( ": Range = Source\n" );
    else
        Print( ":  Range = ", Q, " with generating sets:\n  " );
        Print( GeneratorsOfGroup( Source( Q ) ), "\n" );
        Print( "  ", GeneratorsOfGroup( Range( Q ) ), "\n" );
    fi;
    Print( ": Source Homomorphism maps source generators to:\n" );
    Print( "  ", List( gensrc, s -> Image( morsrc, s ) ), "\n" );
    Print( ": Range Homomorphism maps range generators to:\n" );
    Print( "  ", List( genrng, r -> Image( morrng, r ) ), "\n" );
end ); 

#############################################################################
##
#M  IsPreCat1Morphism . . . . . . . . . check diagram of group homs commutes
##
InstallMethod( IsPreCat1Morphism, "generic method for morphisms of 2d-groups", 
    true, [ Is2dGroupMorphism ], 0,
function( mor )

    local  PCG, Prng, Psrc, Pt, Ph, Pe, QCG, Qrng, Qsrc, Qt, Qh, Qe,
           morsrc, morrng, x2, x1, y2, z2, y1, z1, gensrc, genrng;

    PCG := Source( mor );
    QCG := Range( mor );
    if not ( IsPreCat1( PCG ) and IsPreCat1( QCG ) ) then
        return false;
    fi;
    Psrc := Source( PCG );
    Prng := Range( PCG );
    Pt := TailMap( PCG );
    Ph := HeadMap( PCG );
    Pe := RangeEmbedding( PCG );
    Qsrc := Source( QCG );
    Qrng := Range( QCG );
    Qt := TailMap( QCG );
    Qh := HeadMap( QCG );
    Qe := RangeEmbedding( QCG );
    morsrc := SourceHom( mor );
    morrng := RangeHom( mor );
    # now check that the homomorphisms commute
    gensrc := GeneratorsOfGroup( Psrc );
    genrng := GeneratorsOfGroup( Prng );
    Info( InfoXMod, 3,
          "Checking that the diagrams commute :- \n",
          "    tail/head( morsrc( x ) ) = morrng( tail/head( x ) )" );
    for x2 in gensrc do
        y1 := ( x2 ^ morsrc ) ^ Qt;
        z1 := ( x2 ^ Pt ) ^ morrng;
        y2 := ( x2 ^ morsrc ) ^ Qh;
        z2 := ( x2 ^ Ph ) ^ morrng;
        if not ( ( y1 = z1 ) and ( y2 = z2 ) ) then
            Info( InfoXMod, 3, "Square does not commute! \n",
                  "when x2= ", x2, " Qt( morsrc(x2) )= ", y1, "\n",
                  "              and morrng( Pt(x2) )= ", z1, "\n",
                  "              and Qh( morsrc(x2) )= ", y2, "\n",
                  "              and morrng( Ph(x2) )= ", z2 );
            return false;
        fi;
    od;
    for x2 in genrng do
        y1 := ( x2 ^ morrng ) ^ Qe;
        z1 := ( x2 ^ Pe ) ^ morsrc;
        if not ( y1 = z1 ) then
            Info( InfoXMod, 3, "Square does not commute! \n",
                  "when x2= ", x2, " Qe( morrng(x2) )= ", y1, "\n",
                  "              and morsrc( Pe(x2) )= ", z1 );
            return false;
        fi;
    od;
    return true;
end );

#############################################################################
##
#M  IsCat1Morphism
##
InstallMethod( IsCat1Morphism, "generic method for cat1-group homomorphisms",
    true, [ IsPreCat1Morphism ], 0,
function( mor )
    return ( IsCat1( Source( mor ) ) and IsCat1(  Range( mor ) ) );
end );

#############################################################################
##
#F  Display( <mor> ) . . . . . . print details of a (pre-)cat1-group morphism
##
InstallMethod( Display, "display a morphism of pre-cat1 groups", true,
    [ IsPreCat1Morphism ], 0,
function( mor )

    local  morsrc, morrng, gensrc, genrng, P, Q, name, ok;

    if not HasName( mor ) then
        # name := PreCat1MorphismName( mor );
        SetName( mor, "[..=>..]=>[..=>..]" );
    fi;
    name := Name( mor );
    P := Source( mor );
    Q := Range( mor );
    morsrc := SourceHom( mor );
    gensrc := GeneratorsOfGroup( Source( P ) );
    morrng := RangeHom( mor );
    genrng := GeneratorsOfGroup( Range( P ) );
    if IsCat1Morphism( mor ) then
        Print( "Morphism of cat1-groups :- \n" );
    else
        Print( "Morphism of pre-cat1 groups :- \n" );
    fi;
    Print( ": Source = ", P, " with generating sets:\n  " );
    Print( gensrc, "\n  ", genrng, "\n" );
    if ( Q = P ) then
        Print( ": Range = Source\n" );
    else
        Print( ":  Range = ", Q, " with generating sets:\n  " );
        Print( GeneratorsOfGroup( Source( Q ) ), "\n" );
        Print( "  ", GeneratorsOfGroup( Range( Q ) ), "\n" );
    fi;
    Print( ": Source Homomorphism maps source generators to:\n" );
    Print( "  ", List( gensrc, s -> Image( morsrc, s ) ), "\n" );
    Print( ": Range Homomorphism maps range generators to:\n" );
    Print( "  ", List( genrng, r -> Image( morrng, r ) ), "\n" );
end ); 

##############################################################################
##
#M  CompositionMorphism  . . . . . . . . . . . . . . . . . for two 2d-mappings
##
InstallOtherMethod( CompositionMorphism, "generic method for 2d-mappings",
    IsIdenticalObj, [ Is2dMapping, Is2dMapping ], 0,
function( mor2, mor1 )

    local  srchom, rnghom, comp, ok;

    if not ( Range( mor1 ) = Source( mor2 ) ) then
        Info( InfoXMod, 2, "Range(mor1) <> Source(mor2)" );
        return fail;
    fi;
    srchom := CompositionMapping2( SourceHom( mor2 ), SourceHom( mor1 ) );
    rnghom := CompositionMapping2( RangeHom( mor2 ), RangeHom( mor1 ) );
    comp := Make2dGroupMorphism( Source( mor1 ), Range( mor2 ), srchom, rnghom );
    if IsPreCat1( Source( mor1 ) ) then
        if ( IsPreCat1Morphism( mor1 ) and IsPreCat1Morphism( mor2 ) ) then
            SetIsPreCat1Morphism( comp, true );
        fi;
        if ( IsCat1Morphism( mor1 ) and IsCat1Morphism( mor2 ) ) then
            SetIsCat1Morphism( comp, true );
        fi;
    else
        if ( IsPreXModMorphism( mor1 ) and 
             IsPreXModMorphism( mor2 ) ) then
            SetIsPreXModMorphism( comp, true );
        fi;
        if ( IsXModMorphism( mor1 ) and IsXModMorphism( mor2 ) ) then
            SetIsXModMorphism( comp, true );
        fi;
    fi;
    return comp;
end );

##############################################################################
##
#M  InverseGeneralMapping . . . . . . . . . . . . . . . . . . for a 2d-mapping
##
#?  (29/06/12) only works if more is _already_ known to be bijective, 
#?             so perhaps move IsBijective into the code ?? 
## 
InstallOtherMethod( InverseGeneralMapping, "generic method for 2d-mapping",
    true, [ Is2dMapping and IsBijective ], 0,
function( mor )
    local inv, ok;
    inv := Make2dGroupMorphism( Range( mor ), Source( mor ),
                          SourceHom( mor )^(-1), RangeHom( mor )^(-1) );
    if IsPreXModMorphism( mor ) then 
        SetIsPreXModMorphism( inv, true );
        if IsXModMorphism( mor ) then 
            SetIsXModMorphism( inv, true );
        fi;
    elif IsPreCat1Morphism( mor ) then 
        SetIsPreCat1Morphism( inv, true );
        if IsCat1Morphism( mor ) then 
            SetIsCat1Morphism( inv, true );
        fi;
    fi;
    SetIsInjective( inv, true );
    SetIsSurjective( inv, true );
    return inv;
end );

##############################################################################
##
#M  IdentityMapping( <obj> )
##
InstallOtherMethod( IdentityMapping, "for 2d-group object", true,
    [ Is2dDomain ], 0,
function( obj )

    local  shom, rhom;

    shom := IdentityMapping( Source( obj ) );
    rhom := IdentityMapping( Range( obj ) );
    if IsPreXModObj( obj ) then
        return PreXModMorphismByHoms( obj, obj, shom, rhom );
    elif IsPreCat1Obj( obj ) then
        return PreCat1MorphismByHoms( obj, obj, shom, rhom );
    else
        return fail;
    fi;
end );

##############################################################################
##
#M  InclusionMorphism2dDomains( <obj>, <sub> )
##
InstallMethod( InclusionMorphism2dDomains, "of one 2d-object in another", 
    true, [ Is2dDomain, Is2dDomain ], 0,
function( obj, sub )

    local  shom, rhom;
    shom := InclusionMappingGroups( Source( obj ), Source( sub ) );
    rhom := InclusionMappingGroups( Range( obj ), Range( sub ) );
    if IsPreXModObj( obj ) then
        return PreXModMorphismByHoms( sub, obj, shom, rhom );
    elif IsPreCat1Obj( obj ) then
        return PreCat1MorphismByHoms( sub, obj, shom, rhom );
    else
        return fail;
    fi;
end );

##############################################################################
##
#F  PreXModMorphism( <src>,<rng>,<srchom>,<rnghom> ) pre-crossed mod morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( PreXModMorphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two pre-xmods and two homomorphisms
    if ( ( nargs = 4 ) and IsPreXMod( arg[1] ) and IsPreXMod( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return PreXModMorphismByHoms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: PreXModMorphism( src, rng, srchom, rnghom );" );
    return fail;
end );

###############################################################################
##
#F  XModMorphism( <src>, <rng>, <srchom>, <rnghom> )    crossed module morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( XModMorphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two xmods and two homomorphisms
    if ( ( nargs = 4 ) and IsXMod( arg[1] ) and IsXMod( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return XModMorphismByHoms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: XModMorphism( src, rng, srchom, rnghom );" );
    return fail;
end );

###############################################################################
##
#F  PreCat1Morphism( <src>,<rng>,<srchom>,<rnghom> )    pre-cat1-group morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( PreCat1Morphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two pre-cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsPreCat1( arg[1] ) and IsPreCat1( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return PreCat1MorphismByHoms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: PreCat1Morphism( src, rng, srchom, rnghom );" );
    return fail;
end );

###############################################################################
##
#F  Cat1Morphism( <src>, <rng>, <srchom>, <rnghom> )        cat1-group morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( Cat1Morphism, function( arg )

    local  nargs;
    nargs := Length( arg );

    # two cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsCat1( arg[1] ) and IsCat1( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return Cat1MorphismByHoms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: Cat1Morphism( src, rng, srchom, rnghom );" );
    return fail;
end );

##############################################################################
##
#M  XModMorphismByHoms( <Xs>, <Xr>, <hsrc>, <hrng> )  . . . make xmod morphism
##
InstallMethod( XModMorphismByHoms, "for 2 xmods and 2 homomorphisms", true,
    [ IsXMod, IsXMod, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local  mor, ok;
    mor := PreXModMorphismByHoms( src, rng, srchom, rnghom );
    ok := IsXModMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  InnerAutomorphismXMod( <XM>, <r> ) . . . . . . . .  conjugation of an xmod
##
InstallMethod( InnerAutomorphismXMod, "method for crossed modules", true,
    [ IsPreXMod, IsMultiplicativeElementWithInverse ], 0,
function( XM, r )
    local  Xrng, Xsrc, genrng, gensrc, rhom, shom, s;
    Xrng := Range( XM );
    if not ( r in Xrng ) then
        Info( InfoXMod, 2, "conjugating element must be in the range group" );
        return fail;
    fi;
    Xsrc := Source( XM );
    gensrc := GeneratorsOfGroup( Xsrc );
    genrng := GeneratorsOfGroup( Xrng );
    rhom := GroupHomomorphismByImages( Xrng, Xrng, genrng,
                List( genrng, g -> g^r ) );
    s := Image( XModAction( XM ), r );
    shom := GroupHomomorphismByImages( Xsrc, Xsrc, gensrc, 
                List( gensrc, g -> g^s ) );
    return XModMorphismByHoms( XM, XM, shom, rhom );
end );

##############################################################################
##
#M  InnerAutomorphismCat1( <C1G>, <r> ) . . . . .  conjugation of a cat1-group
##
InstallMethod( InnerAutomorphismCat1, "method for cat1-groups", true,
    [ IsPreCat1, IsMultiplicativeElementWithInverse ], 0,
function( C1G, r )
    local  Crng, Csrc, genrng, gensrc, rhom, shom, s;
    Crng := Range( C1G );
    if not ( r in Crng ) then
        Info( InfoXMod, 2, "conjugating element must be in the range group" );
        return fail;
    fi;
    Csrc := Source( C1G );
    gensrc := GeneratorsOfGroup( Csrc );
    genrng := GeneratorsOfGroup( Crng );
    rhom := GroupHomomorphismByImages( Crng, Crng, genrng,
                List( genrng, g -> g^r ) );
    s := Image( RangeEmbedding( C1G ), r );
    shom := GroupHomomorphismByImages( Csrc, Csrc, gensrc, 
                List( gensrc, g -> g^s ) );
    return Cat1MorphismByHoms( C1G, C1G, shom, rhom );
end );

##############################################################################
##
#M  ViewObj( <mor> ) . . . . . . . . . .  view a (pre-)crossed module morphism
##
InstallMethod( ViewObj, "method for a morphism of pre-crossed modules", true,
    [ IsPreXModMorphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

##############################################################################
##
#M  PrintObj( <mor> ) . . . . . . . . .  print a (pre-)crossed module morphism
##
InstallMethod( PrintObj, "method for a morphism of pre-crossed modules", true,
    [ IsPreXModMorphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

##############################################################################
##
#M  \*( <mor1>, <mor2> ) . . . . . . . . .  for 2 pre-crossed module morphisms
##
InstallOtherMethod( \*, "for two morphisms of pre-crossed modules",
    IsIdenticalObj, [ IsPreXModMorphism, IsPreXModMorphism ], 0,
function( mor1, mor2 )
    local  comp;
    comp := CompositionMorphism( mor2, mor1 );
    ## need to do some checks here !? ##
    return comp;
end );

##############################################################################
##
#M  \^( <mor>, <int> ) . . . . . . . . . . . . . . . . . . . . for a 2dMapping
##
InstallOtherMethod( POW, "for a 2d mapping", true, [ Is2dMapping, IsInt ], 0,
function( map, n )
    local  pow, i, ok;
    if not ( Source( map ) = Range( map ) ) then
        return fail;
    elif ( n = 1 ) then
        return map;
    elif ( n = -1 ) then
        ok := IsBijective( map );
        return InverseGeneralMapping( map );
    elif ( n < -1 ) then
        return InverseGeneralMapping( map^(-n) );
    fi;
    pow := map;
    for i in [2..n] do
        pow := CompositionMorphism( pow, map );
    od;
    return pow;
end );

##############################################################################
##
#M  IsomorphismPerm2dGroup . . . . . . . . constructs isomorphic perm pre-xmod
##
InstallMethod( IsomorphismPerm2dGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local  shom, sinv, rhom, rinv, Psrc, Psgen, Qsrc, Qsgen, 
           Prng, Prgen, Qrng, Qrgen, Qbdy,
           Paut, Pautgen, Qaut, Qautgen, ahom, Qact, QM, iso;

    if IsPermPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsPermGroup( Psrc ) then
        Qsrc := Psrc;
        shom := IdentityMapping( Psrc );
        sinv := shom;
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismSmallPermGroup( Psrc );
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
        sinv := GroupHomomorphismByImages( Qsrc, Psrc, Qsgen, Psgen );
    fi;
    Prng := Range( PM );
    if IsPermGroup( Prng ) then
        Qrng := Prng;
        rhom := IdentityMapping( Prng );
        rinv := rhom;
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismSmallPermGroup( Prng );
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
        rinv := GroupHomomorphismByImages( Qrng, Prng, Qrgen, Prgen );
    fi;
    Qbdy := CompositionMapping( rhom, Boundary( PM ), sinv );
    Paut := AutoGroup( PM );
    Pautgen := GeneratorsOfGroup( Paut );
    Qautgen := List( Pautgen, a -> CompositionMapping( shom, a, sinv ) );
    Qaut := Group( Qautgen );
    ahom := GroupHomomorphismByImages( Paut, Qaut, Pautgen, Qautgen );
    Qact := CompositionMapping( ahom, XModAction( PM ), rinv );
    QM := PreXModByBoundaryAndAction( Qbdy, Qact );
    if HasName( PM ) then
        SetName( QM, Concatenation( "P", Name( PM ) ) );
    fi;
    iso := PreXModMorphismByHoms( PM, QM, shom, rhom );
    SetImagesSource( iso, QM );
    return iso;
end );

##############################################################################
##
#M  IsomorphismPerm2dGroup . . . . . . . . constructs isomorphic perm pre-cat1
##
InstallMethod( IsomorphismPerm2dGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1 ], 0,
function( PCG )

    local  shom, sinv, rhom, rinv, Psrc, Psgen, Qsrc, Qsgen, 
           Prng, Prgen, Qrng, Qrgen, Qt, Qh, Qe, QCG, iso;

    if IsPermPreCat1( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsPermGroup( Psrc ) then
        Qsrc := Psrc;
        shom := IdentityMapping( Psrc );
        sinv := shom;
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismSmallPermGroup( Psrc );
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
        sinv := GroupHomomorphismByImages( Qsrc, Psrc, Qsgen, Psgen );
    fi;
    Prng := Range( PCG );
    if IsPermGroup( Prng ) then
        Qrng := Prng;
        rhom := IdentityMapping( Prng );
        rinv := rhom;
    else
        Prgen := GeneratorsOfGroup( Prng ); 
        if IsEndomorphismPreCat1( PCG ) then 
            rhom := RestrictedMapping( shom, Prng ); 
        else 
            rhom := IsomorphismSmallPermGroup( Prng );
        fi;
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
        rinv := GroupHomomorphismByImages( Qrng, Prng, Qrgen, Prgen );
    fi;
    Qt := CompositionMapping( rhom, TailMap( PCG ), sinv );
    Qh := CompositionMapping( rhom, HeadMap( PCG ), sinv );
    Qe := CompositionMapping( shom, RangeEmbedding( PCG ), rinv );
    QCG := PreCat1ByTailHeadEmbedding( Qt, Qh, Qe );
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "P", Name( PCG ) ) );
    fi;
    iso := PreCat1MorphismByHoms( PCG, QCG, shom, rhom );
    SetImagesSource( iso, QCG );
    return iso;
end );

##############################################################################
##
#M  IsomorphismPc2dGroup . . . . . . . . . . constructs isomorphic pc pre-xmod
##
InstallMethod( IsomorphismPc2dGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local  shom, sinv, rhom, rinv, Psrc, Psgen, Qsrc, Qsgen, 
           Prng, Prgen, Qrng, Qrgen, Qbdy,
           Paut, Pautgen, Qaut, Qautgen, ahom, Qact, QM, iso;

    if IsPcPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsPcGroup( Psrc ) then
        Qsrc := Psrc;
        shom := IdentityMapping( Psrc );
        sinv := shom;
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismPcGroup( Psrc ); 
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
        sinv := GroupHomomorphismByImages( Qsrc, Psrc, Qsgen, Psgen );
    fi;
    Prng := Range( PM ); 
    if ( HasIsNormalSubgroup2dGroup(PM) and IsNormalSubgroup2dGroup(PM) ) then 
        Print( "#!  need to modify IsomorphismPc2dGroup to preserve the\n", 
               "#!  property of being IsNormalSubgroup2dGroup\n" ); 
    fi; 
    if IsPcGroup( Prng ) then
        Qrng := Prng;
        rhom := IdentityMapping( Prng );
        rinv := rhom;
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismPcGroup( Prng ); 
        if ( rhom = false ) then 
            return false; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
        rinv := GroupHomomorphismByImages( Qrng, Prng, Qrgen, Prgen );
    fi;
    Qbdy := CompositionMapping( rhom, Boundary( PM ), sinv );
    Paut := AutoGroup( PM );
    Pautgen := GeneratorsOfGroup( Paut );
    Qautgen := List( Pautgen, a -> CompositionMapping( shom, a, sinv ) );
    Qaut := Group( Qautgen );
    ahom := GroupHomomorphismByImages( Paut, Qaut, Pautgen, Qautgen );
    Qact := CompositionMapping( ahom, XModAction( PM ), rinv );
    QM := PreXModByBoundaryAndAction( Qbdy, Qact );
    if HasName( PM ) then
        SetName( QM, Concatenation( "Pc", Name( PM ) ) );
    fi;
    iso := PreXModMorphismByHoms( PM, QM, shom, rhom );
    SetImagesSource( iso, QM );
    return iso;
end );

##############################################################################
##
#M  IsomorphismPc2dGroup . . . . . . . . . . constructs isomorphic pc pre-cat1
##
InstallMethod( IsomorphismPc2dGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1 ], 0,
function( PCG )

    local  shom, sinv, rhom, rinv, Psrc, Psgen, Qsrc, Qsgen, 
           Prng, Prgen, Qrng, Qrgen, Qt, Qh, Qe, QCG, iso;

    if IsPcPreCat1( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsPcGroup( Psrc ) then
        Qsrc := Psrc;
        shom := IdentityMapping( Psrc );
        sinv := shom;
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismPcGroup( Psrc );
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
        sinv := GroupHomomorphismByImages( Qsrc, Psrc, Qsgen, Psgen );
    fi;
    Prng := Range( PCG );
    if IsPcGroup( Prng ) then
        Qrng := Prng;
        rhom := IdentityMapping( Prng );
        rinv := rhom;
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismPcGroup( Prng ); 
        if ( rhom = fail ) then 
            return fail; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
        rinv := GroupHomomorphismByImages( Qrng, Prng, Qrgen, Prgen );
    fi;
    Qt := CompositionMapping( rhom, TailMap( PCG ), sinv );
    Qh := CompositionMapping( rhom, HeadMap( PCG ), sinv );
    Qe := CompositionMapping( shom, RangeEmbedding( PCG ), rinv );
    QCG := PreCat1ByTailHeadEmbedding( Qt, Qh, Qe );
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "P", Name( PCG ) ) );
    fi;
    iso := PreCat1MorphismByHoms( PCG, QCG, shom, rhom );
    SetImagesSource( iso, QCG );
    return iso;
end );

###############################################################################
##
#M  IsomorphismPreCat1s . . . . . isomorphism between a pair of pre-cat1-groups 
##
InstallMethod( IsomorphismPreCat1s, "generic method for two pre-cat1-groups", 
    true, [ IsPreCat1, IsPreCat1 ], 0,
function( C1, C2 )

    local  t1, h1, e1, t2, h2, e2, G1, G2, R1, R2, A, phi, psi, 
           alpha, gamma, rho;

    G1 := Source( C1 ); 
    G2 := Source( C2 ); 
    if not ( G1 = G2 ) then
        phi := IsomorphismGroups( G1, G2 );
    else
        phi := IdentityMapping( G1 );
    fi;
    if ( phi = fail ) then
        return fail;
    fi;
    R1 := Range( C1 );
    R2 := Range( C2 );
    if not ( R1 = R2 ) then
        psi := IsomorphismGroups( R1, R2 );
    else
        psi := IdentityMapping( R1 );
    fi;
    if ( psi = fail ) then
        return fail;
    fi; 
    t1 := TailMap( C1 );
    h1 := HeadMap( C1 );
    t2 := TailMap( C2 );
    h2 := HeadMap( C2 );
    e1 := RangeEmbedding( C1 ); 
    e2 := RangeEmbedding( C2 ); 
    A := AutomorphismGroup( G1 ); 
    for alpha in A do 
        gamma := alpha * phi; 
        rho := e1 * gamma * t2; 
        if ( ( e1*gamma = rho*e2 ) and 
             ( gamma*h2 = h1*rho ) and 
             ( gamma*t2 = t1*rho ) ) then 
            return PreCat1MorphismByHoms( C1, C2, gamma, rho ); 
        fi; 
    od;
    return fail;
end );          

#############################################################################
##
#M  Name                                                       for a pre-xmod
##
InstallMethod( Name, "method for a 2d-mapping", true, [ Is2dMapping ], 0,
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

###############################################################################
##
#M  PreXModMorphismByHoms( <P>, <Q>, <hsrc>, <hrng> ) . . make prexmod morphism
##
InstallMethod( PreXModMorphismByHoms,
    "for pre-xmod, pre-xmod, homomorphism, homomorphism,", true,
    [ IsPreXMod, IsPreXMod, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local  filter, fam, mor, ok, nsrc, nrng, name;

    if not ( IsGroupHomomorphism(srchom) and IsGroupHomomorphism(rnghom) ) then
        Info( InfoXMod, 2, "source and range mappings must be group homs" );
        return fail;
    fi;
    mor := Make2dGroupMorphism( src, rng, srchom, rnghom );
    if not IsPreXModMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-crossed modules.\n" );
        return fail;
    fi;
    if ( HasName( Source(src) ) and HasName( Range(src) ) ) then
        nsrc := Name( src );
    else
        nsrc := "[..]";
    fi;
    if ( HasName( Source(rng) )and HasName( Range(rng) ) ) then
        nrng := Name( rng );
    else
        nrng := "[..]";
    fi;
    name := Concatenation( "[", nsrc, " => ", nrng, "]" );
    SetName( mor, name );
    ok := IsXModMorphism( mor );
   # ok := IsSourceMorphism( mor );
    return mor;
end );

##############################################################################
##
#M  PreCat1MorphismByHoms( <P>, <Q>, <hsrc>, <hrng> ) . make pre-cat1 morphism
##
InstallMethod( PreCat1MorphismByHoms,
    "for pre-cat1-group, pre-cat1-group, homomorphism, homomorphism,", true,
    [ IsPreCat1, IsPreCat1, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local  filter, fam, mor, ok, nsrc, nrng, name;

    mor := Make2dGroupMorphism( src, rng, srchom, rnghom ); 
    if not IsPreCat1Morphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-cat1 groups.\n" );
        return fail;
    fi;
    if ( HasName( Source(src) ) and HasName( Range(src) ) ) then
        nsrc := Name( src );
    else
        nsrc := "[..]";
    fi;
    if ( HasName( Source(rng) ) and HasName( Range(rng) ) ) then
        nrng := Name( rng );
    else
        nrng := "[..]";
    fi;
    name := Concatenation( "[", nsrc, " => ", nrng, "]" );
    SetName( mor, name );
    ok := IsCat1Morphism( mor );
    return mor;
end );

#############################################################################
##
#M  Cat1MorphismByHoms( <Cs>, <Cr>, <hsrc>, <hrng> ) . . . make cat1 morphism
##
InstallMethod( Cat1MorphismByHoms, "for 2 cat1s and 2 homomorphisms", true,
    [ IsCat1, IsCat1, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local  mor, ok;
    mor := PreCat1MorphismByHoms( src, rng, srchom, rnghom );
    ok := IsCat1Morphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

#############################################################################
##
#M  ViewObj( <PCG> ) . . . . . . . . . . . . . . . . view a (pre-)cat1-group
##
InstallMethod( ViewObj, "method for a morphism of pre-cat1 groups", true,
    [ IsPreCat1Morphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

##############################################################################
##
#M  PrintObj( <PCG> ) . . . . . . . . . . . . . . . . print a (pre-)cat1-group
##
InstallMethod( PrintObj, "method for a morphism of pre-cat1 groups", true,
    [ IsPreCat1Morphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

#############################################################################
##
#M  ReverseIsomorphism                                   for a pre-cat1-group
##
InstallMethod( ReverseIsomorphism, "method for a cat1-group", true,
    [ IsPreCat1 ], 0,
function( C1G )
    local rev, shom, rhom, src, gensrc, t, h, e, im;
    rev := Reverse( C1G );
    src := Source( C1G );
    gensrc := GeneratorsOfGroup( src );
    t := TailMap( C1G );
    h := HeadMap( C1G );
    e := RangeEmbedding( C1G );
    im := List( gensrc, g -> Image(e,Image(h,g))*g^-1*Image(e,Image(t,g)) );
    shom := GroupHomomorphismByImages( src, src, gensrc, im );
    rhom := IdentityMapping( Range( C1G ) );
    return PreCat1MorphismByHoms( C1G, rev, shom, rhom );
end );

##############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . . . . . . . . .  for a 2d-mapping
##
InstallOtherMethod( IsInjective,
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> (     IsInjective( SourceHom( map ) )
             and IsInjective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . . . . . . . . . for a 2d-mapping
##
InstallOtherMethod( IsSurjective,
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> (     IsSurjective( SourceHom( map ) )
             and IsSurjective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . . . . . . . . . for a 2d-mapping
##
InstallOtherMethod( IsSingleValued,
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> (     IsSingleValued( SourceHom( map ) )
             and IsSingleValued( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . . . . . . . . .  for a 2d-mapping
##
InstallOtherMethod( IsTotal,
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> (     IsTotal( SourceHom( map ) )
             and IsTotal( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . . . . . . . .  for a 2d-mapping
##
InstallOtherMethod( IsBijective,
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> (     IsBijective( SourceHom( map ) )
             and IsBijective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsEndomorphism2dDomain( map ) . . . . . . . . . . . . . . for a 2d-mapping
#?  temporary fix 08/01/04  ---  need to check correctness
#M  IsAutomorphism2dDomain( map ) . . . . . . . . . . . . . . for a 2d-mapping
##
InstallMethod( IsEndomorphism2dDomain, 
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map -> IsEndoMapping( SourceHom( map ) ) and 
           IsEndoMapping( RangeHom( map ) ) );

InstallMethod( IsAutomorphism2dDomain, 
    "method for a 2d-mapping", true, [ Is2dMapping ], 0,
    map ->  IsEndomorphism2dDomain( map ) and IsBijective( map ) );

##############################################################################
##
#M  IsSourceMorphism( mor ) . . . . . . . . . . . . . . . for an xmod morphism
##
InstallMethod( IsSourceMorphism,
    "method for a morphism of crossed modules",
    true,
    [ IsXModMorphism ], 0,
function( mor )

    local  Srng, Rrng;

    Srng := Range( Source( mor ) );
    Rrng := Range( Range( mor ) );
    return ( ( Srng = Rrng ) and
             ( RangeHom( mor ) = IdentityMapping( Srng ) ) );
end );

##############################################################################
##
#M  Kernel . . . . . . of morphisms of pre-crossed modules and pre-cat1-groups
##
InstallOtherMethod( Kernel, "generic method for 2d-mappings",
     true, [ Is2dMapping ], 0,
function( map )

    local  kerS, kerR, K;
    if HasKernel2dMapping( map ) then
        return Kernel2dMapping( map );
    fi;
    kerS := Kernel( SourceHom( map ) ); 
    kerR := Kernel( RangeHom( map ) ); 
    K := Sub2dGroup( Source( map ), kerS, kerR );
    SetKernel2dMapping( map, K );
    return K;
end );

##############################################################################
##
#M  PreXModBySourceHom        top PreXMod from a morphism of crossed P-modules
##
InstallMethod( PreXModBySourceHom, "for a pre-crossed module morphism",
    true, [ IsPreXModMorphism ], 0,
function( mor )
        
    local  X1, X2, src1, rng1, src2, rng2, bdy2, y, z, S, Sbdy, Saut, 
           gensrc1, genrng1, gensrc2, ngrng1, ngsrc2, inn, innaut,
           act, images, idsrc1, idrng1, isconj;

    X1 := Source( mor );
    X2 := Range( mor );
    src1 := Source( X1 );
    src2 := Source( X2 );
    rng1 := Range( X1 );
    rng2 := Range( X2 );
    idsrc1 := InclusionMappingGroups( src1, src1 );
    idrng1 := InclusionMappingGroups( rng1, rng1 );

    if   not ( rng1 = rng2 )
      or not ( RangeHom( mor ) = idrng1 ) then
        Info( InfoXMod, 2, 
              "Not a morphism of crossed modules having a common range" );
        return fail;
    fi;
    gensrc1 := GeneratorsOfGroup( src1 );
    genrng1 := GeneratorsOfGroup( rng1 );
    gensrc2 := GeneratorsOfGroup( src2 );
    ngrng1 := Length( genrng1 );
    ngsrc2 := Length( gensrc2 );
    bdy2 := Boundary( X2 );
    innaut := [ ];
    for y in gensrc2 do
        z := Image( bdy2, y );
        images := List( gensrc1, x -> x^z );
        inn := GroupHomomorphismByImages( src1, src1, gensrc1, images );
        Add( innaut, inn );
    od;
    Saut := Group( innaut, idsrc1 );
    act := GroupHomomorphismByImages( src2, Saut, gensrc2, innaut );
    S := XModByBoundaryAndAction( SourceHom( mor ), act );
    isconj := IsNormalSubgroup2dGroup( S );
    return S; 
end );

############################################################################
##
#M  XModMorphismByCat1Morphism
##
InstallMethod( XModMorphismByCat1Morphism, "for a cat1-group morphism",
    true, [ IsCat1Morphism ], 0,
function( phi )

    local  C1, C2, C1src, genC1src, e2, t2, proj2,
           X1, X2, X1src, X1rng, X2src, X2rng,
           genX1src, genX1rng, ek1, eksrc1, sphi, rphi, x,
           imrphi, imsphi, im, images, smor, mor, info1, info2;

    C1 := Source( phi );
    C2 := Range( phi );
    C1src := Source( C1 );
    X1 := XModByCat1( C1 );
    X2 := XModByCat1( C2 );
    X1src := Source( X1 );
    X1rng := Range( X1 );
    X2src := Source( X2 );
    X2rng := Range( X2 );
    ek1 := KernelEmbedding( C1 );
    t2 := TailMap( C2 );
    e2 := RangeEmbedding( C2 );
    proj2 := Projection( Source( C2 ) );
    genC1src := GeneratorsOfGroup( C1src );
    genX1src := GeneratorsOfGroup( X1src );
    genX1rng := GeneratorsOfGroup( X1rng );
    sphi := SourceHom( phi );
    rphi := RangeHom( phi );
    imrphi := List( genX1rng, r -> Image( rphi, r ) );
#    imgen := List( gensrc1, x -> SemidirectProductElement( (), (), x ) );  
    eksrc1 := List( genX1src, s -> Image( ek1, s ) );
    imsphi := List( eksrc1, g -> Image( sphi, g ) );
    im := List( imsphi, x -> Image( sphi, x ) );
    images := List( im, x -> Image( proj2, Image(e2,Image(t2,x^-1)) * x ) );
    smor := GroupHomomorphismByImages( X1src, X2src, genX1src, images );
    mor := XModMorphismByHoms( X1, X2, smor, rphi );
    SetXModMorphismOfCat1Morphism( phi, mor );
    SetCat1MorphismOfXModMorphism( mor, phi );
    return mor;
end );

##############################################################################
##
#M  XModMorphismOfCat1Morphism
##
InstallMethod( XModMorphismOfCat1Morphism, "for cat1-group morphisms",
    true, [ IsCat1Morphism ], 0,
function( mor )
    return XModMorphismByCat1Morphism( mor );
end );

###########################################################################
##
#M Cat1MorphismByXModMorphism
##
InstallMethod( Cat1MorphismByXModMorphism, "for a pre-crossed module morphism",
    true, [ IsXModMorphism ], 0,
function( mor )

    local  X1, X2, C1, C2, act2, C1src, C1rng, C2src, C2rng, C2s2p, 
           genC1src, genC1rng, genX1src, genX1rng, imrmor, imgen, m, images,
           sphi, rphi, phi, smor, rmor, e2, ek2, g, ig, eg, pg, esrc, erng;

    X1 := Source( mor );
    X2 := Range( mor );
    C1 := Cat1ByXMod( X1 );
    C2 := Cat1ByXMod( X2 );
    smor := SourceHom( mor );
    rmor := RangeHom( mor );
    e2 := RangeEmbedding( C2 );
    ## (15/10/13) for correct ek2 see 11 lines below
    ## ek2 := KernelEmbedding( C2 );
    act2 := XModAction( X2 );
    C1src := Source( C1 );
    C1rng := Range( C1 );
    C2src := Source( C2 );
    C2rng := Range( C2 );
    if not ( HasDirectProductInfo( C2src ) or
             HasSemidirectProductInfo( C2src ) ) then
        Info( InfoXMod, 2, "<C2src> must be a direct semidirect product" );
        return fail;
    fi;
    ek2 := SemidirectProductInfo( C2src )!.embeddings[2]; 
    genC1src := GeneratorsOfGroup( C1src );
    genC1rng := GeneratorsOfGroup( C1rng );
    genX1src := GeneratorsOfGroup( Source( X1 ) );
    genX1rng := GeneratorsOfGroup( Range( X1 ) );
    imrmor := List( genX1rng, r -> Image( rmor, r ) );
    rphi := GroupHomomorphismByImages( C1rng, C2rng, genC1rng, imrmor ); 
    if not IsGroupHomomorphism( rphi ) then
        Info( InfoXMod, 2, "<rphi> not a homomorphism" );
        return fail;
    fi;
    images := [ ];
    for g in genX1rng do
        ig := Image( rmor, g );
        eg := Image( e2, ig );
        Add( images, eg );
    od;
    for g in genX1src do
        ig := Image( smor, g );
        eg := Image( ek2, ig );
        Add( images, eg );
    od;
    sphi := GroupHomomorphismByImages( C1src, C2src, genC1src, images );
    if not IsGroupHomomorphism( sphi ) then
        Info( InfoXMod, 2, "<sphi> not a homomorphism" );
        return fail;
    fi;
    phi := Cat1MorphismByHoms( C1, C2, sphi, rphi );
    SetCat1MorphismOfXModMorphism( mor, phi );
    SetXModMorphismOfCat1Morphism( phi, mor );
    return phi;
end );

##############################################################################
##
#M  Cat1MorphismOfXModMorphism
##
InstallMethod( Cat1MorphismOfXModMorphism, "for xmod morphisms",
    true, [ IsXModMorphism ], 0,
function( mor )
    return Cat1MorphismByXModMorphism( mor );
end );

##############################################################################
##
#M  PreXModIsomorphismByIsomorphisms
##
InstallMethod( PreXModIsomorphismByIsomorphisms, "for two isomorphisms",
    true, [ IsPreXMod, IsBijective, IsBijective ], 0,
function( P0, sigma, rho )

    local  S0, R0, bdy0, aut0, act0, S1, R1, bdy1, aut1, act1,
           ok, isigma, gen0, im0, gen1, im1, id1, alpha, P1, mor;
    S0 := Source( P0 );
    R0 := Range( P0 );
    if not ( ( S0 = Source( sigma ) ) and ( R0 = Source( rho ) ) ) then
        Info( InfoXMod, 2, "groups of P0 not sources of sigma, rho" );
        return fail;
    fi;
    isigma := InverseGeneralMapping( sigma );
    bdy0 := Boundary( P0 );
    S1 := Range( sigma );
    R1 := Range( rho );
    bdy1 := isigma * bdy0 * rho;
    ok := IsGroupHomomorphism( bdy1 );
    act0 := XModAction( P0 );
    aut0 := AutoGroup( P0 );
    gen0 := MappingGeneratorsImages( act0 )[1];
    im0 := MappingGeneratorsImages( act0 )[2];
    im1 := List( im0, a -> isigma * a * sigma );
    for alpha in im1 do
        ok := IsGroupHomomorphism( alpha );
    od;
    if HasAutomorphismGroup( R1 ) then
        aut1 := AutomorphismGroup( R1 );
    else
        id1 := IdentityMapping( S1 );
        aut1 := Group( im1, id1 );
        SetIsGroupOfAutomorphisms( aut1, true );
    fi;
    gen1 := List( gen0, r -> Image( rho, r ) );
    act1 := GroupHomomorphismByImages( R1, aut1, gen1, im1 );
    P1 := PreXModByBoundaryAndAction( bdy1, act1 );
    mor := PreXModMorphism( P0, P1, sigma, rho );
    SetIsInjective( mor, true );
    SetIsSurjective( mor, true );
    if ( HasIsXMod( P0 ) and IsXMod( P0 ) ) then
        SetIsXMod( P1, true );
        SetIsXModMorphism( mor, true );
    fi;
    return mor;
end );

##############################################################################
##
#M  PreCat1IsomorphismByIsomorphisms
##
InstallMethod( PreCat1IsomorphismByIsomorphisms, "for two isomorphisms",
    true, [ IsPreCat1, IsBijective, IsBijective ], 0,
function( P0, sigma, rho )

    local  S0, R0, isigma, irho, t0, h0, e0, S1, R1, t1, h1, e1, P1, mor; 

    S0 := Source( P0 );
    R0 := Range( P0 );
    if not ( ( S0 = Source( sigma ) ) and ( R0 = Source( rho ) ) ) then
        Info( InfoXMod, 2, "groups of P0 not sources of sigma, rho" );
        return fail;
    fi;
    isigma := InverseGeneralMapping( sigma );
    t0 := TailMap( P0 ); 
    h0 := HeadMap( P0 ); 
    S1 := Range( sigma );
    R1 := Range( rho );
    t1 := isigma * t0 * rho; 
    h1 := isigma * h0 * rho; 
    irho := InverseGeneralMapping( rho ); 
    e0 := RangeEmbedding( P0 ); 
    e1 := irho * e0 * sigma; 
    P1 := PreCat1ByTailHeadEmbedding( t1, h1, e1  );
    mor := PreCat1Morphism( P0, P1, sigma, rho );
    SetIsInjective( mor, true );
    SetIsSurjective( mor, true );
    if ( HasIsCat1( P0 ) and IsCat1( P0 ) ) then
        SetIsCat1( P1, true );
        SetIsCat1Morphism( mor, true );
    fi;
    return mor;
end );

###############################################################################
##
#F  SmallerDegreePerm2dGroup( <obj> )
##
InstallGlobalFunction( SmallerDegreePerm2dGroup, function( obj )

    local  src, rng, sigma, rho, mor; 
    # for a PreXMod
    if ( IsPreXMod( obj ) and IsPermPreXMod( obj ) ) then
        src := Source( obj );
        rng := Range( obj );
        sigma := SmallerDegreePermutationRepresentation( src );
        rho := SmallerDegreePermutationRepresentation( rng );
        mor := PreXModIsomorphismByIsomorphisms( obj, sigma, rho );
        return mor;
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "at present only implemented for a PermPreXMod" );
    return fail;
end );

##############################################################################
##
#M  IsomorphismXModByNormalSubgroup
##
InstallMethod( IsomorphismXModByNormalSubgroup, 
    "for xmod with injective boundary", true, [ IsXMod ], 0,
function( X0 )

    local  S0, R, S1, bdy0, sigma, rho, ok;
    S0 := Source( X0 );
    R := Range( X0 );
    bdy0 := Boundary( X0 );
    if not IsInjective( bdy0 ) then
        Info( InfoXMod, 2, "boundary is not injective" );
        return fail;
    fi;
    if IsSubgroup( S0, R ) then
        return IdentityMapping( X0 );
    else
        S1 := ImagesSource( bdy0 );
        sigma := GeneralRestrictedMapping( bdy0, S0, S1 );
        rho := IdentityMapping( R );
        ok := IsBijective( sigma ) and IsBijective( rho );
        return PreXModIsomorphismByIsomorphisms( X0, sigma, rho );
    fi;
end );

##############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . . . . . . . . . for a 2d-mapping
##
InstallOtherMethod( Order, "generic method for 2d-mapping",
    true, [ Is2dMapping ], 0,
function( mor )
    if not ( IsEndomorphism2dDomain( mor ) and IsBijective( mor ) ) then
       Info( InfoXMod, 2, "mor is not an automorphism" );
       return fail;
    fi;
    return Lcm( Order( SourceHom( mor ) ), Order( RangeHom( mor ) ) );
end );

##############################################################################
##
#M  ImagesSource( <mor> ) . . . . . . . . . . . . . images of an xmod morphism
##
# InstallOtherMethod( ImagesSource, "for an xmod morphism",
#     true, [ IsXModMorphism ], 0,
# function( mor )
##################### MORE TO DO ON THIS! Perhaps it should be ImagesSet here?

##############################################################################
##
#E  gp2map.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
