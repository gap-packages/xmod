##############################################################################
##
#W  gp2map.gi                  GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
##  This file installs methods for 2DimensionalMappings 
##  for crossed modules and cat1-groups. 
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  Is2DimensionalGroupMorphismData( <list> ) . . . . . . . . . . 2d-group map 
##
##  this functions tests that boundaries are ok but no checks on actions 
##
InstallMethod( Is2DimensionalGroupMorphismData,
    "for list [ 2d-group, 2d-group, homomorphism, homomorphism ]", true,
    [ IsList ], 0,
function( L )

    local src, rng, shom, rhom, mor, ok, sbdy, rbdy, st, sh, se, rt, rh, re, 
          sgen, rgen, im1, im2;

    ok := ( Length(L) = 4 ); 
    if not ok then 
        Info( InfoXMod, 2, "require list with 2 2dgroups and 2 group homs" ); 
        return fail; 
    fi;
    src := L[1];  rng := L[2];  shom := L[3];  rhom := L[4]; 
    ok := ( Is2DimensionalGroup( src ) and Is2DimensionalGroup( rng ) 
            and IsGroupHomomorphism( shom) and IsGroupHomomorphism( rhom ) ); 
    if not ok then 
        Info( InfoXMod, 2, "require two 2dgroups and two group homs" ); 
        return fail; 
    fi;
    ok := ( ( Source( src ) = Source( shom ) ) 
            and (  Range( src ) = Source( rhom ) ) 
            and IsSubgroup( Source( rng ), Range( shom ) ) 
            and IsSubgroup(  Range( rng ), Range( rhom ) ) );
    if not ok then
        Info( InfoXMod, 2, "sources and ranges do not match" );
        return false;
    fi; 
    sgen := GeneratorsOfGroup( Source(src) ); 
    rgen := GeneratorsOfGroup( Range(src) );
    if ( IsPreXMod( src ) and IsPreXMod( rng ) ) then 
        sbdy := Boundary( src ); 
        rbdy := Boundary( rng ); 
        im1 := List( sgen, g -> Image( rbdy, Image(shom,g) ) ); 
        im2 := List( sgen, g -> Image( rhom, Image(sbdy,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "boundaries and homs do not commute" ); 
            return false; 
        fi; 
    elif ( IsPreCat1Group( src ) and IsPreCat1Group( rng ) ) then 
        st := TailMap( src ); 
        rt := TailMap( rng ); 
        im1 := List( sgen, g -> Image( rt, Image(shom,g) ) ); 
        im2 := List( sgen, g -> Image( rhom, Image(st,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "tail maps and homs do not commute" ); 
            return false; 
        fi; 
        sh := HeadMap( src ); 
        rh := HeadMap( rng ); 
        im1 := List( sgen, g -> Image( rh, Image(shom,g) ) ); 
        im2 := List( sgen, g -> Image( rhom, Image(sh,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "head maps and homs do not commute" ); 
            return false; 
        fi; 
        se := RangeEmbedding( src ); 
        re := RangeEmbedding( rng ); 
        im1 := List( rgen, g -> Image( re, Image(rhom,g) ) ); 
        im2 := List( rgen, g -> Image( shom, Image(se,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "range embeddings and homs do not commute" ); 
            return false; 
        fi; 
    else 
        Info( InfoXMod, 2, "require 2 prexmods or precat1s, not one of each" );
    fi; 
    return true; 
end ); 

##############################################################################
##
#M  Make2DimensionalGroupMorphism( <list> ) . . . . . . . . . . . 2d-group map 
##
InstallMethod( Make2DimensionalGroupMorphism,
    "for list [2d-group, 2d-group, homomorphism, homomorphism ]", true,
    [ IsList ], 0,
function( L )

    local mor;

    if not Is2DimensionalGroupMorphismData( L ) then
        return fail; 
    fi;
    mor := rec();
    ObjectifyWithAttributes( mor, Type2DimensionalGroupMorphism, 
        Source, L[1],
        Range, L[2],
        SourceHom, L[3],
        RangeHom, L[4] );
    return mor;
end );

#############################################################################
##
#M  IsPreXModMorphism       check diagram of group homs commutes
##
InstallMethod( IsPreXModMorphism, "generic method for morphisms of 2d-groups",
    true, [ Is2DimensionalGroupMorphism ], 0,
function( mor )

    local PM, Pact, Pbdy, Prng, Psrc, QM, Qact, Qbdy, Qrng, Qsrc,
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
#F  MappingGeneratorsImages( <map> ) . . . . . . . . . . . .  for a 2DimensionalMapping
##
InstallOtherMethod( MappingGeneratorsImages, "for a 2DimensionalMapping", 
    true, [ Is2DimensionalMapping ], 0,
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

    local morsrc, morrng, gensrc, genrng, P, Q, name, ok;

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
    true, [ Is2DimensionalGroupMorphism ], 0,
function( mor )

    local PCG, Prng, Psrc, Pt, Ph, Pe, QCG, Qrng, Qsrc, Qt, Qh, Qe,
          morsrc, morrng, x2, x1, y2, z2, y1, z1, gensrc, genrng;

    PCG := Source( mor );
    QCG := Range( mor );
    if not ( IsPreCat1Group( PCG ) and IsPreCat1Group( QCG ) ) then
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
    return ( IsCat1Group( Source( mor ) ) and IsCat1Group(  Range( mor ) ) );
end );

#############################################################################
##
#F  Display( <mor> ) . . . . . . print details of a (pre-)cat1-group morphism
##
InstallMethod( Display, "display a morphism of pre-cat1 groups", true,
    [ IsPreCat1Morphism ], 0,
function( mor )

    local morsrc, morrng, gensrc, genrng, P, Q, name, ok;

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
#M  CompositionMorphism  . . . . . . . . . . . . for two 2Dimensional-mappings
##
InstallOtherMethod( CompositionMorphism, "generic method for 2d-mappings",
    IsIdenticalObj, [ Is2DimensionalMapping, Is2DimensionalMapping ], 0,
function( mor2, mor1 )

    local srchom, rnghom, comp, ok;

    if not ( Range( mor1 ) = Source( mor2 ) ) then
        Info( InfoXMod, 2, "Range(mor1) <> Source(mor2)" );
        return fail;
    fi;
    srchom := CompositionMapping2( SourceHom( mor2 ), SourceHom( mor1 ) );
    rnghom := CompositionMapping2( RangeHom( mor2 ), RangeHom( mor1 ) );
    comp := Make2DimensionalGroupMorphism( 
                [ Source(mor1), Range(mor2), srchom, rnghom ]);
    if IsPreCat1Group( Source( mor1 ) ) then
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
#M  InverseGeneralMapping . . . . . . . . . . . . . for a 2Dimensional-mapping
##
#?  (29/06/12) only works if mor is _already_ known to be bijective, 
#?             so perhaps move IsBijective into the code ?? 
## 
InstallOtherMethod( InverseGeneralMapping, "generic method for 2d-mapping",
    true, [ Is2DimensionalMapping and IsBijective ], 0, 
function( mor )

    local inv, ok;

    inv := Make2DimensionalGroupMorphism( 
        [ Range(mor), Source(mor), SourceHom(mor)^(-1), RangeHom(mor)^(-1) ] );
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
    [ Is2DimensionalDomain ], 0,
function( obj )

    local shom, rhom;

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
#M  InclusionMorphism2DimensionalDomains( <obj>, <sub> )
##
InstallMethod( InclusionMorphism2DimensionalDomains, "one 2d-object in another", 
    true, [ Is2DimensionalDomain, Is2DimensionalDomain ], 0,
function( obj, sub )

    local shom, rhom;

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

    local ok, mor, nargs;

    nargs := Length( arg );
    # two pre-xmods and two homomorphisms
    if ( nargs = 4 ) then 
        mor := Make2DimensionalGroupMorphism( [arg[1],arg[2],arg[3],arg[4] ] ); 
    else 
        # alternatives not allowed
        Info( InfoXMod, 2, "usage: PreXModMorphism([src,rng,srchom,rnghom]);" );
        return fail;
    fi; 
    ok := IsPreXModMorphism( mor ); 
    return mor;
end );

###############################################################################
##
#F  XModMorphism( <src>, <rng>, <srchom>, <rnghom> )    crossed module morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( XModMorphism, function( arg )

    local nargs;

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

    local nargs;

    nargs := Length( arg );
    # two pre-cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsPreCat1Group( arg[1] ) and IsPreCat1Group( arg[2])
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

    local nargs;

    nargs := Length( arg );
    # two cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsCat1Group( arg[1] ) and IsCat1Group( arg[2])
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

    local mor, ok;

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

    local Xrng, Xsrc, genrng, gensrc, rhom, shom, s;

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
#M  InnerAutomorphismCat1Group( <C1G>, <r> ) . . . conjugation of a cat1-group
##
InstallMethod( InnerAutomorphismCat1Group, "method for cat1-groups", true,
    [ IsPreCat1Group, IsMultiplicativeElementWithInverse ], 0,
function( C1G, r )

    local Crng, Csrc, genrng, gensrc, rhom, shom, s;

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

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . .  for a morphism of pre-crossed modules 
##
InstallMethod( String, "method for a morphism of pre-crossed modules", true, 
    [ IsPreXModMorphism ], 0, 
function( mor ) 
    return( STRINGIFY( "[", String( Source(mor) ), " => ", 
                            String( Range(mor) ), "]" ) ); 
end );

InstallMethod( ViewString, "method for a morphism of pre-crossed modules", 
    true, [ IsPreXModMorphism ], 0, String ); 

InstallMethod( PrintString, "method for a morphism of pre-crossed modules", 
    true, [ IsPreXModMorphism ], 0, String ); 

InstallMethod( ViewObj, "method for a morphism of pre-crossed modules", true,
    [ IsPreXModMorphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

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

    local comp;

    comp := CompositionMorphism( mor2, mor1 );
    ## need to do some checks here !? ##
    return comp;
end );

##############################################################################
##
#M  \^( <mor>, <int> ) . . . . . . . . . . . . . . . for a 2DimensionalMapping
##
InstallOtherMethod( POW, "for a 2d mapping", true, 
    [ Is2DimensionalMapping, IsInt ], 0,
function( map, n )

    local pow, i, ok;

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
#M  IsomorphismByIsomorphisms . . . . . . constructs isomorphic pre-cat1-group
##
InstallMethod( IsomorphismByIsomorphisms, "generic method for pre-cat1-groups", 
    true, [ IsPreCat1Group, IsList ], 0,
function( PC, isos )

    local G, R, t, h, e, isoG, isoR, mgiG, mgiR, invG, invR, 
          G2, R2, t2, h2, e2, PC2, mor;

    G := Source( PC ); 
    R := Range( PC );
    t := TailMap( PC ); 
    h := HeadMap( PC ); 
    e := RangeEmbedding( PC ); 
    isoG := isos[1]; 
    isoR := isos[2]; 
    if not ( ( Source(isoR) = R ) and ( Source(isoG) = G ) ) then 
        Error( "isomorphisms do not have G,R as source" ); 
    fi; 
    G2 := Range( isoG );
    R2 := Range( isoR ); 
    mgiG := MappingGeneratorsImages( isoG ); 
    invG := GroupHomomorphismByImages( G2, G, mgiG[2], mgiG[1] ); 
    mgiR := MappingGeneratorsImages( isoR ); 
    invR := GroupHomomorphismByImages( R2, R, mgiR[2], mgiR[1] ); 
    t2 := CompositionMapping( isoR, t, invG ); 
    h2 := CompositionMapping( isoR, h, invG );
    e2 := CompositionMapping( isoG, e, invR );
    PC2 := PreCat1GroupByTailHeadEmbedding( t2, h2, e2 );
    mor := PreCat1Morphism( PC, PC2, isoG, isoR ); 
    return mor; 
end );

InstallMethod( IsomorphismByIsomorphisms, "generic method for pre-xmods", 
    true, [ IsPreXMod, IsList ], 0,
function( PM, isos )

    local Psrc, Prng, Pbdy, Pact, Paut, Pautgen, siso, smgi, sinv, riso, 
          rmgi, rinv, Qsrc, Qrng, Qbdy, Qaut, Qautgen, ahom, Qact, QM, iso; 

    Psrc := Source( PM ); 
    Prng := Range( PM );
    Pbdy := Boundary( PM );
    siso := isos[1];
    Qsrc := ImagesSource( siso ); 
    smgi := MappingGeneratorsImages( siso );  
    sinv := GroupHomomorphismByImages( Qsrc, Psrc, smgi[2], smgi[1] ); 
    riso := isos[2]; 
    Qrng := ImagesSource( riso );
    rmgi := MappingGeneratorsImages( riso );  
    rinv := GroupHomomorphismByImages( Qrng, Prng, rmgi[2], rmgi[1] ); 
    if not ( ( Psrc = Source(siso) ) and ( Prng = Source(riso) ) ) then 
        Info( InfoXMod, 2, "groups of PM not sources of isomorphisms" ); 
        return fail; 
    fi; 
    Qbdy := CompositionMapping( riso, Pbdy, sinv ); 
    Pact := XModAction( PM );
    Paut := Range( Pact );
    Pautgen := GeneratorsOfGroup( Paut );
    Qautgen := List( Pautgen, a -> CompositionMapping( siso, a, sinv ) );
    Qaut := Group( Qautgen );
    ahom := GroupHomomorphismByImages( Paut, Qaut, Pautgen, Qautgen );
    Qact := CompositionMapping( ahom, Pact, rinv );
    QM := PreXModByBoundaryAndAction( Qbdy, Qact );
    iso := PreXModMorphismByHoms( PM, QM, siso, riso ); 
    SetIsInjective( iso, true );
    SetIsSurjective( iso, true );
    SetImagesSource( iso, QM );
    if ( HasIsXMod( PM ) and IsXMod( PM ) ) then 
        SetIsXMod( QM, true );
        SetIsXModMorphism( iso, true ); 
    fi;
    return iso;
end );

##############################################################################
##
#M  IsomorphismPerm2DimensionalGroup . . . constructs isomorphic perm pre-xmod
#M  IsomorphismPerm2DimensionalGroup . . . constructs isomorphic perm pre-cat1
##
InstallMethod( IsomorphismPerm2DimensionalGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QM, iso;

    if IsPermPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsPermGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismSmallPermGroup( Psrc );
        Qsrc := ImagesSource( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PM );
    if IsPermGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismSmallPermGroup( Prng );
        Qrng := ImagesSource( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi; 
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := ImagesSource( iso );
    if HasName( PM ) then
        SetName( QM, Concatenation( "P", Name( PM ) ) );
    fi;
    iso := PreXModMorphism( PM, QM, shom, rhom );  
    return iso;
end );

InstallMethod( IsomorphismPerm2DimensionalGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QCG, iso;

    if IsPermPreCat1Group( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsPermGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismSmallPermGroup( Psrc );
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PCG );
    if IsPermGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng ); 
        if IsEndomorphismPreCat1Group( PCG ) then 
            rhom := RestrictedMapping( shom, Prng ); 
        else 
            rhom := IsomorphismSmallPermGroup( Prng );
        fi;
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := ImagesSource( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Pc", Name( PCG ) ) );
    fi;
    iso := PreCat1Morphism( PCG, QCG, shom, rhom ); 
    return iso;
end );

##############################################################################
##
#M  IsomorphismPc2DimensionalGroup . . . . . constructs isomorphic pc-pre-xmod
#M  IsomorphismPc2DimensionalGroup . . . . . constructs isomorphic pc-pre-cat1
##
InstallMethod( IsomorphismPc2DimensionalGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QM, iso;

    if IsPcPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsPcGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismPcGroup( Psrc ); 
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PM ); 
    if ( HasIsNormalSubgroup2DimensionalGroup(PM) 
             and IsNormalSubgroup2DimensionalGroup(PM) ) then 
        Print( "#!  modify IsomorphismPc2DimensionalGroup to preserve the\n", 
               "#!  property of being IsNormalSubgroup2DimensionalGroup\n" ); 
    fi; 
    if IsPcGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismPcGroup( Prng ); 
        if ( rhom = false ) then 
            return false; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := ImagesSource( iso ); 
    if HasName( PM ) then
        SetName( QM, Concatenation( "Pc", Name( PM ) ) );
    fi;
    iso := PreXModMorphism( PM, QM, shom, rhom );
    return iso;
end );

InstallMethod( IsomorphismPc2DimensionalGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QCG, iso;

    if IsPcPreCat1Group( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsPcGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismPcGroup( Psrc );
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PCG );
    if IsPcGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismPcGroup( Prng ); 
        if ( rhom = fail ) then 
            return fail; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := ImagesSource( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Pc", Name( PCG ) ) );
    fi;
    iso := PreCat1Morphism( PCG, QCG, shom, rhom );
    return iso;
end );

##############################################################################
##
#M  IsomorphismFp2DimensionalGroup . . . . . constructs isomorphic fp-pre-xmod
#M  IsomorphismFp2DimensionalGroup . . . . . constructs isomorphic fp-pre-cat1
##
InstallMethod( IsomorphismPc2DimensionalGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QM, iso;

    if IsFpPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsFpGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismFpGroup( Psrc ); 
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PM ); 
    if ( HasIsNormalSubgroup2DimensionalGroup(PM) 
             and IsNormalSubgroup2DimensionalGroup(PM) ) then 
        Print( "#!  modify IsomorphismFp2DimensionalGroup to preserve the\n", 
               "#!  property of being IsNormalSubgroup2DimensionalGroup\n" ); 
    fi; 
    if IsFpGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismFpGroup( Prng ); 
        if ( rhom = false ) then 
            return false; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := ImagesSource( iso ); 
    if HasName( PM ) then
        SetName( QM, Concatenation( "Fp", Name( PM ) ) );
    fi; 
    iso := PreXModMorphism( PM, QM, shom, rhom ); 
    return iso;
end );

InstallMethod( IsomorphismFp2DimensionalGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local shom, rhom, Psrc, Psgen, Qsrc, Qsgen, Prng, Prgen, Qrng, Qrgen, 
          QCG, iso;

    if IsFpPreCat1Group( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsFpGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        Psgen := GeneratorsOfGroup( Psrc );
        shom := IsomorphismFpGroup( Psrc );
        if ( shom = fail ) then 
            return fail; 
        fi; 
        Qsrc := Image( shom );
        Qsgen := List( Psgen, s -> Image( shom, s ) );
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PCG );
    if IsFpGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng );
        rhom := IsomorphismFpGroup( Prng ); 
        if ( rhom = fail ) then 
            return fail; 
        fi; 
        Qrng := Image( rhom );
        Qrgen := List( Prgen, r -> Image( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := ImagesSource( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Fp", Name( PCG ) ) );
    fi;
    iso := PreCat1Morphism( PCG, QCG, shom, rhom ); 
    return iso;
end );

###############################################################################
##
#M  IsomorphismPreCat1Groups . . isomorphism between a pair of pre-cat1-groups 
##
InstallMethod( IsomorphismPreCat1Groups, "generic method for 2 pre-cat1-groups", 
    true, [ IsPreCat1Group, IsPreCat1Group ], 0,
function( C1, C2 )

    local t1, h1, e1, t2, h2, e2, G1, G2, R1, R2, A, phi, psi, 
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
InstallMethod( Name, "method for a 2d-mapping", true, 
    [ Is2DimensionalMapping ], 0,
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

###############################################################################
##
#M  PreXModMorphismByHoms( <P>, <Q>, <hsrc>, <hrng> ) . . make prexmod morphism
##
InstallMethod( PreXModMorphismByHoms,
    "for pre-xmod, pre-xmod, homomorphism, homomorphism,", true,
    [ IsPreXMod, IsPreXMod, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local filter, fam, mor, ok, nsrc, nrng, name;

    if not ( IsGroupHomomorphism(srchom) and IsGroupHomomorphism(rnghom) ) then
        Info( InfoXMod, 2, "source and range mappings must be group homs" );
        return fail;
    fi;
    mor := Make2DimensionalGroupMorphism( [ src, rng, srchom, rnghom ] );
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
    [ IsPreCat1Group, IsPreCat1Group, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local filter, fam, mor, ok, nsrc, nrng, name;

    mor := Make2DimensionalGroupMorphism( [ src, rng, srchom, rnghom ] ); 
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
    [ IsCat1Group, IsCat1Group, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local mor, ok;

    mor := PreCat1MorphismByHoms( src, rng, srchom, rnghom );
    ok := IsCat1Morphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . for a morphism of pre-cat1 groups 
##
InstallMethod( String, "method for a morphism of pre-cat1 groups", true, 
    [ IsPreCat1Morphism ], 0, 
function( mor ) 
    return( STRINGIFY( "[", String( Source(mor) ), " => ", 
                            String( Range(mor) ), "]" ) ); 
end );

InstallMethod( ViewString, "method for a morphism of pre-cat1 groups", true, 
    [ IsPreCat1Morphism ], 0, String ); 

InstallMethod( PrintString, "fmethod for a morphism of pre-cat1 groups", true, 
    [ IsPreCat1Morphism ], 0, String ); 

InstallMethod( ViewObj, "method for a morphism of pre-cat1 groups", true,
    [ IsPreCat1Morphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

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
    [ IsPreCat1Group ], 0,
function( C1G )

    local rev, shom, rhom, src, gensrc, t, h, e, im;

    rev := ReverseCat1Group( C1G );
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
#M  IsInjective( map ) . . . . . . . . . . . . . .  for a 2Dimensional-mapping
##
InstallOtherMethod( IsInjective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsInjective( SourceHom( map ) )
             and IsInjective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsSurjective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsSurjective( SourceHom( map ) )
             and IsSurjective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsSingleValued,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsSingleValued( SourceHom( map ) )
             and IsSingleValued( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . . . .  for a 2Dimensional-mapping
##
InstallOtherMethod( IsTotal,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsTotal( SourceHom( map ) )
             and IsTotal( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . . .  for a 2Dimensional-mapping
##
InstallOtherMethod( IsBijective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsBijective( SourceHom( map ) )
             and IsBijective( RangeHom( map ) ) )  );

##############################################################################
##
#M  IsEndomorphism2DimensionalDomain( map ) . . . . . . . . . for a 2Dimensional-mapping
#?  temporary fix 08/01/04  ---  need to check correctness
#M  IsAutomorphism2DimensionalDomain( map ) . . . . . . . . . for a 2Dimensional-mapping
##
InstallMethod( IsEndomorphism2DimensionalDomain, 
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> IsEndoMapping( SourceHom( map ) ) and 
           IsEndoMapping( RangeHom( map ) ) );

InstallMethod( IsAutomorphism2DimensionalDomain, 
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map ->  IsEndomorphism2DimensionalDomain( map ) and IsBijective( map ) );

##############################################################################
##
#M  IsSourceMorphism( mor ) . . . . . . . . . . . . . . . for an xmod morphism
##
InstallMethod( IsSourceMorphism,
    "method for a morphism of crossed modules",
    true,
    [ IsXModMorphism ], 0,
function( mor )

    local Srng, Rrng;

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
     true, [ Is2DimensionalMapping ], 0,
function( map )

    local kerS, kerR, K;

    if HasKernel2DimensionalMapping( map ) then
        return Kernel2DimensionalMapping( map );
    fi;
    kerS := Kernel( SourceHom( map ) ); 
    kerR := Kernel( RangeHom( map ) ); 
    K := Sub2DimensionalGroup( Source( map ), kerS, kerR );
    SetKernel2DimensionalMapping( map, K );
    return K;
end );

##############################################################################
##
#M  PreXModBySourceHom        top PreXMod from a morphism of crossed P-modules
##
InstallMethod( PreXModBySourceHom, "for a pre-crossed module morphism",
    true, [ IsPreXModMorphism ], 0,
function( mor )
        
    local X1, X2, src1, rng1, src2, rng2, bdy2, y, z, S, Sbdy, Saut, 
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
    isconj := IsNormalSubgroup2DimensionalGroup( S );
    return S; 
end );

############################################################################
##
#M  XModMorphismOfCat1Morphism
#M  Cat1MorphismOfXModMorphism
##
InstallMethod( XModMorphismOfCat1Morphism, "for a cat1-group morphism",
    true, [ IsCat1Morphism ], 0,
function( phi )

    local C1, C2, C1src, genC1src, e2, t2, proj2,
          X1, X2, X1src, X1rng, X2src, X2rng,
          genX1src, genX1rng, ek1, eksrc1, sphi, rphi, x,
          imrphi, imsphi, im, images, smor, mor, info1, info2;

    C1 := Source( phi );
    C2 := Range( phi );
    C1src := Source( C1 );
    X1 := XModOfCat1Group( C1 );
    X2 := XModOfCat1Group( C2 );
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

InstallMethod( Cat1MorphismOfXModMorphism, "for a pre-crossed module morphism",
    true, [ IsXModMorphism ], 0,
function( mor )

    local X1, X2, C1, C2, act2, C1src, C1rng, C2src, C2rng, C2s2p, 
          genC1src, genC1rng, genX1src, genX1rng, imrmor, imgen, m, images,
          sphi, rphi, phi, smor, rmor, e2, ek2, g, ig, eg, pg, esrc, erng;

    X1 := Source( mor );
    X2 := Range( mor );
    C1 := Cat1GroupOfXMod( X1 );
    C2 := Cat1GroupOfXMod( X2 );
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
#F  SmallerDegreePerm2DimensionalGroup( <obj> )
##
InstallGlobalFunction( SmallerDegreePerm2DimensionalGroup, function( obj )

    local src, rng, sigma, rho, mor; 

    # for a PreXMod
    if ( IsPreXMod( obj ) and IsPermPreXMod( obj ) ) then
        src := Source( obj );
        rng := Range( obj );
        sigma := SmallerDegreePermutationRepresentation( src );
        rho := SmallerDegreePermutationRepresentation( rng );
        mor := IsomorphismByIsomorphisms( obj, [ sigma, rho ] );
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

    local S0, R, S1, bdy0, sigma, rho, ok;

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
        return IsomorphismByIsomorphisms( X0, [ sigma, rho ] );
    fi;
end );

##############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( Order, "generic method for 2d-mapping",
    true, [ Is2DimensionalMapping ], 0,
function( mor )
    if not ( IsEndomorphism2DimensionalDomain( mor ) 
             and IsBijective( mor ) ) then
       Info( InfoXMod, 2, "mor is not an automorphism" );
       return fail;
    fi;
    return Lcm( Order( SourceHom( mor ) ), Order( RangeHom( mor ) ) );
end );

#############################################################################
##
#M  ImagesSource( <mor> ) . . . . . . . . for pre-xmod and pre-cat1 morphisms
##
InstallOtherMethod( ImagesSource, "image for a pre-xmod or pre-cat1 morphism", 
    true, [ Is2DimensionalMapping ], 0, 
function( mor )
    
    local Shom, Rhom, imS, imR, sub;

    Shom := SourceHom( mor );
    Rhom := RangeHom( mor );
    imS := ImagesSource( Shom );
    imR := ImagesSource( Rhom );
    sub := Sub2DimensionalGroup( Range(mor), imS, imR );
    return sub;
end );

##############################################################################
##
#E  gp2map.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
