#############################################################################
##
#W  gp2map.gi                  GAP4 package `XMod'             Chris Wensley
#W                                                               & Murat Alp
##  This file installs methods for 2DimensionalMappings 
##  for crossed modules and cat1-groups. 
##
#Y  Copyright (C) 2001-2023, Chris Wensley et al, 

#############################################################################
##
#M  Is2DimensionalGroupMorphismData( <list> ) . . . . . . . . . 2d-group map 
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
        return false; 
    fi;
    src := L[1];  rng := L[2];  shom := L[3];  rhom := L[4]; 
    ok := ( Is2DimensionalGroup( src ) and Is2DimensionalGroup( rng ) 
            and IsGroupHomomorphism( shom) and IsGroupHomomorphism( rhom ) ); 
    if not ok then 
        Info( InfoXMod, 2, "require two 2dgroups and two group homs" ); 
        return false; 
    fi;
    ok := ( ( Source( src ) = Source( shom ) ) 
            and (  Range( src ) = Source( rhom ) ) 
            and IsSubgroup( Source( rng ), ImagesSource( shom ) ) 
            and IsSubgroup(  Range( rng ), ImagesSource( rhom ) ) );
    if not ok then
        Info( InfoXMod, 2, "sources and ranges do not match" );
        return false;
    fi; 
    sgen := GeneratorsOfGroup( Source(src) ); 
    rgen := GeneratorsOfGroup( Range(src) );
    if ( IsPreXMod( src ) and IsPreXMod( rng ) ) then 
        sbdy := Boundary( src ); 
        rbdy := Boundary( rng ); 
        im1 := List( sgen, g -> ImageElm( rbdy, ImageElm(shom,g) ) ); 
        im2 := List( sgen, g -> ImageElm( rhom, ImageElm(sbdy,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "boundaries and homs do not commute" ); 
            return false; 
        fi; 
    elif ( IsPreCat1Group( src ) and IsPreCat1Group( rng ) ) then 
        st := TailMap( src ); 
        rt := TailMap( rng ); 
        im1 := List( sgen, g -> ImageElm( rt, ImageElm(shom,g) ) ); 
        im2 := List( sgen, g -> ImageElm( rhom, ImageElm(st,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "tail maps and homs do not commute" ); 
            return false; 
        fi; 
        sh := HeadMap( src ); 
        rh := HeadMap( rng ); 
        im1 := List( sgen, g -> ImageElm( rh, ImageElm(shom,g) ) ); 
        im2 := List( sgen, g -> ImageElm( rhom, ImageElm(sh,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "head maps and homs do not commute" ); 
            return false; 
        fi; 
        se := RangeEmbedding( src ); 
        re := RangeEmbedding( rng ); 
        im1 := List( rgen, g -> ImageElm( re, ImageElm(rhom,g) ) ); 
        im2 := List( rgen, g -> ImageElm( shom, ImageElm(se,g) ) ); 
        if not ( im1 = im2 ) then 
            Info( InfoXMod, 2, "range embeddings and homs do not commute" ); 
            return false; 
        fi; 
    else 
        Info( InfoXMod, 2, "require 2 prexmods or precat1s, not one of each" );
    fi; 
    return true; 
end ); 

#############################################################################
##
#M  Make2DimensionalGroupMorphism( <list> ) . . . . . . . . . . 2d-group map 
##
InstallMethod( Make2DimensionalGroupMorphism,
    "for list [ 2d-group, 2d-group, homomorphism, homomorphism ]", true,
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
#F  MappingGeneratorsImages( <map> ) . . . . . . . for a 2DimensionalMapping
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
    Print( "  ", List( gensrc, s -> ImageElm( morsrc, s ) ), "\n" );
    Print( ": Range Homomorphism maps range generators to:\n" );
    Print( "  ", List( genrng, r -> ImageElm( morrng, r ) ), "\n" );
end ); 

#############################################################################
##
#M  IsPreCat1GroupMorphism . . . . . . . check diagram of group homs commutes 
##
InstallMethod( IsPreCat1GroupMorphism, "generic method for morphisms of 2d-groups", 
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
#M  IsCat1GroupMorphism
##
InstallMethod( IsCat1GroupMorphism, "generic method for cat1-group homomorphisms",
    true, [ IsPreCat1GroupMorphism ], 0,
function( mor )
    return ( IsCat1Group( Source( mor ) ) and IsCat1Group(  Range( mor ) ) );
end );

#############################################################################
##
#F  Display( <mor> ) . . . . . . print details of a (pre-)cat1-group morphism
##
InstallMethod( Display, "display a morphism of pre-cat1 groups", true,
    [ IsPreCat1GroupMorphism ], 0,
function( mor )

    local morsrc, morrng, gensrc, genrng, P, Q, name, ok;

    if not HasName( mor ) then
        # name := PreCat1GroupMorphismName( mor );
        SetName( mor, "[..=>..]=>[..=>..]" );
    fi;
    name := Name( mor );
    P := Source( mor );
    Q := Range( mor );
    morsrc := SourceHom( mor );
    gensrc := GeneratorsOfGroup( Source( P ) );
    morrng := RangeHom( mor );
    genrng := GeneratorsOfGroup( Range( P ) );
    if IsCat1GroupMorphism( mor ) then
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
    Print( "  ", List( gensrc, s -> ImageElm( morsrc, s ) ), "\n" );
    Print( ": Range Homomorphism maps range generators to:\n" );
    Print( "  ", List( genrng, r -> ImageElm( morrng, r ) ), "\n" );
end ); 

#############################################################################
##
#M  CompositionMorphism  . . . . . . . . . . . for two 2Dimensional-mappings
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
        if ( IsPreCat1GroupMorphism( mor1 ) 
             and IsPreCat1GroupMorphism( mor2 ) ) then
            SetIsPreCat1GroupMorphism( comp, true );
        fi;
        if ( IsCat1GroupMorphism(mor1) and IsCat1GroupMorphism(mor2) ) then
            SetIsCat1GroupMorphism( comp, true );
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

#############################################################################
##
#M  InverseGeneralMapping . . . . . . . . . . . . for a 2Dimensional-mapping
## 
InstallOtherMethod( InverseGeneralMapping, "generic method for 2d-mapping",
    true, [ Is2DimensionalMapping ], 0, 
function( mor )

    local sinv, rinv, inv, ok;

    if not IsBijective( mor ) then 
        Info( InfoXMod, 1, "mor is not bijective" ); 
        return fail; 
    fi; 
    sinv := InverseGeneralMapping( SourceHom( mor ) ); 
    rinv := InverseGeneralMapping( RangeHom( mor ) ); 
    inv := Make2DimensionalGroupMorphism( [Range(mor),Source(mor),sinv,rinv] );
    if IsPreXModMorphism( mor ) then 
        SetIsPreXModMorphism( inv, true );
        if IsXModMorphism( mor ) then 
            SetIsXModMorphism( inv, true );
        fi;
    elif IsPreCat1GroupMorphism( mor ) then 
        SetIsPreCat1GroupMorphism( inv, true );
        if IsCat1GroupMorphism( mor ) then 
            SetIsCat1GroupMorphism( inv, true );
        fi;
    fi;
    SetIsInjective( inv, true );
    SetIsSurjective( inv, true );
    return inv;
end );

#############################################################################
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
        return PreXModMorphismByGroupHomomorphisms( obj, obj, shom, rhom );
    elif IsPreCat1Obj( obj ) then
        return PreCat1GroupMorphismByGroupHomomorphisms( obj, obj, shom, rhom );
    else
        return fail;
    fi;
end );

#############################################################################
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
        return PreXModMorphismByGroupHomomorphisms( sub, obj, shom, rhom );
    elif IsPreCat1Obj( obj ) then
        return PreCat1GroupMorphismByGroupHomomorphisms( sub, obj, shom, rhom );
    else
        return fail;
    fi;
end );

#############################################################################
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

#############################################################################
##
#F  XModMorphism( <src>, <rng>, <srchom>, <rnghom> )  crossed module morphism
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
        return XModMorphismByGroupHomomorphisms(arg[1],arg[2],arg[3],arg[4]);
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: XModMorphism( src, rng, srchom, rnghom );" );
    return fail;
end );

#############################################################################
##
#F  PreCat1GroupMorphism( <src>,<rng>,<srchom>,<rnghom> ) 
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( PreCat1GroupMorphism, function( arg )

    local nargs;

    nargs := Length( arg );
    # two pre-cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsPreCat1Group( arg[1] ) and IsPreCat1Group( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return PreCat1GroupMorphismByGroupHomomorphisms( 
                   arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, 
          "usage: PreCat1GroupMorphism( src, rng, srchom, rnghom );" );
    return fail;
end );

#############################################################################
##
#F  Cat1GroupMorphism( <src>, <rng>, <srchom>, <rnghom> ) 
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( Cat1GroupMorphism, function( arg )

    local nargs;

    nargs := Length( arg );
    # two cat1s and two homomorphisms
    if ( ( nargs = 4 ) and IsCat1Group( arg[1] ) and IsCat1Group( arg[2])
                       and IsGroupHomomorphism( arg[3] )
                       and IsGroupHomomorphism( arg[4] ) ) then
        return Cat1GroupMorphismByGroupHomomorphisms( arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, 
          "usage: Cat1GroupMorphism( src, rng, srchom, rnghom );" );
    return fail;
end );

#############################################################################
##
#M  XModMorphismByGroupHomomorphisms( <Xs>, <Xr>, <hsrc>, <hrng> ) 
##  . . . make an xmod morphism
##
InstallMethod( XModMorphismByGroupHomomorphisms, "for 2 xmods and 2 homs", 
    true, [ IsXMod, IsXMod, IsGroupHomomorphism, IsGroupHomomorphism ], 0,
function( src, rng, srchom, rnghom )

    local mor, ok;

    mor := PreXModMorphismByGroupHomomorphisms( src, rng, srchom, rnghom );
    ok := IsXModMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

#############################################################################
##
#M  InnerAutomorphismXMod( <XM>, <r> ) . . . . . . .  conjugation of an xmod
##
InstallMethod( InnerAutomorphismXMod, "method for crossed modules", true,
    [ IsPreXMod, IsMultiplicativeElementWithInverse ], 0,
function( XM, r )

    local Xrng, Xsrc, genrng, gensrc, rhom, shom, s;

    Xrng := Range( XM );
    if not ( r in Xrng ) then
        Info( InfoXMod, 2, 
              "conjugating element must be in the range group" );
        return fail;
    fi;
    Xsrc := Source( XM );
    gensrc := GeneratorsOfGroup( Xsrc );
    genrng := GeneratorsOfGroup( Xrng );
    rhom := GroupHomomorphismByImages( Xrng, Xrng, genrng,
                List( genrng, g -> g^r ) );
    s := ImageElm( XModAction( XM ), r );
    shom := GroupHomomorphismByImages( Xsrc, Xsrc, gensrc, 
                List( gensrc, g -> g^s ) );
    return XModMorphismByGroupHomomorphisms( XM, XM, shom, rhom );
end );

#############################################################################
##
#M  InnerAutomorphismCat1Group( <C1G>, <r> ) . . conjugation of a cat1-group
##
InstallMethod( InnerAutomorphismCat1Group, "method for cat1-groups", true,
    [ IsPreCat1Group, IsMultiplicativeElementWithInverse ], 0,
function( C1G, r )

    local Crng, Csrc, genrng, gensrc, rhom, shom, s;

    Crng := Range( C1G );
    if not ( r in Crng ) then
        Info( InfoXMod, 2, 
              "conjugating element must be in the range group" );
        return fail;
    fi;
    Csrc := Source( C1G );
    gensrc := GeneratorsOfGroup( Csrc );
    genrng := GeneratorsOfGroup( Crng );
    rhom := GroupHomomorphismByImages( Crng, Crng, genrng,
                List( genrng, g -> g^r ) );
    s := ImageElm( RangeEmbedding( C1G ), r );
    shom := GroupHomomorphismByImages( Csrc, Csrc, gensrc, 
                List( gensrc, g -> g^s ) );
    return Cat1GroupMorphismByGroupHomomorphisms( C1G, C1G, shom, rhom );
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

#############################################################################
##
#M  \*( <mor1>, <mor2> ) . . . . . . . . for 2 pre-crossed module morphisms
##
InstallOtherMethod( \*, "for two morphisms of pre-crossed modules",
    IsIdenticalObj, [ Is2DimensionalMapping, Is2DimensionalMapping ], 0,
function( mor1, mor2 )

    local comp;

    comp := CompositionMorphism( mor2, mor1 );
    ## need to do some checks here !? ##
    return comp;
end );

#############################################################################
##
#M  \^( <mor>, <int> ) . . . . . . . . . . . . . . for a 2DimensionalMapping
##
InstallOtherMethod( \^, "for a 2d mapping", true,
    [ Is2DimensionalMapping, IsInt ], 0,
function( map, n )

    local pow, i, ok;

    if ( n = 1 ) then
        return map; 
    elif ( n = 0 ) then 
        return fail; 
    elif ( n > 1 ) then 
        if not ( Source( map ) = Range( map ) ) then 
            Info( InfoXMod, 1, "unequal source and range" ); 
            return fail; 
        fi; 
        pow := map;
        for i in [2..n] do
            pow := CompositionMorphism( pow, map );
        od;
        return pow;
    else 
        ok := IsBijective( map );
        if not ok then 
            Info( InfoXMod, 1, "map not bijective" ); 
            return fail; 
        else 
            pow := InverseGeneralMapping( map ); 
            if ( n = -1 ) then 
                return pow; 
            else 
                if not ( Source( map ) = Range( map ) ) then 
                    Info( InfoXMod, 1, "unequal source and range" ); 
                    return fail; 
                fi; 
                for i in [2..-n] do
                    pow := CompositionMorphism( pow, map );
                od;
                return pow;
            fi;
        fi; 
    fi;
end );

#############################################################################
##
#M  IsomorphismByIsomorphisms . . . . . constructs isomorphic pre-cat1-group
##
InstallMethod( IsomorphismByIsomorphisms, "generic method for pre-cat1-groups", 
    true, [ IsPreCat1Group, IsList ], 0,
function( PC, isos )

    local G, R, t, h, e, isoG, isoR, mgiG, mgiR, 
          G2, R2, imt2, t2, imh2, h2, ime2, e2, PC2, mor;

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
    mgiR := MappingGeneratorsImages( isoR ); 
    imt2 := List( mgiG[1], g -> ImageElm( isoR, ImageElm( t, g ) ) ); 
    t2 := GroupHomomorphismByImages( G2, R2, mgiG[2], imt2 ); 
    imh2 := List( mgiG[1], g -> ImageElm( isoR, ImageElm( h, g ) ) ); 
    h2 := GroupHomomorphismByImages( G2, R2, mgiG[2], imh2 ); 
    ime2 := List( mgiR[1], r -> ImageElm( isoG, ImageElm( e, r ) ) ); 
    e2 := GroupHomomorphismByImages( R2, G2, mgiR[2], ime2 ); 
    PC2 := PreCat1GroupByTailHeadEmbedding( t2, h2, e2 );
    mor := PreCat1GroupMorphism( PC, PC2, isoG, isoR ); 
    return mor; 
end );

InstallMethod( IsomorphismByIsomorphisms, "generic method for pre-xmods", 
    true, [ IsPreXMod, IsList ], 0,
function( PM, isos )

    local Psrc, Prng, Pbdy, Pact, Paut, Pautgen, siso, smgi, riso, rmgi, 
          Qsrc, Qrng, imQbdy, Qbdy, Qaut, len, Qautgen, i, a, ima, ahom, 
          imQact, Qact, QM, iso; 

    Psrc := Source( PM ); 
    Prng := Range( PM );
    Pbdy := Boundary( PM );
    siso := isos[1];
    Qsrc := Range( siso ); 
    smgi := MappingGeneratorsImages( siso );  
    riso := isos[2]; 
    Qrng := Range( riso );
    rmgi := MappingGeneratorsImages( riso );  
    if not ( ( Psrc = Source(siso) ) and ( Prng = Source(riso) ) ) then 
        Info( InfoXMod, 2, "groups of PM not sources of isomorphisms" ); 
        return fail; 
    fi; 
    imQbdy := List( smgi[1], s -> ImageElm( riso, ImageElm( Pbdy, s ) ) ); 
    Qbdy := GroupHomomorphismByImages( Qsrc, Qrng, smgi[2], imQbdy );  
    Pact := XModAction( PM );
    Paut := Range( Pact );
    Pautgen := GeneratorsOfGroup( Paut );
    len := Length( Pautgen ); 
    Qautgen := ListWithIdenticalEntries( len, 0 ); 
    for i in [1..len] do 
        a := Pautgen[i]; 
        ima := List( smgi[1], s -> ImageElm( siso, ImageElm( a, s ) ) ); 
        Qautgen[i] := GroupHomomorphismByImages( Qsrc, Qsrc, smgi[2], ima ); 
    od; 
    Qaut := Group( Qautgen ); 
    ahom := GroupHomomorphismByImages( Paut, Qaut, Pautgen, Qautgen ); 
    imQact := List( rmgi[1], r -> ImageElm( ahom, ImageElm( Pact, r ) ) ); 
    Qact := GroupHomomorphismByImages( Qrng, Qaut, rmgi[2], imQact ); 
    QM := PreXModByBoundaryAndAction( Qbdy, Qact );
    iso := PreXModMorphismByGroupHomomorphisms( PM, QM, siso, riso ); 
    SetIsInjective( iso, true );
    SetIsSurjective( iso, true );
    SetRange( iso, QM );
    if ( HasIsXMod( PM ) and IsXMod( PM ) ) then 
        SetIsXMod( QM, true );
        SetIsXModMorphism( iso, true ); 
    fi;
    return iso;
end ); 

#############################################################################
##
#M  IsomorphismPerm2DimensionalGroup . . isomorphic perm pre-xmod & pre-cat1
##
InstallMethod( IsomorphismPerm2DimensionalGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local shom, ishom, rhom, irhom, Psrc, Psgen, Qsrc, Qsgen, 
          Prng, Prgen, Qrng, Qrgen, QM, iso;

    if IsPermPreXMod( PM ) then
        return IdentityMapping( PM );
    fi;
    Psrc := Source( PM );
    if IsPermGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        shom := IsomorphismPermGroup( Psrc );
        Qsrc := Image( shom ); 
        shom := shom * SmallerDegreePermutationRepresentation( Qsrc );
        Qsrc := ImagesSource( shom ); 
        Qsgen := SmallGeneratingSet( Qsrc ); 
        ishom := InverseGeneralMapping( shom ); 
        Psgen := List( Qsgen, g -> ImageElm( ishom, g ) ); 
        if HasName( Psrc ) then 
            SetName( Qsrc, Concatenation( "P", Name( Psrc ) ) ); 
        fi;
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PM );
    if IsPermGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        rhom := IsomorphismPermGroup( Prng );
        Qrng := Image( rhom ); 
        rhom := rhom * SmallerDegreePermutationRepresentation( Qrng );
        Qrng := ImagesSource( rhom );
        Qrgen := SmallGeneratingSet ( Qrng ); 
        irhom := InverseGeneralMapping( rhom ); 
        Prgen := List( Qrgen, g -> ImageElm( irhom, g ) ); 
        if HasName( Prng ) then 
            SetName( Qrng, Concatenation( "P", Name( Prng ) ) ); 
        fi;
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := Range( iso );
    if HasName( PM ) then
        SetName( QM, Concatenation( "P", Name( PM ) ) );
    fi;
    return iso;
end );

InstallMethod( IsomorphismPerm2DimensionalGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local shom, rhom, ishom, irhom, Psrc, Psgen, Qsrc, Qsgen, 
          Prng, Prgen, Qrng, Qrgen, QCG, iso, mgi;

    if IsPermPreCat1Group( PCG ) then
        return IdentityMapping( PCG );
    fi;
    Psrc := Source( PCG );
    if IsPermGroup( Psrc ) then
        shom := IdentityMapping( Psrc );
    else
        shom := IsomorphismPermGroup( Psrc );
        Qsrc := Image( shom ); 
        shom := shom * SmallerDegreePermutationRepresentation( Qsrc );
        Qsrc := ImagesSource( shom );
        Qsgen := SmallGeneratingSet( Qsrc ); 
        ishom := InverseGeneralMapping( shom ); 
        Psgen := List( Qsgen, g -> ImageElm( ishom, g ) ); 
        shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    fi;
    Prng := Range( PCG );
    if IsPermGroup( Prng ) then
        rhom := IdentityMapping( Prng );
    else
        Prgen := GeneratorsOfGroup( Prng ); 
        if IsPreCat1GroupWithIdentityEmbedding( PCG ) then 
            rhom := RestrictedMapping( shom, Prng ); 
        else 
            rhom := IsomorphismPermGroup( Prng );
            Qrng := Image( rhom ); 
            rhom := rhom * SmallerDegreePermutationRepresentation( Qrng );
        fi;
        Qrng := ImagesSource( rhom );
        Qrgen := SmallGeneratingSet( Qrng );
        mgi := MappingGeneratorsImages( rhom ); 
        irhom := GroupHomomorphismByImages( Qrng, Prng, mgi[2], mgi[1] );
        ## irhom := InverseGeneralMapping( rhom ); 
        Prgen := List( Qrgen, g -> ImageElm( irhom, g ) ); 
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := Range( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Pc", Name( PCG ) ) );
    fi;
    iso := PreCat1GroupMorphism( PCG, QCG, shom, rhom ); 
    return iso;
end );

#############################################################################
##
#M  RegularActionHomomorphism2DimensionalGroup . isomorphic pre-xmod & -cat1
##
InstallMethod( RegularActionHomomorphism2DimensionalGroup,
     "generic method for pre-crossed modules", true, [ IsPreXMod ], 0,
function( PM )

    local shom, ishom, rhom, irhom, Psrc, Psgen, Qsrc, Qsgen, 
          Prng, Prgen, Qrng, Qrgen, QM, iso;

    Psrc := Source( PM );
    shom := RegularActionHomomorphism( Psrc );
    Qsrc := Image( shom ); 
    Qsrc := ImagesSource( shom ); 
    Qsgen := SmallGeneratingSet( Qsrc ); 
    ishom := InverseGeneralMapping( shom ); 
    Psgen := List( Qsgen, g -> ImageElm( ishom, g ) ); 
    if HasName( Psrc ) then 
        SetName( Qsrc, Concatenation( "Reg", Name( Psrc ) ) ); 
    fi;
    shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    Prng := Range( PM );
    rhom := RegularActionHomomorphism( Prng );
    Qrng := Image( rhom ); 
    Qrng := ImagesSource( rhom );
    Qrgen := SmallGeneratingSet ( Qrng ); 
    irhom := InverseGeneralMapping( rhom ); 
    Prgen := List( Qrgen, g -> ImageElm( irhom, g ) ); 
    if HasName( Prng ) then 
        SetName( Qrng, Concatenation( "Reg", Name( Prng ) ) ); 
    fi;
    rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := Range( iso );
    if HasName( PM ) then
        SetName( QM, Concatenation( "Reg", Name( PM ) ) );
    fi;
    return iso;
end );

InstallMethod( RegularActionHomomorphism2DimensionalGroup,
     "generic method for pre-cat1-groups", true, [ IsPreCat1Group ], 0,
function( PCG )

    local shom, rhom, ishom, irhom, Psrc, Psgen, Qsrc, Qsgen, 
          Prng, Prgen, Qrng, Qrgen, QCG, iso, mgi;

    Psrc := Source( PCG );
    shom := RegularActionHomomorphism( Psrc );
    Qsrc := Image( shom ); 
    shom := shom * SmallerDegreePermutationRepresentation( Qsrc );
    Qsrc := ImagesSource( shom );
    Qsgen := SmallGeneratingSet( Qsrc ); 
    ishom := InverseGeneralMapping( shom ); 
    Psgen := List( Qsgen, g -> ImageElm( ishom, g ) ); 
    shom := GroupHomomorphismByImages( Psrc, Qsrc, Psgen, Qsgen );
    Prng := Range( PCG );
    Prgen := GeneratorsOfGroup( Prng ); 
    rhom := RegularActionHomomorphism( Prng );
    Qrng := ImagesSource( rhom );
    Qrgen := SmallGeneratingSet( Qrng );
    mgi := MappingGeneratorsImages( rhom ); 
    irhom := GroupHomomorphismByImages( Qrng, Prng, mgi[2], mgi[1] );
    Prgen := List( Qrgen, g -> ImageElm( irhom, g ) ); 
    rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := Range( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Reg", Name( PCG ) ) );
    fi;
    iso := PreCat1GroupMorphism( PCG, QCG, shom, rhom ); 
    return iso;
end );

#############################################################################
##
#M  IsomorphismPc2DimensionalGroup . . . . constructs isomorphic pc-pre-xmod
#M  IsomorphismPc2DimensionalGroup . . . . constructs isomorphic pc-pre-cat1
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
        Qsrc := ImagesSource( shom );
        Qsgen := List( Psgen, s -> ImageElm( shom, s ) );
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
        Qrng := ImagesSource( rhom );
        Qrgen := List( Prgen, r -> ImageElm( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := Range( iso ); 
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
        Qsrc := ImagesSource( shom );
        Qsgen := List( Psgen, s -> ImageElm( shom, s ) );
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
        Qrng := ImagesSource( rhom );
        Qrgen := List( Prgen, r -> ImageElm( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := Range( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Pc", Name( PCG ) ) );
    fi;
    iso := PreCat1GroupMorphism( PCG, QCG, shom, rhom );
    return iso;
end );

#############################################################################
##
#M  IsomorphismFp2DimensionalGroup . . . . constructs isomorphic fp-pre-xmod
#M  IsomorphismFp2DimensionalGroup . . . . constructs isomorphic fp-pre-cat1
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
        Qsrc := ImagesSource( shom );
        Qsgen := List( Psgen, s -> ImageElm( shom, s ) );
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
        Qrng := ImagesSource( rhom );
        Qrgen := List( Prgen, r -> ImageElm( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PM, [ shom, rhom ] ); 
    QM := Range( iso ); 
    if HasName( PM ) then
        SetName( QM, Concatenation( "Pc", Name( PM ) ) );
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
        Qsrc := ImagesSource( shom );
        Qsgen := List( Psgen, s -> ImageElm( shom, s ) );
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
        Qrng := ImagesSource( rhom );
        Qrgen := List( Prgen, r -> ImageElm( rhom, r ) );
        rhom := GroupHomomorphismByImages( Prng, Qrng, Prgen, Qrgen );
    fi;
    iso := IsomorphismByIsomorphisms( PCG, [ shom, rhom ] ); 
    QCG := Range( iso ); 
    if HasName( PCG ) then
        SetName( QCG, Concatenation( "Fp", Name( PCG ) ) );
    fi;
    iso := PreCat1GroupMorphism( PCG, QCG, shom, rhom ); 
    return iso;
end );

#############################################################################
##
#M  IsomorphismPreCat1Groups . isomorphism between a pair of pre-cat1-groups 
#M  IsomorphismCat1Groups  . . isomorphism between a pair of cat1-groups 
##
InstallMethod( IsomorphismPreCat1Groups, 
    "generic method for 2 pre-cat1-groups", true, 
    [ IsPreCat1Group, IsPreCat1Group ], 0, 
function( C1G1, C1G2 )

    local sym1, sym2, ok1, ok2, C1, C2, end1, end2, iend2, mor, 
          G1, G2, sphi, isphi, R1, R2, rphi, mgirphi, R3, 
          t1, h1, t2, h2, t3, h3, C3, phi, autG2, iterG2, 
          salpha, ralpha, mgira, isalpha, R4, t4, h4, C4, smor, rmor;

    if not ( Size2d( C1G1 ) = Size2d( C1G2 ) ) then 
        return fail; 
    fi; 
    ok1 := IsPreCat1GroupWithIdentityEmbedding( C1G1 ); 
    ok2 := IsPreCat1GroupWithIdentityEmbedding( C1G2 ); 
    C1 := C1G1; 
    C2 := C1G2; 
    if not ( ok1 and ok2 ) then 
        if not ok1 then 
            C1 := IsomorphicPreCat1GroupWithIdentityEmbedding( C1G1 ); 
            end1 := IsomorphismToPreCat1GroupWithIdentityEmbedding( C1G1 ); 
        else 
            C1 := C1G1; 
            end1 := IdentityMapping( C1 );
        fi; 
        if not ok2 then 
            C2 := IsomorphicPreCat1GroupWithIdentityEmbedding( C1G2 ); 
            end2 := IsomorphismToPreCat1GroupWithIdentityEmbedding( C1G2 ); 
            iend2 := InverseGeneralMapping( end2 ); 
        else 
            C2 := C1G2; 
            end2 := IdentityMapping( C2 ); 
            iend2 := end2; 
        fi; 
        mor := IsomorphismPreCat1Groups( C1, C2 ); 
        if ( mor = fail ) then 
            return fail; 
        fi; 
        return end1 * mor * iend2;
    fi; 
    sym1 := IsSymmetric2DimensionalGroup( C1 ); 
    sym2 := IsSymmetric2DimensionalGroup( C2 ); 
    if ( sym1 <> sym2 ) then 
        Info( InfoXMod, 2, "sym1 <> sym2" ); 
        return fail; 
    fi; 
    G1 := Source( C1 ); 
    G2 := Source( C2 ); 
    if not ( G1 = G2 ) then
        sphi := IsomorphismGroups( G1, G2 ); 
        if ( sphi = fail ) then 
            Info( InfoXMod, 2, "G1, G2 not isomorphic" ); 
            return fail; 
        fi; 
        isphi := InverseGeneralMapping( sphi ); 
    else
        sphi := IdentityMapping( G1 ); 
        isphi := IdentityMapping( G1 ); 
    fi; 
    R1 := Range( C1 );
    R2 := Range( C2 ); 
    rphi := RestrictedMapping( sphi, R1 ); 
    mgirphi := MappingGeneratorsImages( rphi ); 
    R3 := Image( rphi ); 
    rphi := GroupHomomorphismByImages( R1, R3, mgirphi[1], mgirphi[2] ); 
    t1 := TailMap( C1 );
    t2 := TailMap( C2 );
    h1 := HeadMap( C1 );
    h2 := HeadMap( C2 ); 
    t3 := isphi * t1 * rphi; 
    h3 := isphi * h1 * rphi; 
    if not ( IsGroupHomomorphism( t3 ) and 
             IsGroupHomomorphism( h3 ) ) then 
        Error( "t3,h3 fail to be group homomorphisms" ); 
    fi; 
    C3 := PreCat1GroupWithIdentityEmbedding( t3, h3 ); 
    phi := PreCat1GroupMorphism( C1, C3, sphi, rphi ); 
    autG2 := AutomorphismGroup( G2 ); 
    iterG2 := Iterator( autG2 ); 
    while not IsDoneIterator( iterG2 ) do 
        salpha := NextIterator( iterG2 ); 
        ralpha := RestrictedMapping( salpha, R3 ); 
        R4 := Image( ralpha ); 
        mgira := MappingGeneratorsImages( ralpha ); 
        ralpha := GroupHomomorphismByImages( R3, R4, mgira[1], mgira[2] );
        isalpha := InverseGeneralMapping( salpha ); 
        t4 := isalpha * t3 * ralpha; 
        h4 := isalpha * h3 * ralpha; 
        C4 := PreCat1GroupWithIdentityEmbedding( t4, h4 ); 
        if ( C4 = C2 ) then  
            smor := sphi * salpha;
            rmor := rphi * ralpha; 
            mor := PreCat1GroupMorphism( C1, C2, smor, rmor ); 
            return mor; 
        fi;
    od; 
    Info( InfoXMod, 2, "tried all of autG2 without success" ); 
    return fail;
end );          

InstallMethod( IsomorphismCat1Groups, "generic method for 2 cat1-groups", 
    true, [ IsCat1Group, IsCat1Group ], 0,
function( C1, C2 )

    local iso, ok; 

    iso := IsomorphismPreCat1Groups( C1, C2 ); 
    if ( iso = fail ) then 
        return fail; 
    fi;
    ok := IsCat1GroupMorphism( iso );
    if not ok then 
        Error( "found a pre-cat1 morphism which is not a cat1-morphism" ); 
    fi; 
    return iso;
end );

#############################################################################
##
#M  Name                                                      for a pre-xmod
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

#############################################################################
##
#M  PreXModMorphismByGroupHomomorphisms( <P>, <Q>, <hsrc>, <hrng> ) 
##  . . . make a prexmod morphism
##
InstallMethod( PreXModMorphismByGroupHomomorphisms,
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

#############################################################################
##
#M  PreCat1GroupMorphismByGroupHomomorphisms( <P>, <Q>, <hsrc>, <hrng> ) 
##
InstallMethod( PreCat1GroupMorphismByGroupHomomorphisms,
    "for pre-cat1-group, pre-cat1-group, homomorphism, homomorphism,", true,
    [IsPreCat1Group,IsPreCat1Group,IsGroupHomomorphism,IsGroupHomomorphism], 
    0,
function( src, rng, srchom, rnghom )

    local filter, fam, mor, ok, nsrc, nrng, name;

    mor := Make2DimensionalGroupMorphism( [ src, rng, srchom, rnghom ] ); 
    if not ( ( mor <> fail ) and IsPreCat1GroupMorphism( mor ) ) then
        Info( InfoXMod, 2, "not a morphism of pre-cat1 groups\n" );
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
    ok := IsCat1GroupMorphism( mor );
    return mor;
end );

#############################################################################
##
#M  Cat1GroupMorphismByGroupHomomorphisms( <Cs>, <Cr>, <hsrc>, <hrng> ) 
##      . . . make cat1 morphism
##
InstallMethod( Cat1GroupMorphismByGroupHomomorphisms, "for 2 cat1s and 2 homomorphisms", true,
    [ IsCat1Group, IsCat1Group, IsGroupHomomorphism, IsGroupHomomorphism ], 
    0,
function( src, rng, srchom, rnghom )

    local mor, ok;

    mor := PreCat1GroupMorphismByGroupHomomorphisms( src, rng, srchom, rnghom );
    ok := not ( mor = fail ) and IsCat1GroupMorphism( mor );
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
    [ IsPreCat1GroupMorphism ], 0, 
function( mor ) 
    return( STRINGIFY( "[", String( Source(mor) ), " => ", 
                            String( Range(mor) ), "]" ) ); 
end );

InstallMethod( ViewString, "method for a morphism of pre-cat1 groups", true, 
    [ IsPreCat1GroupMorphism ], 0, String ); 

InstallMethod( PrintString, "fmethod for a morphism of pre-cat1 groups", true, 
    [ IsPreCat1GroupMorphism ], 0, String ); 

InstallMethod( ViewObj, "method for a morphism of pre-cat1 groups", true,
    [ IsPreCat1GroupMorphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

InstallMethod( PrintObj, "method for a morphism of pre-cat1 groups", true,
    [ IsPreCat1GroupMorphism ], 0,
function( mor )
    if HasName( mor ) then
        Print( Name( mor ), "\n" );
    else
        Print( "[", Source( mor ), " => ", Range( mor ), "]" );
    fi;
end );

#############################################################################
##
#M  TransposeIsomorphism                                 for a pre-cat1-group
##
InstallMethod( TransposeIsomorphism, "method for a cat1-group", true,
    [ IsPreCat1Group ], 0,
function( C1G )

    local rev, shom, rhom, src, gensrc, t, h, e, im;

    rev := TransposeCat1Group( C1G );
    src := Source( C1G );
    gensrc := GeneratorsOfGroup( src );
    t := TailMap( C1G );
    h := HeadMap( C1G );
    e := RangeEmbedding( C1G );
    im := List( gensrc, 
        g -> ImageElm(e,ImageElm(h,g)) * g^-1 * ImageElm(e,ImageElm(t,g)) );
    shom := GroupHomomorphismByImages( src, src, gensrc, im );
    rhom := IdentityMapping( Range( C1G ) );
    return PreCat1GroupMorphismByGroupHomomorphisms( C1G, rev, shom, rhom );
end );

#############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsInjective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsInjective( SourceHom( map ) )
             and IsInjective( RangeHom( map ) ) )  );

#############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsSurjective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsSurjective( SourceHom( map ) )
             and IsSurjective( RangeHom( map ) ) )  );

#############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . .  for a 2Dimensional-mapping
##
InstallOtherMethod( IsSingleValued,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsSingleValued( SourceHom( map ) )
             and IsSingleValued( RangeHom( map ) ) )  );

#############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsTotal,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsTotal( SourceHom( map ) )
             and IsTotal( RangeHom( map ) ) )  );

#############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . . for a 2Dimensional-mapping
##
InstallOtherMethod( IsBijective,
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> (     IsBijective( SourceHom( map ) )
             and IsBijective( RangeHom( map ) ) )  );

#############################################################################
##
#M  IsEndomorphism2DimensionalDomain( map ) . .  for a 2Dimensional-mapping
#?  temporary fix 08/01/04  ---  need to check correctness
#M  IsAutomorphism2DimensionalDomain( map ) . .  for a 2Dimensional-mapping
##
InstallMethod( IsEndomorphism2DimensionalDomain, 
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map -> IsEndoMapping( SourceHom( map ) ) and 
           IsEndoMapping( RangeHom( map ) ) );

InstallMethod( IsAutomorphism2DimensionalDomain, 
    "method for a 2d-mapping", true, [ Is2DimensionalMapping ], 0,
    map ->  IsEndomorphism2DimensionalDomain( map ) and IsBijective( map ) );

#############################################################################
##
#M  IsSourceMorphism( mor ) . . . . . . . . . . . . . . for an xmod morphism
##
InstallMethod( IsSourceMorphism,
    "method for a morphism of crossed modules", true,
    [ IsXModMorphism ], 0,
function( mor )

    local Srng, Rrng;

    Srng := Range( Source( mor ) );
    Rrng := Range( Range( mor ) );
    return ( ( Srng = Rrng ) and
             ( RangeHom( mor ) = IdentityMapping( Srng ) ) );
end );

#############################################################################
##
#M  Kernel . . . . . of morphisms of pre-crossed modules and pre-cat1-groups
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

#############################################################################
##
#M  PreXModBySourceHom . . . top PreXMod from a morphism of crossed P-modules
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
        z := ImageElm( bdy2, y );
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
#M  XModMorphismOfCat1GroupMorphism
#M  Cat1GroupMorphismOfXModMorphism
##
InstallMethod( XModMorphismOfCat1GroupMorphism, "for a cat1-group morphism",
    true, [ IsCat1GroupMorphism ], 0,
function( phi )

    local C1, C2, X1, X2, S1, R1, S2, R2, t1, K1, genS1, gamma, rho, 
          imsigma, sigma, mor;

    C1 := Source( phi );
    C2 := Range( phi );
    X1 := XModOfCat1Group( C1 );
    X2 := XModOfCat1Group( C2 );
    S1 := Source( X1 );
    R1 := Range( X1 );
    S2 := Source( X2 );
    R2 := Range( X2 );
    t1 := TailMap( C1 ); 
    K1 := Kernel( t1 ); 
    if not ( K1 = S1 ) then 
        Error( "expercting Kernel(t1) = S1" ); 
    fi; 
    genS1 := GeneratorsOfGroup( S1 );
    gamma := SourceHom( phi );
    rho := RangeHom( phi );
    imsigma := List( genS1, s -> ImageElm( gamma, s ) ); 
    sigma := GroupHomomorphismByImages( S1, S2, genS1, imsigma ); 
    mor := XModMorphismByGroupHomomorphisms( X1, X2, sigma, rho );
    SetXModMorphismOfCat1GroupMorphism( phi, mor );
    SetCat1GroupMorphismOfXModMorphism( mor, phi );
    return mor;
end );

InstallMethod( Cat1GroupMorphismOfXModMorphism, "for an xmod morphism", 
    true, [ IsXModMorphism ], 0,
function( mor )

    local X1, X2, rec1, rec2, C1, C2, G1, G2, smor, rmor, semb1, remb1, 
          semb2, remb2, isemb1, iremb1, geneS1, geneR1, genG1, imeS1, imeR1, 
          imsphi, sphi, phi;

    X1 := Source( mor );
    X2 := Range( mor );
    rec1 := PreCat1GroupRecordOfPreXMod( X1 ); 
    if ( X1 = X2 ) then 
        rec2 := rec1; 
    else 
        rec2 := PreCat1GroupRecordOfPreXMod( X2 ); 
    fi;
    C1 := rec1.precat1; 
    C2 := rec2.precat1; 
    G1 := Source( C1 ); 
    G2 := Source( C2 ); 
    smor := SourceHom( mor );
    rmor := RangeHom( mor ); 
    semb1 := rec1.xmodSourceEmbeddingIsomorphism; 
    remb1 := rec1.xmodRangeEmbeddingIsomorphism; 
    semb2 := rec2.xmodSourceEmbeddingIsomorphism; 
    remb2 := rec2.xmodRangeEmbeddingIsomorphism; 
    isemb1 := RestrictedInverseGeneralMapping( semb1 );
    iremb1 := RestrictedInverseGeneralMapping( remb1 );
    geneS1 := GeneratorsOfGroup( rec1.xmodSourceEmbedding ); 
    geneR1 := GeneratorsOfGroup( rec1.xmodRangeEmbedding ); 
    genG1 := Concatenation( geneR1, geneS1 ); 
    imeS1 := List( geneS1, s -> ImageElm( semb2, 
                                ImageElm( smor, 
                                ImageElm( isemb1, s ) ) ) ); 
    imeR1 := List( geneR1, r -> ImageElm( remb2, 
                                ImageElm( rmor, 
                                ImageElm( iremb1, r ) ) ) ); 
    imsphi := Concatenation( imeR1, imeS1 );
    sphi := GroupHomomorphismByImages( G1, G2, genG1, imsphi ); 
    phi := Cat1GroupMorphismByGroupHomomorphisms( C1, C2, sphi, rmor );
    SetCat1GroupMorphismOfXModMorphism( mor, phi );
    SetXModMorphismOfCat1GroupMorphism( phi, mor );
    return phi; 
end );

#############################################################################
##
#F  SmallerDegreePermutationRepresentation2DimensionalGroup( <obj> )
##
InstallMethod( SmallerDegreePermutationRepresentation2DimensionalGroup, 
    "method for a pre-xmod or a pre-cat1-group", true,
    [ IsPerm2DimensionalGroup ], 0,
function( obj )

    local src, rng, sigma, rho, mor; 

    if IsPerm2DimensionalGroup( obj ) then
        src := Source( obj ); 
        rng := Range( obj ); 
        sigma := SmallerDegreePermutationRepresentation( src );
        rho := SmallerDegreePermutationRepresentation( rng );
        mor := IsomorphismByIsomorphisms( obj, [ sigma, rho ] );
        return mor;
    else
        return fail;
    fi;
end ); 

#############################################################################
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

#############################################################################
##
#M  Order . . . . . . . . . . . . . . . . . . . . for a 2Dimensional-mapping
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
#M  ImagesSource( <mor> ) . . . . . . . for pre-xmod and pre-cat1 morphisms
##
InstallOtherMethod( ImagesSource, 
    "image for a pre-xmod or pre-cat1 morphism", true, 
    [ Is2DimensionalMapping ], 0, 
function( mor )
    
    local Shom, Rhom, imS, imR, sub;

    Shom := SourceHom( mor );
    Rhom := RangeHom( mor );
    imS := ImagesSource( Shom );
    imR := ImagesSource( Rhom );
    sub := Sub2DimensionalGroup( Range(mor), imS, imR );
    return sub;
end );

#############################################################################
##
#M  AllCat1GroupMorphisms  . . . . morphisms from one cat1-group to another 
##
InstallMethod( AllCat1GroupMorphisms, "for two cat1-groups", true, 
    [ IsCat1Group, IsCat1Group ], 0,
function( C1G1, C1G2 ) 

    local G1, R1, G2, R2, homG, homR, mors, gamma, rho, mor; 

    G1 := Source( C1G1 );
    R1 := Range( C1G1 );
    G2 := Source( C1G2 );
    R2 := Range( C1G2 );
    homG := AllHomomorphisms( G1, G2 );    
    homR := AllHomomorphisms( R1, R2 );    
    mors := [ ];    
    for gamma in homG do
        for rho in homR do
            mor := PreCat1GroupMorphismByGroupHomomorphisms( 
                        C1G1, C1G2, gamma, rho );
            if ( not( mor = fail ) and IsCat1GroupMorphism( mor ) ) then
                Add( mors, mor );
            fi;
        od;
    od;    
    return mors; 
end );

#############################################################################
##
#M  SubQuasiIsomorphism( <cat1>, <print> )
##  
InstallMethod( SubQuasiIsomorphism, "for a cat1-group", true, 
    [ IsCat1Group, IsBool ], 0, 
function( C1, show ) 

    local tot, ids, maps, G1, R1, X1, S1, bdy1, LS, CLS, LR, CLR, K1, idK1, 
          KS1, KR1, nat1, J1, CP, P, genP, CQ, Q, genQ, X2, bdy2, K2, idK2, 
          KP, KQ, J2, isoK, C2, incX, incP, incQ, alpha, beta, genKQ, imKQ, 
          imgamma, gamma, nat2, preKQ, incC, G2, gen2, idgp, idcat; 

    tot := 0; 
    ids := [ ];
    maps := [ ]; 
    G1 := Source( C1 ); 
    R1 := Range( C1 ); 
    X1 := XModOfCat1Group( C1 ); 
    S1 := Source( X1 ); 
    bdy1 := Boundary( X1 ); 
    K1 := KernelCokernelXMod( X1 ); 
    KS1 := Source( K1 ); 
    KR1 := Range( K1 ); 
    J1 := ImagesSource( bdy1 ); 
    nat1 := NaturalHomomorphismByNormalSubgroup( R1, J1 ); 
    KR1 := Image( nat1, R1 ); 
    LS := LatticeSubgroups( S1 ); 
    CLS := ConjugacyClassesSubgroups( LS );
    LR := LatticeSubgroups( R1 ); 
    CLR := ConjugacyClassesSubgroups( LR );
    idK1 := IdGroup( K1 ); 
    if show then 
        Print( "kernel-cokernel: ", StructureDescription( K1 ), "\n" );
    fi; 
    for CP in CLS do 
       for P in CP do 
          for CQ in CLR do 
             for Q in CQ do 
                if not ( ( P = S1 ) and ( Q = R1 ) ) then 
                   X2 := SubXMod( X1, P, Q ); 
                   if ( X2 <> fail ) then 
                      bdy2 := Boundary( X2 ); 
                      K2 := KernelCokernelXMod( X2 );             
                      idK2 := IdGroup( K2 ); 
                      if ( idK2 = idK1 ) then 
                         KP := Source( K2 ); 
                         KQ := Range( K2 ); 
                         J2 := ImagesSource( bdy2 ); 
                         genP := GeneratorsOfGroup( P ); 
                         genQ := GeneratorsOfGroup( Q ); 
                         incX := InclusionMorphism2DimensionalDomains(X1,X2); 
                         incP := SourceHom( incX ); 
                         incQ := RangeHom( incX ); 
                         alpha := InclusionMappingGroups( KS1, KP ); 
                         beta := InclusionMappingGroups( J1, J2 ); 
                         nat2 := NaturalHomomorphismByNormalSubgroup(Q,J2); 
                         genKQ := GeneratorsOfGroup( KQ ); 
                         preKQ := List( genKQ, 
                                  g -> PreImagesRepresentativeNC(nat2,g) ); 
                         imgamma := List( preKQ, 
                                    g -> Image( nat1, Image( incQ, g ) ) ); 
                         gamma := GroupHomomorphismByImages( 
                                       KQ, KR1, genKQ, imgamma ); 
                         isoK := PreXModMorphismByGroupHomomorphisms( 
                                       K2, K1, alpha, gamma ); 
                         if IsBijective( isoK ) then 
                            if show then 
                               Print( "(sub:) sub-xmod: ", 
                                      StructureDescription(X2), "\n" ); 
                            fi; 
                            C2 := Cat1GroupOfXMod( X2 ); 
                            incC := Cat1GroupMorphismOfXModMorphism( incX );                       
                            G2 := Source( C2 ); 
                            gen2 := GeneratorsOfGroup( G2 ); 
                            idgp := IdGroup( C2 ); 
                            idcat := IdCat1Group( C2 ); 
                            if show then 
                               Print( idgp, ", ", idcat, ", ", 
                                      StructureDescription( C2 ), "\n" ); 
                               Print( "bdy2: ", 
                                      MappingGeneratorsImages(bdy2), "\n");
                            fi; 
                            tot := tot + 1;
                            Add( ids, idcat ); 
                            Add( maps, incC ); 
                         fi; 
                      fi; 
                   fi;
                fi;
             od; 
          od; 
       od; 
    od; 
    return ids; 
end ); 

#############################################################################
##
#M  QuotientQuasiIsomorphism( <cat1>, <print> )
##  
InstallMethod( QuotientQuasiIsomorphism, "for a cat1-group", true, 
    [ IsCat1Group, IsBool ], 0, 
function( C1, show ) 

    local tot, ids, G1, R1, X1, idX1, S1, bdy1, LS, LR, K1, KS1, KR1, 
          nat1, J1, idK1, P, genP, Q, genQ, X2, bdy2, natX, natP, natQ, 
          F2, FP, FQ, bdyF, K2, idK2, J2, KP, KQ, natKQ, JF, genKS1, 
          imKS1, alpha, natKR1, genKR1, preKR1, im1KR1, im2KR1, gamma, isoK, 
          C2, natC, G2, gen2, idgp, idcat, maps; 

    tot := 0; 
    ids := [ ];
    maps := [ ]; 
    G1 := Source( C1 ); 
    R1 := Range( C1 ); 
    X1 := XModOfCat1Group( C1 ); 
    idX1 := IdentityMapping( X1 ); 
    S1 := Source( X1 ); 
    bdy1 := Boundary( X1 ); 
    K1 := KernelCokernelXMod( X1 ); 
    KS1 := Source( K1 ); 
    KR1 := Range( K1 ); 
    J1 := ImagesSource( bdy1 ); 
    nat1 := NaturalHomomorphismByNormalSubgroup( R1, J1 ); 
    LS := NormalSubgroups( S1 ); 
    LR := NormalSubgroups( R1 ); 
    idK1 := IdGroup( K1 ); 
    if show then 
        Print( "kernel-cokernel: ", StructureDescription( K1 ), "\n" );
    fi; 
    for P in LS do 
       for Q in LR do 
          if not ( ( Size(P) = 1 ) and ( Size(Q) = 1 ) ) then 
             X2 := SubXMod( X1, P, Q ); 
             if ( X2 <> fail ) and IsNormal( X1, X2 ) then 
                bdy2 := Boundary( X2 ); 
                genP := GeneratorsOfGroup( P ); 
                genQ := GeneratorsOfGroup( Q ); 
                natX := NaturalMorphismByNormalSubPreXMod( X1, X2 ); 
                natP := SourceHom( natX ); 
                natQ := RangeHom( natX ); 
                F2 := Range( natX ); 
                FP := Source( F2 ); 
                FQ := Range( F2 ); 
                bdyF := Boundary( F2 ); 
                K2 := KernelCokernelXMod( F2 );             
                idK2 := IdGroup( K2 ); 
                J2 := ImagesSource( bdy2 ); 
                if ( idK2 = idK1 ) then 
                   KP := Source( K2 ); 
                   KQ := Range( K2 ); 
                   JF := ImagesSource( bdyF ); 
                   natKQ := NaturalHomomorphismByNormalSubgroup( FQ, JF );
                   genKS1 := GeneratorsOfGroup( KS1 ); 
                   imKS1 := List( genKS1, g -> ImageElm( natP, g ) ); 
                   alpha := GroupHomomorphismByImages( 
                                KS1, KP, genKS1, imKS1 ); 
                   natKR1 := NaturalHomomorphismByNormalSubgroup( R1, J1 );
                   genKR1 := GeneratorsOfGroup( KR1 );
                   preKR1 := List( genKR1, 
                               g -> PreImagesRepresentativeNC( natKR1, g ) ); 
                   im1KR1 := List( preKR1, g -> ImageElm( natQ, g ) ); 
                   natKQ := NaturalHomomorphismByNormalSubgroup( FQ, JF ); 
                   im2KR1 := List( im1KR1, g -> ImageElm( natKQ, g ) ); 
                   gamma := GroupHomomorphismByImages( 
                               KR1, KQ, genKR1, im2KR1 ); 
                   isoK := PreXModMorphismByGroupHomomorphisms( 
                               K1, K2, alpha, gamma ); 
                   if IsBijective( isoK ) then 
                      if show then 
                          Print( "(quo:) normal subxmod: ", 
                                 StructureDescription( X2 ), "\n" ); 
                      fi; 
                      natC := Cat1GroupMorphismOfXModMorphism( natX ); 
                      C2 := Range( natC ); 
                      G2 := Source( C2 ); 
                      gen2 := GeneratorsOfGroup( G2 ); 
                      idgp := IdGroup( C2 ); 
                      idcat := IdCat1Group( C2 ); 
                      if idcat = "not known" then
                         Display( XModOfCat1Group(C2) ); 
                      fi; 
                      if show then 
                         Print( idgp, ", ", idcat, ", ", 
                                StructureDescription(C2), "\n" ); 
                         Print( "bdy2: ", 
                                MappingGeneratorsImages(bdy2), "\n" );
                      fi; 
                      tot := tot + 1;
                      Add( ids, idcat ); 
                      Add( maps, natC ); 
                   fi;
                fi; 
             fi; 
          fi; 
       od; 
    od; 
    return ids; 
end ); 

#############################################################################
##
#M  QuasiIsomorphism( <cat1>, <id>, <print> )
##  
InstallMethod( QuasiIsomorphism, "for a cat1-group", true, 
    [ IsCat1Group, IsList, IsBool ], 0, 
function( C, id, show ) 

    local ids, maps, K, smor, qmor, D, idQ; 

    ## deal with trivial cases first 
    ids := [ id ]; 
    K := KernelCokernelXMod( C ); 
    if Product( Size2d(K) ) = ids[1][1] then 
        return [ IdentityMapping( C ) ]; 
    fi; 
    qmor := QuotientQuasiIsomorphism( C, show ); 
    smor := SubQuasiIsomorphism( C, show ); 
    ## maps := Concatenation( qmor[1], smor[1] ); 
    ids := Set( Concatenation( ids, qmor, smor ) ); 
    ## Print( "combined ids", ids, "\n" );
    return ids; 
end );

InstallMethod( QuasiIsomorphism, "for a crossed module", true, 
    [ IsXMod, IsList, IsBool ], 0, 
function( X0, idX, show ) 

    local C, id, D;

    C := Cat1GroupOfXMod( X0 ); 
    id := IdCat1Group( C );
    D :=  QuasiIsomorphism( C, id, show );
    return XModOfCat1Group( D );
end );
