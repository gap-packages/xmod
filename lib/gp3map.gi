##############################################################################
##
#W  gp3map.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for 3Dimensional Mappings for 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  IsPreCrossedSquareMorphism      check the axioms for a pre-crossed square
##
InstallMethod( IsPreCrossedSquareMorphism,
    "generic method for pre-crossed module homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PS, QS, homs, upmor, ltmor, dnmor, rtmor, ok;

    PS := Source( mor );
    QS := Range( mor );
    homs := ListOfHomomorphisms( mor ); 
    if not ( IsPreCrossedSquare( PS ) and IsPreCrossedSquare( QS ) ) then
        return false;
    fi;
    ### (1) check that the morphisms commute
    upmor := Up2DimensionalMorphism( mor );
    ltmor := Left2DimensionalMorphism( mor );
    dnmor := Down2DimensionalMorphism( mor );
    rtmor := Right2DimensionalMorphism( mor );
    ok := ( ( SourceHom( upmor ) = SourceHom( ltmor ) ) and
            ( RangeHom( upmor ) = SourceHom( rtmor ) ) and
            ( RangeHom( ltmor ) = SourceHom( dnmor ) ) and
            ( RangeHom( rtmor ) = RangeHom( dnmor ) ) );
    if not ok then
        return false;
    fi;
    ### (2) check the remaining axioms
    Info( InfoXMod, 1, 
        "Warning: IsPreCrossedSquareMorphism not fully implemented!" );
    return true;
end );

#############################################################################
##
#M  IsCrossedSquareMorphism
##
InstallMethod( IsCrossedSquareMorphism, 
    "generic method for pre-crossed square morphisms", true, 
    [ IsPreCrossedSquareMorphism ], 0,
function( mor )
    return ( IsCrossedSquare( Source( mor ) ) 
             and IsCrossedSquare(  Range( mor ) ) );
end );

InstallMethod( IsCrossedSquareMorphism, "generic method for 3d-mappings", true,
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local ispre;

    ispre := IsPreCrossedSquareMorphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCrossedSquare( Source( mor ) ) 
                 and IsCrossedSquare(  Range( mor ) ) );
    fi;
end );

#############################################################################
##
#M  IsPreCat2GroupMorphism      check the axioms for a pre-cat2 
##
InstallMethod( IsPreCat2GroupMorphism,
    "generic method for pre-cat2 homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PC, QC, upmor, dnmor, ok, d1, d2, u1, u2, G1, G2, P1, P2, p1, q1, 
          comp1, G11, G12, P11, P12, p2, q2, comp2;

    if not ( IsPreCatnMorphism( mor ) and ( HigherDimension = 2 ) ) then
        return true;
    else
        return false;
    fi;
end );

#############################################################################
##
#M  IsCat2GroupMorphism
##
InstallMethod( IsCat2GroupMorphism, "generic method for cat2 morphisms", true, 
    [ IsPreCat2GroupMorphism ], 0,
function( mor )
    return ( IsCat2Group( Source( mor ) ) and IsCat2Group(  Range( mor ) ) );
end );

InstallMethod( IsCat2GroupMorphism, "generic method for 3d-mappings", true,
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local ispre;

    ispre := IsPreCat2GroupMorphism( mor );
    if not ispre then
        return false;
    else
        return ( IsCat2Group( Source(mor) ) and IsCat2Group( Range(mor) ) );
    fi;
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . . . . test if two morphisms of 3d-objects are equal
##
InstallMethod( \=,
    "generic method for two 3d-morphisms", IsIdenticalObj, 
    [ IsPreCrossedSquareMorphism, IsPreCrossedSquareMorphism ], 0,
function ( mor, phi )
    return ( ( Source( mor ) = Source( phi ) )
         and ( Range( mor ) = Range( phi ) )
         and ( Up2DimensionalMorphism( mor ) 
               = Up2DimensionalMorphism( phi ) )
         and ( Left2DimensionalMorphism( mor ) 
               = Left2DimensionalMorphism( phi ) )
         and ( Right2DimensionalMorphism( mor ) 
               = Right2DimensionalMorphism( phi ) )
         and ( Down2DimensionalMorphism( mor ) 
               = Down2DimensionalMorphism( phi ) ) );
end );

#############################################################################
##
#F  MappingGeneratorsImages( <map> ) . . . . . for a HigherDimensionalMapping
##
InstallOtherMethod( MappingGeneratorsImages, "for a HigherDimensionalMapping", 
    true, [ IsPreCrossedSquareMorphism ], 0,
function( map )
    return [ MappingGeneratorsImages( Up2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Left2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Right2DimensionalMorphism( map ) ), 
             MappingGeneratorsImages( Down2DimensionalMorphism( map ) ) ];
end );

#############################################################################
##
#M  Name                                              for a pre-CrossedSquare
##
InstallMethod( Name, "method for a 3d-mapping", true, 
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
#F  Display3DimensionalMorphism( <mor> ) . . . . display a 3d-group morphism 
##
InstallMethod( Display3DimensionalMorphism, "display a morphism of 3d-groups", 
    true, [ IsHigherDimensionalMapping ], 0,
function( mor )

    local dim, upmor, downmor, P, Q;

    P := Source( mor );
    Q := Range( mor );
    dim := HigherDimension( P ); 
    if not ( dim = 3 ) then 
        Error( "expecting a 3-dimensional group morphism\n" ); 
    fi;
    upmor := Up2DimensionalMorphism( mor );
    downmor := Down2DimensionalMorphism( mor );
    if ( HasIsPreCrossedSquareMorphism( mor ) and 
         IsPreCrossedSquareMorphism( mor ) ) then 
        if ( HasIsCrossedSquareMorphism( mor ) and 
            IsCrossedSquareMorphism( mor ) ) then
            Print( "Morphism of crossed squares :- \n" );
        else
            Print( "Morphism of pre-crossed squares :- \n" );
        fi; 
    else 
        if ( HasIsCat2GroupMorphism( mor ) and IsCat2GroupMorphism( mor ) ) then
            Print( "Morphism of cat2-groups :- \n" );
        else
            Print( "Morphism of pre-cat2-groups :- \n" );
        fi; 
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
        Print( ":     order = ", Order( mor ), "\n" );
    fi;
    Print( ":    up-left: ", MappingGeneratorsImages(SourceHom(upmor)),"\n");
    Print( ":   up-right: ", MappingGeneratorsImages(RangeHom(upmor)),"\n");
    Print( ":  down-left: ", MappingGeneratorsImages(SourceHom(downmor)),"\n");
    Print( ": down-right: ", MappingGeneratorsImages(RangeHom(downmor)),"\n");
end ); 

##############################################################################
##
#F  CrossedSquareMorphism( <src>, <rng>, <list> ) 
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( CrossedSquareMorphism, function( arg )

    local nargs, src, rng, list, f, ok, isxs, ishom;

    nargs := Length( arg );

    # two CrossedSquares and list of four xmod morphisms
    if ( nargs = 3 ) then 
        src := arg[1];
        rng := arg[2]; 
        list := arg[3]; 
        ok := true; 
        if IsCrossedSquare( src ) and IsCrossedSquare( rng ) then 
            isxs := true; 
        elif IsCat2Group( src ) and IsCat2Group( rng ) then 
            isxs := false; 
        else 
            ok := false; 
        fi;
        if  ok and IsList( list ) and ( Length( list ) = 4 ) then 
            f := list[1]; 
            if HasIsGroupHomomorphism(f) and IsGroupHomomorphism(f) then 
                if ForAll( list, m -> IsGroupHomomorphism(m) ) then 
                    ishom := true;
                else 
                    ok := false; 
                fi; 
            elif isxs and HasIsXModMorphism( f ) and IsXModMorphism( f ) then 
                if ForAll( list, m -> IsXModMorphism(m) ) then 
                    ishom := false;
                else 
                    ok := false; 
                fi;
            elif (not isxs) and HasIsCat1GroupMorphism( f ) 
                            and IsCat1GroupMorphism( f ) then 
                if ForAll( list, m -> IsCat1GroupMorphism(m) ) then 
                    ishom := false;
                else 
                    ok := false; 
                fi;
            else 
                ok := false; 
            fi;
        fi; 
        if ok then 
            if isxs and ishom then 
                return CrossedSquareMorphismByGroupHomomorphisms(src,rng,list); 
            elif isxs and not ishom then 
                return CrossedSquareMorphismByXModMorphisms(src,rng,list); 
            elif not isxs and ishom then 
                return Cat2GroupMorphismByGroupHomomorphisms(src,rng,list); 
            elif not isxs and not ishom then 
                return Cat2GroupMorphismByCat1GroupMorphisms(src,rng,list); 
            fi;
        fi;
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, 
        "usage: CrossedSquareMorphism( src, rng, list of maps );" );
    return fail;
end );

###############################################################################
##
#M  PreCrossedSquareMorphismByPreXModMorphisms( <src>, <rng>, <list of mors> ) 
##
InstallMethod( PreCrossedSquareMorphismByPreXModMorphisms,
    "for two pre-crossed squares and four pre-xmod morphisms,", true,
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ( Length( list ) = 4 ) and 
           ForAll( list, m -> IsPreXModMorphism(m) ) then 
        Error( "third argument should be a list of 4 pre-xmod-morphisms" ); 
    fi; 
    mor := MakeHigherDimensionalGroupMorphism( src, rng, list );
    if not IsPreCrossedSquareMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-crossed squares.\n" );
        return fail;
    fi;
    ok := IsCrossedSquareMorphism( mor );
    return mor;
end );

###############################################################################
##
#M  PreCrossedSquareMorphismByGroupHomomorphisms( <src>, <rng>, <list of homs> ) 
##
InstallMethod( PreCrossedSquareMorphismByGroupHomomorphisms,
    "for two pre-crossed squares and four group homomorphisms,", true,
    [ IsPreCrossedSquare, IsPreCrossedSquare, IsList ], 0,
function( xs1, xs2, list )

    local up1, lt1, dn1, rt1, up2, lt2, dn2, rt2, L1, M1, N1, P1, 
          L2, M2, N2, P2, l, m, n, p, upmor, ltmor, dnmor, rtmor, mor, ok;

    if not ( Length( list ) = 4 ) and 
           ForAll( list, m -> IsGroupHomomorphism(m) ) then 
        Error( "third argument should be a list of 4 group homomorphisms" ); 
    fi; 
    up1 := Up2DimensionalGroup( xs1 ); 
    lt1 := Left2DimensionalGroup( xs1 ); 
    dn1 := Down2DimensionalGroup( xs1 ); 
    rt1 := Right2DimensionalGroup( xs1 ); 
    up2 := Up2DimensionalGroup( xs2 ); 
    lt2 := Left2DimensionalGroup( xs2 ); 
    dn2 := Down2DimensionalGroup( xs2 ); 
    rt2 := Right2DimensionalGroup( xs2 ); 
    L1 := Source( up1 ); 
    M1 := Range( up1 );
    N1 := Source( dn1 );
    P1 := Range( dn1 );
    L2 := Source( up2 ); 
    M2 := Range( up2 );
    N2 := Source( dn2 );
    P2 := Range( dn2 );
    l := list[1];
    m := list[2];
    n := list[3]; 
    p := list[4];
    if not ( Source(l) = L1 ) and ( Range(l) = L2 ) then 
        Error( "l : L1 -> L2  fails" ); 
    fi; 
    if not ( Source(m) = M1 ) and ( Range(m) = M2 ) then 
        Error( "m : M1 -> M2  fails" ); 
    fi; 
    if not ( Source(n) = N1 ) and ( Range(n) = N2 ) then 
        Error( "n : N1 -> N2  fails" ); 
    fi; 
    if not ( Source(p) = P1 ) and ( Range(p) = P2 ) then 
        Error( "p : P1 -> P2  fails" ); 
    fi; 
    upmor := PreXModMorphismByGroupHomomorphisms( up1, up2, l, m ); 
    ltmor := PreXModMorphismByGroupHomomorphisms( lt1, lt2, l, n ); 
    rtmor := PreXModMorphismByGroupHomomorphisms( rt1, rt2, m, p ); 
    dnmor := PreXModMorphismByGroupHomomorphisms( dn1, dn2, n, p ); 
    mor := MakeHigherDimensionalGroupMorphism( xs1, xs2, 
               [ upmor, ltmor, rtmor, dnmor ] );
    if not IsPreCrossedSquareMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-crossed squares.\n" );
        return fail;
    fi;
    ok := IsCrossedSquareMorphism( mor );
    return mor;
end );

##############################################################################
##
#M  CrossedSquareMorphismByXModMorphisms( <Xs>, <Xr>, <list> )  
##
InstallMethod( CrossedSquareMorphismByXModMorphisms, 
    "for 2 crossed squares and 4 morphisms", true,
    [ IsCrossedSquare, IsCrossedSquare, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ( Length( list ) = 4 ) and 
           ForAll( list, m -> IsXModMorphism(m) ) then 
        Error( "third argument should be a list of xmod morphisms" ); 
    fi; 
    mor := PreCrossedSquareMorphismByPreXModMorphisms( src, rng, list );
    ok := IsCrossedSquareMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  CrossedSquareMorphismByGroupHomomorphisms( <Xs>, <Xr>, <list> )  
##
InstallMethod( CrossedSquareMorphismByGroupHomomorphisms, 
    "for 2 crossed squares and 4 morphisms", true,
    [ IsCrossedSquare, IsCrossedSquare, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ( Length( list ) = 4 ) and 
           ForAll( list, m -> IsGroupHomomorphism(m) ) then 
        Error( "third argument should be a list of xmod morphisms" ); 
    fi; 
    mor := PreCrossedSquareMorphismByGroupHomomorphisms( src, rng, list );
    ok := IsCrossedSquareMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#F  Cat2GroupMorphism( <src>, <rng>, <up>, <dn> ) . . cat2 morphism
##
##  (need to extend to other sets of parameters)
##
InstallGlobalFunction( Cat2GroupMorphism, function( arg )

    local nargs;

    nargs := Length( arg );
    # two cat2-groups and two homomorphisms
    if ( nargs = 3 ) then 
        if IsCat2Group( arg[1] ) and IsCat2Group( arg[2])
               and ForAll( arg[3], m -> IsCat1GroupMorphism(m) ) then 
            return Cat2GroupMorphismByCat1GroupMorphisms(arg[1],arg[2],arg[3]); 
        elif IsCat2Group( arg[1] ) and IsCat2Group( arg[2]) 
               and ForAll( arg[3], m -> IsGroupHomomorphism(m) )  then 
            return 
               Cat2GroupMorphismByGroupHomomorphisms(arg[1],arg[2],arg[3]); 
        fi;
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, "usage: Cat2GroupMorphism( src, rng, list of maps );" );
    return fail;
end );

###############################################################################
##
#M  PreCat2GroupMorphismByPreCat1GroupMorphisms( <src>, <rng>, <list of mors> ) 
##
InstallMethod( PreCat2GroupMorphismByPreCat1GroupMorphisms,
    "for two pre-cat2 and two pre-cat1 morphisms,", true,
    [ IsPreCat2Group, IsPreCat2Group, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ( Length( list ) = 4 ) and 
           ForAll( list, m-> IsPreCat2GroupMorphism(m) ) then 
        Error( "third argument should be a list of 4 pre-cat2-morphisms" );
    fi;
    mor := MakeHigherDimensionalGroupMorphism( src, rng, list );
    if not IsPreCat2GroupMorphism( mor ) then
        Info( InfoXMod, 2, "not a morphism of pre-cat2.\n" );
        return fail;
    fi;
    ok := IsCat2GroupMorphism( mor );
    return mor;
end );

##############################################################################
##
#M  Cat2GroupMorphismByCat1GroupMorphisms( <Cs>, <Cr>, <list> ) 
##  . . . make cat2-group mapping
##
InstallMethod( Cat2GroupMorphismByCat1GroupMorphisms, 
    "for two cat2-groups and 2 morphisms", true, 
    [ IsCat2Group, IsCat2Group, IsList ], 0,
function( src, rng, list )

    local mor, ok;

    if not ForAll( list, m -> IsCat1GroupMorphism(m) ) then 
        Error( "third argument should be a list of cat1-morphisms" ); 
    fi; 
    mor := PreCat2GroupMorphismByPreCat1GroupMorphisms( src, rng, list );
    ok := IsCat2GroupMorphism( mor );
    if not ok then
        return fail;
    fi;
    return mor;
end );

##############################################################################
##
#M  InclusionMorphismHigherDimensionalDomains( <obj>, <sub> )
##
InstallMethod( InclusionMorphismHigherDimensionalDomains, 
    "one n-dimensional object in another (but with n=3)", true, 
    [ IsHigherDimensionalDomain, IsHigherDimensionalDomain ], 0,
function( obj, sub )

    local up, lt, rt, dn;

    up := InclusionMorphism2DimensionalDomains( 
              Up2DimensionalGroup(obj), Up2DimensionalGroup(sub) );
    lt := InclusionMorphism2DimensionalDomains( 
              Left2DimensionalGroup(obj), Left2DimensionalGroup(sub) );
    rt := InclusionMorphism2DimensionalDomains( 
              Right2DimensionalGroup(obj), Right2DimensionalGroup(sub) );
    dn := InclusionMorphism2DimensionalDomains( 
              Down2DimensionalGroup(obj), Down2DimensionalGroup(sub) );    
    if IsPreCrossedSquare( obj ) then
        return PreCrossedSquareMorphismByPreXModMorphisms( 
                   sub, obj, [ up, lt, rt, dn ] );
    elif IsPreCat2Group( obj ) then
        return PreCat2GroupMorphismByPreCat1GroupMorphisms( 
                   sub, obj, [ up, lt, rt, dn ] );
    else
        return fail;
    fi;
end );

##############################################################################
##
#E  gp3map.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
