##############################################################################
##
#W  gp3map.gi                    GAP4package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements functions for 3Dimensional Mappings for 
##  (pre-)crossed squares and (pre-)cat2-groups. 
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  IsPreCrossedSquareMorphism      check the axioms for a pre-crossed square
##
InstallMethod( IsPreCrossedSquareMorphism,
    "generic method for pre-crossed module homomorphisms", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PS, QS, upmor, ltmor, dnmor, rtmor, ok;

    PS := Source( mor );
    QS := Range( mor );
    if not ( IsPreCrossedSquare( PS ) and IsPreCrossedSquare( QS ) ) then
        return false;
    fi;
    ### (1) check that the morphisms commute
    upmor := Up2DimensionalMorphism( mor );
    ltmor := Left2DimensionalMorphism( mor );
    rtmor := Right2DimensionalMorphism( mor );
    dnmor := Down2DimensionalMorphism( mor );
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
#M  IsPreCat2GroupMorphism . . check the axioms for a pre-cat2-group morphism
##
InstallMethod( IsPreCat2GroupMorphism,
    "generic method for homomorphisms of pre-cat2-groups", true, 
    [ IsHigherDimensionalGroupMorphism ], 0,
function( mor )

    local PC, QC, upmor, ltmor, rtmor, dnmor, genPC, upP, ltP, genQC, upQ, ltQ;

    PC := Source( mor );
    QC := Range( mor );
    upmor := Up2DimensionalMorphism( mor ); 
    ltmor := Left2DimensionalMorphism( mor ); 
    rtmor := Right2DimensionalMorphism( mor ); 
    dnmor := Down2DimensionalMorphism( mor ); 
    genPC := GeneratingCat1Groups( PC ); 
    upP := genPC[1]; 
    ltP := genPC[2]; 
    genQC := GeneratingCat1Groups( QC ); 
    upQ := genQC[1]; 
    ltQ := genQC[2];     
    if not ( Source( upmor ) = upP ) and ( Range( upmor ) = upQ ) then 
        Error( "seems to be a problem with upmor" ); 
    fi; 
    if not ( Source( ltmor ) = ltP ) and ( Range( ltmor ) = ltQ ) then 
        Error( "seems to be a problem with ltmor" ); 
    fi; 
    # check equality of vertex homomorphisms 
    if not ( SourceHom(upmor) = SourceHom(ltmor) ) then 
        Info( InfoXMod, 2, "SourceHom(upmor) <> SourceHom(ltmor)" ); 
        return false; 
    fi; 
    if not ( RangeHom(upmor) = SourceHom(rtmor) ) then 
        Info( InfoXMod, 2, "RangeHom(upmor) <> SourceHom(rtmor)" ); 
        return false; 
    fi; 
    if not ( RangeHom(ltmor) = SourceHom(dnmor) ) then 
        Info( InfoXMod, 2, "RangeHom(ltmor) <> SourceHom(dnmor)" ); 
        return false; 
    fi; 
    if not ( RangeHom(rtmor) = RangeHom(dnmor) ) then 
        Info( InfoXMod, 2, "RangeHom(rtmor) <> RangeHom(dnmor)" ); 
        return false; 
    fi; 
    #? are there any more checks that should be done? 
    return true;
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
    rt1 := Right2DimensionalGroup( xs1 ); 
    dn1 := Down2DimensionalGroup( xs1 ); 
    up2 := Up2DimensionalGroup( xs2 ); 
    lt2 := Left2DimensionalGroup( xs2 ); 
    rt2 := Right2DimensionalGroup( xs2 ); 
    dn2 := Down2DimensionalGroup( xs2 ); 
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
#F  PreCat2GroupMorphism( <src>,<rng>,<upmor>,<ltmor> ) pre-cat2-grp morphism
##
InstallGlobalFunction( PreCat2GroupMorphism, function( arg )

    local nargs;

    nargs := Length( arg );
    # two pre-cat2-groups and two pre-cat1-group morphisms
    if ( ( nargs = 4 ) and IsPreCat2Group( arg[1] ) and IsPreCat2Group( arg[2])
                       and IsPreCat1GroupMorphism( arg[3] )
                       and IsPreCat1GroupMorphism( arg[4] ) ) then
        return PreCat2GroupMorphismByPreCat1GroupMorphisms( 
                   arg[1], arg[2], arg[3], arg[4] );
    fi;
    # alternatives not allowed
    Info( InfoXMod, 2, 
          "usage: PreCat2GroupMorphism( src, rng, upmor, ltmor );" );
    return fail;
end );

###############################################################################
##
#M  PreCat2GroupMorphismByPreCat1GroupMorphisms( <src>, <rng>, <upm>, <ltm> ) 
##
InstallMethod( PreCat2GroupMorphismByPreCat1GroupMorphisms,
    "for two pre-cat2-groups and two pre-cat1-group morphisms,", true,
    [ IsPreCat2Group, IsPreCat2Group, IsPreCat1GroupMorphism, 
      IsPreCat1GroupMorphism ], 0,
function( C1, C2, upmor, ltmor )

    local homG, homR, homQ, dn1, edn1, P1, dn2, tdn2, P2, genP1, imP1, 
          rt1, ert1, rt2, trt2, L, homP, dnmor, rtmor, mor, ok;

    homG := SourceHom( upmor ); 
    homR := RangeHom( upmor );
    if not ( SourceHom( ltmor ) = homG ) then 
        return fail; 
    fi; 
    homQ := RangeHom( ltmor ); 
    dn1 := Down2DimensionalGroup( C1 ); 
    edn1 := RangeEmbedding( dn1 );
    P1 := Range( dn1 ); 
    dn2 := Down2DimensionalGroup( C2 ); 
    tdn2 := TailMap( dn2 ); 
    P2 := Range( dn2 ); 
    genP1 := GeneratorsOfGroup( P1 ); 
    imP1 := List( genP1, 
                p -> ImageElm( tdn2, ImageElm( homQ, ImageElm( edn1, p ) ) ) ); 
    ## do some checks 
    rt1 := Right2DimensionalGroup( C1 ); 
    ert1 := RangeEmbedding( rt1 );
    rt2 := Right2DimensionalGroup( C2 ); 
    trt2 := TailMap( rt2 ); 
    L := List( genP1, 
             p -> ImageElm( trt2, ImageElm( homR, ImageElm( ert1, p ) ) ) ); 
    if not ( L = imP1 ) then 
        Error( "image lists for homP do not agree" ); 
    fi; 
    homP := GroupHomomorphismByImages( P1, P2, genP1, imP1 ); 
    dnmor := Cat1GroupMorphismByGroupHomomorphisms( dn1, dn2, homQ, homP ); 
    rtmor := Cat1GroupMorphismByGroupHomomorphisms( rt1, rt2, homR, homP ); 
    mor := MakeHigherDimensionalGroupMorphism( C1, C2, 
               [ upmor, ltmor, rtmor, dnmor ] );
    if not IsPreCat2GroupMorphism( mor ) then
        Info( InfoXMod, 1, "not a morphism of pre-cat2-groups.\n" );
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

###############################################################################
##
#M  IsomorphismPreCat2GroupsNoTranspose . . . . iso between two pre-cat2-groups 
#M  IsomorphismCat2GroupsNoTranspose  . . . . . . . iso between two cat2-groups 
#M  IsomorphismPreCat2Groups  . . . . . . . . . iso between two pre-cat2-groups 
#M  IsomorphismCat2Groups . . . . . . . . . . . . . iso between two cat2-groups 
## 
InstallMethod( IsomorphismPreCat2GroupsNoTranspose, "for 2 pre-cat2-groups", 
    true, [ IsPreCat2Group, IsPreCat2Group ], 0,
function( D1, D2 )

    local up1, lt1, dn1, rt1, up2, lt2, dn2, rt2, 
          G1, R1, Q1, P1, G2, R2, Q2, P2, 
          t11, h11, e11, t22, h22, e22, 
          tau11, theta11, eps11, tau22, theta22, eps22, 
          t1, h1, e1, t2, h2, e2, 
          tau1, theta1, eps1, tau2, theta2, eps2, 
          phi, alpha, gamma, rho, sigma, pi, isoup, isolt, isort, isodn;

    up1 := Up2DimensionalGroup( D1 );
    lt1 := Left2DimensionalGroup( D1 ); 
    rt1 := Right2DimensionalGroup( D1 );
    dn1 := Down2DimensionalGroup( D1 );
    up2 := Up2DimensionalGroup( D2 );
    lt2 := Left2DimensionalGroup( D2 ); 
    rt2 := Right2DimensionalGroup( D2 );
    dn2 := Down2DimensionalGroup( D2 );
    G1 := Source( up1 ); 
    G2 := Source( up2 ); 
    if not ( G1 = G2 ) then
        phi := IsomorphismGroups( G1, G2 );
    else
        phi := IdentityMapping( G1 );
    fi;
    if ( phi = fail ) then
        Info( InfoXMod, 2, "G1 !~ G2" );
        return fail;
    fi;
    R1 := Range( up1 );
    R2 := Range( up2 );
    if IsomorphismGroups( R1, R2 ) = fail then
        Info( InfoXMod, 2, "R1 !~ R2" );
        return fail;
    fi; 
    Q1 := Source( dn1 );
    Q2 := Source( dn2 );
    if IsomorphismGroups( Q1, Q2 ) = fail  then
        Info( InfoXMod, 2, "Q1 !~ Q2" );
        return fail;
    fi;
    P1 := Range( dn1 );
    P2 := Range( dn2 );
    if IsomorphismGroups( P1, P2 ) = fail  then
        Info( InfoXMod, 2, "P1 !~ P2" );
        return fail;
    fi;
    t11 := TailMap( up1 );
    h11 := HeadMap( up1 );
    t22 := TailMap( up2 );
    h22 := HeadMap( up2 );
    e11 := RangeEmbedding( up1 ); 
    e22 := RangeEmbedding( up2 ); 
    tau11 := TailMap( lt1 );
    theta11 := HeadMap( lt1 );
    tau22 := TailMap( lt2 );
    theta22 := HeadMap( lt2 );
    eps11 := RangeEmbedding( lt1 ); 
    eps22 := RangeEmbedding( lt2 ); 
    for alpha in AutomorphismGroup( G1 ) do 
        gamma := alpha * phi; 
        rho := e11 * gamma * t22; 
        if ( ( e11*gamma = rho*e22 ) and 
             ( gamma*h22 = h11*rho ) and 
             ( gamma*t22 = t11*rho ) ) then 
            isoup := PreCat1GroupMorphismByGroupHomomorphisms
                        ( up1, up2, gamma, rho ); 
        else  
            isoup := fail; 
        fi; 
        sigma := eps11 * gamma * tau22; 
        if ( ( eps11*gamma = sigma*eps22 ) and 
             ( gamma*theta22 = theta11*sigma ) and 
             ( gamma*tau22 = tau11*sigma ) ) then 
            isolt := PreCat1GroupMorphismByGroupHomomorphisms
                        ( lt1, lt2, gamma, sigma ); 
        else 
            isolt := fail; 
        fi; 
        if not ( ( isoup = fail ) or ( isolt = fail ) ) then 
            if ( SourceHom( isoup ) <> SourceHom( isolt ) ) then 
                Error( "SourceHoms do not agree" ); 
            fi; 
            ## calculate pi : P1 -> P2 
            e1 := RangeEmbedding( dn1 );
            t2 := TailMap( dn2 ); 
            pi := e1*sigma*t2; 
            eps1 := RangeEmbedding( rt1 );
            tau2 := TailMap( rt2 ); 
            if not ( pi = eps1*rho*tau2 ) then 
                Error( "e1*sigma*t2 <> eps1*rho*tau2" ); 
            fi; 
            isort := PreCat1GroupMorphismByGroupHomomorphisms
                        ( rt1, rt2, rho, pi ); 
            isodn := PreCat1GroupMorphismByGroupHomomorphisms
                        ( dn1, dn2, sigma, pi ); 
            return PreCat2GroupMorphismByPreCat1GroupMorphisms 
                       ( D1, D2, isoup, isolt ); 
        fi;
    od;
    return fail;
end );          

InstallMethod( IsomorphismPreCat2Groups, "for two pre-cat2-groups", true, 
    [ IsPreCat2Group, IsPreCat2Group ], 0,
function( C1, C2 ) 

    local iso, TC2; 

    iso := IsomorphismPreCat2GroupsNoTranspose( C1, C2 ); 
    if ( iso = fail ) then 
        TC2 := Transpose3DimensionalGroup( C2 ); 
        iso := IsomorphismPreCat2GroupsNoTranspose( C1, TC2 ); 
    fi; 
    return iso; 
end ); 

InstallMethod( IsomorphismCat2Groups, "for two cat2-groups", true, 
    [ IsCat2Group, IsCat2Group ], 0,
function( C1, C2 ) 

    local iso, TC2; 

    iso := IsomorphismPreCat2Groups( C1, C2 ); 
    if ( iso = fail ) then 
        return fail; 
    elif IsCat2GroupMorphism( iso ) then 
        return iso;  
    else 
        return fail; 
    fi; 
end ); 

###############################################################################
##
#M  AllCat2GroupMorphisms  . . . . . . morphisms from one cat2-group to another 
##
InstallMethod( AllCat2GroupMorphisms, "for two cat2-groups", true, 
    [ IsCat2Group, IsCat2Group ], 0,
function( C2G1, C2G2 ) 

    local up1, lt1, up2, lt2, uphoms, lthoms, L, up, lt, mor2;

    up1 := Up2DimensionalGroup( C2G1 );
    lt1 := Left2DimensionalGroup( C2G1 );
    up2 := Up2DimensionalGroup( C2G2 );
    lt2 := Left2DimensionalGroup( C2G2 );
    uphoms := AllCat1GroupMorphisms( up1, up2 ); 
    lthoms := AllCat1GroupMorphisms( lt1, lt2 ); 
    L := []; 
    for up in uphoms do
        for lt in lthoms do 
            mor2 := PreCat2GroupMorphismByPreCat1GroupMorphisms( 
                       C2G1, C2G2, up, lt );
            if ( not ( mor2 = fail ) and IsCat2GroupMorphism( mor2 ) ) then
                Add( L, mor2);  
            fi;
        od;
    od;    
    return L;
end );
