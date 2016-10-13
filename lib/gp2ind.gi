##############################################################################
##
#W  gp2ind.gi                      XMOD Package                  Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file implements functions for induced crossed modules. 

##############################################################################
##
#F  InducedXMod( <grp>, <grp>, <grp>, <trans> )         crossed module induced 
#F  InducedXMod( <xmod>, <hom> [, <trans>] )             by group homomorphism
##
InstallGlobalFunction( InducedXMod, function( arg )

    local  usage, nargs, X0, bdy0, M, P, Q, iota, ires, T, mono, surj, iP,
           mor01, X1, I1, genI1, bdy1, im1, P1, inc, mor12, IX;

    usage := function( u )
        Print("\nUsage: InducedXMod( Q, P, M [, T] );");
        Print("\n where  Q >= P |>= M  and  T  is a transversal for Q/P");
        Print("\n   or: InducedXMod( X, iota [, T] );");
        Print("\n where  X  is a conjugation XMod and iota is injective.\n\n");
    end;
    nargs := Length( arg );
    if ( ( nargs < 2 ) or ( nargs > 4 ) ) then
        Info( InfoXMod, 2, "expecting 2 or 4 arguments" );
        usage( 0 );
        return fail;
    fi;
    T := [ ];
    if ( Is2dDomain( arg[1] ) and IsXMod( arg[1] ) ) then
        X0 := arg[1];
        M := Source( X0 );
        P := Range( X0 );
        iota := arg[2];
        if not ( IsGroupGeneralMapping(iota) and ( Source(iota) = P ) ) then
            usage( 0 );
            Info( InfoXMod, 2, "iota not a GroupGeneralMapping" );
            return fail;
        fi;
        if not IsInjective( iota ) then
            usage( 0 );
            Info( InfoXMod, 2, "iota not injective" );
            return fail;
        fi;
        Q := Range( iota );
        if ( ( nargs = 3 ) and IsList( arg[3] ) ) then
            T := arg[3];
        fi;
    elif ( IsGroup( arg[1] ) and ( nargs >= 3 ) ) then
        Q := arg[1];
        P := arg[2];
        M := arg[3];
        if not ( IsSubgroup( Q, P ) and IsNormal( P, M ) ) then
        Info( InfoXMod, 2, "expecting Q >= P and P |> M" );
            usage( 0 );
            return fail;
        fi;
        X0 := XModByNormalSubgroup( P, M );
        iota := InclusionMappingGroups( Q, P );
        if ( ( nargs = 4 ) and IsList( arg[4] ) ) then
            T := arg[4];
        fi;
    fi;
    Info( InfoXMod, 1, "X0, iota, M, P, Q all defined" );
    if ( T <> [ ] ) then
        Info( InfoXMod, 2, "T specified: ", T );
    fi;
    ## we have now defined X0, iota, M, P, Q in both cases ##
    mono := IsInjective( iota );
    surj := IsSurjective( iota );
    if mono then
        Info( InfoXMod, 2, "iota is mono" );
        if not IsSubgroup( P, M ) then
            mor01 := IsomorphismXModByNormalSubgroup( X0 );
            X1 := Range( mor01 );
        else
            mor01 := IdentityMapping( X0 );
            X1 := X0;
        fi;
        IX := InclusionInducedXModByCopower( X1, iota, T );
    elif surj then
        Info( InfoXMod, 2, "iota is surj" );
        IX := SurjectiveInducedXMod( X0, iota );
    else  ## split in two ##
        Info( InfoXMod, 2, "splitting into surjective and injective cases" );
        iP := ImagesSource( iota );
        ires := GeneralRestrictedMapping( iota, P, iP );
        Info( InfoXMod, 2, "iota splits: ires =", ires );
        X1 := SurjectiveInducedXMod( X0, ires );
        if ( InfoLevel( InfoXMod ) > 0 ) then
            Print( "surjective induced xmod:\n" );
            Display( X1 );
        fi;
        I1 := Source( X1 );
        genI1 := GeneratorsOfGroup( I1 );
        bdy1 := Boundary( X1 );
        im1 := List( genI1, s -> Image( bdy1, s ) );
        P1 := Subgroup( iP, im1 );
        if not ( P1 = iP ) then
            Print( "VERY SURPRISING RESULT : P1 <> iP\n" );
        fi;
        mor12 := IsomorphismXModByNormalSubgroup( X1 );
        inc := InclusionMappingGroups( Q, iP );
        IX := InclusionInducedXModByCopower( X1, inc, [ ] );
    fi;
    if HasName( M ) then
        SetName( IX, Concatenation( "i*(", Name( M ), ")" ) );
    elif HasName( X0 ) then
        SetName( IX, Concatenation( "i*(Source(", Name( X0 ), "))" ) );
    fi;
    return IX;
end );

##############################################################################
##
#M  InclusionInducedXModByCopower( <xmod>, <hom>, <trans> ) . . . induced xmod
##
InstallMethod( InclusionInducedXModByCopower, 
    "for a group, a homomorphism, and a transversal", true, 

    [ IsXMod, IsGroupHomomorphism, IsList ], 0,
function( X0, iota, trans )

    local  imgeniota, T, t, kept, diff, ok, l1, l2, m1, m2, 
           qP, qT, pos, posn, posm, words, total, tietze,
           Q, oQ, q, elQ, genQ, ngQ, Qinfo, actQ, p2fQ, FQ, degQ, indQP,
           P, oP, p, elP, genP, Pinfo, FP, iP, eliP,
           M, oM, m, elM, genM, ngM, Minfo, g2fpM, fp2gM, mgiM, imM, xgM, 
           FM, genFM, ngFM, presFM, relFM, nrFM, 
           freeM, genfreeM, free1, genfree1, free2, genfree2, 
           N, n1, n2, genN, ngN, 
           iN, geniN, FN, genFN, relFN, nrFN, fN, genfN, subfN, defN,  
           I, info, ngI, xgI, nxI, genI, relI, degI, imI, idI, 
           FI1, presFI1, genFI1, f2p, gensFI1, 
           ofpi, oFI2, FI2, genFI2, ngFI2, gensFI2, imact, 
           imIQ, homFIQ, FK, genFK, genK, oK, K, big,
           fpi, Idi, ck, cl, cm, qk, fl, gk, gl, gm, hk, hl, hm,
           i, ik, il, im, j, jk, jl, jm, k, nk, nl, pm, rm,
           tk, tl, tm, zk, zl, zm, u, v, x, y, z, ix, iy, 
           imD, gimD, elimD, genD, ind, gpos, vpos, imrem, genpos, genim,
           IX, bdy, act, aut, mor, morsrc, Iname, Xname, IdGp,
           degi, imold, prenew, iso12, pres2, 
           comp, mgicomp, series, idseries, 
           imFIQ, mgiFIQ, relFI, genFI, ishom, ispc, ispcI;

    Info( InfoXMod,2,"calling InclusionInducedXModByCopower" ); 
    Q := Range( iota );
    genQ := GeneratorsOfGroup( Q );
    oQ := Size( Q );
    ngQ := Length( genQ );
    elQ := Elements( Q );
    P := Range( X0 );;
    genP := GeneratorsOfGroup( P );
    oP := Size( P );
    elP := Elements( P );
    M := Source( X0 );
    oM := Size( M );
    #? need to generalise beyond normal subgroup xmods?    
    if not IsNormal( P, M ) then
        Error( "M not a normal subgroup of P" );
    fi;
    Minfo := IsomorphismFpInfo( M );
    FM := Minfo!.fp;
    fp2gM := Minfo!.fp2g;
    g2fpM := Minfo!.g2fp;
    mgiM := MappingGeneratorsImages( g2fpM );
    Info( InfoXMod, 2, "MappingGeneratorsImages for M :-" );
    Info( InfoXMod, 2, mgiM, "\n" );
    genM := mgiM[1];
    genFM := mgiM[2];
    ngM := Length( genM );
    ngFM := Length( genFM );
    ## image of P under iota
    iP := ImagesSource( iota );
    eliP := List( elP, p -> Image( iota, p ) );
    indQP := oQ/oP;
    presFM := PresentationFpGroup( FM );
    TzInitGeneratorImages( presFM );
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "presentation of FM :-\n" );
        TzPrint( presFM );
    fi;
    freeM := FreeGroupOfFpGroup( FM );
    genfreeM := GeneratorsOfGroup( freeM );
    relFM := RelatorsOfFpGroup( FM );
    nrFM := Length( relFM );

    # determine  genN = closure of  genM  under conjugation by  P 
    genN := ShallowCopy( genM );
    genFN := ShallowCopy( genFM );
    i := 0;
    ngN := Length( genN );
    Info( InfoXMod, 2, "finding closure of GeneratorsOfGroup(M) by P-action");
    while ( i < ngN ) do
        i := i + 1;
        n1 := genN[i];
        for p in genP do
            n2 := n1^p;
            posn := Position( genN, n2 );
            if ( posn = fail ) then
                Add( genN, n2 );
                m2 := Image( g2fpM, n2 ); ## is this better?
                if not ( n2 in M ) then 
                    Error( "M is not a normal subgroup of P" ); 
                fi; 
                Add( genFN, m2 );
                ngN := ngN + 1;
            fi;
        od;
    od;
    Info( InfoXMod, 1, "genN = P-closure of GeneratorsOfGroup(M) :- ");
    Info( InfoXMod, 1, genN );

    # prepare copy FN of FM with more generators
    fN := FreeGroup( ngN, "fN" );
    genfN := GeneratorsOfGroup( fN );
    subfN := genfN{[1..ngFM]};
    Info( InfoXMod, 2, "genFM = ", genFM ); 
    Info( InfoXMod, 2, "subfN = ", subfN ); 
    Info( InfoXMod, 2, "genFN = ", genFN ); 
    defN := List( genFN, g -> MappedWord( g, genFM, subfN ) );
    relFN := List( relFM, r -> MappedWord( r, genfreeM, subfN ) );
    for i in [ (ngFM+1)..ngN ] do
        n1 := defN[i];
        n2 := genfN[i]^(-1);
        Add( relFN, n1*n2 );
    od;
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "FN iso to FM but with generators closed under P-action\n" );
        Print( "Extended set of relators for N :- \n", relFN, "\n" );
    fi;
    nrFN := Length( relFN );
    
    genFN :=  [ ];
    for n1 in genN do
        n2 := PreImagesRepresentative( fp2gM, n1 );
        m1 := Image( g2fpM, n1 ); 
        l1 := Length( m1 );
        l2 := Length( n2 );
        if ( l2 < l1 ) then
            Add( genFN, n2 );
        else
            Add( genFN, m1 );
        fi;
    od;
    Info( InfoXMod, 2, "genFN = ", genFN );
    diff := ngN - ngFM;
    N := Subgroup( P, genN );
    geniN := List( genN, r -> Image( iota, r ) );
    iN := Subgroup( Q, geniN );
    if ( ( InfoLevel( InfoXMod ) > 1 ) and ( diff > 0 ) ) then
        Print( "Closed generating set for N :-\n", genN, "\n" );
    fi;
    FN := Subgroup( FM, genFN );

    ## process transversal
    if ( trans <> [ ] ) then
        T := trans;
    else
        T := CommonTransversal( Q, P );
    fi;
    ok := IsCommonTransversal( Q, P, T );
    if not ok then
        Error( "T fails to be a common transversal" );
    fi;
    Info( InfoXMod, 2, "Using transversal :- \n", T );
    ## express each  q in Q  as  p.t with  p in iP, t in T
    qP := 0 * [1..oQ];
    qT := 0 * [1..oQ];
    for t in T do
        for p in eliP do
            q := p*t;
            pos := Position( elQ, q );
            qP[pos] := p;
            qT[pos] := t;
        od;
    od;
    Info( InfoXMod, 1, "qP = ", qP, "\nqT = ", qT );

    Info( InfoXMod, 3, "\nstarting InclusionInducedXModByCopower here" );
    xgM := ngN-ngM;
    ngI := ngN*indQP;
    nxI := xgM*indQP;
    free1 := FreeGroup( ngI, "f1" );
    genfree1 := GeneratorsOfGroup( free1 );
    genFI := [ ];
    for i in [1..indQP] do
        j := (i-1) * ngN + 1;
        k := i*ngN;
        Add( genFI, genfree1{[j..k]} );
    od;
    ofpi := 0 * [1..ngI];
    actQ := 0 * [1..ngQ];
    for i in [1..ngQ] do
        actQ[i] := ShallowCopy( ofpi );
    od;
    for i in [1..ngN] do
        ofpi[i] := Order( genN[i] );
    od;
    for i in [(ngN+1)..ngI] do
        ofpi[i] := ofpi[i-ngN];
    od;
    Info( InfoXMod, 1, "Orders of the generators of I :- \n", ofpi );
    # Images of the generators of I in Q
    imFIQ := 0 * [1..ngI];
    for i in [1..indQP] do
        t := T[i];
        for j in [1..ngN] do
            x := geniN[j];
            y := x^t;
            k := (i-1)*ngN + j;
            imFIQ[k] := y;
        od;
    od;

    Info( InfoXMod, 2, "  ngQ = ", ngQ ); 
    Info( InfoXMod, 2, "indQP = ", indQP ); 
    Info( InfoXMod, 2, "  ngN = ", ngN ); 
    Info( InfoXMod, 2, " genQ = ", genQ ); 
    Info( InfoXMod, 2, "geniN = ", geniN ); 
    Info( InfoXMod, 2, "  elQ = ", elQ );
    # Action of the generators of Q on the generators of I
    Info( InfoXMod, 1, "Images in Q of the generators of I :- \n", imFIQ );
    for jk in [1..ngQ] do
        qk := genQ[jk];
        for il in [1..indQP] do
            tl := T[il];
            nl := (il-1)*ngN;
            zl := tl*qk;
            Info( InfoXMod, 3, "[jk,qk,il,tl,zl] = ", [jk,qk,il,tl,zl] );
            pm := Position( elQ, zl );
            tm := qT[pm];
            im := Position( T, tm );
            rm := qP[pm];
            Info( InfoXMod, 3, "[pm,tm,im,rm] = ", [pm,tm,im,rm] );
            for jl in [1..ngN] do
                fl := geniN[jl];
                cl := nl + jl;
                zm := fl^rm;
                jm := Position( geniN, zm );
                if ( jm = fail ) then
                    Print( "\n\n !!! Position Error !!! \n\n" );
                fi;
                cm := (im-1)*ngN + jm;
                Info( InfoXMod, 3, "[jl,fl,cl,zm,jm,cm] = " );
                Info( InfoXMod, 3, [jl,fl,cl,zm,jm,cm] );
                actQ[jk][cl] := cm;
            od;
        od;
    od;
    if ( InfoLevel( InfoXMod ) > 0 ) then
        Print( "\nAction of Q on generators of I :- \n" );
        for i in [1..ngQ] do
            Print( "  ", genQ[i], " : ", PermList( actQ[i] ), "\n" );
        od;
        Print( "\n" );
    fi;
    relFI := [ ];
    for i in [1..indQP] do
        for j in [1..nrFN] do
            u := relFN[j];
            v := MappedWord( u, genfN, genFI[i] );
            Add( relFI, v );
            Info( InfoXMod, 2, "u,v = ", u, " -> ", v );
        od;
    od;
    ## make list of relators for FI1
    gimD := [];
    for ik in [1..indQP] do
        tk := T[ik];
        nk := (ik-1)*ngN;
        Info( InfoXMod, 3, "[ik,tk] = ", [ik,tk] );
        for jk in [1..ngM] do                 # no longer [1..ngN]
            qk := geniN[jk];
            gk := genFI[ik][jk];
            ck := nk + jk;
            if ( ofpi[ck] > 2 ) then
                hk := gk^-1;
            else
                hk := gk;
            fi;
            zk := qk^tk;
            gpos := Position( gimD, zk );
            if ( gpos = fail ) then
                Add( gimD, zk );
            fi;
            Info( InfoXMod, 3, "[jk,qk,gk,ck,hk,zk,gpos] = " );
            Info( InfoXMod, 3, [jk,qk,gk,ck,hk,zk,gpos] );
            for il in [1..indQP] do
                tl := T[il];
                nl := (il-1)*ngN;
                Info( InfoXMod, 3, "[il,tl] = ", [il,tl] );
                for jl in [1..ngM] do         # no longer [1..ngN]
                    fl := geniN[jl];
                    gl := genFI[il][jl];
                    cl := nl + jl;
                    if ( ofpi[cl] > 2 ) then
                        hl := gl^-1;
                    else 
                        hl := gl;
                    fi;
                    zl := tl*zk;
                    Info( InfoXMod, 3, "[jl,fl,gl,cl,hl,zl] = " );
                    Info( InfoXMod, 3, [jl,fl,gl,cl,hl,zl] );
                    m := Position( elQ, zl );
                    tm := qT[m];
                    im := Position( T, tm );
                    rm := qP[m];
                    zm := fl^rm;
                    jm := Position( geniN, zm );
                    if ( jm = fail ) then
                        Print("\n\n !!! Position Error !!! \n\n");
                    fi;
                    gm := genFI[im][jm];
                    cm := (im-1)*ngN + jm;
                    if ( ofpi[cm] > 2 ) then
                        hm := gm^-1;
                    else
                        hm := gm; 
                    fi;
                    Info( InfoXMod, 3, "[m,tm,im,rm,zm,jm,gm,cm,hm] = " );
                    Info( InfoXMod, 3, [m,tm,im,rm,zm,jm,gm,cm,hm] );
                    if (ik <> il) then
                        big := cm;
                        if ( big < cl ) then 
                            big := cl; 
                        fi;
                        if ( big < ck ) then 
                            big := ck;
                        fi;
                        if ( big = cm ) then 
                            v := gm * hk * hl * gk;
                        elif ( big = cl ) then 
                            v := gl * gk * hm * hk;
                        elif ( ( hk = gk ) and ( cl < cm ) ) then
                            v := hk * hl * gk * gm;
                        else
                            v := gk * gm * hk * hl;
                        fi;
                        vpos := Position( relFI, v );
                        Info( InfoXMod, 2, "[big,v,vpos] = ", [big,v,vpos] );
                        if ( vpos = fail ) then 
                            Add( relFI, v );            # new relator!
                        fi;
                    fi;
                od;
            od;
        od;
    od;
    imD := Subgroup( Q, gimD );
    ind := Index( Q, imD );
    Info( InfoXMod, 2, "Image of I has index ", ind, 
                       " in Q, and is generated by" );
    Info( InfoXMod, 2, gimD );
    FI1 := free1 / relFI;
    presFI1 := PresentationFpGroup( FI1 );
    gensFI1 := GeneratorsOfPresentation( presFI1 );
    TzOptions( presFI1 ).printLevel := InfoLevel( InfoXMod );
    TzInitGeneratorImages( presFI1 );
    presFI1!.protected := ngM;
    ##  presFI1!.oldGenerators := ShallowCopy( presFI1!.generators ); ??
    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\n#I protecting the first ", ngM, " generators\n" );
        TzPrint( presFI1 );
        Print( "\n#I Full set of relators for FI1 :- \n", relFI, "\n" );
        Print( "\n#I Applying PresentationFpGroup, TzPartition & TzGo ", 
               "to FI1 :- \n" );
    fi;
    i := 0;
    repeat  ##??  why 9 times ?? 
        i := i + 1; 
        Info( InfoXMod, 3, "TzGo interation number ", i ); 
        tietze := presFI1!.tietze;
        total := tietze[TZ_TOTAL];
        TzGo( presFI1 ); 
        if ( InfoLevel( InfoXMod ) > 2 ) then 
            TzPrint( presFI1 ); 
        fi; 
    until ( ( total = tietze[TZ_TOTAL] ) or ( i > 9 ) );
    if ( InfoLevel( InfoXMod ) > 0 ) then 
        Print( "\nSimplified pres. for induced group:\n", presFI1, "\n" );
        TzPrint( presFI1 );
    fi;
    FI2 := FpGroupPresentation( presFI1 );
    genFI2 := GeneratorsOfGroup( FI2 ); 
    Info( InfoXMod, 3, "genFI2 = ", genFI2 ); 
    ngFI2 := Length( genFI2 );
    oFI2 := Size( FI2 );
    free2 := FreeGroupOfFpGroup( FI2 );
    genfree2 := GeneratorsOfGroup( free2 );
    gensFI2 := GeneratorsOfPresentation( presFI1 );    ## ????? 
    #? (10/07/10)  do we need both of genFI2 and gensFI2  ?????  
    Info( InfoXMod, 3, "gensFI2 = ", gensFI2 ); 
    Info( InfoXMod, 3, "genFI2 = gensFI2 ? ", gensFI2 = gensFI2 ); 
    Print( "#I induced group has Size: ", oFI2, "\n"); 
    #? (19/07/11) : example InducedXMod( s4, s3b, s3b ) fails 
    #? because of a pc isomorphism instead of a perm isomorphism,
    #? so revert, for now, to the perm case only: 
    if ( oFI2 >= 1 ) then 
        #?  info := IsomorphismPermOrPcInfo( FI2 ); 
        info := IsomorphismPermInfo( FI2 ); 
        #?  ispc := ( info!.type = "pc" ); 
        ispc := false; 
        if ispc then 
            I := info!.pc; 
            f2p := info!.g2pc; 
        else 
            I := info!.perm;
            degi := NrMovedPoints( I );
            f2p := info!.g2perm; 
        fi; 
        Info( InfoXMod, 2, "IsomorphismPermOrPcInfo: ", info ); 
    else 
        Print( "\n#I  unexpected order(I) = 1\n" ); 
        I := Group( () );
    fi; 

    # now identify I (if possible)
    if IsAbelian( I ) then
        Print( "#I factor ", i, " is abelian" ); 
        Print( "  with invariants: ", AbelianInvariants( I ), "\n" );
    elif ( ( oFI2 < 2000 ) and ( oFI2 <> 1024 ) ) then 
        series := CompositionSeries( I );
        if ( InfoLevel( InfoXMod ) > 1 ) then
            DisplayCompositionSeries( series );
        fi; 
        ## (21/01/10, 06/07/10) changed this condition - also changed below 
        if ( Size( series[1] ) < 2000 ) then 
            if ( RemInt( Size(series[1]), 128 ) <> 0 ) then 
                idseries := List( series, g -> StructureDescription( g ) ); 
            else 
                idseries := List( series, g -> IdGroup( g ) ); 
            fi; 
            Info( InfoXMod, 1, "CompositionSeries for induced group:" );
            Info( InfoXMod, 1, idseries, "\n" ); 
        fi; 
    fi;
    if HasName( M ) then
        SetName( I, Concatenation( "i*(", Name( M ), ")" ) );
    else
        SetName( I, "i*M" );
    fi;
    Info( InfoXMod, 2, "presFI1!.oldGenerators = ",presFI1!.oldGenerators);
    Info( InfoXMod, 2, "genFI2 = ", genFI2 );
    imold := TzImagesOldGens( presFI1 );
    prenew := TzPreImagesNewGens( presFI1 );
    Info( InfoXMod, 2, "   ImagesOldGens: ", imold );
    Info( InfoXMod, 2, "PreImagesNewGens: ", prenew );        
    genD := 0 * [1..ngFI2];
    for i in [1..ngFI2] do
        x := prenew[i];
        ##  change made as a test (14/01/04)
        ##  j := Position( gensFI1, x );
        j := Position( imold, x );
        genD[i] := imFIQ[j];
    od;
    Info( InfoXMod, 2, "genD = ", genD );
    homFIQ := GroupHomomorphismByImages( FI2, Q, genFI2, genD );
    FK := Kernel( homFIQ );
    genFK := GeneratorsOfGroup( FK );
    genK := List( genFK, k -> Image( f2p, k ) );
    Info( InfoXMod, 2, "genFK = ", genFK );
    Info( InfoXMod, 2, " genK = ", genK );
    K := Subgroup( I, genK );
    oK := Size( K );
    if ( ( InfoLevel( InfoXMod ) > 2 ) and ( oK > 1 ) ) then
        Print( "K has size: ", oK, "\n" );
        if ( oK <= 4000 ) then
            Print( "K has IdGroup\n", IdGroup( K ), "\n" );
        fi;
    fi;
    mgiFIQ := MappingGeneratorsImages( f2p );
    Info( InfoXMod, 2, "mgiFIQ[1] = genFI2 ?? ", mgiFIQ[1] = genFI2 );
    words := List( imold, w -> MappedWord( w, gensFI2, genFI2 ) );
    Info( InfoXMod, 2, "words = ", words ); 
    imrem := List( words, w -> Image( f2p, w ) );
    Info( InfoXMod, 2, "imrem = ", imrem ); 

    if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\nInitial generators in terms of final generators :- \n" );
        for i in [ 1..Length(imold) ] do
            Print( gensFI1[i]," : ",imold[i]," --> ",imrem[ i ],"\n" );
        od;
    Print( "\n" );
    fi;

    idI := IdentityMapping( I );
    genI := GeneratorsOfGroup( I );
    genpos := List( genI, g -> Position( imrem, g ) );
    Info( InfoXMod, 2, "genpos = ", genpos );
    imI := 0 * [1..ngQ];
    imact := 0 * [1..ngQ];
    for i in [1..ngQ] do
        imI[i] := List( actQ[i], j -> imrem[j] );
        ## imI[i] := List( actQ[i], j -> mgicomp[2][j] );
        genim := List( genpos, p -> imI[i][p] );
        imact[i] := GroupHomomorphismByImages( I, I, genI, genim );
    od;
    imIQ := List( genpos, p -> imFIQ[p] );
    Info( InfoXMod, 2, " imFIQ = ", imFIQ );
    Info( InfoXMod, 2, "imIQ = ", imIQ );
    bdy := GroupHomomorphismByImages( I, Q, genI, imIQ );
    ishom := IsGroupHomomorphism( bdy );
    imM := imrem{[1..ngM]};
    Info( InfoXMod, 1, [ M, I, genM, imM ] );
    Info( InfoXMod, 1, "------------------------------------------------" );
    morsrc := GroupHomomorphismByImages( M, I, genM, imM );
    Info( InfoXMod, 2, "morsrc: ", morsrc, "\n" );
    aut := GroupWithGenerators( imact, idI );
    SetName( aut, "aut(i*)" );
    act := GroupHomomorphismByImages( Q, aut, genQ, imact );
    IX := XModByBoundaryAndAction( bdy, act );
    SetIsInducedXMod( IX, true );
    ## IX!.xmod := X0;
    SetName( IX, Concatenation( "i*(", Name( X0 ), ")" ) );
    mor := PreXModMorphism( X0, IX, morsrc, iota );
    if not IsXModMorphism( mor ) then
        Print( "mor: X0 -> IX  not an xmod morphism!\n" );
    fi;
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

###############################################################################
##
#M  SurjectiveInducedXMod( <xmod>, <hom> ) . . induced xmod
##
InstallMethod( SurjectiveInducedXMod, "for xmod and homomorphism",
    true, [ IsXMod, IsGroupHomomorphism ], 0,
function( X0, iota )

    local  ispc, S, genS, R, bdy, act, K, genK, s, r, a, x, 
           H, genH, rcos, reps, Q, lenQ, genQ, preQ, PI, actPI, 
           isoI, I, genI, imi, istar, acthom, imb, bdystar, i, 
           autgen, imI, imS, actstar, autstar, idI, IX, mor;

    R := Range( X0 );
    S := Source( X0 );
    ispc := IsPc2dGroup( X0 ); 
    genS := GeneratorsOfGroup( S );
    bdy := Boundary( X0 );
    act := XModAction( X0 );
    K := Kernel( iota );
    genK := GeneratorsOfGroup( K );
    genH := [ ];
    H := Subgroup( S, genH );
    for r in genK do
        a := Image( act, r );
        for s in genS do
            x := s^(-1) * Image( a, s );
            if not ( x in H ) then
                Add( genH, x );
                H := Subgroup( S, genH );
            fi;
        od;
    od;
    Q := Range( iota );
    genQ := GeneratorsOfGroup( Q );
    preQ := List( genQ, q -> PreImagesRepresentative( iota, q ) );
    rcos := RightCosets( S, H );
    reps := List( rcos, r -> Representative( r ) );
    imb := List( genS, r -> Image( iota, Image( bdy, r ) ) );
    #? (06/07/10) modified to make Pc2dDomain 
    PI := Action( S, rcos, OnRight ); 
    actPI := ActionHomomorphism( S, PI ); 
    if ispc then 
        isoI := IsomorphismPcGroup( PI ); 
        ispc := not ( isoI = fail ); 
    fi; 
    if ispc then 
        I := Image( isoI ); 
        acthom := actPI * isoI; 
    else 
        I := PI; 
        acthom := actPI; 
    fi;
    genI := GeneratorsOfGroup( I );
    Info( InfoXMod, 2, "genI = ", genI ); 
    if HasName( S ) then
        SetName( I, Concatenation( Name( S ), "/ker" ) );
    fi;
    imi := List( genS, s -> Image( acthom, s ) ); 
    #? (06/07/10)  removed when adding pcgroup option 
    ##  if ( genI <> imi ) then
    ##    Error( "unequal images: genI <> imi" );
    ##  fi;
    istar := GroupHomomorphismByImages( S, I, genS, imi );
    bdystar := GroupHomomorphismByImages( I, Q, imi, imb );
    Info( InfoXMod, 3, "bdystar = ", bdystar ); 
    lenQ := Length( genQ );
    autgen := 0 * [1..lenQ];
    for i in [1..lenQ] do
        a := Image( act, preQ[i] );
        imS := List( genS, s -> Image( a, s ) );
        imI := List( imS, s -> Image( acthom, s ) );
        autgen[i] := GroupHomomorphismByImages( I, I, imi, imI );
    od;
    idI := InclusionMappingGroups( I, I );
    autstar := Group( autgen, idI );
    actstar := GroupHomomorphismByImages( Q, autstar, genQ, autgen );
    Info( InfoXMod, 3, "actstar = ", actstar ); 
    IX := XMod( bdystar, actstar );
    SetIsInducedXMod( IX, true );
    if HasName( X0 ) then
        SetName( IX, Concatenation( "i*(", Name( X0 ), ")" ) );
    fi;
    mor := XModMorphism( X0, IX, istar, iota );
    SetMorphismOfInducedXMod( IX, mor );
    return IX;
end );

#############################################################################
##
#M  AllInducedXMods( <grp> ) given Q, finds all XMods induced as  M <= P <= Q
##
InstallGlobalFunction( AllInducedXMods, function( args )

    local  nargs, rrange,nrange, usage, L, lenL, reps, nreps, r, i, j, k, a, b,  
           norm, nnorm, n, sizes, keep, coll, Q, P, M, id, XQ, SQ, num, line, 
           descrip, Msd, Psd, Qsd, SQsd, Ksd; 

    descrip := [ ]; 
    nargs := Length( args ); 
    Q := args[1]; 
    Qsd := StructureDescription( Q ); 
    Print( "\nInduced crossed modules with Q = ", Qsd, "\n\n" ); 
    L := LatticeSubgroups( Q ); 
    norm := NormalSubgroups( Q );
    Info( InfoXMod, 1, "normal subgroups of Q: ", norm );
    reps := Reversed( List( ConjugacyClassesSubgroups( L ), 
                      c -> Representative( c ) ) );
    nreps := Length( reps );
    Info( InfoXMod, 1, "non-trivial reps = ", [2..nreps-1] );
    for r in [ 1 .. nreps-1 ] do
        Info( InfoXMod, 1, StructureDescription( reps[r] ), " = ", 
reps[r] );
    od;
    num := 0;
    line := "--------------------------------------";
    if ( InfoLevel( InfoXMod ) > 0 ) then
        Print( "\nAll induced crossed modules  M --> IM" );
        Print( "\n                             |     | " );
        Print( "\n                             P --> Q\n");
        Print( "\ngenQ = ", GeneratorsOfGroup( Q ), "\n" );
        Print( "\n", line, line, "\n\n" );
    fi; 
    if ( nargs > 1 ) then 
        rrange := args[2]; 
    else 
        rrange := [2..nreps-1];
    fi;
    for r in rrange do
        P := reps[r]; 
        Psd := StructureDescription( P ); 
        norm := NormalSubgroups( P );
        # find representatives of conjugacy classes in Q
        sizes := List( norm, n -> Size( n ) );
        coll := Collected( sizes );
        keep := List( norm, n -> true );
        k := 1;
        for i in [ 2 .. ( Length( coll ) - 1 ) ] do
            j := k + 1;
            k := k + coll[i][2];
            for a in [ j .. k-1 ] do
                if keep[a] then
                    for b in [ a+1 ..k ] do
                        if IsConjugate( Q, norm[a], norm[b] ) then
                            keep[b] := false;
                        fi;
                    od;
                fi;
            od;
        od;
        nnorm := Length( norm );
        Info( InfoXMod, 2, "nnorm = ", nnorm ); 
        ##  ??  (16/01/04)
        ##  norm[ nnorm ] := P;
        if ( ( nargs > 2 ) and ( Length(rrange) = 1 ) ) then 
            nrange := args[3]; 
        else 
            nrange := [1..nnorm]; 
        fi;
        for n in nrange do
        ## for n in [1..nnorm-1] do
            if keep[n] then
                M := norm[n]; 
                Msd := StructureDescription( M ); 
                Print( "[ ", Msd, " -> ", Psd, " ]\n" ); 
                num := num + 1;
                Info( InfoXMod, 1, num, ". : " );
                Info( InfoXMod, 1, "genM = ", GeneratorsOfGroup( M ) );
                Info( InfoXMod, 1, "genP = ", GeneratorsOfGroup( P ) );
                XQ := InducedXMod( Q, P, M );
                SQ := Source( XQ ); 
                SQsd := StructureDescription( SQ ); 
                if ( InfoLevel( InfoXMod ) > 0 ) then
                    Display( XQ );
                    Print( "SQ has structure description: ", SQsd, "\n\n" );
                fi;
                Print( line, "\n\n" ); 
                Ksd := StructureDescription( Kernel( Boundary( XQ ) ) ); 
                Add( descrip, [ Msd, Psd, SQsd, Qsd, Ksd ] ); 
            fi;
        od;
    od;
    Info( InfoXMod, 1, 
              "Number of induced crossed modules calculated = ", num );
    PrintListOneItemPerLine( descrip );
    return descrip;
end );

###############################################################################
##
#M  InclusionInducedCat1Data( <cat1>, <hom>, <trans> ) . . 
##
##  ?? do we really want the trans ???
##
InstallMethod( InclusionInducedCat1Data, "for cat1-group, homomorphism, list", 
    true, [ IsCat1, IsGroupHomomorphism, IsList ], 0,
function( C, iota, trans )

local  Q, R, G,                    # 3 permutation groups
       Qinfo, Rinfo, Ginfo,        # IsomorphismFpInfos
       # C, iota,                  # C = [G ==iota==> R]
       ICGinfo,                    # InducedCat1Info
       nargs,                      # number of arg
       FQ, FR, FG,                 # Fin. Pres. Group
       elQ, elR, elG,              # elements of groups
       genQ, genR, genG,           # generating sets of groups
       oQ, oR, oG,                 # size of groups
       ngG,                        # number of generating set for G
       indQ,                       # oQ/oR
       degQ,                       # NrMovedPoints( Q )
       genFG,                      # generating set for Fin.Pres. Group FG
       ngFG,                       # number of generating set for FG
       relFG,                      # relators for FG
       nrFG,                       # number of all relators of FG
       elFG,                       # elements of FG
       presFG,                     # PresentationViaCosetTable for FG
       fp2gG,                      # record field for G
       qR, qT,                     # record field for Q
       posg2f, posf2g,             # positions of isomorphic images G <-> FG
       pos,                        # position variable
       T,                          # Tinfo
       ok,                         # checking variable
       t, i, q, p, m, rm;          # variables

    G := Source( C );
    Ginfo := IsomorphismFpInfo( G );
    R := Range( C );
    Rinfo := IsomorphismFpInfo( R );
    Q := ImagesSource( iota );
    Qinfo := IsomorphismFpInfo( Q );

    oQ := Size( Q );
    elQ := Elements( Q );
    genQ := GeneratorsOfGroup( Q );
    if IsPermGroup( Q ) then
        degQ := NrMovedPoints( Q );
    fi;

   # if not IsSubgroup( Q, R ) then
   #     Error( " R not a subgroup of Q" );
   # fi;
    oR := Size( R );
    elR := Elements( R );
    genR := GeneratorsOfGroup( R );
    indQ := oQ/oR;
    
    #if not IsNormal( R, G ) then
    #    Error( " R not a normal subgroup of G" );
    #fi;
    oG := Size( G );
    elG := Elements( G );
    genG := GeneratorsOfGroup( G );
    ngG := Length( genG );
    
    presFG := PresentationFpGroup( FG );
    TzOptions( presFG ).printLevel := InfoLevel( InfoXMod );
    TzInitGeneratorImages( presFG );
    if ( InfoLevel( InfoXMod ) > 2 ) then
        Print( "initial presentation for G :-\n\n" );
        TzPrint( presFG );
    fi;
    relFG := RelatorsOfFpGroup( FG );
    nrFG := Length( relFG );
    elFG := Elements( FG );   

    # Determine the positions of isomorphic images G <-> FG
    posf2g := 0 * [1..oG];
    posg2f := 0 * [1..oG];
    fp2gG := Ginfo!.fp2g;
    for i in [1..oG] do
        m := elFG[i];
        rm := Image( fp2gG, m );
        pos := Position( elG, rm );
        posf2g[i] := pos;
        posg2f[pos] := i;
    od;
    Info( InfoXMod, 1, "posf2g = ", posf2g );
    Info( InfoXMod, 1, "posg2f = ", posg2f );

    ICGinfo := rec( 
      Qinfo := Qinfo,
      Rinfo := Rinfo,
      Ginfo := Ginfo,
      cat1 := C,
      iota := iota,
      isInducedCat1Info := true );
    return ICGinfo;
end );

###############################################################################
##
#M  InducedCat1ByFreeProduct( <list> ) . . 
##
InstallMethod( InducedCat1ByFreeProduct, "for a list", true, [ IsList ], 0,
function( info )

    local  FQ,             # Fin. Pres. Group
           Q,              # Perm Group 
           Qinfo,           # Record field 
           oQ,             # Size of Q
           genQ,           # generating set for Perm group Q 
           ngQ,            # number of generating set of Perm Group Q
           genG,           # generating set perm group G
           ngG,            # number of generating set of perm G
           ngI,            # total length of ngPG+ngPQ
           C,              # Cat1 
           Csrc,           # Cat1 source 
           Crng,           # Cat1 range 
           t, h, e,        # tail, head, embedding 
           genCsrc,        # generating set of sourxe group
           genCrng,        # generating set of range group
           fI,             # Free group 
           genfI,          # generating set of free group 
           imG, imQ, 
           relI,           # all relators
           Gfp,            # IsomorphismFpInfo 
           genGfp,         # generating set  
           FGrel,           # relators
           len,            # Length of relators
           Qfp,            # IsomorphismFpInfo for Q
           genQfp,         # generating set 
           FQrel,           # relators
           iota,            # inclusion map from Crng to PQ
           imembed,        # relations produced by embedding
           imiota,         # relations produced by iota
           uuQ,            # List variable
           wQ, uQ, wG, uG, uQG,
           kert, kerh,     # kernel of tail and head
           genkert,        # generating set of kert
           genkerh,        # generating set of kerh
           imt, imh, 
           tG, hG, 
           com,            # Commutator subgroup
           Yh, Yt,         # conjugations for tail and head
           YYt, YYh,       # List of conjugations
           I, genI,        # new free group and its generating set
           presFI,          # Presentation
           newFIfp,         # IsomorphismFpInfo
           PI,             # new permutational group
           oFI2, genPI,      # Size and generating set of new perm group
           iotastar,       # homomorphism from Csrc to nep perm group
           imh1, imh2, 
           hstar,          # new head homomorphism for Induced Cat1 group
           imt1, imt2, 
           tstar,          # new tail homomorphism for Induced Cat1-Group
           imm, imag, images, 
           estar,          # new embed homomorphism for Ind.cat1
           IC,              # Induced Cat1 variable
           mor,            # Cat1Morphism from C to IC 
           u, v, j, x, i, g;# using variables

    Q := info!.Qinfo!.perm;
    C := info!.cat1;
    iota := info!.iota;
    Csrc := C!.source;
    Crng := C!.range;
    t := C!.tailMap;
    h := C!.headMap;
    e := C!.embedRange;
    genQ := GeneratorsOfGroup( Q );
    genCsrc := Csrc!.generators;
    genCrng := Crng!.generators;
    Info( InfoXMod, 2, "genCrng = ", genCrng );
    ngG := Length( genCsrc );
    ngQ := Length( genQ );
    ngI := ngG+ngQ;
    fI := FreeGroup( ngI, "fI" );
    genfI := fI!.generators;
    imQ := genfI{[ngG+1..ngI]};
    imG := genfI{[1..ngG]}; 
    relI := [ ];
    # Creating the relations of G
    Gfp := IsomorphismFpInfo( Csrc );
    genGfp := GeneratorsOfGroup( Gfp!.fp );
    FGrel := RelatorsOfFpGroup( Gfp!.fp );
    len := Length( FGrel );
    for j in [1..len] do
        u := FGrel[j];
        v := MappedWord( u, genGfp, imG );
        Add( relI, v );
    od;
    # Adding extra relations from Q
    Qfp := IsomorphismFpInfo( Q );
    genQfp := GeneratorsOfGroup( Qfp!.fp );
    FQrel := RelatorsOfFpGroup( Qfp!.fp );
    len := Length( FQrel );
    for j in [1..len] do
        u := FQrel[j];
        v := MappedWord( u, genQfp, imQ );
        Add( relI, v );
    od;
    # Adding extra relations from embedding and iota
    uuQ := [ ];
    imembed := List( genCrng, x -> Image( e, x ) );
    wG := List( imembed, x -> Image( Gfp!.p2f, x ) ); 
    uG := List( wG, g -> MappedWord( g, genGfp, imG ) );
    imiota := List( genCrng, x -> Image( iota, x ) ); 
    wQ := List( imiota, x -> Image( Qfp!.p2f, x ) );
    uQ := List( wQ, u -> MappedWord( u, genQfp, imQ ) );
    for i in [1..Length(uG)] do     
        uQG := uG[i]*uQ[i]^-1;
        Add( uuQ, uQG );
    od;
    relI := Concatenation( relI, uuQ );
    # Finding the Peiffer subgroup
    YYt := [ ];
    YYh := [ ];
    kert := Kernel( t );
    kerh := Kernel( h );
    genkert := kert!.generators;
    genkerh := kerh!.generators;
    imt := List( genkert, x -> Image( Gfp!.p2f, x ) );
    tG := List( imt, i -> MappedWord( i, genGfp, imG ) );
    imh := List( genkerh, x -> Image( Gfp!.p2f, x ) );
    hG := List( imh, i -> MappedWord( i, genGfp, imG ) );
    for u in genfI do
    Yt := List( tG, x -> x^u );
    YYt := Concatenation( YYt, Yt );
    Yh := List( hG, x -> x^u );
    YYh := Concatenation( YYh, Yh );
    od;
    for i in YYt do
        for j in YYh do
            com := Comm( i, j );
            Add( relI, com );
        od;
    od;
    I := fI / relI;
    genI := I!.generators;
    presFI := PresentationFpGroup( I );
    TzInitGeneratorImages( presFI );
    presFI!.protected := Length( genI );
    Print( "#I  Protecting the first ", ngG, " generators!.\n" );
    presFI!.oldGenerators := ShallowCopy( presFI!.generators );
     if ( InfoLevel( InfoXMod ) > 1 ) then
        Print( "\nFull set of relators for I :- \n", relI, "\n" );
        Print( "\nApplying PresentationFpGroup to I :- \n" );
        TzPrint( presFI );
        Print( "\nApplying TzPartition & TzGo to I :- \n" );
    fi;
    TzPrint( presFI ); 
    TzGoGo( presFI );
    TzPrint( presFI );
    imQ := genI{[ngG+1..ngI]};
    imG := genI{[1..ngG]}; 
    newFIfp := IsomorphismFpInfo( I );
    PI := newFIfp!.perm;
    oFI2 := Size( PI );
    genPI := PI!.generators;
    Print("new perm group size ", oFI2, "\n");
    Print("******************** \n");
    imG := genPI{[1..ngG]};
    iotastar := GroupHomomorphismByImages( Csrc, PI, genCsrc, imG );
    imh1 := List( genCsrc, x -> Image( h, x ) );
    imh2 := List( imh1, x -> Image( iota, x ) );
    imh := Concatenation( imh2, genQ );
    hstar := GroupHomomorphismByImages( PI, Q, genPI, imh );
    imt1 := List( genCsrc, x -> Image( t, x ) );
    imt2 := List( imt1, x -> Image( iota, x ) );
    imt := Concatenation( imt2, genQ );
    tstar := GroupHomomorphismByImages( PI, Q, genPI, imt );
    imm := List( genQ, x -> Image( Qfp!.p2f, x ) );
    imag := List( imm, x -> MappedWord( x, genQfp, imQ ) );
    images := List( imag, x -> Image( newFIfp!.f2p, x ) );
    estar := GroupHomomorphismByImages( Q, PI, genQ, images );
    IC := Cat1( PI, tstar, hstar, estar );
    IC!.isCat1 := IsCat1( IC );
    mor := Cat1Morphism( C, IC, [ iotastar, iota ] );
    if not ( IsCat1Morphism( mor ) ) then
        Print( " mor : C --> IC not a cat1-group morphism \n" );
    fi;
    IC := rec( 
      morphism := mor,
      name := Concatenation( "<ICG(", Name( C ), ")>" ),
      cat1 := C,
      isInducedCat1 := true );
    return IC;
end );

###############################################################################
##
#F  InducedCat1( <arg> ) . . induced cat1-groups
##
InstallGlobalFunction( InducedCat1, function( arg )

    local  nargs, info, Q, Qinfo, P, Pinfo, G, Ginfo, C, iota, IC;

    nargs := Length( arg );
    if ( nargs > 2 ) then
        return false;
    fi;
    if not IsRecord( arg[1] ) then
        return false;
    fi;
    if IsGroup( arg[1] ) then
        if ( ( nargs < 3 ) or not IsNormal( arg[2], arg[1] ) ) then
            return false;
        fi;
        Qinfo := arg[1];
        Pinfo := arg[2];
        Ginfo := arg[3];
        C := Cat1( Pinfo, Ginfo );
        G := Source( C );
        Ginfo := IsomorphismFpInfo( G );
        iota := InclusionMappingGroups( Pinfo, Qinfo );
    elif IsCat1( arg[1] ) then
        C := arg[1];
        G := Source( C );
        Ginfo := IsomorphismFpInfo( G );
        P := Range( C );
        Pinfo := IsomorphismFpInfo( P );
        iota := arg[2];
        #if ( ( nargs > 2 ) or ( iota!.source <> Pinfo )
        #                   or not IsInjective( iota ) ) then
        #    return false;
        #fi;
        Q := ImagesSource( iota );
        Qinfo := IsomorphismFpInfo( Q );
    fi;

    info := InclusionInducedCat1Data( C, iota );
    IC := InducedCat1ByFreeProduct( info );
    return IC;
end );

###############################################################################
##
#M  AllInducedCat1s( <grp> ) . . induced cat1s
##
InstallGlobalFunction( AllInducedCat1s, function( args )

    local  nargs, rrange, nrange, L, lenL, reps, nreps, r, i, j, k, a, b,
           norm, nnorm, n, sizes, keep, coll, Q, P, M, id, info, 
           IC, num, line, C, iota;

    nargs := Length( args ); 
    Q := args[1]; 
    L := LatticeSubgroups( Q );
    reps := Reversed( List( ConjugacyClassesSubgroups(L), 
                      c -> Representative( c ) ) );
    nreps := Length( reps );
    Print( "non-trivial reps = ", [2..nreps-1], "\n" );
    for r in [ 2 .. nreps-1 ] do
        Print( reps[r], "\n" );
    od;
    num := 0;
    Print( "\n All induced cat1-groups       M --> IM" );
    Print( "\n                              ||    || " );
    Print( "\n                               P --> Q\n");
    Print( "\n genQ = ", GeneratorsOfGroup( Q ), "\n" );
    Print( "\n", line, line, "\n\n" );
    if ( nargs > 1 ) then 
        rrange := args[1]; 
    else 
        rrange := [2..nreps-1]; 
    fi;
    for r in rrange do
        P := reps[r];
        norm := NormalSubgroups( P );
        # find representatives of conjugacy classes in Q
        sizes := List( norm, n -> Size( n ) );
        coll := Collected( sizes );
        keep := List( norm, n -> true );
        k := 1;
        for i in [ 2 .. ( Length( coll ) - 1 ) ] do
            j := k + 1;
            k := k + coll[i][2];
            for a in [ j .. k-1 ] do
                if keep[a] then
                    for b in [ a+1 ..k ] do
                        if IsConjugate( Q, norm[a], norm[b] ) then
                            keep[b] := false;
                        fi;
                    od;
                fi;
            od;
        od;
        nnorm := Length( norm );
        norm[ nnorm ] := P;
        if ( ( nargs > 2 ) and ( Length(rrange) = 1 ) ) then 
            nrange := args[3]; 
        else 
            nrange := [2..nnorm]; 
        fi;
        for n in nrange do
            if keep[n] then
                M := norm[n];
                Print( "genM = ", GeneratorsOfGroup( M ), "\n" );
                Print( "genP = ", GeneratorsOfGroup( P ), "\n" );
                C := Cat1( P, M );
                iota := InclusionMappingGroups( P, Q );
                IC := InducedCat1( C, iota );
                Display( IC );
                num := num + 1;
                Print( line, line, "\n\n" );
            fi;
        od;
    od;
    Print( "Number of induced cat1-groups calculated = " );
    return num;
end );

#############################################################################
##
#E  gp2ind.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
