##############################################################################
##
#W  cat1data.gi                GAP4 package `XMod'               Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2014, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 

##  These functions are used in the construction of the data file cat1data.g 
##  and are not intended for the general user. 

##  In order to use the operations which write intermediate data to file, 
##  start the GAP session as follows: 
##  gap> SetInfoLevel( InfoXMod, 2 );       ## or some higher value 
##  gap> gp := SmallGroup( nn, kk );        ## choose an appropriate group 
##  gap> gens := GeneratorsOfGroup( gp ); 
##  gap> ngens := Length( gens ); 
##  gap> f1 := gens[1]; 
##  gap>    . . .                           ## repeat for each generator 
##  gap> id := One( gp ); 
##  gap> Cat1RepresentativesToFile( gp );   ## then edit  nn.kk.rep 
##                                          ## changing the entry 1 in ireps 
##                                          ## to TrivialSubgroup(gp).\n" ); 
##  gap> Read( "lib/nn.kk.rep" );           ## read in the list ireps 
##  gap> Cat1IdempotentsToFile( gp, ireps, 1, jj ); 
##  gap> Cat1IdempotentsToFile( gp, ireps, jj+1, jjj );   ## etc. etc. 
##                                          ## then look at  nn.kk.ids 
##  gap> Read( "lib/nn.kk.ids" );           ## read in the list of idempotents 
##  gap> AllCat1sInParts( gp, ireps, idems, [1,jj], [ ] ); 
##  gap> Cjj := ???  
##  gap> AllCat1sInParts( gp, ireps, idems, [jj+1,jjj], Cjj );  ## etc. etc. 
##  
##  Finally, edit  nn.kk.out, deleting the final entry, 
##  and changing the last line of the file to finish with:  ] ] ] ] ],  

##############################################################################
##
#M  AllCat1s . . . . . . . . . . . . . . . . . . . . . . . . . . . for a group
##
InstallGlobalFunction( AllCat1s, function( arg )

    local  nargs; 

    nargs := Length( arg ); 
    if ( nargs = 1 ) then 
        return AllCat1sBasic( arg[1] ); 
    elif ( nargs = 3 ) then 
        return AllCat1sInParts( arg[1], arg[2], arg[3] ); 
    else 
        Error( "length of arg neither 1 nor 3" ); 
    fi;  
end ); 

##############################################################################
##
#M  AllCat1sBasic . . . . . . . . . . . . . . . . . . . . . . . . for a group
##
InstallMethod( AllCat1sBasic, "construct all cat1-groups on a given group", 
    true, [ IsCyclic and IsPGroup ], 0,
function( gp ) 

    local  C, zero, one; 

    C := [ 0, 0 ]; 
    zero := MappingToOne( gp, gp ); 
    C[1] := PreCat1ByEndomorphisms( zero, zero ); 
    IsCat1( C[1] ); 
    one := IdentityMapping( gp ); 
    C[2] := PreCat1ByEndomorphisms( one, one ); 
    IsCat1( C[2] ); 
    return C; 
end ); 

InstallMethod( AllCat1sBasic, "construct all cat1-groups on a given group", 
    true, [ IsGroup ], 0,
function( gp )

    local  gpid, size, num, gens, ngens, path, name, out, gensgp, egensgp, 
           idem, ranges, idems, inum, i, im, R, j, numrng, aut, 
           reps, nreps, ireps, nireps, quot, autR, a, q, b, mgi, 
           keep, iranges, iidems, genR, igenR, aR, pos, C, Cnum, numirng, 
           idemi, inumi, h, kerh, k, t, kert, kerth, CC, g, AICG, isocg, 
           bdy, kerbdy, z, gens1, list1, len1, gens2, list2, len2, 
           struc, fst, genr, egenr, imt, eimt, imh, eimh; 

    gpid := IdGroup( gp ); 
    size := gpid[1]; 
    num := gpid[2]; 
    gens := GeneratorsOfGroup( gp ); 
    ngens := Length( gens ); 
    gensgp := SmallGeneratingSet( gp ); 
    egensgp := List( gensgp, g -> ExtRepOfObj(g) ); 
    path := DirectoriesPackageLibrary( "xmod", "lib" )[1]; 
    name := Concatenation( String(size), ".", String(num), ".out" ); 
    out := Filename( path, name ); 
    ## find isomorphism classes of subgroups 
    if not IsPcGroup( gp ) then 
        return fail; 
    fi; 
    aut := AutomorphismGroup( gp ); 
    reps := List( ConjugacyClassesSubgroups(gp), c -> Representative(c) ); 
    nreps := Length( reps ); 
    Info( InfoXMod, 2, "nreps = ", nreps ); 
    keep := ListWithIdenticalEntries( nreps, 1 ); 
    ireps := [ ]; 
    for i in [1..nreps] do 
        if ( keep[i] = 1 ) then 
            Info( InfoXMod, 2, " rep number: ", i ); 
            R := reps[i]; 
            Add( ireps, R ); 
            genR := GeneratorsOfGroup( R ); 
            for a in aut do 
                igenR := List( genR, g -> Image( a, g ) ); 
                aR := Subgroup( gp, igenR ); 
                pos := Position( reps, aR ); 
                if ( not( pos = fail ) and ( pos > i ) ) then 
                    keep[pos] := 0; 
                fi; 
            od; 
        fi; 
    od; 
    nireps := Length( ireps ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( nreps, " reps reduced to ", nireps, "\n" ); 
        for i in [1..nireps] do 
            Print( ireps[i], " <--> ", 
                   StructureDescription( ireps[i] ), "\n" ); 
        od; 
    elif ( InfoLevel( InfoXMod ) > 0 ) then 
        PrintListOneItemPerLine( ireps ); 
    fi;
    idems := ListWithIdenticalEntries( nireps, 0 ); 
    for i in [1..nireps] do 
        Info( InfoXMod, 2, "subgroup number: ", i ); 
        idems[i] := [ ]; 
        R := ireps[i]; 
        quot := GQuotients( gp, R ); 
        autR := AutomorphismGroup( R ); 
        for q in quot do 
            for a in autR do 
                b := q*a; 
                if ( b*b = b ) then 
                    pos := Position( idems[i], b ); 
                    if ( pos = fail ) then 
                        Add( idems[i], b ); 
                    fi; 
                fi; 
            od;
        od; 
    od; 
    ## now convert entries in idems back to endomorphisms of gp 
    ## GQuotients may have picked an alternative generating set for gp 
    for i in [1..nireps] do 
        idem := idems[i]; 
        for j in [1..Length(idem)] do 
            mgi := List( gens, g -> Image( idem[j], g ) ); 
            idem[j] := GroupHomomorphismByImages( gp, gp, gens, mgi ); 
        od;  
    od; 
    if ( InfoLevel( InfoXMod ) > 0 ) then 
        Print( "# idempotents = ", Sum( List( idems, i -> Length(i) ) ), "\n" ); 
        Print( List( idems, i -> Length(i) ), "\n" ); 
    fi; 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( "\nidems = \n" ); 
        PrintListOneItemPerLine( idems ); 
    fi; 

    C := [ ];
    Cnum := 0; 
    for i in [1..nireps] do 
        R := ireps[i];
        idemi := idems[i]; 
        inumi := Length( idemi ); 
        for j in [1..inumi] do
            h := idemi[j]; 
            kerh := Kernel( h );
            # (e;t,h:G->R) isomorphic to (e;h,t:G->R), so take k>=j
            for k in [j..inumi] do 
                t := idemi[k]; 
                kert := Kernel( t );
                kerth := CommutatorSubgroup( kert, kerh ); 
                if ( Size( kerth ) = 1 ) then 
                    if ( InfoLevel( InfoXMod ) > 1 ) then 
                        Print( "t: ", MappingGeneratorsImages(t)[2], "\n" ); 
                        Print( "h: ", MappingGeneratorsImages(h)[2], "\n" ); 
                    fi; 
                    CC := PreCat1ByEndomorphisms( t, h ); 
                    if not IsCat1( CC ) then 
                        Error( "not a cat1-group" ); 
                    fi; 
                    g := Cnum; 
                    AICG := false;
                    while ( not AICG and ( g > 0 ) ) do
                        #? is this expensive? 
                        isocg := IsomorphismPreCat1s( C[g], CC ); 
                        AICG := not ( isocg = fail ); 
                        g := g-1;
                    od;
                    if ( AICG = false ) then
                        Add( C, CC ); 
                        Cnum := Cnum + 1; 
                        Info( InfoXMod, 2, "\nC[", Cnum, "] = ", CC ); 
                    else 
                        ## maybe CC is a simpler construction than C[g] ? 
                        gens1 := GeneratorsOfGroup( Range( CC ) ); 
                        list1 := Flat( List( gens1, g -> ExtRepOfObj(g) ) ); 
                        for z in [1..Length(list1)] do 
                            if IsOddInt(z) then list1[z] := 0; fi; 
                        od; 
                        len1 := Sum( list1 );
                        g := g+1; 
                        Info( InfoXMod, 2, "isomorphic to C[g], g = ", g ); 
                        gens2 := GeneratorsOfGroup( Range( C[g] ) ); 
                        list2 := Flat( List( gens2, g -> ExtRepOfObj(g) ) ); 
                        for z in [1..Length(list2)] do 
                            if IsOddInt(z) then list2[z] := 0; fi; 
                        od; 
                        len2 := Sum( list2 ); 
                        Info( InfoXMod, 2, "len1 = ", len1, ",  len2 = ", 
                                           len2, " at [i,j,k] = ", [i,j,k] ); 
                        if ( len1 < len2 ) then 
                            C[g] := CC;                             
                            Info( InfoXMod, 2, "swapping due to lengths:" ); 
                            Info( InfoXMod, 2, "gens1 = ", gens1 ); 
                            Info( InfoXMod, 2, "gens2 = ", gens2 ); 
                        elif ( len1 = len2 ) then 
                            # check no. generators fixed by tail map 
                            len1 := 0; 
                            for z in gens1 do 
                                if ( Image(TailMap(CC),z) = z ) then 
                                    len1 := len1 + 1; 
                                fi; 
                            od;
                            len2 := 0; 
                            for z in gens2 do 
                                if ( Image(TailMap(CC),z) = z ) then 
                                    len2 := len2 + 1; 
                                fi; 
                            od; 
                            if ( len1 < len2 ) then 
                                C[g] := CC;                             
                                Info( InfoXMod, 2, 
                                      "swapping due to fixed points" ); 
                                Info( InfoXMod, 2, "gens1 = ", gens1 ); 
                                Info( InfoXMod, 2, "gens2 = ", gens2 ); 
                            fi; 
                        fi; 
                    fi; 
                fi;
            od;
        od;
    od; 
    if ( InfoLevel( InfoXMod ) > 0 ) then 
        for i in [1..Cnum] do
            CC := C[i];
            Print( "Isomorphism class ", i, "\n" );
            if ( Size( Kernel( CC ) ) < 101 ) then
                Print( ": kernel of tail = ", IdGroup( Kernel( CC) ), "\n" );
            else
                Print( ": kernel has size = ", Size( Kernel( CC ) ), "\n" );
            fi;
            if ( Size( Range( CC ) ) < 101 ) then
                Print( ":    range group = ", IdGroup( Range( CC ) ), "\n" );
            else
                Print( ":  range has size = ", Size( Range( CC ) ), "\n" );
            fi;
            bdy := Boundary( CC );
            kerbdy := Kernel( bdy );
            if ( kerbdy <> Kernel( CC ) ) then
                if ( Size( kerbdy ) < 101 ) then
                    Print( ":  kernel of bdy = ", IdGroup( kerbdy ), "\n" );
                else 
                    Print( ": kernel of bdy has size ", Size(kerbdy), "\n" );  
                fi;
            fi;
        Print( "\n" );
        od; 
    fi; 
    ##  now output in the form required for the cat1data.g file 
    struc := StructureDescription( gp ); 
    PrintTo( out, "[",size,",",num,",\"",struc,"\",",egensgp,",[\n" ); 
    if IsAbelian( gp ) then 
        fst := 2; 
    else 
        fst := 1; 
    fi; 
    for j in [fst..Cnum-1] do 
        a := C[j]; 
        genr := SmallGeneratingSet( Range(a) ); 
        egenr := List( genr, g -> ExtRepOfObj(g) ); 
        t := TailMap( a ); 
        imt := List( gensgp, g -> Image( t, g ) ); 
        eimt := List( imt, g -> ExtRepOfObj(g) ); 
        h := HeadMap( a ); 
        imh := List( gensgp, g -> Image( h, g ) ); 
        eimh := List( imh, g -> ExtRepOfObj(g) ); 
        AppendTo( out,"[ ",egenr,",\n  ",eimt,",\n  ",eimh," ],\n" ); 
    od; 
    Print( "#I Edit last line of .../xmod/lib/nn.kk.out to end with ] ] ] ] ]\n" ); 
    return C;
end );

##############################################################################
##
#M  Cat1RepresentativesToFile . . . . . . . . . . . . . . . . . . for a group
##
InstallMethod( Cat1RepresentativesToFile, "construct potential idempotents", 
    true, [ IsGroup ], 0,
function( gp )

    local  gpid, size, num, name, path, rname, rout, iname, iout, gens, ngens, 
           aut, reps, nreps, keep, ireps, i, R, genR, a, igenR, aR, pos, 
           nireps, idems, quot, autR, q, b, idem, j, k, id, mgi, nidems, GHBI;  

    gpid := IdGroup( gp ); 
    size := gpid[1]; 
    num := gpid[2]; 
    gens := GeneratorsOfGroup( gp ); 
    ngens := Length( gens ); 
    path := DirectoriesPackageLibrary( "xmod", "lib" )[1]; 
    gp := SmallGroup( size, num ); 
    rname := Concatenation( String(size), ".", String(num), ".rep" ); 
    rout := Filename( path, rname ); 
    ## find isomorphism classes of subgroups 
    if not IsPcGroup( gp ) then 
        return fail; 
    fi; 
    aut := AutomorphismGroup( gp ); 
    reps := List( ConjugacyClassesSubgroups(gp), c -> Representative(c) ); 
    nreps := Length( reps ); 
    Print( "nreps = ", nreps, "\n" ); 
    keep := ListWithIdenticalEntries( nreps, 1 ); 
    ireps := [ ]; 
    for i in [1..nreps] do 
        if ( keep[i] = 1 ) then 
            Info( InfoXMod, 2, " rep number: ", i ); 
            R := reps[i]; 
            Add( ireps, R ); 
            genR := GeneratorsOfGroup( R ); 
            for a in aut do 
                igenR := List( genR, g -> Image( a, g ) ); 
                aR := Subgroup( gp, igenR ); 
                pos := Position( reps, aR ); 
                if ( not( pos = fail ) and ( pos > i ) ) then 
                    keep[pos] := 0; 
                fi; 
            od; 
        fi; 
    od; 
    nireps := Length( ireps ); 
    if ( InfoLevel( InfoXMod ) > 1 ) then 
        Print( nreps, " reps reduced to ", nireps, "\n" ); 
        for i in [1..nireps] do 
            Print( ireps[i], " <--> ", StructureDescription(ireps[i]), "\n" ); 
        od; 
    else 
        PrintListOneItemPerLine( ireps ); 
    fi; 
    PrintTo( rout, "ireps := ", ireps, ";\n" ); 
    iname := Concatenation( String(size), ".", String(num), ".ids" ); 
    iout := Filename( path, iname ); 
    PrintTo( iout, "GHBI := GroupHomomorphismByImages;\n" ); 
    AppendTo( iout, "idems := [ \n" ); 
    Print( "Remember to edit the file ", rname, ",\n" ); 
    Print( "changing the first entry in ireps to TrivialSubgroup(gp).\n" ); 
    return nireps; 
end ); 

##############################################################################
##
#M  Cat1IdempotentsToFile . . . . . . . . . . . . . . . . . . . . for a group
##
InstallMethod( Cat1IdempotentsToFile, "construct potential idempotents", true, 
    [ IsGroup, IsList, IsPosInt, IsPosInt ], 0,
function( gp, ireps, fst, lst )

    local  gpid, size, num, path, iname, iout, gens, ngens, aut, 
           reps, ist, nreps, keep, i, R, genR, a, igenR, aR, pos, nireps, 
           quot, autR, q, b, idem, j, k, id, mgi, nidems, GHBI;  

    gpid := IdGroup( gp ); 
    size := gpid[1]; 
    num := gpid[2]; 
    gens := GeneratorsOfGroup( gp ); 
    ngens := Length( gens ); 
    path := DirectoriesPackageLibrary( "xmod", "lib" )[1]; 
    iname := Concatenation( String(size), ".", String(num), ".ids" ); 
    iout := Filename( path, iname ); 
    id := One( gp ); 
    GHBI := GroupHomomorphismByImages;
    nireps := Length( ireps ); 
    if ( lst >= nireps ) then 
        ist := nireps - 1; 
    else 
        ist := lst; 
    fi; 
    nidems := 0; 
    Info( InfoXMod, 2, "determining idempotents in range ", [fst,ist] ); 
    for i in [fst..ist] do 
        idem := [ ]; 
        R := ireps[i]; 
        Info( InfoXMod, 2, "subgroup number: ", i, ", ", 
                            StructureDescription( R ) ); 
        quot := GQuotients( gp, R ); 
        autR := AutomorphismGroup( R ); 
        Info( InfoXMod, 2, "[quot,autR] has sizes ", 
                           [ Length(quot), Size(autR) ] ); 
        for q in quot do 
            for a in autR do 
                b := q*a; 
                if ( b*b = b ) then 
                    pos := Position( idem, b ); 
                    if ( pos = fail ) then 
                        Add( idem, b ); 
                    fi; 
                fi; 
            od;
        od; 
        ## now convert entries in idem back to endomorphisms of gp 
        ## GQuotients may have picked an alternative generating set for gp 
        for j in [1..Length(idem)] do 
            mgi := List( gens, g -> Image( idem[j], g ) ); 
            idem[j] := GroupHomomorphismByImages( gp, gp, gens, mgi ); 
        od;  
        Print( "# idempotents = ", Length( idem ), "\n" ); 
        if ( InfoLevel( InfoXMod ) >= 2 ) then 
            PrintListOneItemPerLine( idem ); 
        fi; 
        if ( idem = [ ] ) then 
            AppendTo( iout, "[ ]" ); 
        else 
            AppendTo( iout, "[ " ); 
            for j in [1..Length( idem )] do 
                mgi := MappingGeneratorsImages( idem[j] )[2];  
                AppendTo( iout, "GHBI( gp, gp, gens, [" ); 
                for k in [1..ngens] do 
                    if ( mgi[k] = id ) then 
                        AppendTo( iout, "id" ); 
                    else 
                        AppendTo( iout, mgi[k] ); 
                    fi; 
                    if ( k < ngens ) then 
                        AppendTo( iout, "," ); 
                    else 
                        AppendTo( iout, "] )" ); 
                    fi; 
                od; 
                if ( j = Length(idem) ) then 
                    AppendTo( iout, " ]" ); 
                else 
                    AppendTo( iout, ",\n" ); 
                fi; 
            od; 
        fi; 
        AppendTo( iout, ",\n" ); 
        if ( i = nireps ) then 
            AppendTo( iout, "GHBI( gp, gp, gens, gens ) ] ];\n" ); 
        fi; 
    nidems := nidems + Length( idem ); 
    od; 
    return nidems; 
end ); 

##############################################################################
##
#M  CollectPartsAlreadyDone . . . . . . . . . . . . . . . . . . . for a group
##
InstallMethod( CollectPartsAlreadyDone, "preparation for AllCat1sInParts", 
    true, [ IsGroup, IsPosInt, IsPosInt, IsList ], 0,
function( gp, nn, kk, range )

    local  C0, j, C1, Clen, Cj, gpj, iso, Rj, res, ok, mor; 

    C0 := List( range, j -> Cat1Select(nn,kk,j) ); 
    for j in C0 do 
        Display(j); 
    od; 
    C1 := ShallowCopy( C0 );
    Clen := Length( C0 ); 
    for j in [1..Clen] do 
        Cj := C0[j]; 
        gpj := Source( Cj ); 
        iso := IsomorphismGroups( gpj, gp );
        Rj := Range( Cj ); 
        res := RestrictionMappingGroups( iso, Rj, gp );
        res := RestrictionMappingGroups( iso, Rj, Image(res) ); 
        ok := IsBijective( iso ) and IsBijective( res ); 
        mor := PreCat1IsomorphismByIsomorphisms( Cj, iso, res ); 
        C1[j] := Range( mor ); 
    od; 
    return C1; 
end ); 

##############################################################################
##
#M  AllCat1sInParts . . . . . . . . . . . . . . . . . . . . . . . for a group
##
InstallMethod( AllCat1sInParts, "construct all cat1-groups on a given group", 
    true, [ IsGroup, IsList, IsList, IsList, IsList ], 0,
function( gp, ireps, idems, range, C )

    local  gpid, size, num, gens, ngens, path, name, out, id, Cnum, Cnum0, 
           i, R, j, nireps, fst, lst, ist, single, sj, sk, 
           numirng, idemi, inumi, h, kerh, k, t, kert, kerth, CC, g, AICG, 
           isocg, bdy, kerbdy, z, gens1, list1, len1, gens2, list2, len2, 
           struc, gensgp, egensgp, a, genr, egenr, imt, eimt, imh, eimh; 

    gpid := IdGroup( gp ); 
    size := gpid[1]; 
    num := gpid[2]; 
    gens := GeneratorsOfGroup( gp ); 
    ngens := Length( gens ); 
    path := DirectoriesPackageLibrary( "xmod", "lib" )[1]; 
    name := Concatenation( String(size), ".", String(num), ".out" ); 
    out := Filename( path, name ); 
    id := One( gp ); 
    gens := GeneratorsOfGroup( gp ); 
    gensgp := SmallGeneratingSet( gp ); 
    egensgp := List( gensgp, g -> ExtRepOfObj(g) ); 
    nireps := Length( ireps ); 
    fst := range[1]; 
    lst := range[2]; 
    if ( lst > nireps ) then 
        ist := nireps; 
    else 
        ist := lst; 
    fi; 
    single := ( ( fst = ist ) and ( Length( range ) = 4 ) ); 
    if single then 
        sj := range[3];  sk := range[4]; 
    else 
        sj := 1;  sk := 0; 
    fi; 
    Cnum := Length( C ); 
    Cnum0 := Cnum; 
    for i in [fst..ist] do 
        R := ireps[i];
        idemi := idems[i]; 
        inumi := Length( idemi ); 
        if not single then 
            sk := inumi; 
        fi;  
        for j in [sj..sk] do
            h := idemi[j]; 
            kerh := Kernel( h );
            # (e;t,h:G->R) isomorphic to (e;h,t:G->R), so take k>=j
            for k in [j..inumi] do 
                Print( "\n[i,j,k] = ", [i,j,k], "\n" ); 
                t := idemi[k]; 
                kert := Kernel( t );
                kerth := CommutatorSubgroup( kert, kerh ); 
                if ( Size( kerth ) = 1 ) then 
                    if ( InfoLevel( InfoXMod ) > 1 ) then 
                        Print( "t : ", MappingGeneratorsImages(t)[2], "\n",  
                               "h : ", MappingGeneratorsImages(h)[2], "\n" ); 
                        fi; 
                    CC := PreCat1ByEndomorphisms( t, h ); 
                    if not IsCat1( CC ) then 
                        Error( "not a cat1-group" ); 
                    fi; 
                    g := Cnum; 
                    AICG := false;
                    while ( not AICG and ( g > 0 ) ) do
                        #? is this expensive? 
                        isocg := IsomorphismPreCat1s( C[g], CC ); 
                        AICG := not ( isocg = fail ); 
                        g := g-1;
                    od;
                    if ( AICG = false ) then
                        Add( C, CC ); 
                        Cnum := Cnum + 1; 
                        Info( InfoXMod, 2, "C[", Cnum, "] = ", CC ); 
                    else 
                        ## maybe CC is a simpler construction than C[g] ? 
                        gens1 := GeneratorsOfGroup( Range( CC ) ); 
                        list1 := Flat( List( gens1, g -> ExtRepOfObj(g) ) ); 
                        for z in [1..Length(list1)] do 
                            if IsOddInt(z) then list1[z] := 0; fi; 
                        od; 
                        len1 := Sum( list1 );
                        g := g+1; 
                        Info( InfoXMod, 2, "isomorphic to C[g], g = ", g ); 
                        gens2 := GeneratorsOfGroup( Range( C[g] ) ); 
                        list2 := Flat( List( gens2, g -> ExtRepOfObj(g) ) ); 
                        for z in [1..Length(list2)] do 
                            if IsOddInt(z) then list2[z] := 0; fi; 
                        od; 
                        len2 := Sum( list2 ); 
                        Info( InfoXMod, 2, "len1 = ", len1, ",  len2 = ", 
                                            len2, " at [i,j,k] = ", [i,j,k] ); 
                        if ( len1 < len2 ) then 
                            C[g] := CC;                             
                            Info( InfoXMod, 2, "swapping due to lengths:"); 
                            Info( InfoXMod, 2, "gens1 = ", gens1 ); 
                            Info( InfoXMod, 2, "gens2 = ", gens2 ); 
                        elif ( len1 = len2 ) then 
                            # check no. generators fixed by tail map 
                            len1 := 0; 
                            for z in gens1 do 
                                if ( Image(TailMap(CC),z) = z ) then 
                                    len1 := len1 + 1; 
                                fi; 
                            od;
                            len2 := 0; 
                            for z in gens2 do 
                                if ( Image(TailMap(CC),z) = z ) then 
                                    len2 := len2 + 1; 
                                fi; 
                            od; 
                            if ( len1 < len2 ) then 
                                C[g] := CC;                             
                                Info( InfoXMod, 2, 
                                      "swapping due to fixed points" ); 
                                Info( InfoXMod, 2, "gens1 = ", gens1 ); 
                                Info( InfoXMod, 2, "gens2 = ", gens2 ); 
                            fi; 
                        fi; 
                    fi; 
                fi;
            od;
        od;
        if ( InfoLevel( InfoXMod ) > 2 ) then 
        Print( "\nvvvvv  when i = ", i, " Cnum = ", Cnum, "\n" ); 
            for j in [1..Cnum] do
                CC := C[j];
                Print( "Isomorphism class ", j, "\n" );
                if ( Size( Kernel( CC ) ) < 101 ) then
                    Print( ": kernel(tail) = ", IdGroup( Kernel( CC) ), "\n" );
                else
                    Print( ": size(kernel) = ", Size( Kernel( CC ) ), "\n" );
                fi;
                if ( Size( Range( CC ) ) < 101 ) then
                    Print( ":  range group = ", IdGroup( Range( CC ) ), "\n" );
                else
                    Print( ": range has size = ", Size( Range( CC ) ), "\n" );
                fi;
                bdy := Boundary( CC );
                kerbdy := Kernel( bdy );
                if ( kerbdy <> Kernel( CC ) ) then
                    if ( Size( kerbdy ) < 101 ) then
                        Print( ": ker(bdy) = ", IdGroup( kerbdy ), "\n" );
                    else
                        Print( ": ker(bdy) has size = ", Size( kerbdy ), "\n" );
                    fi;
                fi;
            Print( "\n" );
            od; 
            Print( "^^^^^  when i = ", i, " Cnum = ", Cnum, "\n" ); 
        fi; 
    od; 
    ##  now convert to the form required in the cat1data.g file 
    if ( fst = 1 ) then 
        struc := StructureDescription( gp ); 
        PrintTo( out, "[",size,",",num,",\"",struc,"\",",egensgp,",[\n" ); 
    fi; 
    for j in [(Cnum0+1)..Cnum] do 
        a := C[j]; 
        genr := SmallGeneratingSet( Range(a) ); 
        egenr := List( genr, g -> ExtRepOfObj(g) ); 
        t := TailMap( a ); 
        imt := List( gensgp, g -> Image( t, g ) ); 
        eimt := List( imt, g -> ExtRepOfObj(g) ); 
        h := HeadMap( a ); 
        imh := List( gensgp, g -> Image( h, g ) ); 
        eimh := List( imh, g -> ExtRepOfObj(g) ); 
        AppendTo( out,"[ ",egenr,",\n  ",eimt,",\n  ",eimh," ],\n" ); 
    od; 
    if ( ist = nireps ) then  
        Print( "Delete the final cat1-group in the size.num.out file\n" ); 
        Print( "Edit the last line of the file to end with ] ] ] ] ]\n" ); 
    fi; 
    return C;
end );

##############################################################################
##
#M  MakeAllCat1s . . . . . . . . . . . . . . . . . for three positive integers
##
InstallMethod( MakeAllCat1s, "construct all cat1-groups of a chosen order", 
    true, [ IsPosInt, IsPosInt, IsPosInt ], 0,
function( n, fst, lst )

    local  sgp, gensgp, egensgp, all, len, i, a, t, h, gens, fam, genr, egenr, 
           num, j, k, imt, eimt, imh, eimh, path, name, out, struc; 
    path := DirectoriesPackageLibrary("xmod","lib")[1]; 
    name := Concatenation( String(n), ".out" ); 
    out := Filename( path, name ); 
    num := NumberSmallGroups( n ); 
    if ( lst > num ) then 
        lst := num; 
    fi; 
    for j in [fst..lst] do 
        sgp := SmallGroup( n, j ); 
        struc := StructureDescription( sgp ); 
        Print( struc, "\n" ); 
        if IsPermGroup( sgp ) then 
            Print( "#I  only works for PcGroups at present" ); 
            gensgp := GeneratorsOfGroup( sgp ); 
            AppendTo( out, "[",n,",",j,",\"",struc,"\",",gensgp,",[\n" ); 
        else 
            gensgp := SmallGeneratingSet( sgp ); 
            fam := FamilyObj( gensgp[1] ); 
            egensgp := List( gensgp, g -> ExtRepOfObj(g) ); 
            Print( "small group with n = ", n, ", j = ", j, "\n" ); 
            Print( "representatives of isomorphism classes of subgroups:\n" ); 
            if ( j = fst ) then 
                PrintTo( out, "[",n,",",j,",\"",struc,"\",",egensgp,",[\n" ); 
            else 
                AppendTo(out, "[",n,",",j,",\"",struc,"\",",egensgp,",[\n" ); 
            fi; 
            all := AllCat1sBasic( sgp ); 
            len := Length( all ); 
            if ( (len = 1) or ( (len = 2) and IsCommutative( sgp ) ) ) then 
                AppendTo( out, " ] ],\n" ); 
            fi; 
            if IsCommutative( sgp ) then 
                k := 2; 
            else 
                k := 1; 
            fi;
            for i in [k..(len-1)] do 
                a := all[i]; 
                genr := SmallGeneratingSet( Range(a) ); 
                egenr := List( genr, g -> ExtRepOfObj(g) ); 
                t := TailMap( a ); 
                imt := List( gensgp, g -> Image( t, g ) ); 
                eimt := List( imt, g -> ExtRepOfObj(g) ); 
                h := HeadMap( a ); 
                imh := List( gensgp, g -> Image( h, g ) ); 
                eimh := List( imh, g -> ExtRepOfObj(g) ); 
                AppendTo( out,"[ ",egenr,",\n  ",eimt,",\n  ",eimh," ]" ); 
                if ( i < len-1 ) then 
                    AppendTo( out, ",\n" ); 
                else 
                    AppendTo( out, " ] ],\n" ); 
                fi; 
            od; 
        fi; 
    od; 
    return all; 
end );

############################################################################## 
##  the following was placed in removed.gi, then brought back here 27/09/12 ## 
############################################################################## 

##############################################################################
##
#M  EndomorphismClassObj( <nat>, <iso>, <aut>, <conj> ) . . . . . make a class
##
InstallMethod( EndomorphismClassObj,
  "generic method for an endomorphism class", true,
  [ IsGroupHomomorphism, IsGroupHomomorphism, IsGroupOfAutomorphisms, IsList ],
  0,
function( nat, iso, aut, conj )

    local  filter, fam, class;

    fam := FamilyObj( [ nat, iso, aut, conj ] );
    filter := IsEndomorphismClassObj;
    class := rec(); 
    ObjectifyWithAttributes( class, NewType( fam, filter ), 
      IsEndomorphismClass, true,
      EndoClassNaturalHom, nat,
      EndoClassIsomorphism, iso,
      AutoGroup, aut,
      EndoClassConjugators, conj );
    return class;
end );

#############################################################################
##
#M  Display . . . . . . . . . . . . . . . . . . . . . for endomorphism classes
##
InstallMethod( Display, "generic method for endomorphism classes",
    true, [ IsEndomorphismClassObj ], 0,
function( class )
    Print( "class of group endomorphisms with\n" );
    Print( "natural hom: " );
    ViewObj( EndoClassNaturalHom( class ) );
    Print( "\n" );
    Print( "isomorphism: " );
    ViewObj( EndoClassIsomorphism( class ) );
    Print( "\n" );
    Print( "autogp gens: ", GeneratorsOfGroup( AutoGroup( class ) ), "\n" );
    Print( "conjugators: ", EndoClassConjugators( class ), "\n" );
end );

#############################################################################
##
#M  ZeroEndomorphismClass( <G> )
##
InstallMethod( ZeroEndomorphismClass, "generic method for a group",
    true, [ IsGroup ], 0, 
function( G )

    local  Q, nat, iso, aut, conj, idgp;

    Q := Group( One( G ) );
    nat := MappingToOne( G, Q );
    iso := IdentityMapping( Q );
    aut := Group( iso );
    SetIsAutomorphismGroup( aut, true );
    conj := [ One( G ) ];
    return EndomorphismClassObj( nat, iso, aut, conj );
end );

#############################################################################
##
#M  AutomorphismClass( <G> )
##
InstallMethod( AutomorphismClass, "generic method for a group", true,
    [ IsGroup ], 0, 
function( G )

    local  iso, aut, conj;

    aut := AutomorphismGroup( G );
    iso := InclusionMappingGroups( G, G );
    conj := [ One( G ) ];
    return EndomorphismClassObj( iso, iso, aut, conj );
end );

#############################################################################
##
#M  NontrivialEndomorphismClasses( <G> )
##
InstallMethod( NontrivialEndomorphismClasses, "generic method for a group",
    true, [ IsGroup ], 0, 
function( G )

    local  nargs, valid, case, switch, oldcase, normG, rcosG, N, nnum,
           natG, quotG, genG, oneG, Qnum, ccs, reps, cnum, Q, R, iso, L, im,
           phi, nat, proj, comp, normal, cosets, conj, Cnum, g, gim, zero,
           i, j, k, l, Lnum, aut, idgp, Ecl, Enum, classes, class, name, ok;

    genG := GeneratorsOfGroup( G );
    oneG := One( G );
    idgp := Subgroup( G, [ One(G) ] );
    # determine non-monomorphic endomorphisms by kernel
    normG := NormalSubgroups( G );
    nnum := Length( normG );
    ccs := ConjugacyClassesSubgroups( G );
    cnum :=Length( ccs );
    reps := List( ccs, c -> Representative( c ) );
    rcosG := List( normG, N -> RightCosets( G, N ) );
    natG := List( normG, N -> NaturalHomomorphismByNormalSubgroup( G, N ) );
    quotG := List( natG, n -> Image( n ) );
    normal := List( reps, R -> ( Normalizer( G, R ) = G ) );
    classes := [ ];
    for i in [2..(nnum-1)] do
        N := normG[i];
        Q := quotG[i];
        proj := natG[i];
        aut := AutomorphismGroup( Q );
        for j in [2..(cnum-1)] do
            R := reps[j];
            ok := ( Intersection( N, R ) = idgp );
            iso := IsomorphismGroups( Q, R );
            if not ( iso = fail ) then
                # (unnecessary?) check that this gives a homomorphism
                comp := CompositionMapping( iso, proj );
                im := List( genG, x -> Image( comp, x ) );
                phi := GroupHomomorphismByImages( G, R, genG, im );
                if not IsGroupHomomorphism( phi ) then
                    Error( "phi not a homomorphism" );
                fi;
                if not normal[j] then
                    cosets := RightCosets( G, Normalizer( G, R ) );
                    conj := List( cosets, c -> Representative( c ) );
                else
                    conj := [ oneG ];
                fi;
                class := EndomorphismClassObj( proj, iso, aut, conj );
                Add( classes, class );
            fi;
        od;
    od;
    return classes;
end );

#############################################################################
##
#M  NonIntersectingEndomorphismClasses( <G> )
##
InstallMethod( NonIntersectingEndomorphismClasses,
    "generic method for a group", true, [ IsGroup ], 0, 
function( G )

    local  nargs, valid, case, switch, oldcase, normG, rcosG, N, nnum,
           natG, quotG, genG, oneG, Qnum, ccs, reps, cnum, Q, R, iso, L, im,
           phi, nat, proj, comp, normal, cosets, conj, Cnum, g, gim, zero,
           i, j, k, l, Lnum, aut, idgp, Ecl, Enum, classes, class, name, ok;

    genG := GeneratorsOfGroup( G );
    oneG := One( G );
    idgp := Subgroup( G, [ One(G) ] );
    # determine non-monomorphic endomorphisms by kernel
    normG := NormalSubgroups( G );
    nnum := Length( normG );
    ccs := ConjugacyClassesSubgroups( G );
    cnum :=Length( ccs );
    reps := List( ccs, c -> Representative( c ) );
    rcosG := List( normG, N -> RightCosets( G, N ) );
    natG := List( normG, N -> NaturalHomomorphismByNormalSubgroup( G, N ) );
    quotG := List( natG, n -> Image( n ) );
    normal := List( reps, R -> ( Normalizer( G, R ) = G ) );

    classes := [ ];
    for i in [2..(nnum-1)] do
        N := normG[i];
        Q := quotG[i];
        proj := natG[i];
        aut := AutomorphismGroup( Q );
        for j in [2..(cnum-1)] do
            R := reps[j];
            ok := ( Intersection( N, R ) = idgp );
            if ok then
                iso := IsomorphismGroups( Q, R );
            fi;
            if ( ok and not ( iso = fail ) ) then
                # (unnecessary?) check that this gives a homomorphism
                comp := CompositionMapping( iso, proj );
                im := List( genG, x -> Image( comp, x ) );
                phi := GroupHomomorphismByImages( G, R, genG, im );
                if not IsGroupHomomorphism( phi ) then
                    Error( "phi not a homomorphism" );
                fi;
                if not normal[j] then
                    cosets := RightCosets( G, Normalizer( G, R ) );
                    conj := List( cosets, c -> Representative( c ) );
                else
                    conj := [ oneG ];
                fi;
                class := EndomorphismClassObj( proj, iso, aut, conj );
                Add( classes, class );
            fi;
        od;
    od;
    return classes;
end );

##############################################################################
##
#F  EndomorphismClasses                       finds all homomorphisms  G -> G
##
InstallGlobalFunction( EndomorphismClasses, 
function( arg )
    
    local  nargs, valid, G, case, classes, auts, ends, zero, disj;

    nargs := Length( arg );
    G := arg[1];
    valid := ( IsGroup( G ) and ( nargs = 2 ) and ( arg[2] in [0..3] ) );
    if not valid then
         Print( "\nUsage:  EndomorphismClasses( G [, case] );\n" );
         Print( " choose  case = 1  to include automorphisms & zero,\n" );
         Print( "default  case = 2  to exclude automorphisms & zero,\n" );
         Print( "         case = 3  when  N meet H  trivial,\n" );
         return fail;
    fi;
    if ( Length( arg ) = 1 ) then
        case := 2;
    else
        case := arg[2];
    fi;
    ends := NontrivialEndomorphismClasses( G );
    if ( case = 1 ) then
        zero := ZeroEndomorphismClass( G );
        auts := AutomorphismClass( G );
    fi;
    if ( case = 2 ) then
        classes := ends;
    elif ( case = 1 ) then
        classes := Concatenation( [auts], ends, [zero] );
    else
        classes := NonIntersectingEndomorphismClasses( G );
    fi;
    return classes;
end );

#############################################################################
##
#M  EndomorphismImages         finds all homomorphisms  G -> G  by converting
##                               EndomorphismClasses into a list of genimages
##
InstallMethod( EndomorphismImages,
    "generic method for a list of endomorphism classes", true, [ IsList ], 0,
function( classes )

    local  clnum, G, genG, Q, autos, rho, psi, phi, L, Lnum, LR, k, l,
           im, g, gim, i, c, nat, iso, aut, conj, cjnum, comp, R;

    if ( ( classes = [] ) or not IsEndomorphismClass( classes[1] ) ) then
        Error( "usage: EndomorphismImages( <list of endo classes> );" );
    fi;
    c := classes[1];
    nat := EndoClassNaturalHom( c );
    G := Source( nat );
    genG := GeneratorsOfGroup( G );
    clnum := Length( classes );
    L := [ ];
    for i in [1..clnum] do
        c := classes[i];
        nat := EndoClassNaturalHom( c );
        iso := EndoClassIsomorphism( c );
        aut := AutoGroup( c );
        conj := EndoClassConjugators( c );
        comp := CompositionMapping( iso, nat );
        R := Image( comp );
        cjnum := Length( conj );
        Q := Image( nat );
        autos := Elements( aut );
        LR := [ ];
        for k in [ 1..Length( autos ) ] do
            rho := autos[k];
            psi := nat * rho * iso;
            im := List( genG, x -> Image( psi, x ) );
            Add( LR, im );
        od;
        Lnum := Length( LR );
        for k in [2..cjnum] do
            g := conj[k];
            for l in [1..Lnum] do
                im := LR[l];
                gim := List( im, x -> x^g );
                Add( LR, gim );
            od;
        od;
        L := Concatenation( L, LR ); 
    od;
    return L;
end );

###############################################################################
##
#M  IdempotentImages          finds all homomorphisms  h : G -> G  with h^2 = h
##                            by converting a list of endomorphism classes 
##                            into a list of genimages, using the notation 
##                            of Alp/Wensley IJAC 2000, section 4.4. 
##  
InstallMethod( IdempotentImages,
    "generic method for a list of endomorphism classes", true, [ IsList ], 0,
function( classes )

    local  R, genR, Q0, Q, alpha0, psi, phi, L, L1, L2, k, im, im2, 
           psi2, c, cim, i, cl, nu, theta, autQ, conj, cjnum;

    if ( classes = [] ) then 
        return [ ]; 
    fi; 
    if not IsEndomorphismClass( classes[1] ) then
        Error( "usage: EndomorphismImages( <list of endo classes> );" );
    fi;
    cl := classes[1];
    nu := EndoClassNaturalHom( cl );
    R := Source( nu );
    genR := GeneratorsOfGroup( R ); 
    L := [ ];
    for cl in classes do 
        nu := EndoClassNaturalHom( cl );
        theta := EndoClassIsomorphism( cl );
        autQ := AutoGroup( cl );
        conj := EndoClassConjugators( cl ); 
        psi := CompositionMapping( theta, nu );
        Q := Image( psi ); 
        Q0 := Image( nu ); 
        L1 := [ ];
        for alpha0 in autQ do
            psi := nu * alpha0 * theta;
            im := List( genR, x -> Image( psi, x ) );
            Add( L1, im );
        od; 
        L2 := [ ]; 
        for c in conj do 
            for im in L1 do 
                cim := List( im, x -> x^c ); 
                psi2 := GroupHomomorphismByImages( R, R, genR, cim );
                im2 := List( cim, x -> Image( psi2, x ) ); 
                if ( cim = im2 ) then
                    Add( L2, cim );
                fi;
            od;
        od;
        L := Concatenation( L, L2 ); 
    od;
    return L; 
end ); 

#############################################################################
##
#E  cat1data.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
