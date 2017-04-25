##############################################################################
##
#W  cat1test.g                   GAP4 package `XMod'             Chris Wensley
#W             
#Y  Copyright (C) 2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##############################################################################

Print("\nXMod test file cat1test.g (version 23/04/17) :-");
Print("\nThis is an example of running a test over all cat1-groups,\n");
Print("or their associated crossed modules, in the database.\n");
Print("In this test we check whether the coproduct square of (S->R)\n"); 
Print("has as source the direct product SxS.\n\n"); 

m := CAT1_LIST_MAX_SIZE;
L := CAT1_LIST_CLASS_SIZES; 
count := 0; 
failures := 0; 
file := "examples/cat1test.log"; 
PrintTo( file, "Output from cat1test.g\n" ); 
for i in [1..m] do 
    n := L[i];  ## number of isomorphism classes of groups of size i 
    for j in [1..n] do 
        p := Cat1Select( i, j );  ## number of cat1-structures on group j 
        for k in [1..p] do 
            Print( [i,j,k], "\n" ); 
            C0 := Cat1Select( i, j, k ); 
            if not IsPerm2DimensionalGroup( C0 ) then 
                iso := IsomorphismPermObject( C0 ); 
                P0 := Image( iso ); 
                X0 := XModOfCat1Group( P0 ); 
            else 
                X0 := XModOfCat1Group( C0 );
            fi;
            ## now apply the required test to X0 
            XX0 := CoproductXMod( X0, X0 ); 
            S0 := Source( X0 ); 
            SS0 := Source( XX0 ); 
            f := IsomorphismGroups( DirectProduct(S0,S0), SS0 ); 
            if ( f = fail ) then 
                ## Error( "no isomorphism found in this case" ); 
                failures := failures + 1; 
                AppendTo( file, [i,j,k], " : ", StructureDescription(X0), 
                     "  -->  ", StructureDescription(XX0), "\n" ); 
            fi; 
            count := count + 1; 
        od;
    od;
od;
Print( count, " crossed modules processed\n" ); 
Print( failures, " is the number of cases when no isomorphism is found\n" ); 

#############################################################################
##
#E  cat1test.g  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
