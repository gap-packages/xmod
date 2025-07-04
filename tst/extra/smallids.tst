#############################################################################
##
#W  smallids.tst                  XMOD test file                Chris Wensley
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 
##
##  compares the Cat1Group data in XMod and HAP - checks ids are the same 
##
gap> START_TEST( "XMod package: smallids.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> L := [ ];;
gap> for n in [1..CAT1_LIST_MAX_SIZE] do             
>        num := NumberSmallGroups( n );           
>        for i in [1..num] do                     
>            m := CAT1_LIST_NUMBERS[n][i];        
>            for j in [1..m] do                   
>                C := Cat1Select( n, i, j );      
>                C0 := Cat1GroupToHAP( C );       
>                idC0 := IdCatOneGroup( C0 );     
>                if ( idC0 <> [n,i,j] ) then      
>                    Add( L, [ n,i,j, idC0[3] ] );
>                fi; 
>            od; 
>        od;
>    od;
gap> L; 
[  ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "smallids.tst", 10000 );

#############################################################################
##
#E  smallids.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
