#############################################################################
##
#W  gpd2obj.g                XMOD example files                 Chris Wensley
##
##  version 2.43, 21/10/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod example file gpd2obj.g (version 18/09/15) :-");
Print("\nTHIS EXAMPLE FAILS IN GAP >= 4.6"); 
Print("\ntesting functions for crossed modules of groupoids\n\n");

SetInfoLevel( InfoXMod, 2 );
SetInfoLevel( InfoGpd, 2 ); 

d8 := Group( (1,2,3,4), (1,3) );;
SetName( d8, "d8" );
Gd8 := SinglePieceGroupoid( d8, [-9,-8,-7] );
Display( Gd8 ); 

k4 := Subgroup( d8, [ (1,2)(3,4), (1,3)(2,4) ] ); 
PX0 := DiscreteNormalPreXModWithObjects( Gd8, k4 ); 
Print( "PX0 = ", PX0, "\n" ); 

##############################################################################
##
#E  gpd2obj.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
