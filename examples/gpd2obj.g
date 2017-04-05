#############################################################################
##
#W  gpd2obj.g                XMOD example files                 Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod example file gpd2obj.g (version 05/04/17) :-");
Print("\ntesting functions for crossed modules of groupoids\n\n");
levelX := InfoLevel( InfoXMod );
SetInfoLevel( InfoXMod, 2 );
levelG := InfoLevel( InfoGpd); 
SetInfoLevel( InfoGpd, 2 ); 

d8 := Group( (1,2,3,4), (1,3) );;
SetName( d8, "d8" );
Gd8 := SinglePieceGroupoid( d8, [-9,-8,-7] );
Display( Gd8 ); 

k4 := Subgroup( d8, [ (1,2)(3,4), (1,3)(2,4) ] ); 
PX0 := DiscreteNormalPreXModWithObjects( Gd8, k4 ); 
Print( "PX0 = ", PX0, "\n" ); 

SetInfoLevel( InfoXMod, levelX );
SetInfoLevel( InfoGpd, levelG );
##############################################################################
##
#E  gpd2obj.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
