##############################################################################
##
#W  readall.g                     GAP4 package `XMod'            Chris Wensley
#W             
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "xmod", false ); 
xmod_examples_dir := DirectoriesPackageLibrary( "xmod", "examples" ); 

Read( Filename( xmod_examples_dir, "gp2obj.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2map.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2up.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2act.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2ind.g" ) ); 
Read( Filename( xmod_examples_dir, "isoclinic.g" ) ); 
Read( Filename( xmod_examples_dir, "gp3objmap.g" ) ); 
Read( Filename( xmod_examples_dir, "gpd2obj.g" ) ); 
Read( Filename( xmod_examples_dir, "sl25.g" ) ); 
## Read( Filename( xmod_examples_dir, "sl25pc.g" ) ); 
Print( "\n#I not running sl25pc.g because of errors there\n" );
