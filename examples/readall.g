##############################################################################
##
#W  readall.g                     GAP4 package `XMod'            Chris Wensley
#W             
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "xmod", false ); 
xmod_examples_dir := DirectoriesPackageLibrary( "xmod", "examples" ); 

Read( Filename( xmod_examples_dir, "gp2obj.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2map.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2up.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2act.g" ) ); 
Read( Filename( xmod_examples_dir, "gp2ind.g" ) ); 
Read( Filename( xmod_examples_dir, "gp3objmap.g" ) ); 
Read( Filename( xmod_examples_dir, "gpd2obj.g" ) ); 
Read( Filename( xmod_examples_dir, "sl25.g" ) ); 

