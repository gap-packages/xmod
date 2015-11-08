#############################################################################
##
#W  read.g                 The XMOD package                     Chris Wensley
#W                                                                & Murat Alp
##  version 2.43, 07/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
##

## read the actual code 

ReadPackage( "xmod", "lib/util.gi" ); 
ReadPackage( "xmod", "lib/dom2d3d.gi" ); 
ReadPackage( "xmod", "lib/map2d3d.gi" ); 
ReadPackage( "xmod", "lib/gp2obj.gi" ); 
ReadPackage( "xmod", "lib/gp2map.gi" ); 
ReadPackage( "xmod", "lib/gp2up.gi" );
ReadPackage( "xmod", "lib/gp2act.gi" );
ReadPackage( "xmod", "lib/gp2ind.gi" );
ReadPackage( "xmod", "lib/map2arg.gi" );
ReadPackage( "xmod", "lib/isoclinic.gi" ); 
ReadPackage( "xmod", "lib/gp3obj.gi" ); 
ReadPackage( "xmod", "lib/gp3map.gi" ); 
ReadPackage( "xmod", "lib/gpd2obj.gi" ); 
ReadPackage( "xmod", "lib/cat1data.gi" ); 

## define some directories 
##  XMod_dir := Filename( DirectoriesPackageLibrary( "XMod", "" ), "" ); 
##  XMod_doc := Filename( DirectoriesPackageLibrary( "XMod", "doc" ), "" ); 
##  XMod_lib := Filename( DirectoriesPackageLibrary( "XMod", "lib" ), "" ); 
##  XMod_tst := Filename( DirectoriesPackageLibrary( "XMod", "tst" ), "" ); 
##  XMod_make_doc := Concatenation( XMod_dir, "makedocrel.g" ); 
