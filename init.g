#############################################################################
##
#W  init.g                 GAP package `XMod'                   Chris Wensley
#W 
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  
##

if not IsBound( PreImagesRepresentativeNC ) then 
    BindGlobal( "PreImagesRepresentativeNC", PreImagesRepresentative ); 
fi; 

##  read the function declarations

ReadPackage( "xmod", "lib/util.gd" );
ReadPackage( "xmod", "lib/dom2dnd.gd" );
ReadPackage( "xmod", "lib/map2dnd.gd" );
ReadPackage( "xmod", "lib/gp2obj.gd" ); 
ReadPackage( "xmod", "lib/gp2map.gd" ); 
ReadPackage( "xmod", "lib/gp2up.gd" );
ReadPackage( "xmod", "lib/gp2act.gd" );
ReadPackage( "xmod", "lib/gp2ind.gd" );
ReadPackage( "xmod", "lib/gpd2obj.gd" ); 
ReadPackage( "xmod", "lib/isoclinic.gd" );  
ReadPackage( "xmod", "lib/hap.gd" );
ReadPackage( "xmod", "lib/double.gd" );
ReadPackage( "xmod", "lib/gpnobj.gd" );
ReadPackage( "xmod", "lib/gpnmap.gd" );
ReadPackage( "xmod", "lib/gp3obj.gd" ); 
ReadPackage( "xmod", "lib/gp3map.gd" ); 
ReadPackage( "xmod", "lib/gp4obj.gd" ); 
ReadPackage( "xmod", "lib/apps.gd" );
ReadPackage( "xmod", "lib/gpgpd.gd" ); 
