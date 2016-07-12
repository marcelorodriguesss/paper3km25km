/*############################ Change Log ##################################
! 2.3.1.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!#########################################################################*/

#include <stdio.h> 
#include <tk.h>
#include "ringi.h"

extern Params par;
extern RAMSinfo RAMS;

int plotCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  if (RAMS_AVALIABLE) update_plot(interp,0,1);

  /*Tcl_Eval(interp,".top.unzoom configure -state disabled");*/

  return(TCL_OK);
}

int saveCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
   int ibackgnd=1;

   if (RAMS_AVALIABLE)
   {
      startmeta(&ibackgnd);
      update_plot(interp,1,0);
      endmeta();
      return(TCL_OK);
   }
}

int cleanupCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
   /* Insert Callback code here. */

   return(TCL_OK);
}

int fileCB(clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
   char buffer[100], *ptr, *prefix;

     strcpy(buffer,argv);
     /* Look for RAMS prefix path+fileprefix */
     ptr = strrchr(buffer,'/');  /* find the last dir slash */
     ptr = strstr(ptr,".a");     /* look for the *.a* string */
     ptr += 2;
     *ptr = '\0';
     prefix = buffer;

     printf("Selected RAMS with prefix=\"%s\"\n",prefix);
     init_rams(prefix);

   /* Rams is loaded; cool eh? */
   return(TCL_OK);
}

int values (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
   char wid[6],hgt[6],x1[6],y1[6],x2[6],y2[6];
   /* fprintf(stderr,"values: %s %s\n",*(argv+1),*(argv+2)); */

   if (strncmp(*(argv+1),"poin",4) == 0) {
     sscanf(*(argv+2),"%s %s %s %s",wid,hgt,x1,y1);
     Tcl_SetVar(interp,"vwid",wid,SFLG);
     Tcl_SetVar(interp,"vhgt",hgt,SFLG);
     Tcl_SetVar(interp,"vx1",x1,SFLG);
     Tcl_SetVar(interp,"vy1",y1,SFLG);
     Tcl_SetVar(interp,"vx2","",SFLG);
     Tcl_SetVar(interp,"vy2","",SFLG);
   } else {
     sscanf(*(argv+2),"%s %s %s %s %s %s",wid,hgt,x1,y1,x2,y2);
     Tcl_SetVar(interp,"vwid",wid,SFLG);
     Tcl_SetVar(interp,"vhgt",hgt,SFLG);
     Tcl_SetVar(interp,"vx1",x1,SFLG);
     Tcl_SetVar(interp,"vy1",y1,SFLG);
     Tcl_SetVar(interp,"vx2",x2,SFLG);
     Tcl_SetVar(interp,"vy2",y1,SFLG);
   }

   return(TCL_OK);
}

/*======================================================================*/
