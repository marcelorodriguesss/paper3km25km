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
#include <strings.h>
#include "ringi.h"

int loadCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[120],*ptr, *prefix;
  static char *fsp[] = {"Loader","FileSel",""};
  /*Window rw,lw;*/
  int i;


fprintf(stderr,"In loadCB\n");fflush(stderr);
for (i=0; i<argc; i++)

  strcpy(buffer,*(argv+1));
  if (validprefix(buffer)) {
    /* Look for RAMS prefix path+fileprefix */
    ptr = strrchr(buffer,(int)'/'); /* find the last dir slash */
/*
 *     ptr = strstr(ptr,"-");
 *     ptr += 1;
 *     *ptr = '\0';
 */
    ptr += 6;
    *ptr ='\0';
    prefix = buffer;

    printf("Selected RAMS with prefix=\"%s\"\n",prefix);
    init_rams(prefix);
  }

  /* Rams is loaded; cool eh? */
  return(TCL_OK);
}

