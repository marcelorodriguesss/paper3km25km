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

static char *months[]={"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                      "Oct","Nov","Dec"};
char *strdate(date)
  int date;
{
  static char sdate[14];
  int yr,mo,da;

  yr = date/10000;
  mo = (date%10000) /100;
  da = date % 100;

  sprintf(sdate,"%s %2d, %4d",months[mo-1],da,yr);
  return(sdate);
}

char *strtime(time)
  int time;
{
  static char stime[14];
  int hh,mm,ss;

  hh = time/10000;
  mm = (time%10000) / 100;
  ss = time % 100;

  sprintf(stime,"%02d:%02d",hh,mm);
  return(stime);
}



int timeCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[20];
  int i;
   
  if (RAMS_NOTAVALIABLE) return(TCL_OK); /* No RAMS Avaliable */

  strcpy(buffer,Tcl_GetVar(interp,"timeslideval",SFLG));

  RAMS.file = atoi(buffer);
  sprintf(par.ctime,"%d",RAMS.file);

  Tcl_SetVar(main_interp,"dateval",strdate(RAMS.dates[RAMS.file-1]) ,SFLG);

  Tcl_SetVar(main_interp,"timeval",strtime(RAMS.times[RAMS.file-1]) ,SFLG);
  newtime();

  return(TCL_OK);
}
