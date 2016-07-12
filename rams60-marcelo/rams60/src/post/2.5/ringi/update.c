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
#include "post_sub_names.h"

static char buffer[240];

newmodel()
{
  extern char *vars[];
  extern int numvars;
  extern char *strdate();
  extern char *strtime();
  char buf[14];
  int i;

  /* Fill in the default variables avaliable */
  Tcl_Eval(main_interp,".top.parframe.frame4.var.m delete 0 100");
  update_varlist();
  for (i=0; i<numvars; i++) {
    strcpy(buffer,".top.parframe.frame4.var.m add command -command {set varval ");
    strcat(buffer,vars[i]);
    strcat(buffer,"; show_cont_set  $varval {1} } -label {");
    strcat(buffer,vars[i]);
    strcat(buffer,"}");
    Tcl_Eval(main_interp,buffer);
  }

  Tcl_SetVar(main_interp,"varval",vars[1],SFLG);
  Tcl_SetVar(main_interp,"vartypeval","3-D",SFLG);
  show_cont_set0(main_interp,vars[1],1);

  Tcl_Eval(main_interp,".top.parframe.sec_frame.var.m delete 0 100");
  for (i=0; i<numvars; i++) {
    strcpy(buffer,".top.parframe.sec_frame.var.m add command -command {set overvarval ");
    strcat(buffer,vars[i]);
    strcat(buffer,"; show_cont_set $overvarval {2} } -label {");
    strcat(buffer,vars[i]);
    strcat(buffer,"}");
    Tcl_Eval(main_interp,buffer);
  }                              
  Tcl_SetVar(main_interp,"overvarval","none",SFLG);
  Tcl_SetVar(main_interp,"overvartypeval","3-D",SFLG);
  show_cont_set0(main_interp,"none",2);

  /* Put time in time widget */
  sprintf(par.ctime,"%d",RAMS.file);
  Tcl_SetVar(main_interp,"dateval",strdate(RAMS.dates[RAMS.file-1]) ,SFLG);
  Tcl_SetVar(main_interp,"timeval",strtime(RAMS.times[RAMS.file-1]) ,SFLG);
  
  /* Set up the time scrolling widget */
  strcpy(buffer,".top.timeframe.timeslide configure -from 1");
  Tcl_Eval(main_interp,buffer);
  strcpy(buffer,".top.timeframe.timeslide configure -to ");
  sprintf(buf,"%d",RAMS.nfiles);
  strcat(buffer,buf);
  Tcl_Eval(main_interp,buffer);

  /* Put height at this level in widget */
  sprintf(buf,"{%8.0f m}",*(RAMS.heights+par.icoor-1));
  strcpy(buffer,".top.slabframe.frame25.vallab configure -text ");
  strcat(buffer,buf);
  Tcl_Eval(main_interp,buffer);

  /* Now do all the things that are done each new time */
  newtime();
}
newtime()
{
  int i,req,n,ngrids;
  char wname[8];
  int ix1,ix2,iy1,iy2,iz1,iz2;
  char buf[12];

  /* Check out info about the Grids */
  /* first get new grid information */
  req=1;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&ngrids,&n);

  if (RAMS.grid > ngrids)
  {
     RAMS.grid=1; /* Old grid not avaliable */
     /* Now get all the heights */
     req=0;rams_get_fdata(&req,&RAMS.file,&RAMS.grid,RAMS.heights,&n);
     Tcl_SetVar(main_interp,"grid","Grid 1",SFLG);
  }

  /* Now edit the visible list of grids so it reflects the proper number */
  if (ngrids < RAMS.ngrids)
  {
    sprintf(buffer,".top.gridframe.grid.m delete %d end",ngrids+1);
    if (Tcl_Eval(main_interp,buffer) == TCL_ERROR)
      fprintf(stderr, "Tcl_Eval failed: %s\n",
              Tcl_GetStringResult(main_interp));
  }
  else if (ngrids > RAMS.ngrids)
  {
    for (i=RAMS.ngrids+1; i<=ngrids; i++)
    {
      sprintf(wname,"Grid %1d",i);
      strcpy(buffer,".top.gridframe.grid.m add command -command {set grid {");
      strcat(buffer,wname);
      strcat(buffer,"};gridCB $grid} -label {");
      strcat(buffer,wname);
      strcat(buffer,"}");
      if (Tcl_Eval(main_interp,buffer) == TCL_ERROR)
        fprintf(stderr, "Tcl_Eval failed: %s\n",
              Tcl_GetStringResult(main_interp));
    }
  }

  RAMS.ngrids = ngrids;

  /* Now change the maximums on the grid offset sliders */
  update_offsets();

  /* Now check the slab coordinate and see what needs fixing or changing */
  i = par.icoor;
  /* Change the max value for slab offset */

  var_getparams(buf,&ix1,&ix2,&iy1,&iy2,&iz1,&iz2,strlen(buf));

  switch(par.islab)
  {
    case 3:
      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
      sprintf(buf,"%d",1+iz1);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);

      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
      sprintf(buf,"%d",RAMS.coords[2]-iz2);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);

      /* printf("===== in newtime xy %d %d %d %d \n",par.islab,RAMS.coords[2]
	     ,iz1,iz2); */
      if (i > RAMS.coords[2])
      {
         Tcl_SetVar(main_interp,"slabscaleval","0",SFLG);
         par.islab=0;
      }
      break;
    case 2:
      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
      sprintf(buf,"%d",1+ix1);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);

      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
      sprintf(buf,"%d",RAMS.coords[0]-ix2);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);


      /* printf("===== in newtime yz %d %d %d %d \n",par.islab,RAMS.coords[0]
	     ,ix1,ix2); */
      if (i > RAMS.coords[0])
      {
         Tcl_SetVar(main_interp,"slabscaleval","0",SFLG);
         par.islab=0;
      }
      break;
    case 1:
      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
      sprintf(buf,"%d",1+iy1);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);

      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
      sprintf(buf,"%d",RAMS.coords[1]-iy1);
      strcat(buffer,buf);
      Tcl_Eval(main_interp,buffer);

      /* printf("===== in newtime xz %d %d %d %d \n",par.islab,RAMS.coords[1]
	     ,iy1,iy2); */
      if (i > RAMS.coords[1])
      { 
        Tcl_SetVar(main_interp,"slabscaleval","0",SFLG);
        par.islab=0;
      }
      break;
   }
}

update_offsets()
{
  int req,n;
  char **ptr;

  req=0;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&RAMS.coords,&n);
  
  switch(par.islab)
  {
    case 3:
            if (par.iwoffl>RAMS.coords[0]-1 || par.iwoffr>RAMS.coords[0]-1
              ||par.iwoffb>RAMS.coords[1]-1 || par.iwofft>RAMS.coords[1]-1)
            {
               par.iwoffl=0; par.iwoffr=0; par.iwofft=0; par.iwofft=0;
            }
	    break;
    case 2: 
            if (par.iwoffl>RAMS.coords[1]-1 || par.iwoffr>RAMS.coords[1]-1
              ||par.iwoffb>RAMS.coords[2]-1 || par.iwofft>RAMS.coords[2]-1)
            {
               par.iwoffl=0; par.iwoffr=0; par.iwofft=0; par.iwofft=0;
            }
            break;
    case 1:
            if (par.iwoffl>RAMS.coords[0]-1 || par.iwoffr>RAMS.coords[0]-1
              ||par.iwoffb>RAMS.coords[2]-1 || par.iwofft>RAMS.coords[2]-1)
            {
               par.iwoffl=0; par.iwoffr=0; par.iwofft=0; par.iwofft=0;
            }
            break;
  }

}
