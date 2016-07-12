/*############################ Change Log ##################################
! 2.3.1.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!#########################################################################*/

#include <stdio.h> 
#include <stdlib.h> 
#include <tk.h>
#include "ringi.h"

int varCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[200];
  extern Params par;
  extern RAMSinfo RAMS;

  extern char *vars[];
  extern int numvars;
  extern RAMSinfo RAMS;
  int i;
  
  /* extern void var_contours(); */


    /* Get new buffer for widget's string */
    strcpy(buffer,Tcl_GetVar(interp,"vartypeval",SFLG));

    /* Find what was selected */
    if (strncmp(buffer,"3-D",3)==0)
      strcpy(par.ctype,"3");
    else if (strncmp(buffer,"2-D",3)==0)
      strcpy(par.ctype,"2");
    else if (strncmp(buffer,"Soil",4)==0)
      strcpy(par.ctype,"s");

    /* Unset the none button */
    Tcl_SetVar(interp,"varnoneval","0",SFLG);
    strcpy(par.ccont,"y");

    /* Delete old list from widget */
    Tcl_Eval(interp,".top.parframe.frame4.var.m delete 0 100");

    /* Get a new list of variables from RAMS */
    update_varlist();

    /* Insert new list of variables into widget */
    for (i=0; i<numvars; i++) {
      strcpy(buffer,".top.parframe.frame4.var.m add command -command {set varval ");
      strcat(buffer,vars[i]);
      strcat(buffer,"; show_cont_set $varval {1}} -label {");
      strcat(buffer,vars[i]);
      strcat(buffer,"}");
      Tcl_Eval(interp,buffer);
    }
    Tcl_SetVar(main_interp,"varval",vars[0],SFLG);

  return(TCL_OK);
}

int overvarCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[200];
  extern Params par;
  extern RAMSinfo RAMS;

  extern char *vars[];
  extern int numvars;
  extern RAMSinfo RAMS;
  int i;


    /* Get new buffer for widget's string */
    strcpy(buffer,Tcl_GetVar(interp,"overvartypeval",SFLG));

    /* Find what was selected */
    if (strncmp(buffer,"3-D",3)==0)
      strcpy(par.ctype,"3");
    else if (strncmp(buffer,"2-D",3)==0)
      strcpy(par.ctype,"2");
    else if (strncmp(buffer,"Soil",4)==0)
      strcpy(par.ctype,"s");

  
    /* Delete old list from widget */
    Tcl_Eval(interp,".top.parframe.sec_frame.var.m delete 0 100");

    /* Get a new list of variables from RAMS */
    update_varlist();
      printf("===> %d  \n",numvars);

    /* Insert new list of variables into widget */
    for (i=0; i<numvars; i++) {
      printf("===> %d %s \n",numvars,vars[i]);
      strcpy(buffer,".top.parframe.sec_frame.var.m add command -command {set overvarval ");
      strcat(buffer,vars[i]);
      strcat(buffer,"; show_cont_set $overvarval {2}} -label {");
      strcat(buffer,vars[i]);
      strcat(buffer,"}");
      Tcl_Eval(interp,buffer);
    }
    Tcl_SetVar(main_interp,"overvarval",vars[0],SFLG);


  return(TCL_OK);
}

int show_cont_set (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[200], vname[32];

  extern char *vars[];
  extern int numvars;
  int i;
  float clo,chi,cinc;

/*
 * printf("===> in show_cont_set: %d: %s: %s: %s \n",argc,*(argv)
 *                ,*(argv+1),*(argv+2));
 */

  strcpy(vname,*(argv+1));

  if(strncmp(*(argv+2),"1",1)==0)
    {

      if(strncmp(vname,"none",4)==0) {
	Tcl_SetVar(interp,"contminval"," ",SFLG);
	Tcl_SetVar(interp,"contmaxval"," ",SFLG);
	Tcl_SetVar(interp,"contincval"," ",SFLG);
      }
      else {
	var_contours(vname,&clo,&chi,&cinc,strlen(vname));
	sprintf(buffer,"%f",clo);
	Tcl_SetVar(interp,"contminval",buffer,SFLG);
	sprintf(buffer,"%f",chi);
	Tcl_SetVar(interp,"contmaxval",buffer,SFLG);
	sprintf(buffer,"%f",cinc);
	Tcl_SetVar(interp,"contincval",buffer,SFLG);
      }
    }    
  else if(strncmp(*(argv+2),"2",1)==0){

    if(strncmp(vname,"none",4)==0) {
      Tcl_SetVar(interp,"overminval"," ",SFLG);
      Tcl_SetVar(interp,"overmaxval"," ",SFLG);
      Tcl_SetVar(interp,"overincval"," ",SFLG);
    }
    else {
      var_contours(vname,&clo,&chi,&cinc,strlen(vname));
      sprintf(buffer,"%f",clo);
      Tcl_SetVar(interp,"overminval",buffer,SFLG);
      sprintf(buffer,"%f",chi);
      Tcl_SetVar(interp,"overmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"overincval",buffer,SFLG);
    }
  }
  
  return(TCL_OK);
}

int show_cont_set0 ( interp, vname, ifield)
  Tcl_Interp *interp;
  int ifield;
  char vname[];
{
  char buffer[200];

  int i;
  float clo,chi,cinc;

/* printf("===> in show_cont_set0: %d: %s: \n",ifield,vname); */


  if(ifield == 1)
    {

      if(strncmp(vname,"none",4)==0) {
	Tcl_SetVar(interp,"contminval"," ",SFLG);
	Tcl_SetVar(interp,"contmaxval"," ",SFLG);
	Tcl_SetVar(interp,"contincval"," ",SFLG);
      }
      else {
	var_contours(vname,&clo,&chi,&cinc,strlen(vname));
	sprintf(buffer,"%f",clo);
	Tcl_SetVar(interp,"contminval",buffer,SFLG);
	sprintf(buffer,"%f",chi);
	Tcl_SetVar(interp,"contmaxval",buffer,SFLG);
	sprintf(buffer,"%f",cinc);
	Tcl_SetVar(interp,"contincval",buffer,SFLG);
      }
    }    
  else if(ifield == 2){

    if(strncmp(vname,"none",4)==0) {
      Tcl_SetVar(interp,"overminval"," ",SFLG);
      Tcl_SetVar(interp,"overmaxval"," ",SFLG);
      Tcl_SetVar(interp,"overincval"," ",SFLG);
    }
    else {
      var_contours(vname,&clo,&chi,&cinc,strlen(vname));
      sprintf(buffer,"%f",clo);
      Tcl_SetVar(interp,"overminval",buffer,SFLG);
      sprintf(buffer,"%f",chi);
      Tcl_SetVar(interp,"overmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"overincval",buffer,SFLG);
    }
  }
  
  return(TCL_OK);
}



