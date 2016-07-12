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

int contCB (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv; 
{
  char buffer[20],varname[20];
  float value;
  float cmin,cmax,cinc;
  extern Params par;
  double val;
  int ifield;
  
  /* Find out which button did the calling and handle it */
  fprintf(stderr,"contCB: widget -  %s %s \n",*(argv+1),*(argv+2));
  
  if (RAMS_NOTAVALIABLE) return(TCL_OK); /* No RAMS Avaliable */
  
  /*  Find which field we are doing */
  sscanf(*(argv+2),"%d",&ifield);
  
  printf("===> ifield: %d\n",ifield);
  
  if (ifield == 1) {
    
    
    /*  Find which variable we are doing */
    strcpy(varname,Tcl_GetVar(interp,"varval",SFLG));
    
    if (strncmp(*(argv+1),"Set",3) == 0)  {
      strcpy(buffer,Tcl_GetVar(interp,"contminval",SFLG));
      sscanf(buffer,"%f",&cmin);
      strcpy(buffer,Tcl_GetVar(interp,"contmaxval",SFLG));
      sscanf(buffer,"%f",&cmax);
      strcpy(buffer,Tcl_GetVar(interp,"contincval",SFLG));
      sscanf(buffer,"%f",&cinc);
      
      fprintf(stderr,"    setting to: %f %f %f\n",cmin,cmax,cinc);
      var_setcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
    }
    else if (strncmp(*(argv+1),"Reset",5) == 0) {
      var_resetcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
      fprintf(stderr,"    resetting to: %f %f %f %s\n",cmin,cmax,cinc,varname);
      sprintf(buffer,"%f",cmin);
      Tcl_SetVar(interp,"contminval",buffer,SFLG);
      sprintf(buffer,"%f",cmax);
      Tcl_SetVar(interp,"contmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"contincval",buffer,SFLG);
    }
    else if (strncmp(*(argv+1),"Default",7) == 0) {
      var_defcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
      fprintf(stderr,"    defaulting to: %f %f %f %s\n",cmin,cmax,cinc,varname);
      sprintf(buffer,"%f",cmin);
      Tcl_SetVar(interp,"contminval",buffer,SFLG);
      sprintf(buffer,"%f",cmax);
      Tcl_SetVar(interp,"contmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"contincval",buffer,SFLG);
    }
  }    
  else if (ifield == 2) {
      
    /*  Find which variable we are doing */
    strcpy(varname,Tcl_GetVar(interp,"overvarval",SFLG));
    
    if (strncmp(*(argv+1),"Set",3) == 0) {
      strcpy(buffer,Tcl_GetVar(interp,"overminval",SFLG));
      sscanf(buffer,"%f",&cmin);
      strcpy(buffer,Tcl_GetVar(interp,"overmaxval",SFLG));
      sscanf(buffer,"%f",&cmax);
      strcpy(buffer,Tcl_GetVar(interp,"overincval",SFLG));
      sscanf(buffer,"%f",&cinc);
      
      fprintf(stderr,"    setting to: %f %f %f\n",cmin,cmax,cinc);
      var_setcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
    }
    else if (strncmp(*(argv+1),"Reset",5) == 0) {
      var_resetcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
      fprintf(stderr,"    resetting to: %f %f %f %s\n",cmin,cmax,cinc,varname);
      sprintf(buffer,"%f",cmin);
      Tcl_SetVar(interp,"overminval",buffer,SFLG);
      sprintf(buffer,"%f",cmax);
      Tcl_SetVar(interp,"overmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"overincval",buffer,SFLG);
    }
    else if (strncmp(*(argv+1),"Default",7) == 0) {
      var_defcontours(&varname,&cmin,&cmax,&cinc,strlen(varname));
      fprintf(stderr,"    defaulting to: %f %f %f %s\n",cmin,cmax,cinc,varname);
      sprintf(buffer,"%f",cmin);
      Tcl_SetVar(interp,"overminval",buffer,SFLG);
      sprintf(buffer,"%f",cmax);
      Tcl_SetVar(interp,"overmaxval",buffer,SFLG);
      sprintf(buffer,"%f",cinc);
      Tcl_SetVar(interp,"overincval",buffer,SFLG);
    }
    
  }
  
  
  return(TCL_OK);
}

  
  
  
