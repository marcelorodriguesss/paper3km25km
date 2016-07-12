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

extern RAMSinfo RAMS;
extern Params par;

int slabCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buffer[80], buf[12];
  int oldslab;
  float lat,lon,value,x,y;
  double dval;
  int vws,islx,isly,ix1,ix2,iy1,iy2,iz1,iz2;
  static int nplevs,plevs[100],old_coor=1;
  int    iwid,ihgt;

  if (RAMS_NOTAVALIABLE) return(TCL_OK);

  /* Find out which widget called */
  if (strncmp(*(argv+1),"Type",4) == 0)
  {

    strcpy(buffer,*(argv+2));
    oldslab = par.islab;
    if (strncmp(buffer,"X-Y",3)==0)
    {
      if(oldslab != 3) {
        sscanf(*(argv+3),"%d %d %d %d",&iwid,&ihgt,&ix1,&iy1);
        if (adjustpoint(iwid,ihgt,&ix1,&iy1,&x,&y)) {
          fprintf(stderr,"Point completely outsize square\n");
        }   
        getmappos(&x,&y,&lat,&lon);
        loc_slab(&par.islab,&x,&y,&islx,&isly,&RAMS.grid);
        par.icoor=isly;
      }
      par.islab=3;
      Tcl_SetVar(interp,"slablabval","Height",SFLG);

      switch(buffer[4]) {
        case 'S': par.itrans=1;
 	  break;
        case 'C': par.itrans=2;
	  break;
        case 'P': par.itrans=3;
	  Tcl_SetVar(interp,"slablabval","Pressure",SFLG);
	  break;
      };
    }
    else if (strncmp(buffer,"Y-Z",3)==0) 
    {
      fprintf(stderr,"y-Z\n");
      if(oldslab == 2) return(TCL_OK);
      sscanf(*(argv+3),"%d %d %d %d",&iwid,&ihgt,&ix1,&iy1);
      if (adjustpoint(iwid,ihgt,&ix1,&iy1,&x,&y)) {
        fprintf(stderr,"Point completely outsize square\n");
      }   
      getmappos(&x,&y,&lat,&lon);
      loc_slab(&par.islab,&x,&y,&islx,&isly,&RAMS.grid);
  
      printf("in Y-Z: %f %f %f %f %d %d \n",x,y,lat,lon,islx,isly);
      par.islab=2;
      par.icoor=islx;

      Tcl_SetVar(interp,"slablabval","Longitude",SFLG);
    }
    else if (strncmp(buffer,"X-Z",3)==0)
    {
      if(oldslab == 1) return(TCL_OK);
      sscanf(*(argv+3),"%d %d %d %d",&iwid,&ihgt,&ix1,&iy1);
      if (adjustpoint(iwid,ihgt,&ix1,&iy1,&x,&y)) {
        fprintf(stderr,"Point completely outsize square\n");
      }   

      getmappos(&x,&y,&lat,&lon);
      loc_slab(&par.islab,&x,&y,&islx,&isly,&RAMS.grid);

      printf("in X-Z: %f %f %f %f %d %d \n",x,y,lat,lon,islx,isly);
      par.islab=1;
      par.icoor=isly;
      
      Tcl_SetVar(interp,"slablabval","Latitude",SFLG);
    }

    /* See if slab changed */
    if (par.islab != oldslab || par.islab==3)
    {
      /* Change the max value for slab offset */
      strcpy(buf,"offsets"); 
      var_getparams(buf,&ix1,&ix2,&iy1,&iy2,&iz1,&iz2,strlen(buf));

      switch(par.islab)
      {
        case 3: 
	
	if(strncmp(*(argv+2),"X-Y S",5)==0 || strncmp(*(argv+2),"X-Y C",5)==0)
	  {
	    if(old_coor == 2) 
	      get_closez(&RAMS.grid,&par.icoor);

	      if (par.icoor > RAMS.coords[2]-iz2) par.icoor=RAMS.coords[2]-iz2;
	      if (par.icoor < iz1+1) par.icoor=iz1+1;
	      slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);

	      
	      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
	      sprintf(buf,"%d",1+iz1);
	      strcat(buffer,buf);
	      Tcl_Eval(interp,buffer);
	      
	      strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
	      sprintf(buf,"%d",RAMS.coords[2]-iz2);
	      strcat(buffer,buf);
	      Tcl_Eval(interp,buffer);
	    
	    sprintf(buf,"%8.0f m",value);
	    old_coor=1;
	  }
	if(strncmp(*(argv+2),"X-Y P",5)==0)
	  {
	    get_plevs(&nplevs,plevs);
	    printf("pressure: %d %d \n",nplevs,plevs[0]);
	    
	    strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
	    sprintf(buf,"%d",1);
	    strcat(buffer,buf);
	    Tcl_Eval(interp,buffer);
	    
	    strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
	    sprintf(buf,"%d",nplevs);
	    strcat(buffer,buf);
	    Tcl_Eval(interp,buffer);

	    if (old_coor == 1) {
	      slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);
	      get_closep(&RAMS.grid,&par.icoor);
	      sprintf(buf,"%d mb",plevs[par.icoor-1]);
	    }
	    old_coor=2;
	  }
	  break;
        case 2: 
	  if (par.icoor > RAMS.coords[0]-ix2) par.icoor=RAMS.coords[0]-ix2;
	  if (par.icoor < ix1+1) par.icoor=ix1+1;
	  slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);

          strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
          sprintf(buf,"%d",1+ix1);
          strcat(buffer,buf);
          Tcl_Eval(interp,buffer);

          strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
          sprintf(buf,"%d",RAMS.coords[0]-ix2);
          strcat(buffer,buf);
          Tcl_Eval(interp,buffer);

	  sprintf(buf,"%8.2f deg",value);
	  break;
        case 1: 
	  if (par.icoor > RAMS.coords[1]-iy2) par.icoor=RAMS.coords[1]-iy2;
  	  if (par.icoor < iy1+1) par.icoor=iy1+1;
	  slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);

          strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -from ");
          sprintf(buf,"%d",1+iy1);
          strcat(buffer,buf);
          Tcl_Eval(interp,buffer);

          strcpy(buffer,".top.slabframe.frame25.slaboffscale configure -to ");
          sprintf(buf,"%d",RAMS.coords[1]-iy1);
          strcat(buffer,buf);
          Tcl_Eval(interp,buffer);

	  sprintf(buf,"%8.2f deg",value);
	  break;
        }

      Tcl_SetVar(interp,"vallabval",buf,SFLG);

      sprintf(buf,"%d",par.icoor);
      Tcl_SetVar(interp,"slabscaleval",buf,SFLG);
    }

    update_offsets();
    sprintf(buf,"%d",par.icoor);
    Tcl_SetVar(interp,"slabscaleval",buf,SFLG);

    par.x1 = -1.0;
    update_plot(interp,0,1);
  }


  if (strncmp(*(argv+1),"slaboffscale",12) == 0)
  {
    strcpy(buffer,Tcl_GetVar(interp,"slabtype",SFLG));

    strcpy(buf,Tcl_GetVar(interp,"slabscaleval",SFLG));
    par.icoor=atoi(buf);
    /*printf("slabtype: %s %d \n",buffer,par.icoor);*/
    slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);

    if (par.islab==3) /* Change height shown */
    {
      if(strncmp(buffer,"1",1)==0 ||strncmp(buffer,"2",1)==0 ) 
        sprintf(buf,"%8.0f m",value);
      if(strncmp(buffer,"5",1)==0) 
        sprintf(buf,"%d mb",plevs[par.icoor-1]);
      if(strncmp(buffer,"5",1)==0) 
        printf("p label: %d %d \n",par.icoor,plevs[par.icoor]);
    }
    else
    {
      sprintf(buf,"%8.2f deg",value);
    }
    Tcl_SetVar(interp,"vallabval",buf,SFLG);
  }

return(TCL_OK);
}

