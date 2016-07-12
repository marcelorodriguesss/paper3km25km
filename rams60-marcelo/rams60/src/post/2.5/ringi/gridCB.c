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

int gridCB (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
   char buffer[100], *ptr, *prefix;
   char grid, buf[12];
   int  gn;
   float x1=0,y1=0,x2=0,y2=0,tmp,nymax,nymin;
   static float x1save=0,y1save=0,x2save=0,y2save=0;
   int  ix1,iy1,ix2,iy2,iwid,ihgt;
   static int oldgrid=0,zooming=0,i;
   float value;
   int req,n,imax,imin;
   char domain[32];

   if (RAMS_NOTAVALIABLE) return(TCL_OK);

  /* Find out which Widget called */
  if (strncmp(*(argv+1),"Grid",4) == 0)
  {
    /* Get grid number from buffer */
    ptr = *(argv+1)+5;
    grid = *ptr;

    if (grid < '1' || grid > '9')
    {
      fprintf(stderr,"gridCB: bad grid \"%c\"\n",grid);
      return(TCL_OK);
    } 

    if (grid == *par.cgrid)
    {
      /* Hey, we already got that grid selected */
      return(TCL_OK);
    } 

    gn = grid - '0';
    if (gn > RAMS.ngrids)
    {
       value=0; /* Space filler */
    }
    else
      sprintf(par.cgrid,"%c",grid);

    /* Set the zoom parmeters back to zero */
    par.x1 = -1.0;

    RAMS.grid = gn;
    /* Now get all the heights */
    req=0;rams_get_fdata(&req,&RAMS.file,&RAMS.grid,RAMS.heights,&n);

    if (zooming) update_plot(interp,0,1);
    zooming = False;

    if (Tcl_Eval(interp,".top.unzoom configure -state disabled") == TCL_ERROR)
      fprintf(stderr, "Tcl_Eval failed: %s\n", Tcl_GetStringResult(interp));

    /* Update coodinate widget */
    slab_coor(&par.islab,&par.icoor,&value,&RAMS.grid);
    
    /* Change height shown */
    if (par.islab==3) {
        sprintf(buf,"%8.0f m",value);
        Tcl_SetVar(interp,"vallabval",buf,SFLG);
    } else {
      sprintf(buf,"%8.2f deg",value);
      Tcl_SetVar(interp,"vallabval",buf,SFLG);
    }

  } else if (strncmp(*(argv+1),"Zoom",4) == 0) {

    x1save=par.x1; y1save=par.y1; x2save=par.x2; y2save=par.y2;
    /* Get corners of selected zoom */
    sscanf(*(argv+2),"%d %d %d %d %d %d",&iwid,&ihgt,&ix1,&iy1,&ix2,&iy2);
    if (adjustrect(iwid,ihgt,&ix1,&iy1,&ix2,&iy2,&x1,&y1,&x2,&y2)) {
      fprintf(stderr,"Rectangle completely outsize square\n");
    }   
    if (x1 > x2) {
      par.x1 = x2; par.x2 = x1;
    } else { 
      par.x1 = x1; par.x2 = x2; 
    }
    if (y1 > y2) { 
      par.y1 = y2; par.y2 = y1;
    } else {
      par.y1 = y1; par.y2 = y2;
    }

    /* get data for grid check */
    req=4;rams_get_fdata(&req,&RAMS.file,&RAMS.grid,&RAMS.delx,&n);
    req=5;rams_get_fdata(&req,&RAMS.file,&RAMS.grid,&RAMS.dely,&n);
/*
 *     fprintf(stderr,"RAMS.grid %d\n",RAMS.grid);
 *     fprintf(stderr,"RAMS.delx RAMS.dely %f %f\n",RAMS.delx,RAMS.dely);
 *     fprintf(stderr,"    x1 y1 - x2 y2: %6.4f %6.4f - %6.4f %6.4f\n",x1,y1,x2,y2);
 *     fprintf(stderr,"par.x1 y1 - x2 y2: %6.4f %6.4f - %6.4f %6.4f\n",par.x1,par.y1,par.x2,par.y2);
 *     fprintf(stderr,"par.xmax par.xmin par.ymax par.ymin %f %f %f %f\n",par.xmax,par.xmin,par.ymax,par.ymin);
 *     fflush(stderr);
 */
	 
    /* check that zoom is big enough */
    if ( par.islab == 3 ) { 
      if ( 1+(par.x2-par.x1)*(par.xmax-par.xmin)/RAMS.delx < 2 ) {
        fprintf(stderr,"horiz slab - lon zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      }
      if ( 1+(par.y2-par.y1)*(par.ymax-par.ymin)/RAMS.dely < 2 ) {
        fprintf(stderr,"horiz slab - lat zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      } 
    } else if ( par.islab == 2 ) {
      if ( 1+(par.x2-par.x1)*(par.xmax-par.xmin)/RAMS.delx < 2 ) {
        fprintf(stderr,"vert slab - lon zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      }
      nymin=par.ymin+(par.ymax-par.ymin)*par.y1;
      nymax=par.ymin+(par.ymax-par.ymin)*par.y2;
      imin=0; imax=0; n=0;
      while ( imin == 0 ) {
        n++;
        if ( RAMS.heights[n] >= nymin ) imin = n;
      }
      while ( imax == 0 ) {
        if ( RAMS.heights[n] >= nymax ) imax = n;
        n++;
      }
      if ( 1+(imax-imin) < 2 ) {
        fprintf(stderr,"vert slab - hgt zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      }
    } else if ( par.islab == 1 ) { 
      if ( 1+(par.x2-par.x1)*(par.xmax-par.xmin)/RAMS.dely < 2 ) {
        fprintf(stderr,"vert slab - lat zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      } 
      nymin=par.ymin+(par.ymax-par.ymin)*par.y1;
      nymax=par.ymin+(par.ymax-par.ymin)*par.y2;
      imin=0; imax=0; n=0;
      while ( imin == 0 ) {
        n++;
        if ( RAMS.heights[n] >= nymin ) imin = n;
      }
      while ( imax == 0 ) {
        if ( RAMS.heights[n] >= nymax ) imax = n;
        n++;
      }
      if ( 1+(imax-imin) < 2 ) {
        fprintf(stderr,"vert slab - hgt zoom too small\n");
        par.x1=x1save; par.y1=y1save; par.x2=x2save; par.y2=y2save;
        return(TCL_OK);
      }
    fflush(stderr);
    }

    /*fprintf(stderr,"ZOOMING:  %f %f %f %f\n",par.x1,par.y1,par.x2,par.y2); */
    update_plot(interp,0,1);
    par.x1 = 0.0;
    /*fprintf(stderr,"ZOOMING=True\n"); */
    zooming = True;
    if (Tcl_Eval(interp,".top.unzoom configure -state normal") == TCL_ERROR)
      fprintf(stderr, "Tcl_Eval failed: %s\n", Tcl_GetStringResult(interp));
  }
  else if (strncmp(*(argv+1),"UnZoom",7) == 0)
  {
    par.x1 = -1.0;

    if (zooming) update_plot(interp,0,1);
    /*fprintf(stderr,"ZOOMING=False\n");*/
    zooming = False;
    if (Tcl_Eval(interp,".top.unzoom configure -state disabled") == TCL_ERROR)
      fprintf(stderr, "Tcl_Eval failed: %s\n", Tcl_GetStringResult(interp));
  }
  return(TCL_OK);
} 

