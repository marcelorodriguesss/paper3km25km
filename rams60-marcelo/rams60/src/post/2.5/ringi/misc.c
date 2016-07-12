/*############################ Change Log ##################################
! 2.3.1.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!#########################################################################*/

#include <stdlib.h>
#include <tk.h>
#include <strings.h>
#include "ringi.h"

extern Params par;
extern RAMSinfo RAMS;

/*======================================================================*/
void ringi_init()
{
  par.islab     = 3;
  par.icoor     = 2;
  par.itrans    = 1;
  par.iwoffl    = 0;
  par.iwoffr    = 0;
  par.iwoffb    = 0;
  par.iwofft    = 0;
  par.wint      = 1;
  par.conrinc   = 0.0;
  par.conrlo    = 0.0;
  par.conrhi    = 0.0;
  strcmp(par.cvar," ");;
  strcpy(par.ctype,"3");
  strcpy(par.cgrid,"1");
  strcpy(par.ctime,"1");
  strcpy(par.cwinds,"n");
  strcpy(par.ccont,"y");
  par.x1 = 0.0;
  par.x2 = 0.0;
  par.y1 = 0.0;
  par.y2 = 0.0;
  par.xmin = 0.0;
  par.xmax = 0.0;
  par.ymin = 0.0;
  par.ymax = 0.0;
} 

/*======================================================================*/
int init_rams(prefix)
  char *prefix;
{
  extern Tcl_Interp *main_interp;
  extern char *vars[MAXVARS];
  int i,req,n;
  extern void update_varlist();

  /* fprintf(stderr,"init_rams 1: %d\n",main_interp); */
  /* fflush(stderr); */

  rams_anal_init(&RAMS.nfiles,prefix,strlen(prefix));
  if (RAMS.nfiles == 0)
    return(1);

  RAMS.file=1;
  RAMS.grid=1;
  RAMS.ngrids=2;
/*  req=1;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&RAMS.ngrids,&n);*/
  req=2;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&RAMS.date,&n);
  req=3;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&RAMS.time,&n);
  req=0;rams_get_idata(&req,&RAMS.file,&RAMS.grid,&RAMS.coords,&n);

  /* Get all the dates and times */
  for (i=1; i<=(RAMS.nfiles<=MAXFILES?RAMS.nfiles:MAXFILES); i++)
  {
    req=2;rams_get_idata(&req,&i,&RAMS.grid,&RAMS.dates[i-1],&n);
    req=3;rams_get_idata(&req,&i,&RAMS.grid,&RAMS.times[i-1],&n);
  }

  /* Now get all the heights */
  req=0;rams_get_fdata(&req,&RAMS.file,&RAMS.grid,RAMS.heights,&n);

  /* Now set our display to a nice default for this model */
  strcpy(par.ctype,"3");
  update_varlist();  /* Get the list of variables in this run */

  RAMS.loaded = True;
  
  /* fprintf(stderr,"init_rams 2: %d\n",main_interp); */
  /* fflush(stderr); */

  /* Now fix up the widgets to reflect a model is loaded */
  newmodel();
  return(0);
}

/*======================================================================*/
void update_plot(interp,typ,fill_flag)
  Tcl_Interp *interp;
  int typ,fill_flag;
{
  double val;
  extern Params par;
  int bscale,intwind,plotinfo;
  float smin,smax,smean,ovmin,ovmax,ovinc;
  char cvalue[20],buffer[20],title[120];

  char varname[32],vartype[20],overtype[20],overvar[32],colorbar[2];
  int wint;
  float clo1,chi1,cinc1,clo2,chi2,cinc2;

  char *buf[20];

  if (typ==0) clearframe(&wsid);


/*------- Get some of the GUI variable values ---------*/

  /* Primary field*/  /* which var and display type */
  strcpy(varname,Tcl_GetVar(interp,"varval",SFLG));
  strcpy(vartype,Tcl_GetVar(interp,"contype1",SFLG));

  /* Secondary field*/   /* which var and display type */
  strcpy(overvar,Tcl_GetVar(interp,"overvarval",SFLG));
  strcpy(overtype,Tcl_GetVar(interp,"overtype1",SFLG));

  /* colorbar */
  if( strncmp(vartype,"f",1)==0 )
  {
    strcpy(colorbar,Tcl_GetVar(interp,"filled_bar",SFLG));
  }
  else if(strncmp(vartype,"t",1)==0)
  {
    strcpy(colorbar,Tcl_GetVar(interp,"tile_bar",SFLG));
  }
  else if(strncmp(vartype,"c",1)==0)
  {
    strcpy(colorbar,Tcl_GetVar(interp,"line_bar",SFLG));
  }
  else {
    printf("update_plot: ERROR - unknown plot type\n");
    exit(1);
  }
  printf("colorbar: %s \n",colorbar);
  
  /* title */
  strcpy(title,Tcl_GetVar(interp,"plot_title",SFLG));

  /* winds */
  strcpy(buffer,Tcl_GetVar(interp,"windvar",SFLG));
  if      (strncmp(buffer,"None",   4)==0) strcpy(par.cwinds,"n");
  else if (strncmp(buffer,"Vectors",7)==0) strcpy(par.cwinds,"v");
  else if (strncmp(buffer,"Turbul" ,6)==0) strcpy(par.cwinds,"t");
  else if (strncmp(buffer,"Stream", 6)==0) strcpy(par.cwinds,"s");
  else if (strncmp(buffer,"Barb",   4)==0) strcpy(par.cwinds,"b");
  
  /* wind interval */
  wint=atoi(Tcl_GetVar(interp,"windintvar",SFLG));
  printf("WindInt : %d \n",wint);
  
  /* barb scale */
  strcpy(buffer,Tcl_GetVar(interp,"barbscaleval",SFLG));
  if      (strncmp(buffer,"50-10",5)==0) bscale=1;
  else if (strncmp(buffer,"20-4-",5)==0) bscale=2;
  else if (strncmp(buffer,"10-2-",5)==0) bscale=3;

  /* Contour min,max,inc */
  clo1=atof(Tcl_GetVar(interp,"contminval",SFLG));
  chi1=atof(Tcl_GetVar(interp,"contmaxval",SFLG));
  cinc1=atof(Tcl_GetVar(interp,"contincval",SFLG));

  /* Overlay field Contour min,max,inc */
  clo2=atof(Tcl_GetVar(interp,"overminval",SFLG));
  chi2=atof(Tcl_GetVar(interp,"overmaxval",SFLG));
  cinc2=atof(Tcl_GetVar(interp,"overincval",SFLG));

  /* Display plot information - added by mjb */
  /* plotinfo=atoi(Tcl_GetVar(interp,"plotinfo",SFLG)); */
  plotinfo=1;
  
/*---------------------------------------------------- */

  cread_rams(&par.islab,&par.icoor,&par.cgrid,&varname
    ,&par.ctime,&par.itrans,&par.ctype,&par.ccont
    ,&par.cwinds,&par.iwoffl,&par.iwoffr, &par.iwoffb
    ,&par.iwofft,&cinc1,&clo1,&chi1
    ,&par.x1,&par.x2,&par.y1,&par.y2,&wint,&bscale
    ,&fill_flag,&plotinfo
    ,&vartype,&overvar,&overtype,&clo2,&chi2,&cinc2,&colorbar
    ,&title,&par.xmin,&par.xmax,&par.ymin,&par.ymax
     /* FORTRAN string lengths */
    ,strlen(par.cgrid),strlen(varname),strlen(par.ctime)
    ,strlen(par.ctype),strlen(par.ccont),strlen(par.cwinds)
    ,strlen(vartype),strlen(overvar),strlen(overtype)
    ,strlen(colorbar),strlen(title)
    );

/*---------------------------------------------------- */

  if(strncmp(varname,"none",4) != 0) {
    slab_stats(&smin,&smax,&smean);
    printf(" stats : %10.3g %10.3g %10.3g \n",smin,smax,smean);
  }
  else {
    smin=0.; smax=0.; smean=0.;
  }

  sprintf(cvalue,"%10.5g",smin);
  Tcl_SetVar(interp,"statminval",cvalue,SFLG);

  sprintf(cvalue,"%10.5g",smax);
  Tcl_SetVar(interp,"statmaxval",cvalue,SFLG);

  sprintf(cvalue,"%10.5g",smean);
  Tcl_SetVar(interp,"statmeanval",cvalue,SFLG);

  /*docursor(w,0);*/

  fprintf(stderr,"==================== finished frame ====================\n");

}


/*======================================================================*/
void update_varlist()
{
  static char varlist[800];
  char *ptr, *lastchar;
  /*extern Params par;*/
  extern char *vars[MAXVARS];
  extern int numvars;
  int nvc,itype;

  /* Call the RAMS routine to get the list of variables */
  
  itype=0;
  if(!strcmp(par.ctype,"3")) itype=3;
  if(!strcmp(par.ctype,"2")) itype=2;
  if(!strcmp(par.ctype,"s")) itype=5;

  /*var_getcat(&itype,&nvc,varlist,strlen(varlist));*/
  var_getcat(&itype,&nvc,varlist,sizeof(varlist));  /* This seems more proper*/

  /* replace delimiters with NULL's and setup pointers to strings */
  /* Also put NULL at end of last string */
  vars[0] = varlist;
  numvars = 1;
  for (ptr=varlist; ptr<varlist+800; ptr++)
  {
    if (*ptr == VAR_DELIM )
    {
      *ptr = '\0';
      vars[numvars++] = ptr+1;
    }
    else if ( *ptr == ' ')
    {
      *ptr = '\0';
      break;
    }
    if (*ptr!='\0'&&*ptr!=' ') lastchar=ptr;
  }
  fprintf(stderr,"%d %d %d\n",varlist,ptr,lastchar);
  *(lastchar+1) = '\0';
}

/*======================================================================*/
/* Does some checks on a file prefix for validity */
validprefix(prefix)
  char *prefix;
{
  char *ptr;
  if (prefix == NULL) return(False);
/*  if (strlen(prefix) < 3) return(False); */
  ptr = (char *)strrchr(prefix,'/');  /* find the last dir slash */
  if (ptr==NULL) ptr=prefix;   /* No slashes; it's in current directory */
  if (*(ptr+1) == '\0') return(False);
/*  ptr = strstr(ptr,".a");*/     /* look for the *.a* string */
/*  if (ptr=NULL) return(False); */
  return(True);
}

/*======================================================================*/
int adjustrect(iwid,ihgt,ix1,iy1,ix2,iy2,x1,y1,x2,y2)
  int iwid,ihgt,*ix1,*iy1,*ix2,*iy2;
  float *x1,*y1,*x2,*y2;
{
  /* This function is used to correct for non-square windows. It will
   * adjust the points to within the NCARG/GKS square.  If the entire
   * rectangle falls completely outside the NCARG/GKS square then no
   * points are adjusted and a 1 is returned, otherwise a 0 is returned.
   * x1,y1,x2,y2 are the gks locations withing the NCARG/GKS square.
   */

  int minx,maxx,miny,maxy;
  int nx1,ny1,nx2,ny2;

  /* copy points */
  nx1=*ix1; ny1=*iy1;
  nx2=*ix2; ny2=*iy2;

  /* adjust the points */
  if (iwid < ihgt)  {
    /* find the max's and min's */
    minx = 0;
    maxx = iwid;
    miny = (ihgt-iwid)/2;
    maxy = ihgt-miny;
    if ( (ny1<miny && ny2<miny) || (ny1>maxy && ny2>maxy)) {
      return(1);  /* rectangle is outside square */
    } else {
      if (ny1<miny) ny1 = miny;
      if (ny2<miny) ny2 = miny;
      if (ny1>maxy) ny1 = maxy;
      if (ny2>maxy) ny2 = maxy;
      *x1 = (float)nx1/(float)iwid;
      *x2 = (float)nx2/(float)iwid;
      *y1 = 1.-(float)(ny1-miny)/(float)iwid;
      *y2 = 1.-(float)(ny2-miny)/(float)iwid;
      /* fprintf(stderr,"*x1 =   (float)%4d      /(float)%4d == %f\n",nx1,ihgt,*x1); */
      /* fprintf(stderr,"*x2 =   (float)%4d      /(float)%4d == %f\n",nx2,ihgt,*x2); */
      /* fprintf(stderr,"*y1 = 1-(float)(%4d-%4d)/(float)%4d == %f\n",ny1,minx,ihgt,*y1); */
      /* fprintf(stderr,"*y2 = 1-(float)(%4d-%4d)/(float)%4d == %f\n",ny2,minx,ihgt,*y2); */
      return(0);
    }
  } else if (iwid > ihgt) {
    minx = (iwid-ihgt)/2;
    maxx = iwid-minx;
    miny = 0;
    maxy = ihgt;
    if ( (nx1<minx && nx2<minx) || (nx1>maxx && nx2>maxx)) {
      return(1);  /* rectangle is outside square */
    } else {
      if (nx1<minx) nx1 = minx;
      if (nx2<minx) nx2 = minx;
      if (nx1>maxx) nx1 = maxx;
      if (nx2>maxx) nx2 = maxx;
      *x1 = (float)(nx1-minx)/(float)ihgt;
      *x2 = (float)(nx2-minx)/(float)ihgt;
      *y1 = 1.-(float)ny1/(float)ihgt;
      *y2 = 1.-(float)ny2/(float)ihgt;
      /* fprintf(stderr,"*x1 =   (float)(%4d-%4d)/(float)%4d == %f\n",nx1,minx,ihgt,*x1); */
      /* fprintf(stderr,"*x2 =   (float)(%4d-%4d)/(float)%4d == %f\n",nx2,minx,ihgt,*x2); */
      /* fprintf(stderr,"*y1 = 1-(float)%4d      /(float)%4d == %f\n",ny1,ihgt,*y1); */
      /* fprintf(stderr,"*y2 = 1-(float)%4d      /(float)%4d == %f\n",ny2,ihgt,*y2); */
    return(0);
    }
  } else {
    /* square window, no adjustments */
    *x1 = (float)*ix1/(float)iwid;
    *x2 = (float)*ix2/(float)iwid;
    *y1 = 1.-(float)*iy1/(float)ihgt;
    *y2 = 1.-(float)*iy2/(float)ihgt;
    return(0);
  }
}

/*======================================================================*/
int adjustpoint(iwid,ihgt,ix,iy,x,y)
  int iwid,ihgt,*ix,*iy;
  float *x,*y;
{
  /* This function is used to correct for non-square windows. It will
   * adjust the points to within the NCARG/GKS square.  If the point
   * alls completely outside the NCARG/GKS square then no points
   * are adjusted and a 1 is returned, otherwise a 0 is returned.
   * x,y are the gks locations withing the NCARG/GKS square.
   */

  int minx,maxx,miny,maxy;
  int nx,ny;

  /* copy point */
  nx=*ix; ny=*iy;

  /* adjust the points */
  if (iwid < ihgt)  {
    minx = 0;
    maxx = iwid;
    miny = (ihgt-iwid)/2;
    maxy = ihgt-miny;
    if ( (ny<miny && ny<miny)) {
      return(1);  /* point is outside square */
    } else {
      if (ny<miny) ny = miny;
      if (ny>maxy) ny = maxy;
      *x = (float)nx/(float)iwid;
      *y = 1.-(float)(ny-miny)/(float)iwid;
      return(0);
    }
  } else if (iwid > ihgt) {
    minx = (iwid-ihgt)/2;
    maxx = iwid-minx;
    miny = 0;
    maxy = ihgt;
    if ( (nx<minx && nx<minx)) {
      return(1);  /* rectangle is outside square */
    } else {
      if (nx<minx) nx = minx;
      if (nx>maxx) nx = maxx;
      *x = (float)(nx-minx)/(float)ihgt;
      *y = 1.-(float)ny/(float)ihgt;
    return(0);
    }
  } else {
    *x = (float)*ix/(float)iwid;
    *y = 1.-(float)*iy/(float)ihgt;
    return(0);
  }
}

