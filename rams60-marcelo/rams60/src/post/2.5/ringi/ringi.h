/*############################ Change Log ##################################
! 2.3.1.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!#########################################################################*/

#ifndef _RINGI_H_
#define _RINGI_H_

#include "post_sub_names.h"

#include <stdio.h>

#define MAXFILES 20000
#define MAXVARS 200 /* Maximum number of variable of one type */
#define VAR_DELIM '/' /* Delimiter for varlist from RAMS_varlist */
#ifndef True
#  define True 1
#  define False 0
#endif


#define SFLG  TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG /* For Tcl_SetVars() */


typedef struct
{
  int nfiles;            /* Number of files */
  int dates[MAXFILES];   /* All the dates */
  int times[MAXFILES];   /* All the times */
  int file;              /* Current file # */
  int date;              /* Current date */
  int time;              /* Current time */
  int ngrids;            /* Number of Grids */
  int grid;              /* Current grid */
  int coords[4];         /* Current grid coordinates */
  float heights[400];    /* Current heights for each level */
  float delx;            /* Current x grid dimension */
  float dely;            /* Current y grid dimension */
  int loaded;  /* True or False */
} RAMSinfo;
#define RAMS_NOTAVALIABLE (!RAMS.loaded)
#define RAMS_AVALIABLE    (RAMS.loaded)

typedef struct
{
  int   islab,icoor,itrans,iwoffl,iwoffr,iwoffb,iwofft;
  int   wint;
  float conrinc,conrlo,conrhi;
  float xmin,xmax,ymin,ymax;
  float x1,x2,y1,y2;
  char cgrid[4],cwinds[4],ccont[4];
  char cvar[20],ctype[8];
  char ctime[20];
} Params;

/* Define Globals */
#ifdef _RINGI_MAIN_

  extern int plotCB(),saveCB(),loadCB(),timeCB(), contCB();
  extern int filenumCB(),varCB(), overvarCB(), slabCB(), gridCB();
  extern int show_cont_set(),cleanupCB(),values();


  Params par;     /* Structure of all parameters for cread_RAMS call */
  RAMSinfo RAMS;  /* Holds info about loaded RAMS run */
  char *progname;  /* Points to argv[0] */
  char *vars[MAXVARS]; /* For all variable names from RAMS_varlist */
  int numvars;    /* Number of variables [see update_varlist()] */
  int wsid=1;     /* Workstation ID for XGKS window */
  int wissid=2;   /* Workstation ID for WISS */
  Tcl_Interp *main_interp;
#else

  extern Params par;
  extern RAMSinfo RAMS;
  extern char *progname;
  extern char *vars[MAXVARS];
  extern int numvars;
  extern int wsid;
  extern int wissid;
  extern Tcl_Interp *main_interp;
#endif




#endif
