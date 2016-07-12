/*############################ Change Log ##################################
! 2.3.1.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!#########################################################################*/

#define _RINGI_MAIN_
#include <stdio.h>
#include <tk.h>
#include "ringi.h"

extern char *egetenv(char *);

void Usage(msg)
  char *msg;
{
  if (strlen(msg) > 1)
    fprintf(stderr,"%s: %s\n",progname,msg);
  fprintf(stderr,"Usage: %s [RAMSprefix]\n",progname);
  exit(1);
}

int main (argc, argv)
int argc;
char **argv;
{
  char *ptr,*prefix=NULL;
  char *def_name;
  char line[256];
  int  ret;
  FILE *f;   
  Tk_Window mainWindow;
  Tcl_Interp *interp;
  static char *display = NULL;
  char winc[100];
  int win;

  progname = argv[0];

  /* Check out the parameters */
  if (argc > 2 ) Usage();
  if (argc == 2)
  {
    if (validprefix(argv[1]))
      prefix=argv[1];
    else
      Usage("Invalid RAMS prefix.\n");
  } 

  ringi_init();   /* Sets up some initial parameters */
  interp = Tcl_CreateInterp();
  main_interp = interp;

  if (Tcl_Init(interp) == TCL_ERROR) {
    /* OLD,BAD fprintf(stderr, "Tcl_Init failed: %s\n", interp->result); */
    fprintf(stderr, "Tcl_Init failed: %s\n", Tcl_GetStringResult(interp));

  }
  if (Tk_Init(interp) == TCL_ERROR) {
    /* OLD,BAD fprintf(stderr, "Tk_Init failed: %s\n", interp->result); */
    fprintf(stderr, "Tk_Init failed: %s\n", Tcl_GetStringResult(interp));
  }                                                                     

  /* Set Tcl's argv and argc to avoid errors when parsing interface */
  Tcl_SetVar(interp,"argc","0",TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
  Tcl_SetVar(interp,"argv"," ",TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
  
    /* Create the interface */
  if ( (ptr=egetenv("RINGI_TCL")) != NULL)
  {
    fprintf(stderr, "Evaluating file: %s\n",ptr);
    if (Tcl_EvalFile(interp,ptr) == TCL_ERROR) {
      fprintf(stderr, "Tk_EvalFile failed: %s\n", Tcl_GetStringResult(interp));
      exit(1);
    }
  }
  else
  {
    fprintf(stderr,"Cannot open ringi interface file\n");
    exit(1);
  }

  /* Delete the callback stubs */
  Tcl_DeleteCommand(interp, "slabCB");
  Tcl_DeleteCommand(interp, "loadCB");
  Tcl_DeleteCommand(interp, "saveCB");
  Tcl_DeleteCommand(interp, "gridCB");
  Tcl_DeleteCommand(interp, "timeCB");
  Tcl_DeleteCommand(interp, "plotCB");
  Tcl_DeleteCommand(interp, "contCB");
  Tcl_DeleteCommand(interp, "varCB");
  Tcl_DeleteCommand(interp, "overvarCB");
  Tcl_DeleteCommand(interp, "show_cont_set");

  /* Set up my callbacks */
  Tcl_CreateCommand(interp, "plotCB",plotCB,NULL,NULL);
  Tcl_CreateCommand(interp, "saveCB",saveCB,NULL,NULL);
  Tcl_CreateCommand(interp, "loadCB",loadCB,NULL,NULL);
  Tcl_CreateCommand(interp, "timeCB",timeCB,NULL,NULL);
  Tcl_CreateCommand(interp, "slabCB",slabCB,NULL,NULL);
  Tcl_CreateCommand(interp, "contCB",contCB,NULL,NULL);
  Tcl_CreateCommand(interp, "gridCB",gridCB,NULL,NULL);
  Tcl_CreateCommand(interp, "varCB", varCB, NULL,NULL);
  Tcl_CreateCommand(interp, "overvarCB", overvarCB, NULL,NULL);
  Tcl_CreateCommand(interp, "show_cont_set", show_cont_set, NULL,NULL);
  Tcl_CreateCommand(interp, "cleanupCB", cleanupCB, NULL,NULL);
  Tcl_CreateCommand(interp, "values", values, NULL,NULL);

  /* call routine to get variable names, etc. */
  if ( (def_name=egetenv("CONTDEF")) != NULL)
    var_defaults_read(def_name,strlen(def_name) );
  else
  {
    fprintf(stderr,"Cannot open contour defaults file\n"); exit(1);
  }
  
  /* Now initialize RAMS reading and set up widgets */
  if (prefix != NULL)
    init_rams(prefix);

  tgks_init(interp);

  Tk_MainLoop();

  fgksclose();
  exit(0);
}
