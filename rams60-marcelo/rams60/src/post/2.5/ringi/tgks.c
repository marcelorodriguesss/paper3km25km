#include <stdio.h>
#include "tgks.h"

static TgksWin tw[4];
static int      tw_num;
static Tcl_Interp *intp;


static void
GksEventProc(ClientData clientData, XEvent *ev)
{
  static int x1,y1,x2=-1,y2;
  static int button=0;
  char  buf[100];
  int i;
  Tk_Window *tkwinp;
  extern char *evstring();

  /* fprintf(stderr,"eventPtr->type = %s\n",evstring(ev->type)); */

  /* Get the index for this tgks window */  
  sscanf(clientData,"%d",&i);
  if (i<0 || i>3) {
    fprintf(stderr,"GksEventProc: invalid clientdata %d\n",i);
    return;
   }
  tkwinp = &tw[i].tkwin;

  switch (ev->type)
  {
    case ButtonPress:
      button = ev->xbutton.button;
      x1	 = ev->xmotion.x;
      y1	 = ev->xmotion.y;
      x2	 = -1;
      y2	 = -1;
      if (button == 1) {
        switch (tw[i].m1mode) {
          case TGKS_POINT:
            XDrawPoint(tw[i].dis,tw[i].win,tw[i].gc, x1,y1);
            x2 = 1;
            break;
          case TGKS_HOR:
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,0,y1,tw[i].wd,y1);
            x2 = 1;
            break;
          case TGKS_VER:
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,0,x1,tw[i].ht);
            x2 = 1;
            break;
          case TGKS_H2:
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,
              0,tw[i].ht/2,tw[i].wd,tw[i].ht/2);
            x2 = 1;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,0,y1,tw[i].wd,y1);
            break;
          case TGKS_V2:
            XDrawLine(tw[0].dis,tw[0].win,tw[0].gc,
              tw[0].wd/2,0,tw[0].wd/2,tw[0].ht);
            x2 = 1;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,0,x1,tw[i].ht);
            break;
        }
      }
      break;
    case MotionNotify:
      /* button = ev->xbutton.button; */
      if (button == 1) {
        switch (tw[i].m1mode) {
          case TGKS_RECT:
            if (x2 >= 0) {
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x2,y1);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x1,y2);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y1,x2,y2);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y2,x1,y2);
            }
            x2 = ev->xmotion.x;
            y2 = ev->xmotion.y;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x2,y1);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x1,y2);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y1,x2,y2);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y2,x1,y2);
            break;
          case TGKS_POINT:
            if (x2 >= 0)
              XDrawPoint(tw[i].dis,tw[i].win,tw[i].gc, x1,y1);
            x1 = ev->xmotion.x;
            y1 = ev->xmotion.y;
            XDrawPoint(tw[i].dis,tw[i].win,tw[i].gc, x1,y1);
            x2 = 1;
            break;
          case TGKS_HOR:
          case TGKS_H2:
            if (x2 >= 0)
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,0,y1,tw[i].wd,y1);
            x1 = ev->xmotion.x;
            y1 = ev->xmotion.y;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,0,y1,tw[i].wd,y1);
            x2 = 1;
            break;
          case TGKS_VER:
          case TGKS_V2:
            if (x2 >= 0)
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,0,x1,tw[i].ht);
            x1 = ev->xmotion.x;
            y1 = ev->xmotion.y;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,0,x1,tw[i].ht);
            x2 = 1;
            break;
	}
      } else if (button == 3) {
        switch (tw[i].m3mode) {
          case TGKS_RECT:
            if (x2 >= 0) {
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x2,y1);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x1,y2);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y1,x2,y2);
              XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y2,x1,y2);
            }
            x2 = ev->xmotion.x;
            y2 = ev->xmotion.y;
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x2,y1);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,y1,x1,y2);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y1,x2,y2);
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x2,y2,x1,y2);
            break;
          case TGKS_POINT:
            x1 = ev->xmotion.x;
            y1 = ev->xmotion.y;
            sprintf(buf,"%s point {%4d %4d %4d %4d}",tw[i].m3cmd,
                 tw[i].wd,tw[i].ht,x1,y1);
            if (Tcl_Eval(intp,buf) == TCL_ERROR)
                fprintf(stderr, "Tcl_Eval failed: %s\n",
                      Tcl_GetStringResult(intp));
            break;
	}
      }
      break;
    case ButtonRelease:
      if (button == 1) {
        /* Draw/Undraw */
        switch (tw[i].m1mode) {
          case TGKS_RECT:
            if (x2 >= 0) {
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x1,y1,x2,y1);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x1,y1,x1,y2);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x2,y1,x2,y2);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x2,y2,x1,y2);
              XFlush(tw[i].dis);  /* Erase it right away */
              sprintf(buf,"%s {%4d %4d %4d %4d %4d %4d}",tw[i].m1cmd,
                 tw[i].wd,tw[i].ht,x1,y1,x2,y2);
	      Tcl_Eval(intp,buf);
            }
            break;
          case TGKS_POINT:
            XDrawPoint(tw[i].dis,tw[i].win,tw[i].gc, x1,y1);
            XFlush(tw[i].dis);  /* Erase it right away */
            break;
          case TGKS_HOR:
          case TGKS_H2:
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,0,y1,tw[i].wd,y1);
            XFlush(tw[i].dis);  /* Erase it right away */
            sprintf(buf,"%s {%4d %4d %4d %4d}",tw[i].m1cmd,
                 tw[i].wd,tw[i].ht,x1,y1);
	    Tcl_Eval(intp,buf);
            break;
          case TGKS_VER:
          case TGKS_V2:
            XDrawLine(tw[i].dis,tw[i].win,tw[i].gc,x1,0,x1,tw[i].ht);
            XFlush(tw[i].dis);  /* Erase it right away */
            sprintf(buf,"%s {%4d %4d %4d %4d}",tw[i].m1cmd,
                 tw[i].wd,tw[i].ht,x1,y1);
	    Tcl_Eval(intp,buf);
            break;
         }
         /* Switch back to RECT mode as default */
         tw[i].m1mode = TGKS_RECT;
	 strcpy(tw[i].m1cmd,"gridCB Zoom");

      } else if (button == 3) {
        switch (tw[i].m3mode) {
          case TGKS_RECT:
            if (x2 >= 0) {
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x1,y1,x2,y1);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x1,y1,x1,y2);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x2,y1,x2,y2);
              XDrawLine(tw[i].dis, tw[i].win, tw[i].gc, x2,y2,x1,y2);
              XFlush(tw[i].dis);  /* Erase it right away */
              sprintf(buf,"%s rect {%4d %4d %4d %4d %4d %4d}",tw[i].m3cmd,
                 tw[i].wd,tw[i].ht,x1,y1,x2,y2);
              if (Tcl_Eval(intp,buf) == TCL_ERROR)
                fprintf(stderr, "Tcl_Eval failed: %s\n",
                      Tcl_GetStringResult(intp));
            }
            break;
          case TGKS_POINT:
            sprintf(buf,"%s point {%4d %4d %4d %4d}",tw[i].m3cmd,
                 tw[i].wd,tw[i].ht,x1,y1);
              if (Tcl_Eval(intp,buf) == TCL_ERROR)
                fprintf(stderr, "Tcl_Eval failed: %s\n",
                      Tcl_GetStringResult(intp));
            break;
         }
      }
      button=0;
      break;
    case ConfigureNotify:
      tw[i].wd    = Tk_Width(tw[i].tkwin);
      tw[i].ht    = Tk_Height(tw[i].tkwin);
      break;
/*
 *     default:
 *       fprintf(stderr,"eventPtr->type = %s\n",evstring(ev->type));
 *       break;
 */
  }
}

int tgks (clientData, interp, argc, argv)
  ClientData clientData;
  Tcl_Interp *interp;
  int argc;
  char **argv; 
{
  char buf[100];
  int id, idx;
  Tk_Window tkwinmain;

  intp = interp;

  if (tw_num > 4) {
    Tcl_AppendResult(interp, "Too many tgks windows open, max=4",
        (char *) NULL);
    return TCL_ERROR;
  }

  if (strncmp(*(argv+1),"init",4) == 0) {
    idx =  ++tw_num - 1;

    tkwinmain = Tk_MainWindow (interp);

    tw[idx].tkwin = Tk_NameToWindow(interp, *(argv+2) , tkwinmain);
    tw[idx].win   = Tk_WindowId(tw[idx].tkwin);
    tw[idx].dis   = Tk_Display(tw[idx].tkwin);
    tw[idx].gc    = Tk_GetGC(tw[idx].tkwin,GCFunction ,&tw[idx].gcvalues);
    tw[idx].gcvalues.function = GXinvert;
    XChangeGC(tw[idx].dis,tw[idx].gc,GCFunction,&tw[idx].gcvalues);
    tw[idx].wd    = Tk_Width(tw[idx].tkwin);
    tw[idx].ht    = Tk_Height(tw[idx].tkwin);

    sprintf(tw[idx].cidx,"%d",idx);
    Tk_CreateEventHandler(tw[idx].tkwin,
	 ExposureMask|StructureNotifyMask|FocusChangeMask|
	 ButtonPressMask|ButtonReleaseMask|Button1MotionMask,
	 GksEventProc, tw[idx].cidx);

    /* Initialize NCAR Graphics */
    fgksinit_(&tw[idx].win);

  } else if (strncmp(*(argv+1),"m1mode",4) == 0) {
     /* fprintf(stderr,"C tgks m1mode \"%s\"\n",*(argv+2)); */
     if (strncmp(*(argv+2),"rect",4) == 0) {
        tw[0].m1mode = TGKS_RECT;
     } else if (strncmp(*(argv+2),"point",5) == 0) {
        tw[0].m1mode = TGKS_POINT;
     } else if (strncmp(*(argv+2),"hor",3) == 0) {
        tw[0].m1mode = TGKS_HOR;
     } else if (strncmp(*(argv+2),"ver",3) == 0) {
        tw[0].m1mode = TGKS_VER;
     } else if (strncmp(*(argv+2),"h2",3) == 0) {
        tw[0].m1mode = TGKS_H2;
        XDrawLine(tw[0].dis,tw[0].win,tw[0].gc,
          0,tw[0].ht/2,tw[0].wd,tw[0].ht/2);
     } else if (strncmp(*(argv+2),"v2",3) == 0) {
        tw[0].m1mode = TGKS_V2;
        XDrawLine(tw[0].dis,tw[0].win,tw[0].gc,
          tw[0].wd/2,0,tw[0].wd/2,tw[0].ht);
     } else if (strncmp(*(argv+2),"none",4) == 0) {
        tw[0].m1mode = TGKS_NONE;
     }
  } else if (strncmp(*(argv+1),"m3mode",4) == 0) {
     /* fprintf(stderr,"C tgks m3mode \"%s\"\n",*(argv+2)); */
     if (strncmp(*(argv+2),"rect",4) == 0) {
        tw[0].m3mode = TGKS_RECT;
     } else if (strncmp(*(argv+2),"point",5) == 0) {
        tw[0].m3mode = TGKS_POINT;
     } else if (strncmp(*(argv+2),"none",4) == 0) {
        tw[0].m3mode = TGKS_NONE;
     }
  } else if (strncmp(*(argv+1),"m1cmd",4) == 0) {
     strcpy(tw[0].m1cmd,*(argv+2));
  } else if (strncmp(*(argv+1),"m3cmd",4) == 0) {
     strcpy(tw[0].m3cmd,*(argv+2));
  } else {
    Tcl_AppendResult(interp, "tgks: unknown command - ",*(argv+1),
          (char *) NULL);
    return TCL_ERROR;
  }
       

  return(TCL_OK);
}

int tgks_init (interp)
  Tcl_Interp *interp;
{
  int i;

  for (i=0; i<4; i++) {
    tw[i].tkwin = 0;
    tw[i].m1mode  = TGKS_NONE;
    tw[i].m3mode  = TGKS_NONE;
  }

  /* Callback changes */
  Tcl_DeleteCommand(interp, "tgks");
  Tcl_CreateCommand(interp, "tgks",tgks,NULL,NULL);

  return(0);
}

