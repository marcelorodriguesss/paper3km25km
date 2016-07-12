#include <tk.h>

#define DEBUG_TGKS 1

/* Event loop button-1 modes */
#define TGKS_NONE  0
#define TGKS_RECT  1
#define TGKS_POINT 2
#define TGKS_HOR   3
#define TGKS_VER   4
#define TGKS_H2    5
#define TGKS_V2    6


typedef struct {
  char cidx[4];        /* Character rep of index to pass to events */
  Tk_Window tkwin;
  Display *dis;
  Window win;
  GC gc;
  XGCValues gcvalues;
  int wd, ht;          /* window width and height */
  char m1cmd[30];      /* For mouse1 command */
  char m3cmd[30];      /* For mouse3 command */
  int  m1mode;         /* Current mode of button-1 mousing around */
  int  m3mode;         /* Current mode of button-3 mousing around */
} TgksWin;

