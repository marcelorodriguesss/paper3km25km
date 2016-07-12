#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * FlagTable.c
 *
 * routines to return the value of the various flags
 *   policy: use this instead of .h files
 *
 * 12/2006: Public Domain Wesley Ebisuzaki
 * 1/2007 cleanup M. Schwarb
 */


/*
 * HEADER:-1:flag_table_3.3:inv:0:flag table 3.3, resolution and component flags
 */

int f_flag_table_3_3(ARG0) {
    int res;
    if (mode >= 0) {
        res = flag_table_3_3(sec);
	if (res >= 0) {
            sprintf(inv_out,"flag table 3.3=%d", res);
	    inv_out += strlen(inv_out);
        }
    }
    return 0;
}
int flag_table_3_3(unsigned char **sec) {
    int res, grid_template;
    unsigned char *gds;

    grid_template = code_table_3_1(sec);
    gds = sec[3];
    switch (grid_template) {
        case 0:
        case 1:
        case 2:
        case 3:
        case 40:
        case 41:
        case 42:
        case 43:
        case 204:
              res = (int) gds[54]; break;
        case 10:
        case 20:
        case 30:
        case 31:
        case 90:
        case 110:
              res = (int) gds[46]; break;

        default: res = -1; break;
    }
    return res;
}

/*
 * HEADER:-1:flag_table_3.4:inv:0:flag table 3.4, scanning mode
 */

int f_flag_table_3_4(ARG0) {
    int scan;
    extern char *scan_order[];
    if (mode >= 0) {
        scan = flag_table_3_4(sec);
	if (scan >= 0) {
            sprintf(inv_out,"flag table 3.4=%d %s", scan, scan_order[scan >> 4]);
	}
    }
    return 0;
}
int flag_table_3_4(unsigned char **sec) {
    int scan, grid_template;
    unsigned char *gds;

    gds = sec[3];
    grid_template = code_table_3_1(sec);

    switch (grid_template) {
        case 0:
        case 1:
        case 2:
        case 3:
        case 40:
        case 41:
        case 42:
        case 43:
                 scan = (int) gds[71]; break;
        case 10: scan = (int) gds[59]; break;
        case 20: scan = (int) gds[64]; break;
        case 30:
        case 31: scan = (int) gds[64]; break;
        case 50:
        case 51:
        case 52:
        case 53:
                 /* spectral modes don't have scan order */
                 scan = -1; break;
        case 90: scan = (int) gds[63]; break;
        case 110: scan = (int) gds[56]; break;
        case 190: 
	case 120: scan = (int) gds[38]; break;
	case 204: scan = (int) gds[71]; break;
        case 1000: scan = (int) gds[50]; break;
        default: scan = -1; break;
    }
    return scan;
}

/*
 * HEADER:-1:flag_table_3.5:inv:0:flag table 3.5 projection center
 */
int f_flag_table_3_5(ARG0) {
    int p;
    if (mode >= 0) {
        p = flag_table_3_5(sec);
	if (p >= 0) {
            sprintf(inv_out,"flag table 3.5=%d", p);
	    inv_out += strlen(inv_out);
        }
    }
    return 0;
}
int flag_table_3_5(unsigned char **sec) {

    unsigned char *gds;

    gds = sec[3];
    switch (code_table_3_1(sec)) {
        case 20:
        case 30:
        case 31: return gds[63];
        case 110: return gds[55];
    }
    return -1;
}

/*
 * HEADER:-1:flag_table_3.9:inv:0:flag table 3.9 numbering order of diamonds seen from corresponding pole
 */

int f_flag_table_3_9(ARG0) {
    int p;
    if (mode >= 0) {
        p = flag_table_3_9(sec);
        if (p >= 0) {
            sprintf(inv_out,"flag table 3.9=%d", p);
            inv_out += strlen(inv_out);
        }
    }
    return 0;
}
int flag_table_3_9(unsigned char **sec) {

    unsigned char *gds;

    gds = sec[3];
    switch (code_table_3_1(sec)) {
        case 100: return gds[32];
    }
    return -1;
}
/*
 * HEADER:-1:flag_table_3.10:inv:0:flag table 3.10 scanning mode for one diamond
 */
int f_flag_table_3_10(ARG0) {
    int p;
    if (mode >= 0) {
        p = flag_table_3_10(sec);
        if (p >= 0) {
            sprintf(inv_out,"flag table 3.10=%d", p);
            inv_out += strlen(inv_out);
        }
    }
    return 0;
}
int flag_table_3_10(unsigned char **sec) {
    unsigned char *gds;
    gds = sec[3];
    switch (code_table_3_1(sec)) {
        case 100: return gds[33];
    }
    return -1;
}

