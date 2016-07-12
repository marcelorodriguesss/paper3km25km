#include <stdio.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * HEADER:400:Sec5:inv:0:Sec 5 values (Data representation section)
 */
int f_Sec5(ARG0) {
    if (mode >= 0) {
        sprintf(inv_out,"Sec5 len=%d #defined data points=%d Data Repr. Template=5.%d",
          uint4(&(sec[5][0])), uint4(&(sec[5][5])), uint2(&(sec[5][9])));
    }
    return 0;
}

/*
 * return number of grid points
 */
unsigned int npts(unsigned char **sec) {
	return  GB2_Sec3_npts(sec);
}

/*
 * HEADER:500:npts:inv:0:number of grid points
 */
int f_npts(ARG0) {
    if (mode >= 0) {
	sprintf(inv_out,"npts=%u", GB2_Sec3_npts(sec));
    }
    return 0;
}


/*
 * return number of grid points with values
 */
unsigned int nval(unsigned char **sec) {
	return uint4(sec[5]+5);
}


/*
 * HEADER:510:packing:inv:0:data packing mode
 */

static const char *packing_msg[] = {
	"grid-simple",
	"matrix-simple",
	"grid-complex",
	"grid-complex,spatial-diff",
	"spectral-simple",
	"spectral-complex"};

int f_packing(ARG0) {

    unsigned char *p;
    unsigned int pack;
    if (mode >= 0) {
	p = sec[5];
	pack = code_table_5_0(sec);

	if (mode >= 0) {
	    sprintf(inv_out,"packing=");
	    inv_out += strlen(inv_out);

	    if (pack <= sizeof (packing_msg) / sizeof (p)) {
		sprintf(inv_out,"%s", packing_msg[pack]);
	    }
	    else if (pack == 40 || pack == 40000) {
	        sprintf(inv_out,"JPEG2000");
	    }
	    else if (pack == 41 || pack == 40010) {
	        sprintf(inv_out,"PNG");
	    }
	    else if (pack == 50) {
	        sprintf(inv_out,"Simple,Spherical Harmonic");
	    }
	    else if (pack == 51) {
	        sprintf(inv_out,"Complex,Spherical Harmonic");
	    }
	    else if (pack == 255) {
	        sprintf(inv_out,"missing");
	    }
	    else {
	        sprintf(inv_out,"unknown packing");
	    }
	    inv_out += strlen(inv_out);
	}
	if (mode > 0) {
            if (pack == 0) {
                sprintf(inv_out," val=(%lg+i)*2^%d*10^%d, i=0..%d (#bits=%d)", 
                ieee2flt(p+11), int2(p+15), -int2(p+17), (1 << p[19])-1, p[19]);
	    }
	    if (pack == 50) {
                sprintf(inv_out," val=(%lg+i)*2^%d*10^%d, i=0..%d (#bits=%d) global_mean=%lg", 
                ieee2flt(p+11), int2(p+15), -int2(p+17), (1 << p[19])-1, p[19], ieee2flt(p+20));
	    }
	    if (pack == 51) {
                sprintf(inv_out," val=(%lg+i)*2^%d*10^%d, i=0..%d (#bits=%d)", 
                ieee2flt(p+11), int2(p+15), -int2(p+17), (1 << p[19])-1, p[19]);
	        inv_out += strlen(inv_out);
		sprintf(inv_out," P-Laplacian scaling factor*10^-6=%d",int4(p+20));
	        inv_out += strlen(inv_out);
		sprintf(inv_out," Js=%d Ks=%d Ms=%d Ts=%d", uint2(p+24), uint2(p+26), uint2(p+28), 
			int4(p+30));
	        inv_out += strlen(inv_out);
		sprintf(inv_out," code_table_5.7=%d", (int) p[34]);
/*		sprintf(inv_out," mean?=%lg", ieee2flt(sec[7]+5)); */
	    }
        }
    }
    return 0;
}
