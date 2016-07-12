#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/* Levels.c
 *   2006: public domain wesley ebisuzaki
 *   1/2007: cleanup M. Schwarb
 *   1/2007: Caser Tejeda Hernandez found error in meter underground
 *   2/2007: level 11
 *   2/2007: spelling error fixed
 */


/*
 * HEADER:200:lev:inv:0:level (code table 4.5)
 */

/* code table 4.5 */

const char *level_table[192] = {
/* 0 */ "reserved",
/* 1 */ "surface",
/* 2 */ "cloud base",
/* 3 */ "cloud top",
/* 4 */ "0C isotherm",
/* 5 */ "level of adiabatic condensation from sfc",
/* 6 */ "max wind",
/* 7 */ "tropopause",
/* 8 */ "top of atmosphere",
/* 9 */ "sea bottom",
/* 10 */ "reserved",
/* 11 */ "reserved",
/* 12 */ "reserved",
/* 13 */ "reserved",
/* 14 */ "reserved",
/* 15 */ "reserved",
/* 16 */ "reserved",
/* 17 */ "reserved",
/* 18 */ "reserved",
/* 19 */ "reserved",
/* 20 */ "%g K level",
/* 21 */ "reserved",
/* 22 */ "reserved",
/* 23 */ "reserved",
/* 24 */ "reserved",
/* 25 */ "reserved",
/* 26 */ "reserved",
/* 27 */ "reserved",
/* 28 */ "reserved",
/* 29 */ "reserved",
/* 30 */ "reserved",
/* 31 */ "reserved",
/* 32 */ "reserved",
/* 33 */ "reserved",
/* 34 */ "reserved",
/* 35 */ "reserved",
/* 36 */ "reserved",
/* 37 */ "reserved",
/* 38 */ "reserved",
/* 39 */ "reserved",
/* 40 */ "reserved",
/* 41 */ "reserved",
/* 42 */ "reserved",
/* 43 */ "reserved",
/* 44 */ "reserved",
/* 45 */ "reserved",
/* 46 */ "reserved",
/* 47 */ "reserved",
/* 48 */ "reserved",
/* 49 */ "reserved",
/* 50 */ "reserved",
/* 51 */ "reserved",
/* 52 */ "reserved",
/* 53 */ "reserved",
/* 54 */ "reserved",
/* 55 */ "reserved",
/* 56 */ "reserved",
/* 57 */ "reserved",
/* 58 */ "reserved",
/* 59 */ "reserved",
/* 60 */ "reserved",
/* 61 */ "reserved",
/* 62 */ "reserved",
/* 63 */ "reserved",
/* 64 */ "reserved",
/* 65 */ "reserved",
/* 66 */ "reserved",
/* 67 */ "reserved",
/* 68 */ "reserved",
/* 69 */ "reserved",
/* 70 */ "reserved",
/* 71 */ "reserved",
/* 72 */ "reserved",
/* 73 */ "reserved",
/* 74 */ "reserved",
/* 75 */ "reserved",
/* 76 */ "reserved",
/* 77 */ "reserved",
/* 78 */ "reserved",
/* 79 */ "reserved",
/* 80 */ "reserved",
/* 81 */ "reserved",
/* 82 */ "reserved",
/* 83 */ "reserved",
/* 84 */ "reserved",
/* 85 */ "reserved",
/* 86 */ "reserved",
/* 87 */ "reserved",
/* 88 */ "reserved",
/* 89 */ "reserved",
/* 90 */ "reserved",
/* 91 */ "reserved",
/* 92 */ "reserved",
/* 93 */ "reserved",
/* 94 */ "reserved",
/* 95 */ "reserved",
/* 96 */ "reserved",
/* 97 */ "reserved",
/* 98 */ "reserved",
/* 99 */ "reserved",
/* 100 */ "%g hPa",
/* 101 */ "mean sea level",
/* 102 */ "%g m above mean sea level",
/* 103 */ "%g m above ground",
/* 104 */ "%g sigma level",
/* 105 */ "%g hybrid level",
/* 106 */ "%g m underground",
/* 107 */ "%g K isentropic level",
/* 108 */ "reserved",
/* 109 */ "reserved",
/* 110 */ "reserved",
/* 111 */ "reserved",
/* 112 */ "reserved",
/* 113 */ "reserved",
/* 114 */ "reserved",
/* 115 */ "reserved",
/* 116 */ "reserved",
/* 117 */ "reserved",
/* 118 */ "reserved",
/* 119 */ "reserved",
/* 120 */ "reserved",
/* 121 */ "reserved",
/* 122 */ "reserved",
/* 123 */ "reserved",
/* 124 */ "reserved",
/* 125 */ "reserved",
/* 126 */ "reserved",
/* 127 */ "reserved",
/* 128 */ "reserved",
/* 129 */ "reserved",
/* 130 */ "reserved",
/* 131 */ "reserved",
/* 132 */ "reserved",
/* 133 */ "reserved",
/* 134 */ "reserved",
/* 135 */ "reserved",
/* 136 */ "reserved",
/* 137 */ "reserved",
/* 138 */ "reserved",
/* 139 */ "reserved",
/* 140 */ "reserved",
/* 141 */ "reserved",
/* 142 */ "reserved",
/* 143 */ "reserved",
/* 144 */ "reserved",
/* 145 */ "reserved",
/* 146 */ "reserved",
/* 147 */ "reserved",
/* 148 */ "reserved",
/* 149 */ "reserved",
/* 150 */ "reserved",
/* 151 */ "reserved",
/* 152 */ "reserved",
/* 153 */ "reserved",
/* 154 */ "reserved",
/* 155 */ "reserved",
/* 156 */ "reserved",
/* 157 */ "reserved",
/* 158 */ "reserved",
/* 159 */ "reserved",
/* 160 */ "%g m below sea level",
/* 161 */ "reserved",
/* 162 */ "reserved",
/* 163 */ "reserved",
/* 164 */ "reserved",
/* 165 */ "reserved",
/* 166 */ "reserved",
/* 167 */ "reserved",
/* 168 */ "reserved",
/* 169 */ "reserved",
/* 170 */ "reserved",
/* 171 */ "reserved",
/* 172 */ "reserved",
/* 173 */ "reserved",
/* 174 */ "reserved",
/* 175 */ "reserved",
/* 176 */ "reserved",
/* 177 */ "reserved",
/* 178 */ "reserved",
/* 179 */ "reserved",
/* 180 */ "reserved",
/* 181 */ "reserved",
/* 182 */ "reserved",
/* 183 */ "reserved",
/* 184 */ "reserved",
/* 185 */ "reserved",
/* 186 */ "reserved",
/* 187 */ "reserved",
/* 188 */ "reserved",
/* 189 */ "reserved",
/* 190 */ "reserved",
/* 191 */ "reserved"
};


int level1(int mode, int type, float value, int center, int subcenter, char *inv_out);
int level2(int mode, int type1, float value1, int type2, float value2, int center, int subcenter, char *inv_out);

int f_lev(ARG0) {

    int level_type1, level_type2;
    float val1, val2;
    int prod_def, center, subcenter;

    if (mode < 0) return 0;
    prod_def = GB2_ProdDefTemplateNo(sec);
    center = GB2_Center(sec);
    subcenter = GB2_Subcenter(sec);

    switch (prod_def) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 1100:
    case 1101:
	level_type1 = sec[4][22];
	val1 = scaled2flt(INT1(sec[4][23]), int4(sec[4] + 24));

	level_type2 = sec[4][28];
	val2 = scaled2flt(INT1(sec[4][29]), int4(sec[4] + 30));
	break;
    case 20:
    case 30:
    case 1000:
    case 1001:
    case 1002:
    case 254:
	return 1;
	break;
    default:
	fprintf(stderr,"levels: product definition template #%d not supported\n", prod_def);
	return 0;
	break;
    }


    if (mode > 1) {
	sprintf(inv_out,"lvl1=(%d,%lg) lvl2=(%d,%lg):",level_type1,val1,level_type2,val2);
	inv_out += strlen(inv_out);
    }

    level2(mode, level_type1, val1, level_type2, val2, center, subcenter, inv_out);
    return 0;
}

/*
 * level2 is for layers
 */

int level2(int mode, int type1, float value1, int type2, float value2, int center, int subcenter,
	char *inv_out) {

    if (type1 == 100 && type2 == 100) {
	sprintf(inv_out,"%g-%g mb",value1/100,value2/100);
    }
    else if (type1 == 104 && type2 == 104) {
	sprintf(inv_out,"%g-%g sigma layer",value1,value2);
    }
    else if (type1 == 106 && type2 == 106) {
	/* sprintf(inv_out,"%g-%g m below ground",value1/100,value2/100); removed 1/2007 */
	sprintf(inv_out,"%g-%g m below ground",value1,value2);
    }
    else if (type1 == 108 && type2 == 108) {
	sprintf(inv_out,"%g-%g mb above ground",value1/100,value2/100);
    }
    else {
        level1(mode, type1, value1, center, subcenter,inv_out);
	inv_out += strlen(inv_out);
        if (type2 != 255) {
	    sprintf(inv_out," - ");
	    inv_out += strlen(inv_out);
	    level1(mode, type2, value2, center, subcenter,inv_out);
        }
    }
    return 0;
}

/*
 * level1 is for a single level (not a layer)
 */

int level1(int mode, int type, float val, int center, int subcenter,char *inv_out) {


    switch (type) {

    case 100:
	sprintf(inv_out,"%lg mb", val/100.0);
	break;
    case 108:
	sprintf(inv_out,"%lg mb above ground", val/100.0);
	break;
    case 255: /* no numeric info */
	return 8;
	break;
    default:
	if (type <= 192) {
	    sprintf(inv_out,level_table[type], val);
	}
	else if (center == 7){
	    sprintf(inv_out,"NCEP level type %d %g", type, val);
	}
        else {
	    sprintf(inv_out,"local level type %d %g", type, val);
	}
    }
    return 0;
}
