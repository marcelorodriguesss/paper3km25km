#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Some routines that involve Sec3 - Grid definition section
 *
 * Public Domain - Wesley Ebisuzaki
 * mod: 1/2007 M. Schwarb - cleanup
 * mod: 8/2007 Boi Vuong - bug fix - mercator variable dimension had wrong location
 */

extern int nx, ny, npnts, res, scan;
extern enum output_order_type output_order;

int n_variable_dim = 0;
int *variable_dim = NULL, *raw_variable_dim = NULL;


/*
 * HEADER:200:Sec3:inv:0:contents of section 3 (Grid Definition Section)
 */

int f_Sec3(ARG0) {

    unsigned char *p;
    if (mode >= 0) {

	p = sec[3];

	if (p[4] != 3) {
	    fatal_error("Sec3 was expected and not found", "");
	}
	sprintf(inv_out,
           "Sec3 len=%u src gdef=%d npts=%d Grid Def Template=3.%d opt arg=%d" ,
	    GB2_Sec3_size(sec), GB2_Sec3_gdef(sec), GB2_Sec3_npts(sec),
	    code_table_3_1(sec), p[10] );
    }
    return 0;
}

/*
 * figures out nx and ny
 *   res = resolution and component flags table 3.3
 *   scan = scan mode table 3.4
 *
 * Use this code rather than .h files for nx, ny, res and scan
 */

int get_nxny(unsigned char **sec, int *nx, int *ny, int *npnts, int *res, int *scan) {
    int grid_template, n_var_dim, i, j, n_octets;
    unsigned int npoints, n;
    unsigned char *gds, *p;

    grid_template = code_table_3_1(sec);
    *res = flag_table_3_3(sec);
    *scan = flag_table_3_4(sec);

    gds = sec[3];

    switch (grid_template) {
        case 0:
	case 1:
	case 2:
	case 3:
	case 10:
	case 20:
	case 30:
	case 31:
	case 40:
	case 41:
	case 42:
	case 43:
	case 90:
	case 110:
	case 204:
		*nx = uint4_missing(gds+30); *ny = uint4_missing(gds+34); break;
	default:
		*nx = *ny = -1; break;
    }

    n_var_dim = 0;
    if (*nx == -1) n_var_dim = *ny;
    if (*ny == -1) n_var_dim = *nx;
    if (n_var_dim == -1) {
        fprintf(stderr," two variable dimensions are not allowed ");
        n_var_dim = 0;
    }
    p = NULL;
    if (n_var_dim) {
        switch (grid_template) {
           case 0: p = gds + 72; break;
           case 1: p = gds + 84; break;
           case 2:
           case 3: p = gds + 96; break;
           case 10: p = gds + 72; break;
           case 40: p = gds + 72; break;
           case 41: p = gds + 84; break;
           case 42: p = gds + 84; break;
           case 43: p = gds + 96; break;
           default: p = NULL; break;
        }
    }

    /* calculate number of grid points, check with GDS */
    npoints = 0;
    if (n_var_dim) {

        if (n_variable_dim != n_var_dim) {
            if (variable_dim != NULL) free(variable_dim);
            if (raw_variable_dim != NULL) free(raw_variable_dim);
            variable_dim = (int *) malloc(n_var_dim * sizeof(int));
	    if (variable_dim == NULL) fatal_error("ran out of memory","");
            raw_variable_dim = (int *) malloc(n_var_dim * sizeof(int));
	    if (raw_variable_dim == NULL) fatal_error("ran out of memory","");
            n_variable_dim = n_var_dim;
        }
        n_octets = (int) gds[10];	/* number of octets per integer */
        for (i = 0; i < n_var_dim; i++) {
            for (n = j = 0; j < n_octets; j++) {
		n = (n << 8) + (int) *p++;
	    }
            raw_variable_dim[i] = variable_dim[i] = (int) n;
            npoints += n;
        }

        /* convert variable_dim to SN order if needed */
        if (*nx == -1 && GDS_Scan_y(*scan) == 0 && output_order == wesn) {
            for (i = 0; i < *ny; i++) {
		variable_dim[i] = raw_variable_dim[*ny-1-i];
	    }
	}
        /* convert variable_dim to NS order if needed */
        else if (*nx == -1 && GDS_Scan_y(*scan) != 0 && output_order == wens) {
            for (i = 0; i < *ny; i++) {
		variable_dim[i] = raw_variable_dim[*ny-1-i];
	    }
	}
    }
    else if (*nx > 0 && *ny > 0) npoints = (unsigned) *nx * *ny;
    *npnts = (int) npoints;
    if (GB2_Sec3_npts(sec) != npoints) {
        fprintf(stderr,"two values for number of points %u (GDS) %u (calculated)\n",
                      GB2_Sec3_npts(sec), npoints);
    }

/*
    for (i = 0; i < n_var_dim; i++) {
      printf("%d ", variable_dim[i]);
    }
*/

    return 0;
}

/*
 * HEADER:200:nxny:inv:0:nx and ny of grid
 */
int f_nxny(ARG0) {
    if (mode >= 0) sprintf(inv_out,"(%d x %d)",nx,ny);
    return 0;
}


/*
 * HEADER:200:scan:inv:0:scan order of grid
 */
char *scan_order[] = {
        "WE:NS", "WE|EW:NS",
        "NS:WE", "NS(W|E)",

        "WE:SN", "WE|EW:SN",
        "SN:WE", "SN(W|E)",

        "EW:NS", "EW|WE:NS",
        "NS:EW", "NS(E|W)",

        "EW:SN", "EW|WE:SN",
        "SN:EW", "SN(E|W)",
 };

int f_scan(ARG0) {
    if (mode >= 0) {
	if (scan == -1) sprintf(inv_out,"scan=? ????");
        else sprintf(inv_out,"scan=%d %s",scan >>4, scan_order[scan >> 4]);
    }
    return 0;
}

/*
 * HEADER:200:nlons:inv:0:number of longitudes for each latitude
 */

int f_nlons(ARG0) {

    int j;

    if (mode < 0) return 0;

    sprintf(inv_out,"nlon (S/N)=");
    inv_out += strlen(inv_out);

    if (n_variable_dim == 0) {
        for (j = 0; j < ny; j++) {
            sprintf(inv_out,"%d ", nx);
            inv_out += strlen(inv_out);
        }
    }
    else {
        for (j = 0; j < ny; j++) {
            sprintf(inv_out, "%d ",variable_dim[j]);
            inv_out += strlen(inv_out);
        }
    }
    if (ny) inv_out[-1] = 0;
    return 0;
}


/*
 * HEADER:200:grid:inv:0:grid definition
 */

/*
 * some really needs to add some meat to this routine.
 */

int f_grid(ARG0) {
    int grid_template;
    int basic_ang, sub_ang;
    double units;
    double lat1, lon1, lat2, lon2, dlat, dlon;
    unsigned char *gds;

    if (mode >= 0) {

	if (GB2_Sec3_gdef(sec) != 00) {
	    sprintf(inv_out,"no grid template");
            return 0;
	}

        gds = sec[3];
        grid_template = code_table_3_1(sec);

        sprintf(inv_out,"grid_template=%d:", grid_template);
	inv_out += strlen(inv_out);
        switch(grid_template) {
            case 0:
            case 1:
            case 2:
            case 3:
		if (grid_template == 0) sprintf(inv_out,"%slat-lon grid:",nl);
		if (grid_template == 1) sprintf(inv_out,"%srotated lat-lon grid:",nl);
		if (grid_template == 2) sprintf(inv_out,"%sstretched lat-lon grid:",nl);
		if (grid_template == 3) sprintf(inv_out,"%sstretched/rotated lat-lon grid:",nl);
		inv_out += strlen(inv_out);

                basic_ang = GDS_LatLon_basic_ang(gds);
                sub_ang = GDS_LatLon_sub_ang(gds);
		units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

	        sprintf(inv_out,"(%d x %d)",nx,ny);
		inv_out += strlen(inv_out);

		sprintf(inv_out," units %g scan %s res %d%s", units, scan_order[scan>>4],res,nl);
		inv_out += strlen(inv_out);

		lat1 = units * GDS_LatLon_lat1(gds);
		lon1 = units * GDS_LatLon_lon1(gds);
		lat2 = units * GDS_LatLon_lat2(gds);
		lon2 = units * GDS_LatLon_lon2(gds);

		if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
		    fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

		dlon = units * GDS_LatLon_dlon(gds);
		dlat = units * GDS_LatLon_dlat(gds);

		sprintf(inv_out, "lat %g to %g by %g%slon %g to %g by %g", lat1, lat2, dlat, nl,
			lon1, lon2, dlon);
		inv_out += strlen(inv_out);

                if (grid_template == 1) {
                    sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl,ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
                }
                if (grid_template == 42) {
                    sprintf(inv_out, "%sstretch lat=%lf lon=%lf factor=%lf",
                        nl,ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
                }
                if (grid_template == 43) {
                    sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl, ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
                    sprintf(inv_out, ", stretch lat=%lf lon=%lf factor=%lf",
                        ieee2flt(gds+84), ieee2flt(gds+88), ieee2flt(gds+92));
                }
                inv_out += strlen(inv_out);

		if (mode > 0) {
		      sprintf(inv_out,"%sbasic_ang=%d sub_angle=%d units=%lf",nl,basic_ang,sub_ang,units);
                      inv_out += strlen(inv_out);
	              sprintf(inv_out,"%sunscaled lat=%d to %d lon=%u to %u",nl, GDS_Gaussian_lat1(gds), 
			GDS_Gaussian_lat2(gds), GDS_Gaussian_lon1(gds), GDS_Gaussian_lon2(gds));
                      inv_out += strlen(inv_out);
		}
                break;

            case 10: sprintf(inv_out,"%sMercator grid: (%d x %d) LatD %g scan %s res %d%s",nl,
			nx, ny, GDS_Mercator_latD(gds), scan_order[scan>>4],res,nl);
		inv_out += strlen(inv_out);
		lon1 = GDS_Mercator_lon1(gds);
		lon2 = GDS_Mercator_lon2(gds);
		sprintf(inv_out, "lat %g to %g by %g km%slon %g to %g by %g km%s", 
			GDS_Mercator_lat1(gds), GDS_Mercator_lat2(gds), GDS_Mercator_dy(gds)*0.001,nl,
			lon1, lon2, GDS_Mercator_dx(gds)*0.001, nl);

		if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
		      fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);
                break;

            case 20: sprintf(inv_out,"%spolar stereographic grid:",nl);
                  break;
            case 30: 
		    sprintf(inv_out,"%sLambert Conformal: (%d x %d) scan %s res %d%s",nl,
			nx, ny, scan_order[scan>>4],res,nl);
		    inv_out += strlen(inv_out);
		    sprintf(inv_out,"Lat1 %g Lon1 %g Lov %g%s"
                       "Latin1 %g Latin2 %g LatSP %g LonSP %g%s"
                       "      %s (%d x %d) Dx %g Dy %g mode %d",
                     GDS_Lambert_La1(gds), GDS_Lambert_Lo1(gds),
                     GDS_Lambert_Lov(gds), nl,
                     GDS_Lambert_Latin1(gds), GDS_Lambert_Latin2(gds),
                     GDS_Lambert_LatSP(gds), GDS_Lambert_LonSP(gds), nl,
                     GDS_Lambert_NP(gds) ? "North Pole": "South Pole",
                     nx, ny, 0.001*GDS_Lambert_dx(gds), 0.001*GDS_Lambert_dy(gds),
                     res);
                  break;
            case 31:  sprintf(inv_out,"Albers equal area");
                  break;
            case 40: 
	    case 41:
            case 42:
            case 43:
                  basic_ang = GDS_Gaussian_basic_ang(gds);
                  sub_ang = GDS_Gaussian_sub_ang(gds);
		  units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

		  lat1 = units * GDS_Gaussian_lat1(gds);
		  lon1 = units * GDS_Gaussian_lon1(gds);
		  lat2 = units * GDS_Gaussian_lat2(gds);
		  lon2 = units * GDS_Gaussian_lon2(gds);

		  if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
		      fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

		  if (grid_template == 40) sprintf(inv_out,"%sGaussian grid:",nl);
		  if (grid_template == 41) sprintf(inv_out,"%sRotated Gaussian grid:",nl);
		  if (grid_template == 42) sprintf(inv_out,"%sStretched Gaussian grid:",nl);
		  if (grid_template == 43) sprintf(inv_out,"%sStretched-Rotated Gaussian grid:",nl);
		  inv_out += strlen(inv_out);

		  sprintf(inv_out," (%d x %d) units %g scan=%s%s", nx, ny, units, scan_order[scan>>4],nl);
		  inv_out += strlen(inv_out);

		  sprintf(inv_out,"number of latitudes between pole-equator=%d%s", 
			GDS_Gaussian_nlat(gds),nl);
		  inv_out += strlen(inv_out);

		  sprintf(inv_out, "lat %g to %g%slon %g to %g ", 
                         lat1, lat2,nl,lon1, lon2);
                  inv_out += strlen(inv_out);

		  if (grid_template == 41) {
		      sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
			nl,ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
		  }
		  if (grid_template == 42) {
		      sprintf(inv_out, "%sstretch lat=%lf lon=%lf factor=%lf",
			nl,ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
		  }
		  if (grid_template == 43) {
		      sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
			nl, ieee2flt(gds+72), ieee2flt(gds+76), ieee2flt(gds+80));
		      sprintf(inv_out, ", stretch lat=%lf lon=%lf factor=%lf",
			ieee2flt(gds+84), ieee2flt(gds+88), ieee2flt(gds+92));
		 }
                 inv_out += strlen(inv_out);

		 if (mode > 0) {
		      sprintf(inv_out,"%sbasic_ang=%d sub_angle=%d units=%lf",nl,basic_ang,sub_ang,units);
                      inv_out += strlen(inv_out);
	              sprintf(inv_out,"%sunscaled lat=%d to %d lon=%u to %u",nl, GDS_Gaussian_lat1(gds), 
			GDS_Gaussian_lat2(gds), GDS_Gaussian_lon1(gds), GDS_Gaussian_lon2(gds));
                      inv_out += strlen(inv_out);
		 }
                 break;

            case 50: sprintf(inv_out,"Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d",
			GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
			GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds));
		  break;
            case 51: sprintf(inv_out,"Rotated Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
			GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
			GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                     inv_out += strlen(inv_out);
                     sprintf(inv_out,"  South pole of proj lat=%f lon=%f rotation angle=%f",
			ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36));
		  break;
            case 52: sprintf(inv_out,"Stretched Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
			GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
			GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                     inv_out += strlen(inv_out);
                     sprintf(inv_out,"  pole of stretching lat=%f lon=%f stretching=%f",
			ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36));
		  break;
            case 53: sprintf(inv_out,"Rotated=Stretched Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
			GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
			GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                     inv_out += strlen(inv_out);
                     sprintf(inv_out,"  South pole of proj lat=%f lon=%f rotation angle=%f%s",
			ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36),nl);
                     inv_out += strlen(inv_out);
                     sprintf(inv_out,"  pole of stretching lat=%f lon=%f stretching=%f",
			ieee2flt(gds+40), ieee2flt(gds+44),ieee2flt(gds+48));

	    case 90: sprintf(inv_out,"Space view perspective or orographic");
		  break;
	    case 100: sprintf(inv_out,"Triangular grid based on icosahedron");
		  break;
	    case 110: sprintf(inv_out,"Equatorial azimuthal equidistant projection");
		  break;
	    case 120: 
		  sprintf(inv_out,"Azimuth-range projection Nb=%u Nr=%u center lat=%lf lon%lf%s",
			uint4(gds+14), uint4(gds+18), ieee2flt(gds+22), ieee2flt(gds+26),nl);
                  inv_out += strlen(inv_out);
		  sprintf(inv_out,"   Dx=%lf Dstart=%lf%s",ieee2flt(gds+30),ieee2flt(gds+34),nl);
		  break;
	    case 204:
	          sprintf(inv_out,"Curvilinear Orthongonal grid: see lat lon fields in this grib file");
		  break;
	    case 1000: sprintf(inv_out,"Cross-section, points equal spaced on horizontal");
		  break;
	    case 1100: sprintf(inv_out,"Hovmoller diagram, equal spaced on horizontal");
		  break;
	    case 1200: sprintf(inv_out,"Time section grid");
		  break;
	    default: sprintf(inv_out,"no other grid info");
		  break;
        
        }
    }
    return 0;
}
