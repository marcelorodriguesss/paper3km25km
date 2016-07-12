/******************************************************************************************
 Copyright (C) 2005-2006  Karl Pfeiffer, Wesley Ebisuzaki
 This file is part of wgrib2 and is distributed under terms of the GNU General Public License 
 For details see, Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, 
    Boston, MA  02110-1301  USA

  4/2006 Bug found by Naoya Suda (lambert2ll) Thanks
*/

/*
 *  kdp 2005-08-22
 *
 *  Routines supporting the -geo option
 *
 * 1/2007 some cleanup M. Schwarb
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/* M_PI, M_PI_2, M_PI_4, and M_SQRT2 are not ANSI C but are commonly defined */
/* values from GNU C library version of math.h copyright Free Software Foundation, Inc. */

#ifndef M_PI
#define M_PI           3.14159265358979323846  /* pi */
#endif
#ifndef M_PI_2
#define M_PI_2         1.57079632679489661923  /* pi/2 */
#endif
#ifndef M_PI_4
#define M_PI_4         0.78539816339744830962  /* pi/4 */
#endif
#ifndef M_SQRT2
#define M_SQRT2        1.41421356237309504880  /* sqrt(2) */
#endif

extern float *lat, *lon;
extern int nx, ny, npnts, res, scan;
extern int n_variable_dim;
extern int *variable_dim, *raw_variable_dim;

double toradians(double x) { return x * (M_PI/180.0); }
double todegrees(double x) { return x * (180.0/M_PI); }

const double EARTH_RADIUS = 6367470.0; /* spherical earth, in meters */ 


int regular2ll(unsigned char **sec, float **lat, float **lon) {
 
    int basic_ang, sub_ang;
    double units, dlat, dlon, lat1, lat2, lon1, lon2;
    double e, w, n, s, dx, dy;
 
    int i, j;
    float *llat, *llon;
    unsigned char *gds;

    gds = sec[3];

    if (ny == -1) {
        fprintf(stderr,"Sorry code does not handle variable ny yet\n");
        return 0;
    }

    if ((*lat = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("regular2ll memory allocation failed","");
    }
    if ((*lon = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("regular2ll memory allocation failed","");
    }

    /* now figure out the grid coordinates mucho silly grib specification */

    basic_ang = GDS_LatLon_basic_ang(gds);
    sub_ang = GDS_LatLon_sub_ang(gds);
    if (basic_ang != 0) {
        units = (double) basic_ang / (double) sub_ang;
    }
    else {
        units = 0.000001;
    }

    dlat = GDS_LatLon_dlat(gds) * units;
    dlon = GDS_LatLon_dlon(gds) * units;
    lat1 = GDS_LatLon_lat1(gds) * units;
    lat2 = GDS_LatLon_lat2(gds) * units;
    lon1 = GDS_LatLon_lon1(gds) * units;
    lon2 = GDS_LatLon_lon2(gds) * units;

    if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0) fatal_error("BAD GDS lon","");
    if (lat1 < -90.0 || lat2 < -90.0 || lat1 > 90.0 || lat2 > 90.0) fatal_error("BAD GDS lat","");

    /* find S latitude and dy */
    if (GDS_Scan_y(scan)) {
	s = lat1;
	n = lat2;
    }
    else {
	s = lat2;
	n = lat1;
    }
    if (s > n) fatal_error("lat-lon grid: lat1 and lat2","");

    dy = (n - s) / (ny - 1);
    if (res & 64) { /* lat increment is valid */
	if (fabs(dy - dlat) > 0.001) fatal_error("lat-lon grid: dlat is inconsistent","");
    }

    /* find W latitude and dx */

    if ( GDS_Scan_row_rev(scan) && (ny % 2 == 0) && ((res & 32) == 0) ) {
	 fatal_error("grib GDS ambiguity","");
    }

    if (GDS_Scan_x(scan)) {
	w = lon1;
	e = lon2;
        if (GDS_Scan_row_rev(scan) && ((res & 32) == 0)) {
	    e = lon1 + (nx-1) * dlon;
	}
    } else {
	w = lon2;
	e = lon1;
        if (GDS_Scan_row_rev(scan) && ((res & 32) == 0)) {
	    w = lon1 - (nx-1) * dlon;
	}
    }

    if (e <= w) e += 360.0;
    if (e-w > 360.0) e -= 360.0;
    if (w < 0) {
	w += 360.0;
	e += 360.0;
    }

    /* lat-lon should be in a WE:SN order */

    llat = *lat;
    if (nx >= 0) {	/* regular grid */
        for (j = 0; j < ny; j++) {
	    for (i = 0; i < nx; i++) {
	        *llat++ = s + j*dy;
	    }
        }
    }
    else {		/* quasi-regular grid */
        for (j = 0; j < ny; j++) {
	    for (i = 0; i < variable_dim[j];  i++) {
	        *llat++ = s + j*dy;
	    }
        }
    }

    llon = *lon;
    if (nx >= 0) {		/* regular grid */
        dx = (e-w) / (nx-1);
        if (res & 128) { /* lon increment is valid */
	    if (fabs(dx - dlon) > 0.001) fatal_error("lat-lon grid: dlon is inconsistent","");
        }
        for (i = 0; i < nx; i++) {
	    llon[i] = w + i*dx >= 360.0 ? w + i*dx - 360.0 : w + i*dx;
        }
        for (i = nx; i < npnts; i++) {
	    llon[i] = llon[i-nx];
        }
    }
    else {			/* quasi-regular grid */
        for (j = 0; j < ny; j++) {
            dx = (e-w) / (variable_dim[j]-1);
            for (i = 0; i < variable_dim[j]; i++) {
	        *llon++ = w + i*dx >= 360.0 ? w + i*dx - 360.0: w + i*dx;
            }
        }
    }
    return 0;
} /* end regular2ll() */ 

/* adapted from iplib */

int polar2ll(unsigned char **sec, float **llat, float **llon) {
    
    float *lat, *lon;
    unsigned char *gds;

    double dx, dy, orient, de, de2, dr, tmp, xp, yp, h, lat1, lon1, dr2;
    double di, dj;
    int ix, iy;

    gds = sec[3];

    if (nx == -1 || ny == -1) {
        fprintf(stderr,"Sorry code does not handle variable nx/ny yet\n");
        return 0;
    }

    if ((*llat = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("polar2ll memory allocation failed","");
    }
    if ((*llon = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("polar2ll memory allocation failed","");
    }
    lat = *llat;
    lon = *llon;


    /* based on iplib */

    lat1 = GDS_Polar_lat1(gds) * (M_PI/180);
    lon1 = GDS_Polar_lon1(gds);
    orient = GDS_Polar_lov(gds);

    lon1 *= (M_PI/180.0);
    orient *= (M_PI/180.0);

    dy  = GDS_Polar_dy(gds);
    dx  = GDS_Polar_dx(gds);

    h = 1.0;
    if (GDS_Polar_sps(gds)) h = -1.0;
    if (! (GDS_Scan_x(scan))) dx = -dx;
    if (! (GDS_Scan_y(scan))) dy = -dy;

    /* 60 probably becomes something else in grib2 */

    de = (1.0 + sin(60.0*(M_PI/180.0))) * radius_earth(sec);
    dr = de * cos(lat1) / (1 + h*sin(lat1));

    xp=-h*sin(lon1-orient)*dr/dx;
    yp= cos(lon1-orient)*dr/dy;

    de2 = de*de;
    for (iy = 0; iy < ny; iy++) {
	for (ix = 0; ix < nx; ix++) {
	    di = (ix - xp) * dx;
	    dj = (iy - yp) * dy;
	    dr2 = di*di + dj*dj;
	    if (dr2 < de2*1e-6) {
		*lon++ = 0.0;
		*lat++ = h*90.0;
	    } else {
		tmp = (orient+h*atan2(di,-dj))*(180.0/M_PI);
		if (tmp < 0.0) tmp += 360.0; 
		if (tmp > 360.0) tmp -= 360.0; 
		*lon++ = tmp;
		*lat++ = h*asin((de2-dr2)/(de2+dr2))*(180.0/M_PI);
	    }
	}
    }
    return 0;
}


int lambert2ll(unsigned char **sec, float **llat, float **llon) {


    double n;
    float *lat, *lon;

    double dx, dy, lat1r, lon1r, lon2d, lon2r, latin1r, latin2r;
    double lond, latd;
    double f, rho, rhoref, theta, startx, starty;
    int i,j,m;
    double x, y, tmp;
    unsigned char *gds;

    gds = sec[3];
    dy      = GDS_Lambert_dy(gds);
    dx      = GDS_Lambert_dx(gds);
    lat1r   = GDS_Lambert_La1(gds) * (M_PI / 180.0);
    lon1r   = GDS_Lambert_Lo1(gds) * (M_PI / 180.0);
    lon2d   = GDS_Lambert_Lov(gds);
    lon2r   = lon2d * (M_PI / 180.0);
    latin1r = GDS_Lambert_Latin1(gds) * (M_PI/180.0);
    latin2r = GDS_Lambert_Latin2(gds) * (M_PI/180.0);

    if (lon1r < 0) fatal_error("bad GDS, lon1r < 0.0","");

    if ( fabs(latin1r - latin2r) < 1E-09 ) {
        n = sin(latin1r);
    }
    else {
        n = log(cos(latin1r)/cos(latin2r)) / 
        log(tan(M_PI_4 + latin2r/2.0) / tan(M_PI_4 + latin1r/2.0));
    }
  
    f = (cos(latin1r) * pow(tan(M_PI_4 + latin1r/2.0), n)) / n;
  
    rho = EARTH_RADIUS * f * pow(tan(M_PI_4 + lat1r/2.0),-n);
    rhoref = EARTH_RADIUS * f * pow(tan(M_PI_4 + latin1r/2.0),-n);
    theta = n * (lon1r - lon2r); 
    startx = rho * sin(theta);
    starty = rhoref - rho * cos(theta);

    if (nx == -1 || ny == -1) {
        fprintf(stderr,"Sorry code does not handle variable nx/ny yet\n");
        return 0;
    }

    if ((*llat = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("lambert2ll memory allocation failed","");
    }
    if ((*llon = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("lambert2ll memory allocation failed","");
    }
    lat = *llat;
    lon = *llon;

    m = 0;
    y = starty;
    for (j = 0; j < ny; j++) {
      y = starty + j*dy;
      tmp = rhoref - y;
      for (i = 0; i < nx; i++) {
        x = startx + i*dx;
        theta = atan(x / tmp); 
        rho = sqrt(x * x + tmp*tmp);
        rho = n > 0 ? rho : -rho;
        lond = lon2d + todegrees(theta/n);
        latd = todegrees(2.0 * atan(pow(EARTH_RADIUS * f/rho,1.0/n)) - M_PI_2);
        if ( lond >= 360.0) lond = lond - 360.0;
        if ( lond < 0.0) lond = lond + 360.0;
        lon[m] = lond;
        lat[m] = latd;
        m++;
      }
    } 
    return 0;
} /* end lambert2ll() */

int mercator2ll(unsigned char **sec, float **lat, float **lon) {

    double dx, dy, lat1, lat2, lon1, lon2;
    float *llat, *llon;
    int i, j;
    double dlon, circum;

    double n,s,e,w,tmp;
    unsigned char *gds;

    gds = sec[3];
    dy     = GDS_Mercator_dy(gds);
    dx     = GDS_Mercator_dx(gds);
    lat1 = GDS_Mercator_lat1(gds);
    lat2 = GDS_Mercator_lat2(gds);
    lon1 = GDS_Mercator_lon1(gds);
    lon2 = GDS_Mercator_lon2(gds);

    if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0) fatal_error("BAD GDS lon","");
    if (lat1 < -90.0 || lat2 < -90.0 || lat1 > 90.0 || lat2 > 90.0) fatal_error("BAD GDS lat","");

    if (GDS_Mercator_ori_angle(gds) != 0.0) {
	fprintf(stderr,"cannot handle non-zero mercator orientation angle %f\n", 
		GDS_Mercator_ori_angle(gds));
	return 0;
    }

    if (nx == -1 || ny == -1) {
        fprintf(stderr,"Sorry code does not handle variable nx/ny yet\n");
        return 0;
    }

    if ((*lat = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("mercator2ll memory allocation failed","");
    }
    if ((*lon = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("mercator2ll memory allocation failed","");
    }

    /* now figure out the grid coordinates mucho silly grib specification */

    /* find S and N latitude */
    if (GDS_Scan_y(scan)) {
        s = lat1;
        n = lat2;
    }
    else {
        s = lat2;
        n = lat1;
    }
    if (s > n) fatal_error("Mercator grid: lat1 and lat2","");

    /* find W and E longitude */

    if ( ((scan & 16) == 16) && (ny % 2 == 0) && ((res & 32) == 0) ) {
         fatal_error("grib GDS ambiguity","");
    }

    if ( ((scan & 16) == 16) && (ny % 2 == 0) ) {
         fatal_error("more code needed to decode GDS","");
    }

    if (GDS_Scan_x(scan)) {
        w = lon1;
        e = lon2;
    } else {
        w = lon2;
        e = lon1;
    }
    if (e <= w) e += 360.0;


    llat = *lat;
    llon = *lon;

    dlon = (e-w) / (nx-1);
    circum = 2.0 * M_PI * radius_earth(sec) * cos(GDS_Mercator_latD(gds) * (M_PI/180.0));
    dx = dx * 360.0 / circum;
    printf("dx %f dlon %f\n", dx , dlon);


    s = log(tan((45+s/2)*M_PI/180));
    n = log(tan((45+n/2)*M_PI/180));
    dy = (n - s) / (ny - 1);

    for (j = 0; j < ny; j++) {
        tmp = (atan(exp(s+j*dy))*180/M_PI-45)*2;
	for (i = 0; i < nx; i++) {
	    *llat++ = tmp;
	}
    }

    for (j = 0; j < nx; i++) {
	llon[j] = w + j*dx >= 360.0 ?  w + j*dx - 360.0 : w + j*dx;
    }
    for (j = nx; j <= npnts; j++) {
        llon[j] = llon[j-nx];
    }
    return 0;
} /* end mercator2ll() */


/*  kdp 2005-08-22
 *  
 *  Code for computing Gaussian latitudes was adapted from
 *  the wonderful gauss2lats.m Matlab program from Tom Holt.
 *  The code gauss2lats.m also works quite well with Octave.
 *
 *  Note that the algorithms used here require a 1-based
 *  array vice the typical 0-based array.  The points are
 *  mapped correctly to the (lat,lon) arrays as zero-based.
 *
 * Note: adapted from an NCAR fortran program by Tom Holt
 */
double gord(int n, double x) {
  
  double colat = acos(x);
  double c1 = M_SQRT2;
  int i;
  
  double fn = (double) n;
  double ang = fn * colat;
  double s1 =  0.0;
  double c4 =  1.0;
  double a  = -1.0;
  double b  =  0.0;
  double fi = (double) i;

  for (i=1; i <= n; i++) {
    c1 = c1 * sqrt(1.0 - 1.0/(4.0*i*i));
  } 
  
  for (i = 0; i <= n; i = i + 2) {
    if ( i == n ) { c4 = 0.5 * c4; }
    s1  = s1 + c4*cos(ang);
    a   = a + 2.0;
    b   = b + 1.0;
    fi = (double) i;
    ang = colat*(fn - fi - 2.0); 
    c4 = (a*(fn-b+1.0)/(b*(fn+fn-a)))*c4;
  }
  
  return ( s1 * c1 );
  
} /* end gord() */


double *gauss2lats(int nlat, double *ylat) {
  
  const double xlim = 1.0E-7;
  
  double *cosc  = (double *) malloc(sizeof(double) * (nlat + 1));
  double *sinc  = (double *) malloc(sizeof(double) * (nlat + 1));
  double *colat = (double *) malloc(sizeof(double) * (nlat + 1));
  
  int nzero = (nlat / 2);
  
  int i;
  double fi = nlat;
  double fi1 = fi + 1.0;
  double a = fi * fi1/sqrt(4.0*fi1*fi1 - 1.0);
  double b = fi1 * fi/sqrt(4.0*fi*fi - 1.0);

    double g, gm, gp, gt, delta, d;

  for (i = 1; i <= nzero; i++) {
    cosc[i] = sin((i - 0.5)*M_PI/nlat + M_PI*0.5);
  }
  
  for (i = 1; i <= nzero; i++) {
    g = gord(nlat, cosc[i]);
    gm = gord(nlat - 1, cosc[i]);
    gp = gord(nlat + 1, cosc[i]);
    gt = (cosc[i]*cosc[i] - 1.0)/(a * gp - b * gm);
    delta = g*gt;
    cosc[i] = cosc[i] - delta;
    
    while ( fabs(delta) > xlim ) {
      g = gord(nlat,cosc[i]);
      gm = gord(nlat - 1, cosc[i]);
      gp = gord(nlat + 1, cosc[i]);
      gt = (cosc[i]*cosc[i] - 1.0)/(a * gp - b * gm);
      delta = g*gt;
      cosc[i] = cosc[i] - delta;
      
    } /* end while */
    
  } /* end for */
  
  for (i = 1; i <= nzero; i++) {
    colat[i] = acos(cosc[i]);
    sinc[i] = sin(colat[i]);
  }
  
  /*
   * ... deal with equator if odd number of points
   */
  if ( ( nlat % 2) != 0 ) {
    i = nzero + 1;
    cosc[i] = 0.0;
    d = gord(nlat - 1, cosc[i]);
    d = d*d*fi*fi;
    colat[i] = M_PI * 0.5;
    sinc[i] = 1.0;
  } /* end if() */
  
  /*
   *  ... deal with southern hemisphere by symmetry
   */
  for (i = nlat - nzero + 1; i <= nlat; i++) {
    cosc[i]  = -cosc[nlat + 1 - i];
    colat[i] = M_PI - colat[nlat + 1 - i];
    sinc[i]  = sinc[nlat + 1 - i];
  } /* end for(i) */
  
  for (i = 1; i <= nlat; i++) {
    ylat[i-1] = todegrees(acos(sinc[i]));
    if ( i > (nlat / 2) ) ylat[i-1] = -ylat[i-1];
    /* change from N-S to S-N */
    ylat[i-1] = -ylat[i-1];
  }

  free(cosc);
  free(sinc);
  free(colat);
  
  return ylat;
  
} /* end gauss2lats() */


int gauss2ll(unsigned char **sec, float **llat, float **llon) {
 
 
    int nlat; /* in grib, number of latitudes must be even! */
  
    double dx, e, w, lat1, lon1, lat2, lon2, *ylat;
    double units;
    float *lat, *lon;
    int basic_ang, sub_ang;
    int i,j,n;
    unsigned char *gds;

    gds = sec[3];
    nlat = 2 * GDS_Gaussian_nlat(gds);

    /* figure out angle units */

    basic_ang = GDS_Gaussian_basic_ang(gds);
    sub_ang = GDS_Gaussian_sub_ang(gds);
    units = basic_ang == 0 ? 0.000001 : (double) basic_ang / (double) sub_ang;

    lat1 = GDS_Gaussian_lat1(gds) * units;
    lat2 = GDS_Gaussian_lat2(gds) * units;
    lon1 = GDS_Gaussian_lon1(gds) * units;
    lon2 = GDS_Gaussian_lon2(gds) * units;

    if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0) fatal_error("BAD GDS lon","");
    if (lat1 < -90.0 || lat2 < -90.0 || lat1 > 90.0 || lat2 > 90.0) fatal_error("BAD GDS lat","");

    if (ny == -1) {
        fprintf(stderr,"Sorry code does not handle variable ny yet\n");
        return 0;
    }

    if (ny != nlat) {
       fatal_error("nx != nlat .. need to fix code to handle this case","");
    }
    if ((*llat = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("gauss2ll memory allocation failed","");
    }
    if ((*llon = (float *) malloc(npnts * sizeof(float))) == NULL) {
        fatal_error("gauss2ll memory allocation failed","");
    }
    lat = *llat;
    lon = *llon;

    /* do latitudes first */
 
    ylat = (double *) malloc(sizeof(double) * nlat);

    gauss2lats(nlat, ylat);

    n = 0;
    if (nx >= 0) {	/* regular grid */
        for (j = 0; j < ny; j++) {
            for (i = 0; i < nx; i++) {
                lat[n++] = ylat[j];
            }
        }
    }
    else {		/* quasi regular grid */
	for (j = 0; j < ny; j++) {
	    for (i = 0; i < variable_dim[j];  i++) {
	        lat[n++] = ylat[j];
	    }
        }
    }

    free(ylat); 

    /* now for the longitudes */
  
    if (GDS_Scan_x(scan)) {
        e = lon1;
        w = lon2;
    }
    else {
        e = lon2;
        w = lon1;
    }
    if (e > w) w += 360.0;
    if (e < 0.0) {
	e += 360.0;
	w += 360.0;
    }
    if (e >= 360.0) {
	e -= 360.0;
	w -= 360.0;
    }

    if (nx >= 0) {
        dx = (w-e) / (nx-1);
        for (j = 0; j < nx; j++ ) {
            lon[j] = e + (dx * j) >= 360.0 ?  e + (dx * j) - 360.0 : e + (dx * j);  
        }
        for (j = nx; j < npnts; j++) {
            lon[j] = lon[j-nx];
        }
    }
    else {
        n = 0;
        for (j = 0; j < ny; j++) {
            dx = (w-e) / (variable_dim[j]-1);
            for (i = 0; i < variable_dim[j]; i++) {
                lon[n++] = e + (dx * i) >= 360.0 ?  e + (dx * i) - 360.0 : e + (dx * i);
            }
        }
    }
    return 0;
} /* end gauss2ll() */


/* find the closest grid point to (plat, plon) */

/* this code needs to be rewritten. too slow */



/* closest_init:  location of grid point in x-y-z space, assume r=1 */ 

static float *x = NULL, *y = NULL, *z = NULL;


int closest_init(unsigned char **sec) {

   int i, npts;
   float s, c;

   npts = GB2_Sec3_npts(sec);
   if (x) {
	free(x);
	free(y);
	free(z);
	x = y = z = NULL;
    }
    if (lat && lon) {
	x = (float *) malloc(npts * sizeof(float));
	y = (float *) malloc(npts * sizeof(float));
	z = (float *) malloc(npts * sizeof(float));
	if (x == NULL || y == NULL || z == NULL) fatal_error("memory allocation closest_init","");

	s = z[0] = sin(lat[0] * (M_PI / 180.0));
	c = sqrt(1.0 - s * s);
	x[0] = c * cos(lon[0] * (M_PI / 180.0));
	y[0] = c * sin(lon[0] * (M_PI / 180.0));

	for (i = 1; i < npts; i++) {
	    if (lat[i] != lat[i-1]) {
	        s = sin(lat[i] * (M_PI / 180.0));
	        c = sqrt(1.0 - s * s);
	    }
	    z[i] = s;
	    x[i] = c * cos(lon[i] * (M_PI / 180.0));
	    y[i] = c * sin(lon[i] * (M_PI / 180.0));
	}
    }
    return 0;
}

int closest(unsigned char **sec, float plat, float plon) {

    int i, j, npts;

    float t, xx, yy, zz, small;
 
    npts = GB2_Sec3_npts(sec);
    if (x == NULL || npts <= 0) return -1;

    zz = sin(plat * (M_PI / 180.0));
    t = sqrt(1.0 - zz*zz);
    xx = t * cos(plon * (M_PI / 180.0));
    yy = t * sin(plon * (M_PI / 180.0));

    small = (x[0]-xx)*(x[0]-xx)+(y[0]-yy)*(y[0]-yy)+(z[0]-zz)*(z[0]-zz);
    j = 0;

    for (i = 1; i < npts; i++) {
        t = (x[i]-xx)*(x[i]-xx)+(y[i]-yy)*(y[i]-yy)+(z[i]-zz)*(z[i]-zz);
	if (t < small) {
	    small = t;
	    j = i;
	}
    }

    return j;
}
