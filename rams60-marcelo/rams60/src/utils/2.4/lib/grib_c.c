/*
!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FILE *pipe;

/**********************************************************************/

void ir_popen_ (int *nbuff, char *buff, char *cmd, int *lenout
                           ,int bufflen, int cmdlen) {

int nch;

//printf(" popen cmd: %s \n",cmd);

*lenout=-1;

if ((pipe = popen(cmd, "r")) != NULL) {

   nch =fread(buff, 1, *nbuff, pipe);

   //printf(" popen nch: %d \n",nch);
   //fflush(stdout);

   if ( nch >= *nbuff) {
      fprintf(stderr,"Maximum ir_popen buffer length exceeded: %d\n",nch);
      fprintf(stderr,"popen command: %s\n",cmd);
      fprintf(stderr,"Increase buffer size in ir_popen call\n");
      exit(1);
   }

*lenout=nch;

pclose(pipe);
}
}

/**********************************************************************/

void grib_inventory2_ (char *filein, int *nrecs, char *buff1) {

  char buff[256],cmd[256];
  int i,l;

  sprintf(cmd,"wgrib -s %s",filein);
/*fprintf(stdout,"command: %s\n",cmd); */

  *buff1 = *(char *) calloc(1000,256); 

  *nrecs = 0;
  if ((pipe = popen(cmd, "r")) != NULL) {
    while ((fgets(buff, 256, pipe)) != NULL) {
/*    printf("%d %s",*nrecs,buff);   */
      l = strlen(buff);
      for (i = l; i < 256; i++) {
	 strcat(buff," ");
      }
      strcat(buff1,buff);  
      ++*nrecs;
    }
  }
}

/**********************************************************************/

void grib_grid_ (char *filein, int *irec, char *buff1) {

  char buff[256],cmd[256];
  int nrecs,cat;

  sprintf(cmd,"wgrib %s -V -d %d",filein,*irec);
/*fprintf(stdout,"command: %s\n",cmd); */
  
  *buff1 = *(char *) malloc(256); 

  nrecs = 0;
  cat = 0;
  if ((pipe = popen(cmd, "r")) != NULL) {
    while ((fgets(buff, 256, pipe)) != NULL) {
      ++nrecs;
      if (strstr(buff,"min/max")) {
	cat = 0;
      }
      if (cat) {
        strcat(buff1,buff); 
      }	
      if (strstr(buff,"center")) {
        strcpy(buff1,"");
        cat = 1;	
      }
    }
  }
}


/**********************************************************************/
/*
 *        $ mkfifo mypipe                               make a named pipe
 *        $ wgrib grib_file -d all -bin -o mypipe &     write to "mypipe"
 *                                                      "&" to put in background
 *        $ process_data <mypipe                        read from named pipe
 */

void grib_get_rec_ (char *filein, int *nxy, float *a, int *inrec) {

  int res;
  char cmd[512];

  sprintf(cmd,"rams_wgrib -d %d -nh -o - %s"
            ,*inrec,filein);

/*printf("executed command=%s\n",cmd); */
  if ((pipe = popen(cmd, "r")) != NULL) {
    res=fread(a, 4, *nxy, pipe);
    pclose(pipe);
  }
}


