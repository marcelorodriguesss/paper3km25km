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

#include "utils_sub_names.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include <math.h>

#ifdef SGI
#include <errno.h>
#endif

/* Prototypes not needed except for C++
int vfscale(float *,int ,double *,double *);
void *malloc(int);
void *free(void *);
*/

#if !defined(PC_NT1)
/*********************************************************/
#include <glob.h>


void c_listfile(char *pattern, char *fstring, int patlen, int fslen ) {
   glob_t pglob;
   int i, nc, nb, err;
   char colon[]=":";  

/* Get a directory listing of the files with "pattern".
   Concat all names into a colon-delimted string.      */


   glob(pattern, GLOB_ERR, NULL, &pglob);
   
   nb=0;
   for (i = 0; i < pglob.gl_pathc; i++) {
      nc = strlen(pglob.gl_pathv[i]);
      
      if (nb+nc+1 > fslen) {
         printf("\n c_listfile: Fatal error: String length not long enough\n\n");
         exit(1);
      }
      
    /*  printf("found: %d %d %s\n",nb,nc,pglob.gl_pathv[i]); */
      
      memcpy (&fstring[nb],pglob.gl_pathv[i],nc);
      nb = nb + nc;
      memcpy (&fstring[nb],colon,1);
      nb = nb + 1;
      
   }
   globfree(&pglob);

}
#endif

/* ******************************************************* */

void irsleep(int *seconds)
{
   extern int sleep(int);

#if !defined (PC_NT1)
   sleep( *seconds );
#endif

   return;
}

/* ******************************************************* */

FILE *ramsfile;


int rams_c_open(char *filename,char *faccess)

{
  extern FILE *ramsfile;

   printf(" C_open-%s \n",filename); 
  ramsfile=fopen(filename,faccess);
 /* perror("rams_c_open"); */
  return(0);
}

/*********************************************************/

int rams_c_close()
{
  extern FILE *ramsfile;
  int istat;

  istat=fclose(ramsfile);
  return(istat);
}

/*********************************************************/

int rams_c_pos(long int *fbyte)
{ 
  int retcode;
  extern FILE *ramsfile;

  retcode=fseek(ramsfile,*fbyte,0);
  return(retcode);
}

/*********************************************************/

void rams_c_tell(int *pos)
{ 
  extern FILE *ramsfile;

  *pos=ftell(ramsfile);
}

/*********************************************************/

int rams_c_read(int *fbyte,int *numbytes,int *a)
{
  int retcode;
  extern FILE *ramsfile;

  retcode=fseek(ramsfile,*fbyte,0);
  fread(a,1,*numbytes,ramsfile);
  return(retcode);
}

/*********************************************************/
int rams_c_read_char(int *fbyte,int *numbytes,int *a)
 
{
  int retcode;
  extern FILE *ramsfile;

  retcode=fseek(ramsfile,*fbyte,0);
  fread(a,1,*numbytes,ramsfile);
  return(retcode);
}

/*********************************************************/

int rams_c_write(int *fbyte,int *numbytes,int *a)
{
  int retcode;
  extern FILE *ramsfile;

  retcode=fseek(ramsfile,*fbyte,0);
  fwrite(a,1,*numbytes,ramsfile);
  return(retcode);
}

/**********************************************************************/
/*   C versions of vfirecr and vforecr written by Peter Olsson, 1993 */

#include <ctype.h>
#define BITFIELDLENGTH 6
#define SMALL_OFFSET 1.e-20
#define VFORMMASK 63 

void vfirecr(int *unit,float *a,int *n,char *type,char *b,int *irec)    
{
  extern FILE *ramsfile;
  int i, j, nn, nbits, nchs;
  float bias, fact, inverse_fact;
  unsigned vnum, char_count;

  fseek( ramsfile, *irec, 0);
  fread(b,1,80, ramsfile);
  sscanf(b,"%d %d %f %f",&nn, &nbits, &bias, &fact);
  inverse_fact = 1./fact;
  nchs=nbits/6;
  fread(b,1,*n * nchs,ramsfile);
  if( nn != *n )
    printf("Word count mismatch on vfirec record\n  Words on record - %d\n  Words expected  - %d\n ",nn,*n);
  for(i = 0, char_count = 0; i < *n; i++)
  {
    for(j = 0,vnum=0; j < nchs; j++, char_count++)
    {
      vnum = vnum << BITFIELDLENGTH;
      if( isdigit( b[char_count] ) ) 
               vnum = vnum | (unsigned)b[char_count] - 48;
      else if( isupper( b[char_count] ) ) 
               vnum = vnum |(unsigned)b[char_count] - 55;
      else 
               vnum = vnum | (unsigned)b[char_count] - 61;
    }
    a[i] = vnum*inverse_fact - bias;
  }
}

/*************************************************************************/

char vc[65] = 
     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz{|";

void vforecr(int *unit,float *a,int *n,int *nbits,float *scr
            ,char *cscr,char *type,int *irec)
{
  extern FILE *ramsfile;
  extern char vc[];
  double amax, amin, bias, fact; 
  int i, j, char_count, nchs;
  float ftemp;
  extern int vfscale(float*, int, double*, double* );

  vfscale( a, *n, &amin, &amax);

  bias = -amin + SMALL_OFFSET;
  fact = (pow( 2.0, (double)*nbits)-1 ) / ( bias + amax +SMALL_OFFSET);
  fprintf(ramsfile,"%8d%8d%20.10e%20.10e                        "
          ,*n,*nbits,bias,fact);

 /* assume for now that transformation is linear  */   

  for( i = 0; i < *n;  i++ )
    scr[i] = ( a[i] + bias ) * fact;

  nchs = *nbits / BITFIELDLENGTH;

  for( j = 0, char_count = 0 ; j < *n; j++ ) {
    ftemp=scr[j];
    for( i = nchs-1; i >= 0; i--, char_count++ )
      cscr[char_count]= 
          vc[( (unsigned)ftemp >> (i * BITFIELDLENGTH) ) & VFORMMASK];
  }

  fwrite(cscr , sizeof( char ), *n * nchs, ramsfile );
  *irec=ftell(ramsfile);
}

/*************************************************************************/

int vfscale(float *a,int n,double *min,double *max )
{ 
  int i;
  
  *min=  1.e20;
  *max = -*min;  

  for( i = 0; i < n; i++)
  {
    if( a[i] > *max ) *max = a[i];
    if( a[i] < *min ) *min = a[i];
  }
  return(0);
}



