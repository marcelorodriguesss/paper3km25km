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

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
char *egetenv(char *, ...);

#ifndef PROGRAM
#include "utils_sub_names.h"
#endif

#define MAXLINELEN 256

#ifndef ENVNAME
#  define ENVNAME "RAMS_PATHS"
#endif

#ifdef PROGRAM
main(int argc,char *argv[])
{
  printf("argc: %d \n",argc);
  printf("argv: %s\n",argv[1]);
  if (argc < 2  || argc > 2)
  {
    fprintf(stderr,"Usage: %s VARNAME\n",argv[0]);
    exit(1);
  }
  printf("%s\n",argv[1]);
  printf("%s\n",egetenv(argv[1],"CG_PATHS"));
  exit(0);
}
#endif

int bills_strncasecmp(const char *s1, const char *s2, int n) {

  char *c1,*c2;
  int  nn;

  for (nn=0; nn<n ; nn++) {
    c1=(char *)s1+nn;
    c2=(char *)s2+nn;
    if (tolower(*c1) != tolower(*c2)) return(1);
    if (*c1=='\0' || *c2=='\0') return(1);
  }
  return(0);
}
/*
#ifdef SGI
char *getenv();
char *index();
#endif
#ifdef IBM
char *getenv();
#endif
*/

char *egetenv(char *variable, ...)

{
  va_list optarg;

  static char value[MAXLINELEN],line[MAXLINELEN];
  char  *env, *envfile;
  FILE *pfile;
  int maxlen, i, icst;
  char *vptr,*valptr;
   
   //printf("eeeeeeeee: %s\n",variable);
   
   strcpy(value,"");
   
  /* First check for an environmental variable */
  if ( (env=getenv(variable)) != NULL)
  {
    strncat(value,env,MAXLINELEN-1);
   //printf("eeeeeeeee: %s\n",variable);
   //printf("eeeeeeeee: %s\n",value);
    return(value);
  }
   
   
   
  /* Now look in our Paths file for this variable */
  /* If optional argument exists, it will be an environmental variable
     pointing to file name. It the argument isn't specified, use default ENVNAME */
     
   va_start(optarg, variable);
   envfile = va_arg(optarg, char *);
   //printf("optional string %s\n", envfile);
   va_end(optarg);
   
   /* According to man page, va_arg gives random errors if arg doesn't exist or if value
      is not compatible with specified type. We will assume that envfile must have
      the string "PATHS" in it... */
   if (strstr(envfile,"PATHS") == NULL) envfile = ENVNAME;
      
   //printf("optional string %s\n", envfile);
   


   if ( (env=getenv(envfile)) == NULL)   {
      fprintf(stderr,"egetenv: WARNING: Environmental variable file name not set or not\n");
      fprintf(stderr,"                  specified as argument. %s \n",envfile);
      return(NULL);
   }
  
   //printf("file: %s\n", env);
    
   if ( (pfile=fopen(env,"r")) == NULL)
   {
      fprintf(stderr,"egetenv: WARNING: could not open file %s\n",env);
      fprintf(stderr,"  Check value of environmental variable %s\n",ENVNAME);
      return(NULL);
   }

  while(fscanf(pfile,"%[^\n]\n",line) != EOF)  /* reads the whole line */
  {
      //printf("line: %s \n",line);
      if (line[0] == '#') continue;                /* Skip comments */
      if (bills_strncasecmp(line,"export ",7)==0) continue;  /* Skip export statement */

      /* See if variable name is in this line */
      /*  NOTE: variable names must not be contained within the values!!! */
      vptr = strstr(line,variable);
      //printf("vptr: %s \n",vptr);
      if (vptr == NULL) continue;
      
      /* Value string starts at character following the variable name */
      maxlen = strlen(variable);
      vptr = vptr + maxlen;
      icst = vptr - &line[0];
      
      /* Strip any leading blanks or = */
      maxlen = strlen(line);
    
      i = icst;
      while (i < maxlen) { 
      if (line[i] != ' ' && line[i] != '=' ) break;
      vptr++; i++;
      }
      valptr = vptr;

    fclose(pfile);
      //printf("result: %s\n", valptr);
      //fprintf(stderr, "Eeeeeeeq file: %d\n",valptr);
    return(valptr);
  }
  fclose(pfile);
  return(NULL);
}

/* FORTRAN INTERFACE */
void fegetenv(var,env,ierr,vlen,elen)
  char *var, *env;
  int  *ierr,vlen,elen;
{
  char tvar[80],*val;
  #ifdef PC_NT1
  extern void exit(int);
  #endif
  
  /* fix up the variable string */
  strncpy(tvar,var,vlen);
  tvar[vlen]='\0';
  val = strchr(tvar,' ');
  if (val != NULL) *val = '\0';

  if ( (val = egetenv(tvar)) != NULL)
  {
   printf("fefefefef: %s\n",val);
    if ( (int)strlen(val) > elen-1)
    {
      fprintf(stderr,"Error: fgetenv: length of env variable too small\n");
      exit(1);
    }
    strcpy(env,val);

    /* now fix up the outgoing string */
    val = (char *)strchr(env,'\0');
    for ( ; val<env+elen; val++) *val=' ';
    *ierr = 0;
  }
  else
  *ierr = 1;
}
