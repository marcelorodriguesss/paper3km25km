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
#include <string.h>

#include "utils_sub_names.h"

/*************************************************************************/
  


int form_tmpname(char *filename,int len1)

{
  int i, ierr;
  extern int mkstemp(char*);

/*
 *   printf("C_tmpnam - %d %s \n",len1,filename);
 */
  ierr = mkstemp(filename);
/*
 *   printf("C_tmpnam - %d %s \n",len1,filename);
 */
  for (i=strlen(filename); i<len1; i++) {
    *(filename+i) = ' ';
  }
  return(ierr);
}
