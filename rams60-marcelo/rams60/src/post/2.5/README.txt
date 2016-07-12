++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                              REVU Version 2.3.1
           Marty Bell - bell@aster.com - MRC/*ASTER - 14th October 2000
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

              *** PRELIMINARY RELEASE NOTES - READ THIS FIRST ***

This is a preliminary (beta) release of REVU version 2.3.1.

New features since REVU version 2.3.0 include...

   * GRIB file format output option (works the same way Vis5D and GrADS).
   
   * The overlay filed may now be filled. Filling does not go beyond the
     specified high/low range. This allows the user to see the map in the
     unfilled portions. Map boundaries can be placed under or on top of the
     color fills. There is also an option for enhanced map boundaries.
   
   * Color options include an array of new color fill schemes for tiles and 
     contours and line colors for contours and vectors. They also include user
     defined color options, control on axis and tile colors, white background
     and grayscale color options and control on landmark plotting.
   
   * Panels option (up to 4 plots per frame).
   
   * Accepts 1 argument, the REVU_IN file name  (i.e.  revu -f <namelist file>)

   * Dump option and correct plotting of soil variables.
   
     In addition, many of the routines now use "implicit none" statements and
     all c iralloc memory allocations are now done with Fortran 90 allocations.
     This has allowed us to do bounds checking on arrays has which in turn has
     resulted in the cleaning up of a number of known and unknown bugs. 

Patches will be made available at...

     http://www.aster.com/revu-2.3.1/patch.shtml

as they made (this list includes all those made for 2.3.0). Notification of new
patches will be sent to the rams-users mailing list and are available for
viewing on the Announce archive...

     http://www.aster.com/lists/announce

You can apply to join the rams-users mailing list at...

     http://www.aster.com/lists/index.shtml
  
While we believe that we are nearing a robust version we know there will be
some issues that we have not uncovered. We would greatly appreciate any reports
of bugs / problems that are found with the code, and of compilation options
that are different / not offered in the distributed makefiles. Please see the
comments at the end of this file before seeking help.

Note also that this code is Fortran 90 Free Format and is not compatible with
any previous versions.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This section describes the compilation of REVU version 2.3.1. This version is 
compatible with RAMS version 4.3.0 and UTILS version 1.0.0.

- Download the Unix gzipped tar file...

     revu-2.3.1.tar.gz

  from the web address...

     http://www.aster.com/revu-2.3.1/revu-2.3.1.tar.gz

- For first time installations, move the tar file into your rams root...

     mv revu-2.3.1.tar.gz rams
     
  unpack the contents of the tar file...

     gunzip < revu-2.3.1.tar.gz | tar -xf -
 
  Upon completion of this step you should find updated versions of the "README"
  and "Copyright" files and updated code, make and input files in the "src",
  "bin" and "run" directories in your rams root.
     
- For upgrade (patch) installations make a temporary directory and move the tar
  file in to that directory...
  
     mkdir tmp
     mv revu-2.3.1-patch.tar.gz tmp

  Unpack the contents of the patch tar file...

     gunzip < revu-2.3.1-patch.tar.gz | tar -xf -
     
  Or, download patched modules individually from our website...
  
     http://www.aster.com/revu-2.3.1/patch.shtml
     
  Replace the modules in your distribution with those new versions contained in
  the patch, noting that since the source comes with read only permissions, you
  will need to modify the permissions of the module in your rams root "src"
  directory before replacing it with the patch version. For example...
  
     chmod u+w <rams root>/src/post/2.3.1/common/vplt.f90
     mv src/post/2.3.1/common/vplt.f90 <rams root>/src/post/2.3.1/common
     
  You may then wish to remove read permissions from the new module
  
     chmod u-w <rams root>/src/post/2.3.1/common/vplt.f90
  
- Before you compile the software, go to the bin directory...

     cd bin
  
  Move the "Makefile-std" to "Makefile" (unless you are already using a non-
  standard version of this file - ie "Makefile-hypact")...
     
     mv Makefile-std Makefile
  
  Either, if this is your first time using this file, move the "include.mk-mrc"
  to "include.mk" and modify it (with vi, for example) to suit your system...
  
     mv include.mk-mrc include.mk
     vi include.mk
  
  As a default, "include.mk-mrc" does not have the NCAR Graphics libraries,
  parallel options, or the compiler flags for any machine type switched on.
  
  ** FIRST TIME USERS MUST CUSTOMIZE THIS FILE BEFORE THEY ATTEMPING TO MAKE **
  
  "include.mk" now contains all the make environment variables that a user might
  need to change in order to compile the code on their machine. It is included
  in all of the make files using the include command.
  
  Or, if you already have a copy of "include.mk" that you have modified to suit
  your system, check your "include.mk" with the new "include.mk-mrc" for
  software system changes in "include.mk-mrc" and make those changes to your
  "include.mk" (with vi, for example)...
  
     diff include.mk include.mk-mrc
     vi include.mk
  
  "dep_revu.mk" contains all the dependencies of the code. This should mean
  that if a file such as "vcomm2.h" is updated, all those modules which use
  this file will be recompiled (noting that "vcomm2.h" is not itself compiled,
  but included in whatever modules require it when they are compiled).
  
  If the include command does not appear to work on your machine, try replacing
  the include command line in each of the make files with the contents of the
  include file. Alternatively, you can download "gnu make" and use that instead
  of your platform version. "GNU Make" is available from the URL...
  
     http://www.gnu.org/software/make/make.html
  
  All readme, make and dependency files are now distributed a version number
  appended. You can optionally remove this from the make file names (do not
  remove them from the dependency file names). For brevity we do not include
  the version numbers in the following instructions.
  
- To compile the software, use either...

     Make -f Make.revu
     
  or to use the global make file "Makefile", enter...
  
     make revu
     
  This should produce the REVU archive libraries "revu-2.3.1.a", the REVU
  executable "revu-2.3.1", and a link from "revu" to "revu-2.3.1".
     
  or the global make file "Makefile" to update all the executables in your "bin"
  directory (uses the file Makefile which in turn points to the above individual
  make files)...
  
     make
  
  This will ensure that all the executables are up to date, noting that there
  are a number of source code cross dependencies in the software system.
  
- If you need the dummy NCAR Graphics routines, also enter...
  
     make ncargd
  
  This should produce the NCAR Graphics dummy archive library 
  "libncarg-1.0.0.a". Note that you need to select an alternative NCAR Graphics
  library set if you do not wish to use the dummies library. NCAR now
  distributes the NCAR Graphics libraries and utilities used by MRC/*ASTeR free
  of charge under the GNU general public license from the URL...
  
     http://ngwww.ucar.edu/ng4.2/download
     
- Note that all the make commands will also run the shell script "check" which
  outputs some advice if it finds a version mismatch (it does not stop the make
  command from completing, although you may then get compilation errors).
  
  "check" compares the versions indicated in the "include.mk" file with the list
  of compatible versions in the "versions" file and the versions installed under
  the rams root directory on your machine.
  
  If the check script fails on your machine you can remove the "check"
  dependency from the "all" target in each of the make files.
  
- "Make.revu" has an "install" target that will install (as a symbolic link) the
   executable to the "run" and "test" directories...
  
     make -f Make.revu install
     
  You can modify "Make.revu" if you wish to install to alternative locations.

- "Make.revu" also has a "clean" target that will remove built components. To
  clean out the compiled libraries and executable...
  
     make -f Make.revu clean
     
  This should remove the REVU archive library "revu-2.3.1.a", the REVU
  executable "revu-2.3.1" and the link from "revu" to "revu-2.3.1".
  
  Warning - "Makefile" also contains a "clean" target. Entering...
  
     make clean
     
  will result in all libraries and executables being removed.

- To recompile REVU when any of the REVU, RAMS library or UTILS library modules
  are updated, repeat the make command...

     make -f Make.revu
     
  or...
  
     make revu

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Notes:

- The full version of REVU version 2.3.1 comes with RAMS version 4.3.0.

- At this point there is very little in the way of user manuals for REVU. We
  have prepared a number of example REVU_IN files for the various applications
  of REVU and these are applied in the test case.
  
- If you have NCAR Graphics ensure the environment variable NCARG_ROOT is set
  in your shell and the appropriate lines in "include.mk" are uncommented.
  
  If you do not have NCAR Graphics and you wish to use the non-visualization
  components of REVU, you will need to link in the NCAR Graphics dummy
  libraries. This is done by uncommenting the relevant lines in "include.mk".

  If you have version 4 of NCAR Graphics, you may need to delete references
  to the -lncarg_loc in "include.mk", as it may not exist in the new
  distribution.
  
  NCAR now distributes the NCAR Graphics libraries and utilities used by
  MRC/*ASTeR free of charge under the GNU general public license from the
  URL...
  
     http://ngwww.ucar.edu/ng4.2/download

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Before you seek help...

- Ensure that you have the latest version of the software. Check for patches at

     http://www.aster.com/rams.4.3.0/patch.shtml  (RAMS)
     http://www.aster.com/revu-2.3.1/patch.shtml  (REVU)
     http://www.aster.com/utils-1.0.0/patch.shtml (UTILS)

  Notification of new patches will be sent to the rams-users mailing list and
  are available for viewing on the Announce archive...

     http://www.aster.com/lists/announce
  
You can apply to join the rams-users mailing list at...

     http://www.aster.com/lists/index.shtml

If you are still having problems...

- Note your machine type, operating system (and version), compiler (and version
  if possible).
  
- Copy the screen output to a file. Try using the script command...

     script -a <file>   (to start the script shell and driect the output)
     revu               (run program) 
     exit               (exit script shell)

  or redirect both standard output and error to a file. For example,
  running revu...
  
     revu 1>&2 <file>  (Korn and Bash shells)
     revu >>& <file>   (C shell)

- Note what configuration or make files were used.

- Send to all of this plus any other supporting information to...

     rams-support@aster.com

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Good Luck! Marty
bell@aster.com
