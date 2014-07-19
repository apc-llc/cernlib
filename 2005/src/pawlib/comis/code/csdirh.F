*
* $Id: csdirh.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: csdirh.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.37  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSDIRH
***--------------------------------
* types help about comis directives
***--------------------------------
      PRINT *,'      List of comis directives:'
      PRINT *,
     +' !edit [cs_routine_name]               : invokes the host editor'
      PRINT *,
     +' !file [lun [,]] file_name     : switch the input stream to file'
      PRINT *,
     +' !log[file] [lun [,]] logfile_name           : switch on logfile'
      PRINT *,
     +' !map[file] [lun [,]] mapfile_name  : opens mapfile.Only for vax'
      PRINT *,
     +' !lib[rary] [lun [,]] libfile_name  : opens libfile.Only for vax'
      PRINT *,
     +' !eof                            : terminate input from the file'
      PRINT *,
     +' !shell  command    : passes the command to the operating system'
      PRINT *,
     +' !for[tran]    : sets the fortran mode of compilation.By default'
      PRINT *,
     +' !comis                     : sets the comis mode of compilation'
      PRINT *,
     +' !show   memory                        : comis memory usage info'
      PRINT *,
     +' !show   routines       : shows list of currently known routines'
      PRINT *,
     +' !show   commons   : shows list of currently known common blocks'
      PRINT *,
     +' !show   names common_name        : shows names of /common-name/'
      PRINT *,
     +' !remove cs_routine_name                 : removes comis_routine'
      PRINT *,
     +' !clear                          : clears comis internal buffers'
      PRINT *,
     +' !nocheckb                  : no check array''s subscript bounds'
      PRINT *,
     +' !checkb          : checks array''s subscript bounds. By default'
      PRINT *,
     +' !notrace                                           : by default'
      PRINT *,
     +' !trace     : very limited info when error during interpretation'
      PRINT *,
     +' !noparam                                           : by default'
      PRINT *,
     +' !param     : provides all facilities for the argument treatment'
      END
