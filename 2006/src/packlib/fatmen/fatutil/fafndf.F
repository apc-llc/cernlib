*
* $Id: fafndf.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fafndf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_UNIX)
      INTEGER FUNCTION FAFNDF(CHPATH,CHFILE,ICONT)
*
*     FORTRAN version of FAFNDF - rough equivalent of LIB$FIND_FILE
*
      CHARACTER*(*) CHPATH,CHFILE
      CHARACTER*255 CHDIR
      CHARACTER*80  PATH,CHTEMP
      INTEGER       SYSTEMF
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
      LOGICAL IOPEN
 
      FAFNDF = 0
*
      LPATH = LENOCC(CHPATH)
      PATH  = CHPATH
 
      LDEF  = LENOCC(DEFAULT)
      CHTEMP = DEFAULT(1:LDEF)//'/'//'fafndf.tmp'
      LTEMP  =  LDEF + 11
 
      IF(IDEBFA.GE.2) PRINT *,'FAFNDF. temporary file is ',
     +   CHTEMP(1:LTEMP)
 
      IF(ICONT.EQ.0) THEN
         INQUIRE(99,OPENED=IOPEN)
         IF(IOPEN) CLOSE(99,STATUS='DELETE')
 
         IC = SYSTEMF('ls '//PATH(1:LPATH)//' > '
     +                //CHTEMP(1:LTEMP)//' 2>&1')
 
         IF(IC.NE.0) GOTO 99
         OPEN(99,FILE=CHTEMP(1:LTEMP),FORM='FORMATTED',
     +        STATUS='UNKNOWN')
      ENDIF
 
      CHFILE = ' '
      READ(99,'(A)',END=99,ERR=99) CHFILE
      RETURN
 
   99 FAFNDF = 1
      CLOSE(99,STATUS='DELETE')
      END
#endif
