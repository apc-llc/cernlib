*
* $Id: fmrmln.F,v 1.1.1.1 1996/03/07 15:18:13 mclareni Exp $
*
* $Log: fmrmln.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:13  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMLN(CHLINK,LUN,CHFILE,CHOPT,IRC)
*
*     Remove links, links and source files, find dangling links
*     etc.
*
#include "fatmen/faust.inc"
      CHARACTER*(*) CHLINK,CHFILE
      LOGICAL       IOPEN
      EXTERNAL      FALN
#include "fatmen/falncm.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
*
*     Options: P - print names of dangling links
*              D - write names of dangling links in 'rm ksn' format
*              R - remove dangling links
*              F - redirect output to CHFILE on LWRITE
*
      NFRLNK = NFRLNK + 1
      IOPTTD = IOPTD
      IOPTTF = IOPTF
      IOPTTP = IOPTP
      IOPTTR = IOPTR
      IRC    = 0
      NFILES = 0
      NLINKS = 0
      LLINK  = LENOCC(CHLINK)
      LFILE  = LENOCC(CHFILE)
      IF(IDEBFA.GE.1) PRINT *,'FMRMLN. enter for ',CHLINK(1:LLINK)
      IF(LUN.EQ.0) THEN
         LWRITE = LPRTFA
      ELSE
         LWRITE = LUN
      ENDIF
*
*     Open output file if requested
*
      IF(IOPTF.NE.0) THEN
         INQUIRE(LWRITE,OPENED=IOPEN)
         IF(.NOT.IOPEN) THEN
            CALL FAFILE(LWRITE,CHFILE,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT 9001,CHFILE(1:LFILE),LWRITE
               GOTO 999
            ENDIF
         ENDIF
      ENDIF
*
*     scan for links
*
      CALL FALINK(CHLINK(1:LLINK),99,FALN,IRC)
*
*     Close the output file if we opened it
*
      IF(IOPTF.NE.0.AND..NOT.IOPEN) CLOSE(LWRITE)
 
 9001 FORMAT(' FMRMLN. error ',I6,' opening output file ',A,
     +       ' on unit ',I2)
      WRITE(LPRTFA,9002) NLINKS,NFILES
 9002 FORMAT(' FMRMLN. total of ',I6,' dangling links found, ',I6,
     +       ' files processed')
 
  999 END
