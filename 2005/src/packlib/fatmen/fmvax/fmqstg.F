*
* $Id: fmqstg.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmqstg.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQSTG(CHFILE,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*255 CHNAME
      CHARACTER*12  CHUSER
      CHARACTER*8   CHHOST,CHTYPE,CHSYS
      INTEGER       FMHOST,FMVUSR,FAFNDF
      INCLUDE       '($RMSDEF)'
#include "fatmen/fmqued.inc"
#include "fatmen/fatbug.inc"
 
      LFILE = LENOCC(CHFILE)
      IRC   = 0
*
*     Check that we can access queue directory
*
      ICONT  = 0
      ICODE  = FAFNDF(CHQUED(1:LQUED)//CHFILE(1:LFILE),
     +   CHNAME,ICONT)
      IF(ICODE.EQ.RMS$_SUC) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQSTG. request file ',
     +      CHQUED(1:LQUED)//CHFILE(1:LFILE),
     +      ' already exists'
         RETURN
      ENDIF
 
      ICODE = LIB$GET_LUN(LUNSTG)
 
      OPEN(LUNSTG,FILE=CHQUED(1:LQUED)//CHFILE(1:LFILE),STATUS='NEW',
     +     FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMQSTG. error ',IRC,
     +      ' opening ',CHQUED(1:LQUED)//CHFILE(1:LFILE)
         GOTO 90
      ENDIF
 
      CALL DATIME(ID,IT)
 
      IC = FMVUSR(CHUSER)
      LUSER = LENOCC(CHUSER)
      IC = FMHOST(CHHOST,CHTYPE,CHSYS)
      LHOST = LENOCC(CHHOST)
 
      WRITE(LUNSTG,9001) CHFILE(1:LFILE),ID,IT,CHUSER,CHHOST
 9001 FORMAT(' Stage request for ',A,' issued at ',I6,1X,I4,
     +       ' by ',A,' on node ',A)
 
      CLOSE(LUNSTG)
 
 90   CONTINUE
      ICODE = LIB$FREE_LUN(LUNSTG)
      END
