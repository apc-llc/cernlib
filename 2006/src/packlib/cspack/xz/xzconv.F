*
* $Id: xzconv.F,v 1.1.1.1 1996/03/08 15:44:29 mclareni Exp $
*
* $Log: xzconv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:29  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZCONV(LUNIN,LUNOUT,BUFFER,NWORDS,CHOPT,IRC)
 
      DIMENSION BUFFER(NWORDS)
 
#include "cspack/czunit.inc"
#include "cspack/czopts.inc"
 
      IF(IOPTC.NE.0) THEN
 
         NREC = 0
10       CONTINUE
         NREC = NREC + 1
         READ(LUNIN,ERR=99,END=99) BUFFER
         WRITE(LUNOUT,REC=NREC) BUFFER
         GOTO 10
 
      ELSEIF(IOPTF.NE.0) THEN
 
         NREC = 0
20       CONTINUE
         NREC = NREC + 1
         READ(LUNIN,REC=NREC,ERR=99) BUFFER
         WRITE(LUNOUT) BUFFER
         GOTO 20
 
      ELSE
*
*     Invalid mode
*
         IRC = 99
 
      ENDIF
 
99    CONTINUE
      IF(NREC.GT.0) NREC = NREC - 1
      IF(IDEBXZ.GE.0) PRINT *,'XZCONV. converted ',NREC,
     +   ' records'
 
      END
