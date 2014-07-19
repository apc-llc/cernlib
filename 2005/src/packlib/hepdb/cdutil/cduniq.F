*
* $Id: cduniq.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cduniq.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUNIQ(FNAME,IRC)
*
*     Generate unique name for journal file
*
#include "hepdb/slate.inc"
      CHARACTER*(*) FNAME
      CHARACTER*8   CHUSER,CHRAND
      CHARACTER*12  CHDATE
      CHARACTER*8   CHHOST
      CHARACTER*6   CHENT
      CHARACTER*1   CHDISK,CMXDSK
      INTEGER       FMUSER
      SAVE NENTRY,CHUSER,CHHOST,LCHU,LCHH
      DATA NENTRY/0/
 
      IF(NENTRY.EQ.0) THEN
         CALL CDUSER(CHUSER,IRC)
         CALL CDHOST(CHHOST,IRC)
         LCHU = LENOCC(CHUSER)
         LCHH = LENOCC(CHHOST)
      ENDIF
 
      NENTRY = NENTRY + 1
      CALL DATIME(ID,IT)
#if !defined(CERNLIB_IBM)
      WRITE(CHENT ,'(I6.6)') NENTRY
      CALL CDRAND(CHRAND,IRC)
*     WRITE(CHRAND,'(I8.8)') MOD(IRNDM(IDUMMY),100000000)
      WRITE(CHDATE,'(I6.6,I4.4,I2.2)') ID,IT,IS(6)
      FNAME = 'ZZ' // CHDATE // CHENT // CHRAND // '.' //
     +                CHUSER(1:LCHU) // '_' // CHHOST(1:LCHH)
#endif
#if defined(CERNLIB_IBMMVS)
      WRITE(CHRAND,'(A1,I5.5,I2.2)') 'R',MOD(IRNDM(IDUMMY),100000),
     +IS(6)
      FNAME = CHUSER(1:LCHU) // '.' // CHHOST(1:LCHH) // '.' //
     +        CHRAND
#endif
#if defined(CERNLIB_IBMVM)
      CALL CDRAND(CHRAND,IRC)
      CHDISK = CMXDSK()
      FNAME  = 'ZZ' // CHRAND(3:) // ' ' // CHUSER(1:LCHU)
     +   // ' ' // CHDISK
#endif
#if defined(CERNLIB_UNIX)
      CALL CUTOL(FNAME)
#endif
 
      END
