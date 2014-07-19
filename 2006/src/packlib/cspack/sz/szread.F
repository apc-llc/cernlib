*
* $Id: szread.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szread.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZREAD(LUN,IBUFF,NREC,NWANT,NGOT,CHOPT,IRC)
#if defined(CERNLIB_APOLLO)
      PARAMETER(IOSEOF=84148232)
#endif
#if defined(CERNLIB_CRAY)
      PARAMETER(IOSEOF=5)
#endif
#if defined(CERNLIB_HPUX)
      PARAMETER(IOSEOF=922)
#endif
#if defined(CERNLIB_IBMRT)
      PARAMETER(IOSEOF=10)
#endif
#if defined(CERNLIB_SGI)
      PARAMETER(IOSEOF=27)
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_QMALPH))
      PARAMETER(IOSEOF=36)
#endif
#if defined(CERNLIB_SUN)||defined(CERNLIB_IBMAIX)||defined(CERNLIB_DECS)||defined(CERNLIB_QMALPH)
      PARAMETER(IOSEOF=-1)
#endif
#include "cspack/zmach.inc"
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
      INTEGER IBUFF(NWANT/IQCHAW)
#include "cspack/czopts.inc"
 
      IRC  = 0
      KERR = 0
      IF(NREC.EQ.0.AND.IOPTV.EQ.0) THEN
         READ(LUN,END=98,ERR=99,IOSTAT=KERR) IBUFF
      ELSEIF(IOPTV.NE.0) THEN
         NGOT = NWANT
         CALL XINB(LUN,IBUFF,NGOT)
         IF(NGOT.LT.0) THEN
            KERR = -NGOT
            GOTO 99
         ENDIF
         IF(NGOT.EQ.0) GOTO 98
         NWANT = NGOT*4
         WRITE(CHMAIL,9002) NGOT
 9002    FORMAT('V',I10)
         CALL CZPUTA(CHMAIL,ISTAT)
      ELSE
         READ(LUN,REC=NREC,ERR=99,IOSTAT=KERR)IBUFF
*
*     Standard defines EOF as negative IOSTAT
*     (unless another errors occurs simulataneously)
*
         IF(KERR.LT.0) GOTO 98
         IF(KERR.GT.0) GOTO 99
      ENDIF
 
      RETURN
 
   98 IRC = -1
      RETURN
   99 IRC = 1
      IF(KERR.EQ.IOSEOF) GOTO 98
      IF(LUNCZ.NE.0) WRITE(LUNCZ,*) 'SZREAD. IOSTAT = ',KERR
 
      END
