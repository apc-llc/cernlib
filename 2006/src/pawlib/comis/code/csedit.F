*
* $Id: csedit.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: csedit.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 11/02/94  17.05.10  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSEDIT(NAME)
***--------------------------
*     this routine invoke host editor
*     a CSTINF puts Text INto File
*     a CSTFRF gets Text FRom File
***--------------------------
      INTEGER CSLTGP
      CHARACTER *(*)NAME
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/csrec.inc"
#include "comis/cstab.inc"
*un+SEQ,CSKEYS.
#include "comis/cspnts.inc"
      CHARACTER *32 FILENM
      INTEGER CSTINF,CSTFRF
#if !defined(CERNLIB_IBM)
      DATA FILENM/'COMIS.EDT'/
#endif
#if defined(CERNLIB_IBMMVS)
      DATA FILENM/'COMIS.EDT'/
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      DATA FILENM/'COMIS EDT'/
#endif
      CALL CSCHID(NAME)
      IP=CSLTGP(IPVS)
      IF(IP.NE.0)THEN
        IF(IFCS.GT.0)THEN
           JPMB=IQ(IADGP-4)
           IF(JPMB.LE.0)THEN
             JPMB=0
             PRINT *,'**CSedit:: no source'
             RETURN
           ENDIF
           I=CSTINF(FILENM)
           IF(I.NE.1)GO TO 77
           CALL CSSEDT(FILENM)
           IF(FILENM(1:1).EQ.'@')THEN
             FILENM(1:1)='C'
             GO TO 77
           ENDIF
           I=CSTFRF(FILENM)
           IF(I.EQ.1)THEN
             KEYRD=-1
             JPMC=JPMB
             IQ(IADGP-4)=0
           ELSE
             KEYRD=0
*??             IQ(IADGP-4)=0
           ENDIF
         ELSE
           REC1='CSEDIT: '//NAME//' -is not comis routine'
           CALL CSSOUT(REC1)
         ENDIF
      ELSE
        REC1='CSEDIT: '//NAME//' - no such routine'
        CALL CSSOUT(REC1)
      ENDIF
      RETURN
 77   KEYRD=0
      END
