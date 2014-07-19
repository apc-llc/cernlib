*
* $Id: csrd.F,v 1.3 1996/04/15 09:22:05 cernlib Exp $
*
* $Log: csrd.F,v $
* Revision 1.3  1996/04/15 09:22:05  cernlib
* Replace tabs by spaces
*
* Revision 1.2  1996/04/02 22:12:51  thakulin
* Remove CERNLIB_PIAF macro and check the run time variable IPIAF in
* paw/pcmode.inc instead.
*
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 23/09/94  11.15.11  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRD(J,N)
***--------------------------
*     reads next line from terminal
*     or from file or (--from text-library--)
*                         now from include 'file'
*     or from (--page-memory--) memory
***--------------------------
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#include "comis/cskeys.inc"
#if defined(CERNLIB_PAW)
#include "paw/pcmode.inc"
#endif
      LOGICAL CSINCL
      CHARACTER *1 K
      INTEGER INCLUN(10)
      SAVE INCLUN
  11  IF(KEYRD.EQ.1)THEN
  1     CONTINUE
        IF(LIBRD.NE.0)THEN
*
#if !defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)
          READ(LUNLIB,88,END=999)REC
   88     FORMAT(A)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
           REC=' '
           READ(LUNLIB,NUM=NBYT,END=999)REC
#endif
          N=MCLINE(REC)
*
        ELSEIF(ISTFIL.NE.0) THEN
*
#if !defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)
          READ(LUNFIL,77,END=99)REC
   77     FORMAT(A)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
           REC=' '
           READ(LUNFIL,NUM=NBYT,END=99)REC
#endif
          N=MCLINE(REC)
        ELSE
          CALL CSSINP(REC,PROMPT(ISTATE),N)
          IF(N.LT.0)RETURN
        ENDIF
        NSTRG=NSTRG+1
        MARK=0
        IF(CSINCL(REC,N,LIBRD,INCLUN))THEN
          REC(1:1) = '*'
          MARK=800
        ENDIF
        IF(N.EQ.0)THEN
          N=4
          REC='* '
        ENDIF
        CALL CSPUTL(JPMC,REC,N,MARK)
        IF(JPMB.EQ.0)JPMB=JPMC
        K=REC(1:1)
        IF(K.EQ.'*')THEN
#if defined(CERNLIB_PAW)
         IF (IPIAF.EQ.1) THEN
            IF(REC(:8).EQ.'*CS*V*L*')CALL CSPIVEC(REC(9:))
         ENDIF
#endif
          GO TO 1
        ENDIF
        IF(IFORS.NE.0 .AND. K.EQ.'C')GO TO 1
      ELSEIF(KEYRD.EQ.0)THEN
        JMB=JTCH
        NMB=NCHS
        JMC=JMB
        NMC=0
        KEYC=0
        NSTRG=0
        IF(JPMB.NE.0)CALL CSLFRE(JPMB)
        JPMC=0
        GO TO 3
      ELSEIF(KEYRD.EQ.-1)THEN
        IF(JPMC.EQ.JPMB)NSTRG=0
  2     IF(JPMC.EQ.0)THEN
          JPMC=JPML
          GO TO 3
        ENDIF
        JPML=JPMC
        CALL CSGETL(JPMC,REC,N,MARK)
        K=REC(1:1)
        IF(K.EQ.'*')GO TO 2
        IF(IFORS.NE.0 .AND. K.EQ.'C')GO TO 2
      ENDIF
      NT=N
      J=JSR
      KF=MKBLAN(J,N)
      KEYC=0
      IF(IFORS.NE.0)THEN
          NT=NT-N
          IF(NT.EQ.5 .AND. REC(6:6).NE.'0')THEN
             J=J+1
             N=N-1
             KEYC=1
          ENDIF
      ELSE
        KEYC=MDELIM(KF)
      ENDIF
      IF(KEYC.EQ.0)THEN
        JMC=JMB
      ELSE
        JMC=JMC+NMC
      ENDIF
      IF(JMC-JMB+N.GT.NMB)THEN
        PRINT *, 'TOO LONG INPUT STRING'
        GO TO 11
      ENDIF
      CALL CCOPYS(J,JMC,N)
      J=JMC
      NMC=N
      RETURN
  3   KEYRD=1
      GO TO 11
  999 CALL CSCLOS(LUNLIB)
      CLOSE(LUNLIB)
      LIBRD=LIBRD-1
      IF(LIBRD.LE.0)THEN
        ISTLIB=0
        LIBRD=0
      ELSE
        LUNLIB=INCLUN(LIBRD)
      ENDIF
      GO TO 11
  99  CLOSE(LUNFIL)
      ISTFIL=0
      IF(ISTATE.NE.5)GO TO 11
      J=JMC
      N=0
      END
