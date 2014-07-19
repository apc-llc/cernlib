*
* $Id: cscomx.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: cscomx.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          01/12/95  10.34.03  by  Julian Bunn
*-- Author : V.Berezhnoi
      SUBROUTINE CSCOMX(CONSNM,N,IADR)
***----------------------------------
*      cscom calls this routine
***----------------------------------
         INTEGER CSLTGB,CSITGB
         CHARACTER*(*) CONSNM, NM*72
         INTEGER IADR(N)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cscbwl.inc"
*+SEQ,CSICHV.
      NM=CONSNM
      CALL CLTOU(NM)
      J=MJSCHA(NM)
      JJ=J
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)||defined(CERNLIB_UNIX)||defined(CERNLIB_WINNT)
      LT=LEN(NM)
#endif
#if defined(CERNLIB_APOLLO)
      LT=80
#endif
      DO 1 K=1,N
      KK=MKBLAN(J,LT)
      L=MIDENT(J,LT,JID,KLENID)
      IF(L.LE.0) THEN
        IF(MLEQS(J,MJSCHA('$BLANK'),6).EQ.1)THEN
          CALL UCTOH('$BLANK',IDEN,4,6)
**          IDEN(2)=KBLN
**          CALL CCOPYS(J,JID,6)
          NCIDEN=6
          NWIDEN=2
          J=J+6
          LT=LT-6
        ELSE
          I=J-JJ+1
          WRITE(*,*)'CSCOM: ERROR IN ',NM(1:I)
          RETURN
        ENDIF
      ENDIF
      I=CSLTGB(IPVS)
      IF(I.EQ.0)THEN
        ICGB=0
        IADGB=IADR(K)
        I=CSITGB(IPVS)
      ELSE
        IF(ICGB.GT.2)THEN
         CALL CSTERR(-4)
        ELSE
          IADGB=IADR(K)
          CALL CSRTGB(I)
        ENDIF
      ENDIF
      KK=J-JJ+1
      IF(NM(KK:KK).EQ.'.')THEN
        KK=KK+1
        IF(NM(KK:KK).EQ.'W')THEN
*  add common to list
*  elem=( next, varlist, 'address', nchs, com_name )
          J=J+2
          LT=LT-2
          NW=NWIDEN+4
          IND=MHLOC(NW)
          IQ(IND)=ICBWL
          ICBWL=IND
          IQ(IND+1)=0
          IQ(IND+2)=I+KSADGB
          IQ(IND+3)=NCIDEN
          CALL CCOPYA(IDEN(1),IQ(IND+4),NWIDEN)
        ENDIF
      ENDIF
      KK=MKBLAN(J,LT)
      J=J+1
      LT=LT-1
  1   CONTINUE
      RETURN
      END
