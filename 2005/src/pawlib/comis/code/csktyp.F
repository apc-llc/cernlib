*
* $Id: csktyp.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: csktyp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 11/02/94  09.54.48  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSKTYP
***-------------------
*    it is interp. command TYPE
*    TYPE 0,NPAR[,IE,IT]*NPAR
***-------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/csbuf.inc"
#include "comis/csrec.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
#include "comis/cslun.inc"
      CHARACTER *80 CHTYPE
      CRWORD(J)=RA(J-JTOPA)
 108  NPAR=KD2(IPC)
      I1=ITA-NPAR+1
      I2=IPC+3
      CALL CSCLRB('*T  ')
      DO 1083 K=1,NPAR
        REC=' '
        JT=JSR
        LT=1
        IPCE=KD(I2)+IBASE
        ITB=KD1(I2)
        JPAR=IA(I1)
        IB1=ITB/100
        IT=MOD(ITB,100)
        IF(IB1.NE.2)THEN
**IF(TYPE NAME)THEN
          NCH=KD(IPCE+1)*NBYTPW
          J=MJCHAR(KD(IPCE+2))
          NCH=MNBLAN(J,NCH)
          NCH=MIN0(NCH,72)
          LT=LT+NCH
          CALL CCOPYS(J,JT,NCH)
          JT=JT+NCH
**ENDIF
        ENDIF
        IF(IB1.GT.6)GO TO 1082
        IF(IB1.EQ.6)GO TO 1081
 1080   IF(IB1.NE.2)THEN
**IF(TYPE NAME)THEN
          REC(LT:)=' = '
          JT=JT+3
          LT=LT+3
**ENDIF
        ENDIF
        N1=81-LT
        J=MJSCHA(CHTYPE)
        CHTYPE=' '
        N=80
        IF(IT.EQ.1)THEN
            I=MIWORD(JPAR)
            WRITE(CHTYPE,5550)I
 5550       FORMAT(1X,I10)
            KK=MKBLAN(J,N)
            N=MNBLAN(J,N)
        ELSE IF(IT.EQ.2)THEN
            R=CRWORD(JPAR)
            WRITE(CHTYPE,5551)R
 5551       FORMAT(1X,G14.7)
            KK=MKBLAN(J,N)
            N=MNBLAN(J,N)
        ELSEIF(IT.EQ.3)THEN
            N=MOD(MIWORD(JPAR),KON3)
            J=MIWORD(JPAR+1)
        ELSEIF(IT.EQ.4)THEN
            I=MIWORD(JPAR)
            IF(I.NE.0)THEN
              J=MJSCHA('.TRUE.')
              N=6
            ELSE
              J=MJSCHA('.FALSE.')
              N=7
            ENDIF
        ELSEIF(IT.EQ.5) THEN
            JWRDD=LOCF(D)
            CALL COPYAJ(JPAR,JWRDD,KDLEN)
            WRITE(CHTYPE,5552)D
 5552          FORMAT(1X,D24.14)
**            KK=MKBLAN(J,N)
**            N=MNBLAN(J,N)
            CALL CSRBLK(CHTYPE(:28),N)
        ELSEIF(IT.EQ.6)THEN
            NW=MIWORD(JPAR-1)
            CALL COPYAJ(JPAR,LOCF(IB(4)),NW)
            N=NW*NBYTPW
            J=JB4
        ELSEIF(IT.EQ.7) THEN
            JWRDD=LOCF(D)
            CALL COPYAJ(JPAR,JWRDD,KDLEN)
            WRITE(CHTYPE,5553)CX
 5553          FORMAT(1X,' (',G14.7,',',G14.7,')' )
            CALL CSRBLK(CHTYPE(:36),N)
**            KK=MKBLAN(J,N)
**            N=MNBLAN(J,N)
        ENDIF
 1084   IF(N1.GE.N+2)THEN
          CALL CCOPYS(J,JT,N)
          CALL CSADDB(REC(:LT+N+1))
        ELSEIF(N1.GE.N)THEN
          CALL CCOPYS(J,JT,N)
          CALL CSADDB(REC)
          CALL CSADDB('  ')
        ELSE
          CALL CCOPYS(J,JT,N1)
          CALL CSADDB(REC)
          J=J+N1
          N=N-N1
          REC=' '
          LT=1
          JT=JSR
          N1=80
          GO TO 1084
        ENDIF
        GO TO 1082
 1081   CONTINUE
        CALL CSOUTB
        CALL CSTARR(JPAR,LT,IT)
 1082   I1=I1+1
        I2=I2+2
 1083 CONTINUE
      CALL CSOUTB
      IPC=IPC+NPAR*2+3
      ITA=ITA-NPAR
***      GO TO 999
      END
