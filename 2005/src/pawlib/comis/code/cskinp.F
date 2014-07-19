*
* $Id: cskinp.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cskinp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.30.02  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSKINP
***-------------------
*    it is interp. command INPUT
***-------------------
*INP 0,NPAR[,IE,IT]*NPAR
      INTEGER CSNUMB, CSCXNU
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/csrec.inc"
#include "comis/csdpvs.inc"
#include "comis/cssysd.inc"
#include "comis/cslun.inc"
#include "comis/csichv.inc"
      CHARACTER *28 CHTYPE
      PARAMETER ( KLCMLX=2 )
 107  NPAR=KD2(IPC)
      I1=ITA-NPAR+1
      I2=IPC+3
        J1=JSR1
      K=1
 1073 CONTINUE
**      DO 1073 K=1,NPAR
        JT=J1+4
        LT=4
        IPCE=KD(I2)+IBASE
        ITB=KD1(I2)
        JPAR=IA(I1)
        IB1=ITB/100
        IT=MOD(ITB,100)
        NCH=KD(IPCE+1)*NBYTPW
        J=MJCHAR(KD(IPCE+2))
        NCH=MNBLAN(J,NCH)
        LT=LT+NCH
        CALL CCOPYS(J,JT,NCH)
        JT=JT+NCH
        IF(IB1.EQ.1.OR.IB1.GT.6)GO TO 1072
        IF(IB1.EQ.6)GO TO 1071
 1070   IF(IB1.EQ.2)THEN
          CALL CSSOUT(REC1(5:LT))
        ELSE
          REC1(LT+1:LT+3)=' = '
          LT=LT+3
          JT=JT+3
        IF(IT.EQ.1)THEN
           REC1(1:4)='*I'
           CHTYPE=' INTEGER !'
           GO TO 1074
          ELSE IF(IT.EQ.2)THEN
            REC1(1:4)='*R'
           CHTYPE=' REAL !'
           GO TO 1074
          ELSEIF(IT.EQ.3) THEN
            L=MOD(MIWORD(JPAR),KON3)
            J=MIWORD(JPAR+1)
         REC1(1:4)='*Ch'
            CALL CSSINP(REC,REC1(1:LT),NSR)
            IF(NSR.GT.0)THEN
              CALL CCOPYS(JSR,JT,NSR)
              LT=LT+NSR
              JT=JT+NSR
              N=MIN0(L,NSR)
              CALL CCOPYS(JSR,J,N)
              IF(N.LT.L) CALL CSSETC(J+N,L-N,ICHBLN)
            ENDIF
          ELSEIF(IT.EQ.4)THEN
            REC1(1:4)='*L'
 1077       CALL CSSINP(REC,REC1(1:LT),NSR)
            IF(NSR.GT.0)THEN
              J=JSR
              N=NSR
              KK=MKBLAN(J,N)
              IF(MLEQS(J,MJSCHA('.TRUE.'),6).EQ.1)THEN
                INUM=1
              ELSEIF(MLEQS(J,MJSCHA('.FALSE.'),7).EQ.1)THEN
                INUM=0
              ELSE
                CALL CSSOUT('INPUT LOGICAL: .TRUE. OR .FALSE.')
                GO TO 1077
              ENDIF
            ENDIF
            CALL COPYAJ(LOCF(INUM),JPAR,1)
          ELSEIF (IT.EQ.5) THEN
* INPUT DOUBLE PRECISION NUMBER
            REC1(1:4)='*D'
            CHTYPE=' DOUBLE PRECISION !'
            GO TO 1074
          ELSEIF (IT.EQ.7) THEN
* INPUT complex NUMBER
            REC1(1:4)='*Cx'
            CHTYPE=' COMPLEX !'
            GO TO 1074
          ENDIF
        ENDIF
        IF(ISTLOG.NE.0)THEN
          WRITE(LUNLOG,5553)REC1(1:LT)
        ENDIF
        GO TO 1072
 1071  IF(IT.NE.3)THEN
         JPAR=IQ(JPAR+1)
       ELSE
         JPAR=LOCF(IQ(JPAR))
       ENDIF
       GO TO 1070
 1072   I1=I1+1
        I2=I2+2
** 1073 CONTINUE
      K=K+1
      IF(K.LE.NPAR)GO TO 1073
****
      IPC=IPC+NPAR*2+3
      ITA=ITA-NPAR
***      GO TO 999
      RETURN
 1074   CALL CSSINP(REC,REC1(1:LT),NSR)
        IF(NSR.GT.0)THEN
          J=JSR
          N=NSR
          KK=MKBLAN(J,N)
          ISIGN=1
          IF(KK.EQ.ICHMINU)THEN
            ISIGN=-1
            J=J+1
            N=N-1
          ENDIF
          KK=MKBLAN(J,N)
          IF(IT.EQ.7)THEN
            ITINU=CSCXNU(J,N,CX)
          ELSE
            ITINU=CSNUMB(J,N,INUM,R,D)
          ENDIF
          IF(ITINU.NE.IT)THEN
            CALL CSSOUT('INPUT'//CHTYPE)
            GO TO 1074
          ENDIF
          CALL CCOPYS(JSR,JT,NSR)
          LT=LT+NSR
          JT=JT+NSR
        IF(IT.EQ.1)THEN
           I=INUM*ISIGN
           CALL COPYAJ(LOCF(I),JPAR,1)
        ELSEIF(IT.EQ.2)THEN
          R=R*ISIGN
          JWRDR=LOCF(R)
          CALL COPYAJ(JWRDR,JPAR,1)
        ELSEIF(IT.EQ.5)THEN
          D=D*ISIGN
          JWRDD=LOCF(D)
          CALL COPYAJ(JWRDD,JPAR,KDLEN)
        ELSEIF(IT.EQ.7)THEN
          CX=CX*ISIGN
          JWRDD=LOCF(CX)
          CALL COPYAJ(JWRDD,JPAR,KLCMLX)
        ENDIF
      ENDIF
      IF(ISTLOG.NE.0)WRITE(LUNLOG,5553)REC1(1:LT)
      GO TO 1072
5553  FORMAT(A)
      END
