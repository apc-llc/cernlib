*
* $Id: csinfn.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csinfn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 07/07/93  15.10.41  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSINFN(NFUN,ITPAR,NPAR)
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
       LOGICAL BTESTS,BTEST
       CHARACTER *8 IFNAME
       INTEGER IEQDVP(2),IEQD(2)
       EQUIVALENCE (IEQDVP,DVPAR), (IEQD,D)
       IF(NPAR.EQ.1)THEN
         J=IA(ITA)
         IF(ITPAR.LT.5)THEN
            IVPAR=MIWORD(J)
         ELSEIF(ITPAR.EQ.5 .OR. ITPAR.EQ.7)THEN
           IEQDVP(1)=IA(J-JTOPA)
           IEQDVP(2)=IA(J-JTOPA+1)
*****        CALL COPYAJ(J,LOCF(DVPAR),KDLEN)
         ENDIF
       ENDIF
      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     +     17,18,19,20,21,22,23,24,25,26,27,28,29,30,
     +     31,32,33,34,35,36,37,38,39,40,41,42,43,44,
     +     45,46,47,48,49,50,51
     +    ,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
     +     66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     +     80,81,82,83,84,85,86,87,88,89,90
     +                                        ),NFUN
      NFUN=-NFUN
      RETURN
 1    IF(ITPAR.EQ.2)THEN
        RA(ITA)= SQRT(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 2
      ELSEIF(ITPAR.EQ.7)THEN
        CX=CSQRT(CXVPAR)
        GO TO 991
      ELSE
        IFNAME='SQRT'
        GO TO 777
      ENDIF
 2    D= DSQRT(DVPAR)
      GO TO 991
 3    IF(ITPAR.EQ.2)THEN
        RA(ITA)=EXP(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 4
      ELSEIF(ITPAR.EQ.7)THEN
        CX=CEXP(CXVPAR)
        GO TO 991
      ELSE
        IFNAME='EXP'
        GO TO 777
      ENDIF
 4    D=DEXP(DVPAR)
      GO TO 991
 5    IF(ITPAR.EQ.2)THEN
        GO TO 6
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 7
      ELSEIF(ITPAR.EQ.7)THEN
        CX=CLOG(CXVPAR)
        GO TO 991
      ELSE
        IFNAME='LOG'
        GO TO 777
      ENDIF
 6    RA(ITA)= ALOG(RVPAR)
      GO TO 999
 7    D= DLOG(DVPAR)
      GO TO 991
 8    IF(ITPAR.EQ.2)THEN
        GO TO 9
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 10
      ELSE
        IFNAME='LOG10'
        GO TO 777
      ENDIF
 9    RA(ITA)= ALOG10(RVPAR)
      GO TO 999
 10   D= DLOG10(DVPAR)
      GO TO 991
 11   IF(ITPAR.EQ.2)THEN
        RA(ITA)= SIN(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 12
      ELSEIF(ITPAR.EQ.7)THEN
        CX=CSIN(CXVPAR)
        GO TO 991
      ELSE
        IFNAME='SIN'
        GO TO 777
      ENDIF
 12   D= DSIN(DVPAR)
      GO TO 991
 13   IF(ITPAR.EQ.2)THEN
        RA(ITA)= COS(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 14
      ELSEIF(ITPAR.EQ.7)THEN
        CX=CCOS(CXVPAR)
        GO TO 991
      ELSE
        IFNAME='COS'
        GO TO 777
      ENDIF
 14   D= DCOS(DVPAR)
      GO TO 991
 15   IF(ITPAR.EQ.2)THEN
        RA(ITA)= TAN(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 16
      ELSE
        IFNAME='TAN'
        GO TO 777
      ENDIF
 16   D= DTAN(DVPAR)
      GO TO 991
 17   IF(ITPAR.EQ.2)THEN
        RA(ITA)= ASIN(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 18
      ELSE
        IFNAME='ASIN'
        GO TO 777
      ENDIF
 18   D= DASIN(DVPAR)
      GO TO 991
 19   IF(ITPAR.EQ.2)THEN
        RA(ITA)= ACOS(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 20
      ELSE
        IFNAME='ACOS'
        GO TO 777
      ENDIF
 20   D= DACOS(DVPAR)
      GO TO 991
 21   IF(ITPAR.EQ.2)THEN
        RA(ITA)= ATAN(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 22
      ELSE
        IFNAME='ATAN'
        GO TO 777
      ENDIF
 22   D= DATAN(DVPAR)
      GO TO 991
 23   IF(ITPAR.EQ.2)THEN
        J=IA(ITA-1)
        J2=IA(ITA)
        ITA=ITA-1
        RA(ITA)=ATAN2( RA(J-JTOPA),RA(J2-JTOPA))
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 24
      ELSE
        IFNAME='ATAN2'
        GO TO 777
      ENDIF
 24   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      CALL COPYAJ(J,LOCF(D1),KDLEN)
      CALL COPYAJ(J2,LOCF(D2),KDLEN)
      D=DATAN2(D1,D2)
      GO TO 991
 25   IF(ITPAR.EQ.2)THEN
        RA(ITA)= ABS(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 27
      ELSEIF(ITPAR.EQ.1)THEN
        GO TO 26
      ELSEIF(ITPAR.EQ.7)THEN
        RA(ITA)=CABS(CXVPAR)
        GO TO 999
      ELSE
        IFNAME='CABS'
        GO TO 777
      ENDIF
 26   IA(ITA)=IABS(IVPAR)
      GO TO 999
 27   D= DABS(DVPAR)
      GO TO 991
 28   IF(ITPAR.EQ.1)THEN
        J=IA(ITA-1)
        J2=IA(ITA)
        ITA=ITA-1
        IA(ITA)= MOD(IA(J-JTOPA),IA(J2-JTOPA))
        GO TO 999
      ELSEIF(ITPAR.EQ.2)THEN
        GO TO 29
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 30
      ELSE
        IFNAME='MOD'
        GO TO 777
      ENDIF
 29   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      RA(ITA)= AMOD(RA(J-JTOPA),RA(J2-JTOPA))
      GO TO 999
 30   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      CALL COPYAJ(J,LOCF(D1),KDLEN)
      CALL COPYAJ(J2,LOCF(D2),KDLEN)
      D= DMOD(D1,D2)
      GO TO 991
 31   IF(ITPAR.EQ.5)GO TO 34
      IF(ITPAR.EQ.2)GO TO 33
      IF(ITPAR.NE.1)THEN
        IFNAME='MAX'
        GO TO 777
      ENDIF
 32   CONTINUE
      M=1
 3201 N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      IVPAR=IA(IA(ITA)-JTOPA)
      DO 3202 K=N1,N2
        I=IA(IA(K)-JTOPA)
        IF(I.GT.IVPAR)IVPAR=I
 3202 CONTINUE
      IF(M.EQ.1)THEN
        IA(ITA)=IVPAR
      ELSE
        RA(ITA)=IVPAR
      ENDIF
      GO TO 999
 33   CONTINUE
      M=1
 3301 N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      RVPAR=RA(IA(ITA)-JTOPA)
      DO 3302 K=N1,N2
        R=RA(IA(K)-JTOPA)
        IF(R.GT.RVPAR)RVPAR=R
 3302 CONTINUE
      IF(M.EQ.1)THEN
        RA(ITA)=RVPAR
      ELSE
        IA(ITA)=RVPAR
      ENDIF
      GO TO 999
 34   CONTINUE
      N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      J=LOCF(D)
      J2=LOCF(D1)
      CALL COPYAJ(IA(ITA),J,KDLEN)
      DO 3402 K=N1,N2
        CALL COPYAJ(IA(K),J2,KDLEN)
        IF(D1.GT.D)D=D1
 3402 CONTINUE
      GO TO 991
 35   M=0
       GO TO 3201
 36   M=0
       GO TO 3301
 37   IF(ITPAR.EQ.5)GO TO 40
      IF(ITPAR.EQ.2)GO TO 39
      IF(ITPAR.NE.1)THEN
        IFNAME='MIN'
        GO TO 777
      ENDIF
 38   CONTINUE
      M=1
 3801 N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      IVPAR=IA(IA(ITA)-JTOPA)
      DO 3802 K=N1,N2
        I=IA(IA(K)-JTOPA)
        IF(I.LT.IVPAR)IVPAR=I
 3802 CONTINUE
      IF(M.EQ.1)THEN
        IA(ITA)=IVPAR
      ELSE
        RA(ITA)=IVPAR
      ENDIF
      GO TO 999
 39   CONTINUE
      M=1
 3901 N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      RVPAR=RA(IA(ITA)-JTOPA)
      DO 3902 K=N1,N2
        R=RA(IA(K)-JTOPA)
        IF(R.LT.RVPAR)RVPAR=R
 3902 CONTINUE
      IF(M.EQ.1)THEN
        RA(ITA)=RVPAR
      ELSE
        IA(ITA)=RVPAR
      ENDIF
      GO TO 999
 40   CONTINUE
      N=NPAR-1
      N2=ITA
      ITA=ITA-N
      N1=ITA+1
      J=LOCF(D)
      J2=LOCF(D1)
      CALL COPYAJ(IA(ITA),J,KDLEN)
      DO 402 K=N1,N2
        CALL COPYAJ(IA(K),J2,KDLEN)
        IF(D1.LT.D)D=D1
  402 CONTINUE
      GO TO 991
 41   M=0
       GO TO 3801
 42   M=0
       GO TO 3901
 43   CONTINUE
       IA(ITA)=MOD(IVPAR,KON3)
       GO TO 999
 44   CONTINUE
      J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      N=MOD(IA(J-JTOPA),KON3)
      N2=MOD(IA(J2-JTOPA),KON3)
      N=MIN0(N,80)
      N2=MIN0(N2,80)
      J=IA(J-JTOPA+1)
      J2=IA(J2-JTOPA+1)
      CALL CCOPYS(J,JSR,N)
      CALL CCOPYS(J2,JSR1,N2)
      IA(ITA)=INDEX(REC(1:N),REC1(1:N2))
      GO TO 999
 45   CONTINUE
      IF(ITPAR.EQ.1)THEN
        IA(ITA)=IVPAR
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 47
      ELSEIF(ITPAR.NE.7)THEN
        IA(ITA)=CXVPAR
      ELSE
        IFNAME='INT'
        GO TO 777
      ENDIF
 46   CONTINUE
      IA(ITA)=RVPAR
      GO TO 999
 47   CONTINUE
      IA(ITA)=DVPAR
      GO TO 999
 48   CONTINUE
      IF(ITPAR.EQ.1)THEN
        GO TO 49
      ELSEIF(ITPAR.EQ.2)THEN
        RA(ITA)=RVPAR
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 50
      ELSEIF(ITPAR.EQ.7)THEN
        RA(ITA)=CXVPAR
        GO TO 999
      ELSE
        IFNAME='REAL'
        GO TO 777
      ENDIF
 49   CONTINUE
      RA(ITA)=IVPAR
      GO TO 999
 50   CONTINUE
      RA(ITA)=DVPAR
      GO TO 999
 51   CONTINUE
      IF(ITPAR.EQ.1)THEN
        D=IVPAR
      ELSEIF(ITPAR.EQ.2)THEN
        D=RVPAR
      ELSEIF(ITPAR.EQ.7)THEN
        D=CXVPAR
      ENDIF
      GO TO 991
 52   CX=CSQRT(CXVPAR)
      GO TO 991
 53   CX=CEXP(CXVPAR)
      GO TO 991
 54   CX=CLOG(CXVPAR)
      GO TO 991
 55   CX=CSIN(CXVPAR)
      GO TO 991
 56   CX=CCOS(CXVPAR)
      GO TO 991
 57   IF(ITPAR.EQ.2)THEN
        RA(ITA)= SINH(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 58
      ELSE
        IFNAME='SINH'
        GO TO 777
      ENDIF
 58   D= DSINH(DVPAR)
      GO TO 991
 59   IF(ITPAR.EQ.2)THEN
        RA(ITA)= COSH(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 60
      ELSE
        IFNAME='COSH'
        GO TO 777
      ENDIF
 60   D= DCOSH(DVPAR)
      GO TO 991
 61   IF(ITPAR.EQ.2)THEN
        RA(ITA)= TANH(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 62
      ELSE
        IFNAME='TANH'
        GO TO 777
      ENDIF
 62   D= DTANH(DVPAR)
      GO TO 991
 63   RA(ITA)=CABS(CXVPAR)
      GO TO 999
 64   IF(ITPAR.EQ.2)THEN
        RA(ITA)= AINT(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 65
      ELSE
        IFNAME='AINT'
        GO TO 777
      ENDIF
 65   D= DINT(DVPAR)
      GO TO 991
 66   IF(ITPAR.EQ.2)THEN
        RA(ITA)= ANINT(RVPAR)
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        D= DNINT(DVPAR)
        GO TO 991
      ELSE
        IFNAME='ANINT'
        GO TO 777
      ENDIF
 67   IF(ITPAR.EQ.1)THEN
        IF(NPAR.EQ.1)THEN
          CX = CMPLX(IVPAR)
        ELSE
          ITA=ITA-1
          J=IA(ITA)
          J2=IA(ITA+1)
          CX = CMPLX(IA(J-JTOPA),IA(J2-JTOPA))
        ENDIF
      ELSEIF(ITPAR.EQ.2)THEN
        IF(NPAR.EQ.1)THEN
          CX = CMPLX(RVPAR)
        ELSE
          ITA=ITA-1
          J=IA(ITA)
          J2=IA(ITA+1)
          CX = CMPLX(RA(J-JTOPA),RA(J2-JTOPA))
        ENDIF
      ELSEIF(ITPAR.EQ.5)THEN
        IF(NPAR.EQ.1)THEN
          CX = CMPLX(DVPAR)
        ELSE
          ITA=ITA-1
          J=LOCF(D1)
          J2=LOCF(D2)
          CALL COPYAJ(IA(ITA),J,KDLEN)
          CALL COPYAJ(IA(ITA+1),J2,KDLEN)
          CX = CMPLX(D1,D2)
        ENDIF
      ELSEIF(ITPAR.EQ.7)THEN
        IF(NPAR.EQ.1)THEN
          CX = CMPLX(CXVPAR)
        ELSE
          IFNAME='CMPLX'
          GO TO 777
        ENDIF
      ELSE
        IFNAME='CMPLX'
        GO TO 777
      ENDIF
      GO TO 991
 68   IF(ITPAR.NE.7)THEN
        IFNAME='AIMAG'
        GO TO 777
      ENDIF
      RA(ITA)=AIMAG(CXVPAR)
      GO TO 999
 69   IF(ITPAR.NE.7)THEN
        IFNAME='CONJG'
        GO TO 777
      ENDIF
      CX=CONJG(CXVPAR)
      GO TO 991
 70   IF(ITPAR.NE.2)THEN
        IFNAME='DPROD'
        GO TO 777
      ENDIF
      D=DPROD(RA(IA(ITA-1)-JTOPA),RA(IA(ITA)-JTOPA))
      ITA=ITA-1
      GO TO 991
 71   IF(ITPAR.EQ.1)THEN
        GO TO 73
      ELSEIF(ITPAR.EQ.2)THEN
        J=IA(ITA-1)
        J2=IA(ITA)
        ITA=ITA-1
        RA(ITA)= DIM(RA(J-JTOPA),RA(J2-JTOPA))
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 72
      ELSE
        IFNAME='DIM'
        GO TO 777
      ENDIF
 72   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      CALL COPYAJ(J,LOCF(D1),KDLEN)
      CALL COPYAJ(J2,LOCF(D2),KDLEN)
      D= DDIM(D1,D2)
      GO TO 991
 73   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IDIM(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 74   IF(ITPAR.EQ.1)THEN
        GO TO 76
      ELSEIF(ITPAR.EQ.2)THEN
        J=IA(ITA-1)
        J2=IA(ITA)
        ITA=ITA-1
        RA(ITA)= SIGN(RA(J-JTOPA),RA(J2-JTOPA))
        GO TO 999
      ELSEIF(ITPAR.EQ.5)THEN
        GO TO 75
      ELSE
        IFNAME='SIGN'
        GO TO 777
      ENDIF
 75   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      CALL COPYAJ(J,LOCF(D1),KDLEN)
      CALL COPYAJ(J2,LOCF(D2),KDLEN)
      D= DSIGN(D1,D2)
      GO TO 991
 76   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= ISIGN(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 77   J=IA(ITA)
      N=MOD(IA(J-JTOPA),KON3)
      N=MIN0(N,80)
      J=IA(J-JTOPA+1)
      CALL CCOPYS(J,JSR,N)
      IA(ITA)=ICHAR(REC(1:1))
      GO TO 999
 78   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IOR(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 79   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IAND(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 80   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IEOR(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 81   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= ISHFT(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 82   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      BTESTS = BTEST(IA(J-JTOPA),IA(J2-JTOPA))
      IF (BTESTS) THEN
        IA(ITA)= 1
      ELSE
        IA(ITA)= 0
      END IF
      GO TO 999
 83   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IBSET(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 84   J=IA(ITA-1)
      J2=IA(ITA)
      ITA=ITA-1
      IA(ITA)= IBCLR(IA(J-JTOPA),IA(J2-JTOPA))
      GO TO 999
 85   IA(ITA)=NOT(IVPAR)
      GO TO 999
 86   IF(ITPAR.EQ.2)THEN
        IA(ITA)= NINT(RVPAR)
      ELSEIF(ITPAR.EQ.5)THEN
        IA(ITA)= NINT(DVPAR)
      ELSE
        IFNAME='NINT'
        GO TO 777
      ENDIF
      GO TO 999
 87   J=IA(ITA-2)
      J2=IA(ITA-1)
      J3=IA(ITA)
      ITA=ITA-2
      IA(ITA)= ISHFTC(IA(J-JTOPA),IA(J2-JTOPA),IA(J3-JTOPA) )
      GO TO 999
 88   J=IA(ITA-2)
      J2=IA(ITA-1)
      J3=IA(ITA)
      ITA=ITA-2
      IA(ITA)= IBITS(IA(J-JTOPA),IA(J2-JTOPA),IA(J3-JTOPA) )
      GO TO 999
 89   IA(ITA)=LOCF(IVPAR)
      GO TO 999
 90   IA(ITA)=LOCB(IVPAR)
      GO TO 999
777   PRINT *,IFNAME,': wrong number(type) of arguments'
      NFUN=-NFUN
      RETURN
 991  IA(ITA)=IEQD(1)
      IA(ITA+1)=IEQD(2)
***       CALL CCOPYA(D,IA(ITA),KDLEN)
      ITA=ITA+KDLEN-1
 999  END
