*
* $Id: csfmtp.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csfmtp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 15/11/94  14.07.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSFMTP(FORM)
      CHARACTER *(*)FORM, FDS*32, CH*1, CSNOTB*1, CSGETN*1
      CHARACTER ERROR*40, DOTS*5
      PARAMETER (LIMIT=502, LCOD=LIMIT+ 10)
      INTEGER   COD(LCOD)
      COMMON/ CSFICD /ICOD,COD
      LOGICAL   SLASH
      SAVE FDS
      DATA FDS/'0123456789LIZOFEDGAHXTPBS/()'',:$'/
*     K=        12345678911111111112222222222 333
*                        01234567890123456789 012
      DATA DOTS/' ... '/
      N=LEN(FORM)
*??      IF(N.GT.256)GO TO 70
      I=1
      ICOD=1
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      IF(CH.NE.'(')GO TO 71
      LEVEL=1
      SLASH=.FALSE.
      NREP=0
 111  CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      IF(CH.NE.'/')GO TO 112
      SLASH=.TRUE.
      COD(ICOD)=5
      ICOD=ICOD+1
      IF(ICOD.GT.LIMIT)GO TO 70
      GO TO 111
 112  K=INDEX( FDS, CH )
      IF(K.EQ.0)GO TO 71
      IF(ICOD.GT.LIMIT)GO TO 70
      GO TO (1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,
*     K=     1  2  3  4  5  6  7  8  9  10
     +       11,11,11,11,15,15,15,15,19,71,
*     K=     11 12 13 14 15 16 17 18 19 20
     +       71,22,71,24,25,26,27,28,29,30,31,31
*     K=     21 22 23 24 25 26 27 28 29 30 31 32
     +       ),K
 1    CONTINUE
*     DIGIT:  nX or nH nP or repc
      NUM=K-1
      CH=CSGETN(FORM,I,N,NUM)
      IF(CH.EQ.' ')GO TO 71
      IF    (CH.EQ.'H')THEN
*       nHc1c2...cn
        COD(ICOD)=INDEX(FDS,CH)
        COD(ICOD+1)=NUM
        COD(ICOD+2)=I
        ICOD=ICOD+3
        I=I+NUM
      ELSEIF(CH.EQ.'X')THEN
*       nX
        COD(ICOD)=INDEX(FDS,CH)
        COD(ICOD+1)=NUM
        ICOD=ICOD+2
      ELSEIF(CH.EQ.'P')THEN
*       nP
        GO TO 72
      ELSE
        NREP=NUM
*        CH=CSNOTB(FORM,I,N)
*        IF(CH.EQ.' ')GO TO 71
        GO TO 112
      ENDIF
      CH=CSNOTB(FORM,I,N)
      GO TO 61
 11   CONTINUE
*     Lw or Iw or Zw or Ow
      COD(ICOD)=K
      KI=K
      IF(NREP.EQ.0)NREP=1
      COD(ICOD+1)=NREP
      COD(ICOD+3)=I-1
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      K=INDEX(FDS,CH)
      IF(K.EQ.0 .OR. K.GE.11) GO TO 71
      NUM=K-1
      CH=CSGETN(FORM,I,N,NUM)
      COD(ICOD+2)=NUM
      IF(KI.EQ.12)THEN
*---  may be Iw.m
        IF(CH.EQ.'.')THEN
          CH=CSNOTB(FORM,I,N)
          IF(CH.EQ.' ')GO TO 71
          K=INDEX(FDS,CH)
          IF(K.EQ.0 .OR. K.GE.11) GO TO 71
          NUM=K-1
          CH=CSGETN(FORM,I,N,NUM)
        ENDIF
      ENDIF
      COD(ICOD+4)=I-2
      ICOD=ICOD+5
      GO TO 61
 15   CONTINUE
*     Fw.d or Ew.d or Dw.d or Gw.d
      COD(ICOD)=K
      IF(NREP.EQ.0)NREP=1
      COD(ICOD+1)=NREP
      COD(ICOD+3)=I-1
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      K=INDEX(FDS,CH)
      IF(K.EQ.0 .OR. K.GE.11) GO TO 71
      NUM=K-1
      CH=CSGETN(FORM,I,N,NUM)
      COD(ICOD+2)=NUM
      IF(CH.NE.'.')GO TO 71
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      K=INDEX(FDS,CH)
      IF(K.EQ.0 .OR. K.GE.11) GO TO 71
      NUM=K-1
      CH=CSGETN(FORM,I,N,NUM)
      COD(ICOD+4)=I-2
      ICOD=ICOD+5
      GO TO 61
 19   CONTINUE
*     A or Aw
      COD(ICOD)=K
      IF(NREP.EQ.0)NREP=1
      COD(ICOD+1)=NREP
      COD(ICOD+3)=I-1
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      K=INDEX(FDS,CH)
      IF(K.EQ.0 .OR. K.GE.11)THEN
        COD(ICOD+2)=0
        COD(ICOD+4)=I-2
      ELSE
        NUM=K-1
        CH=CSGETN(FORM,I,N,NUM)
        COD(ICOD+2)=NUM
        COD(ICOD+4)=I-2
      ENDIF
      ICOD=ICOD+5
      GO TO 61
 22   CONTINUE
*     Tw or TLw or TRw
      COD(ICOD)=K
      IF(NREP.NE.0)GO TO 71
      CH=FORM(I:I)
      IF(CH.EQ.'L')THEN
        I=I+1
        COD(ICOD)=K+1
      ELSEIF(CH.EQ.'R')THEN
        I=I+1
        COD(ICOD)=K+2
      ENDIF
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      K=INDEX(FDS,CH)
      IF(K.EQ.0 .OR. K.GE.11) GO TO 71
      NUM=K-1
      CH=CSGETN(FORM,I,N,NUM)
      COD(ICOD+1)=NUM
      ICOD=ICOD+2
      GO TO 61
 24   CONTINUE
*     BN or BZ
        GO TO 72
 25   CONTINUE
*     S or SS or SP
        GO TO 72
 26   CONTINUE
*     /
        GO TO 71
 27   CONTINUE
*     (
      IF(LEVEL.EQ.1)THEN
        COD(ICOD)=1
        ICOD=ICOD+1
      ENDIF
      LEVEL=LEVEL+1
      IF(LEVEL.GT.9)GO TO 70
      IF(NREP.EQ.0)NREP=1
      COD(ICOD)=2
      COD(ICOD+1)=NREP
      ICOD=ICOD+2
      NREP=0
      GO TO 111
 28   CONTINUE
*     )
      GO TO 71
 29   CONTINUE
*     '....'
      COD(ICOD)=K
      IF(NREP.NE.0)GO TO 71
      COD(ICOD+1)=I
      I=I-1
 290  I=I+1
      IF(I.GT.N)GO TO 71
      CH=FORM(I:I)
      IF(CH.NE.'''')GO TO 290
      I=I+1
      IF(I.GT.N)GO TO 71
      CH=FORM(I:I)
      IF(CH.EQ.'''')GO TO 290
      COD(ICOD+2)=I-2
      ICOD=ICOD+3
      CH=CSNOTB(FORM,I,N)
      GO TO 61
 30   CONTINUE
*     ,
      IF(.NOT.SLASH)GO TO 71
      CH=CSNOTB(FORM,I,N)
      IF(CH.EQ.' ')GO TO 71
      IF(CH.NE.'/')GO TO 112
      GO TO 62
 31   CONTINUE
*     : or $
      COD(ICOD)=K
      ICOD=ICOD+1
      CH=CSNOTB(FORM,I,N)
      GO TO 61
* delimiter
 61   IF(CH.EQ.' ')GO TO 71
      NREP=0
      IF(CH.EQ.',')THEN
*         skip
        CH=CSNOTB(FORM,I,N)
        IF(CH.EQ.' ')GO TO 71
        IF(CH.NE.'/')GO TO 112
      END IF
      SLASH=.FALSE.
 62   IF(CH.EQ.'/')THEN
        SLASH=.TRUE.
        COD(ICOD)=5
        ICOD=ICOD+1
        IF(ICOD.GT.LIMIT)GO TO 70
        CH=CSNOTB(FORM,I,N)
        IF(CH.EQ.' ')GO TO 71
        GO TO 62
      END IF
      IF(SLASH .AND. CH.NE.')' )GO TO 112
 63   IF( CH .EQ. ')' )THEN
        IF(LEVEL.EQ.1)GO TO 99
        LEVEL=LEVEL-1
        COD(ICOD)=3
        ICOD=ICOD+1
        IF(ICOD.GT.LIMIT)GO TO 70
        CH=CSNOTB(FORM,I,N)
        GO TO 61
      ELSE
        GO TO 71
      ENDIF
  70  CONTINUE
      ERROR=' too long or complex format'
      GO TO 77
  71  CONTINUE
      ERROR=' error in format'
      GO TO 77
  72  CONTINUE
      ERROR=' sorry, not implemented yet'
      GO TO 77
  73  CONTINUE
      ERROR=' extra symbol after format statement'
      GO TO 77
  77  CONTINUE
      PRINT *,ERROR
      IF(I.EQ.1)THEN
        PRINT *, DOTS ,FORM
      ELSEIF(I.LT.N)THEN
        PRINT 7771,FORM(1:I-1),DOTS, FORM(I:N)
 7771 FORMAT(1X,A,A,A)
      ELSE
        PRINT *, FORM(1:N)
      ENDIF
      ICOD=1
      RETURN
  99  CONTINUE
*     end of format
      COD(ICOD)=4
      ICOD=ICOD+1
      II=I
      CH=CSNOTB(FORM,II,N)
      IF(CH.NE.' ')GO TO 73
      END
