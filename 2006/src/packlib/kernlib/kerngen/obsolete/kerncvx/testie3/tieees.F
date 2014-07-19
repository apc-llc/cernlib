*
* $Id: tieees.F,v 1.1.1.1 1996/02/15 17:52:30 mclareni Exp $
*
* $Log: tieees.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:30  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM TEST

#if defined(CERNLIB_QMCV64)
C--       converted values = IEEE
      PARAMETER    (IOVPCS = '000000007F800000'X)
      PARAMETER    (IOVNCS = '00000000FF800000'X)
      PARAMETER    (IBADCS = '000000007F80FE00'X)
      PARAMETER    (IONECS = '000000003F800000'X)
      PARAMETER    (IZERCS = '0000000080000000'X)

C--       machine values = native
      PARAMETER    (IOVPMS = '7FF7FFFFFFFFFFFF'X)
      PARAMETER    (IOVNMS = 'FFF7FFFFFFFFFFFF'X)
      PARAMETER    (IBADMS = '80007FFFFFFFFFFF'X)
      PARAMETER    (MSKSIG = 'FFFFFFFE00000000'X)
#endif
#if defined(CERNLIB_QMCV32)
C--       converted values = IEEE
      PARAMETER    (IOVPCS = '7F800000'X)
      PARAMETER    (IOVNCS = 'FF800000'X)
      PARAMETER    (IBADCS = '7F80FE00'X)
      PARAMETER    (IONECS = '3F800000'X)
      PARAMETER    (IZERCS = '80000000'X)

C--       machine values = native
      PARAMETER    (IOVPMS = '7FBFFFFF'X)
      PARAMETER    (IOVNMS = 'FFBFFFFF'X)
      PARAMETER    (IBADMS = '80007FFF'X)
      PARAMETER    (MSKSIG = 'FFFFFFFE'X)
#endif

      PARAMETER    (NEL = 220)

      DIMENSION     ORG(NEL), ORGPR(NEL)
      DIMENSION    IORG(NEL)
      EQUIVALENCE (IORG,ORG)

      DIMENSION     TONEW(NEL), FRNEW(NEL)
      DIMENSION    ITONEW(NEL),IFRNEW(NEL)
      EQUIVALENCE (ITONEW,TONEW)
      EQUIVALENCE (IFRNEW,FRNEW)

      CHARACTER    CBADTO(NEL)*5,  CBADFR(NEL)*5,  CNOTE(NEL)*20

      CHARACTER    CHPG*1
      CHARACTER    CHDATE*8, CHTIME*8
      DIMENSION    MDATE(2), MTIME(2)
      EQUIVALENCE (CHDATE,MDATE), (CHTIME,MTIME)

      INTEGER*4    NEXP(NEL)

      DIMENSION    MSPEC(11)

#if defined(CERNLIB_QMCV64)
C--                Test top of range
      DATA  MSPEC(1)   / '47CFFFFFFFFFFFFF'X /
      DATA  MSPEC(2)   / '47DFFFFFFFFFFFFF'X /
      DATA  MSPEC(3)   / '47EFFFFFFFFFFFFF'X /
      DATA  MSPEC(4)   / '47FFFFFFFFFFFFFF'X /
C--                Special cases
      DATA  MSPEC(5)   / '80007FFFFFFFFFFF'X /
      DATA  MSPEC(6)   / '7FF7FFFFFFFFFFFF'X /
      DATA  MSPEC(7)   / 'FFF7FFFFFFFFFFFF'X /
C--               Bottom of range
      DATA  MSPEC(8)   / '3830000000000000'X /
      DATA  MSPEC(9)   / '3820000000000000'X /
      DATA  MSPEC(10)  / '3810000000000000'X /
      DATA  MSPEC(11)  / '3800000000000000'X /
#endif
#if defined(CERNLIB_QMCV32)
C--                Test top of range
      DATA  MSPEC(1)   / '7DFFFFFF'X /
      DATA  MSPEC(2)   / '7E7FFFFF'X /
      DATA  MSPEC(3)   / '7EFFFFFF'X /
      DATA  MSPEC(4)   / '7F7FFFFF'X /
C--                Special cases
      DATA  MSPEC(5)   / '80007FFF'X /
      DATA  MSPEC(6)   / '7FBFFFFF'X /
      DATA  MSPEC(7)   / 'FFBFFFFF'X /
C--               Bottom of range
      DATA  MSPEC(8)   / '00A00000'X /
      DATA  MSPEC(9)   / '00800000'X /
      DATA  MSPEC(10)  / '00400000'X /
      DATA  MSPEC(11)  / '00200000'X /
#endif


#if defined(CERNLIB_QMCV64)
      OPEN (6,FILE='tie3s_64.lis',STATUS='UNKNOWN')
 9001 FORMAT ('1Program TIEEES 64-bit executing'/1X)
#endif
#if defined(CERNLIB_QMCV32)
      OPEN (6,FILE='tie3s_32.lis',STATUS='UNKNOWN')
 9001 FORMAT ('1Program TIEEES 32-bit executing'/1X)
#endif

      ITER = 0
   12 WRITE (6,9001)

      DO  13  J=1,NEL
      CBADTO(J) = ' '
      CBADFR(J) = ' '
   13 CNOTE(J)  = ' '
      JWHICH = 0

      FACT = 2.**60
      VAL  = 17.123456789012
      VAL  = VAL * FACT

      DO 14 I=1,NEL
      FACT = 1.0 + FLOAT(I) * 0.00001
      VALU = VAL * FACT
      IF (ITER.EQ.0)  THEN
          ORG(I)=  VALU
        ELSE
          ORG(I)= -VALU
        ENDIF
      VAL = VAL/2.
   14 CONTINUE

C--                Special values

      CNOTE(32) = '-> high range'
      IF (ITER.EQ.0)  THEN
          CALL UCOPY (MSPEC,IORG(32),4)
        ELSE
          CALL VCOPYN (MSPEC,IORG(32),4)
        ENDIF
      CBADTO(32)(4:4) = '!'
      CBADTO(33)(4:4) = '!'
      CBADTO(34)(4:4) = '!'
      CBADTO(35)(4:4) = '!'

      CNOTE(71) = '-> exact 1.0'
       ORG(71) = 1.0
       ORG(72) = 1.0
      CBADTO(71)(4:4) = '!'
      CBADTO(72)(4:4) = '!'

      CNOTE(97) = '-> NaN'
      IORG(97) = IBADMS
      CBADTO(97)(4:4) = '!'

      CNOTE(144) = '-> +ve overflow'
      IORG(144) = IOVPMS
      CBADTO(144)(4:4) = '!'

      CNOTE(184) = '-> -ve overflow'
      IORG(184) = IOVNMS
      CBADTO(184)(4:4) = '!'

      CNOTE(161) = '-> test zero'
      IORG(161) = 0
      IORG(162) = 0
      CBADTO(161)(4:4) = '!'

      CNOTE(171) = '-> low range'
      IF (ITER.EQ.0)  THEN
          CALL UCOPY (MSPEC(8),IORG(171),4)
        ELSE
          CALL VCOPYN (MSPEC(8),IORG(171),4)
        ENDIF
      CBADTO(171)(4:4) = '!'
      CBADTO(172)(4:4) = '!'
      CBADTO(173)(4:4) = '!'
      CBADTO(174)(4:4) = '!'

      CALL UCOPY (ORG,ORGPR,NEL)

C----              'Writing'

      NDN  = 0
      NUSE = 0
   42 NUSE = NUSE + 1
      IF (NUSE.EQ.11)  NUSE = 41
      NWDS = MIN (NEL-NDN,NUSE)
      JTE  = NDN + NWDS + 1
      IF (JTE.GT.NEL)  JTE = 0
      CBADTO(NDN+1)(1:1) = '.'

      IF (JTE.NE.0)  ITONEW(JTE) = 77
      CALL IE3FOS (ORG(NDN+1),TONEW(NDN+1),NWDS,JBAD)
      IF (JBAD.LT.0 .OR. JBAD.GT.NWDS)      GO TO 82
      IF (JBAD.NE.0)  CBADTO(NDN+JBAD)(3:3) = 't'
      IF (JTE.NE.0)  THEN
          IF (ITONEW(JTE).NE.77)   GO TO 91
        ENDIF

      NDN = NDN + NWDS
      IF (NDN.LT.NEL)              GO TO 42

C--                Special values

      CNOTE(51) = '<- +ve overflow'
      CNOTE(52) = '<- -ve overflow'
      ITONEW(51)  = IOVPCS
      ITONEW(52)  = IOVNCS
      CBADTO(51)(4:4) = ':'
      CBADTO(52)(4:4) = ':'

      CNOTE(61) = '<- NaN'
      ITONEW(61)  = IBADCS
      CBADTO(61)(4:4) = ':'

      CNOTE(72) = '<- exact 1.0'
      ITONEW(72) = IONECS
      CBADTO(72)(4:4) = ':'

      CNOTE(162) = '<- test -0.0'
      ITONEW(162) = IZERCS
      CBADTO(162)(4:4) = ':'

C----              'reading'

      NDN  = 0
      NUSE = 0
   52 NUSE = NUSE + 1
      IF (NUSE.EQ.11)  NUSE = 41
      NWDS = MIN (NEL-NDN,NUSE)
      JTE  = NDN + NWDS + 1
      IF (JTE.GT.NEL)  JTE = 0

      IF (JTE.NE.0)  IFRNEW(JTE) = 77
      CALL IE3TOS (TONEW(NDN+1),FRNEW(NDN+1),NWDS,JBAD)
      IF (JBAD.LT.0 .OR. JBAD.GT.NWDS)      GO TO 84
      IF (JBAD.NE.0)  CBADFR(NDN+JBAD)(2:2) = 'f'
      IF (JTE.NE.0)  THEN
          IF (IFRNEW(JTE).NE.77)   GO TO 92
        ENDIF

      NDN = NDN + NWDS
      IF (NDN.LT.NEL)              GO TO 52

C----              Compare and print

      DO 69 I=1,NEL

      IF ((IFRNEW(I).AND.MSKSIG) .NE. (IORG(I).AND.MSKSIG))
     +               CBADFR(I)(4:4) = 'N'

#if defined(CERNLIB_QMCV64)
      NEXP(I) = JBYT (IORG(I),53,11) - 1024
#endif
#if defined(CERNLIB_QMCV32)
      NEXP(I) = JBYT (IORG(I),24,9) - 128
#endif
   69 CONTINUE

      CHPG = ' '
      IE = 0
   71 N  = MIN (NEL-IE,50)
      IA = IE + 1
      IE = IE + N

      WRITE (6,9071) CHPG
      CHPG = '1'

      WRITE (6,9074)
     +     (I,CBADTO(I),ORGPR(I),NEXP(I),
     +        IORG(I),IFRNEW(I),ITONEW(I),
     +        CBADFR(I), CNOTE(I),
     +      I=IA,IE)
      IF (IE.LT.NEL)         GO TO 71

 9071 FORMAT (A,'    bad ',13X,
     F'exp',10X,'original',10X,'frnew',13X,'tonew',7X,'flags')
#if defined(CERNLIB_QMCV64)
 9074 FORMAT (/I4,A,E11.4,I5,3Z18.16,1X,A,1X,A)
#endif
#if defined(CERNLIB_QMCV32)
 9074 FORMAT ((/I4,A,E11.4,I5,3(10X,Z8.8),1X,A,1X,A))
#endif

      CALL KERNGT (6)
      CALL DATIMH (MDATE, MTIME)
      WRITE (6,9079) CHDATE,CHTIME
 9079 FORMAT (10X,'Today : ',A,2X,A)

      IF (ITER.NE.0)      STOP
      ITER = 7
      GO TO 12

   84 JWHICH = 1
   83 JWHICH = JWHICH + 1
   82 JWHICH = JWHICH + 1
   81 JWHICH = JWHICH + 1
      WRITE (6,9081) JWHICH
 9081 FORMAT (' Faulty JBAD from',I2)
      STOP

   91 WRITE (6,9091)  NDN,NWDS
 9091 FORMAT (1X/' IE3FOS overshoots for NDN/NWDS =',2I5)
      STOP

   92 WRITE (6,9092)  NDN,NWDS
 9092 FORMAT (1X/' IE3TOS overshoots for NDN/NWDS =',2I5)
      STOP
      END
