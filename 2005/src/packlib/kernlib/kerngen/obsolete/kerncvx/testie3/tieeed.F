*
* $Id: tieeed.F,v 1.1.1.1 1996/02/15 17:52:30 mclareni Exp $
*
* $Log: tieeed.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:30  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM TEST

C--       converted values = IEEE
      INTEGER*8     IOVPCD, IOVNCD, IBADCD, IONECD, IZERCD
      PARAMETER    (IOVPCD = '7FF0000000000000'X)
      PARAMETER    (IOVNCD = 'FFF0000000000000'X)
      PARAMETER    (IBADCD = '7FF01FC000000000'X)
      PARAMETER    (IONECD = '3FF0000000000000'X)
      PARAMETER    (IZERCD = '8000000000000000'X)

C--       machine values = native
      INTEGER*8     IOVPMD, IOVNMD, IBADMD
      PARAMETER    (IOVPMD = '7FF7FFFFFFFFFFFF'X)
      PARAMETER    (IOVNMD = 'FFF7FFFFFFFFFFFF'X)
      PARAMETER    (IBADMD = '80007FFFFFFFFFFF'X)

      INTEGER*8     MSKSIG, MSK32R
      PARAMETER    (MSKSIG = 'FFFFFFFFFFFFFFF0'X)
      PARAMETER    (MSK32R = '00000000FFFFFFFF'X)

      PARAMETER    (NEL = 328)
      PARAMETER    (NED = 2*NEL)

      REAL*8        ORG(NEL), ORGPR(NEL)
      INTEGER*8    IORG(NEL)
      EQUIVALENCE (IORG,ORG)

      REAL*8        TONEW(NEL), FRNEW(NEL)
      INTEGER*8    ITONEW(NEL),IFRNEW(NEL)
      EQUIVALENCE (ITONEW,TONEW)
      EQUIVALENCE (IFRNEW,FRNEW)

      CHARACTER    CBADTO(NEL)*5,  CBADFR(NEL)*5,  CNOTE(NEL)*20

      CHARACTER    CHPG*1
      CHARACTER    CHDATE*8, CHTIME*8
      DIMENSION    MDATE(2), MTIME(2)
      EQUIVALENCE (CHDATE,MDATE), (CHTIME,MTIME)

      INTEGER*4    NEXP(NEL)
      REAL*8       VAL, VALU, FACT
      DIMENSION    MTRANA(NED), MTRANB(NED)

      INTEGER*8    MSPEC(5), MIEEE(5)

C--                Test top of range
      DATA  MSPEC(1)   / '7FBFFFFFFFFFFFFF'X /
      DATA  MSPEC(2)   / '7FCFFFFFFFFFFFFF'X /
      DATA  MSPEC(3)   / '7FDFFFFFFFFFFFFF'X /
      DATA  MSPEC(4)   / '7FEFFFFFFFFFFFFF'X /
C--                Special cases
      DATA  MSPEC(5)   / 'C000000000000000'X /

C--                Test top of range
      DATA  MIEEE(1)   / '7FE7777777777777'X /
      DATA  MIEEE(2)   / '7FE0000077777777'X /
C--                Test bottom of range
      DATA  MIEEE(3)   / '0010000100000000'X /
      DATA  MIEEE(4)   / '0001000000000000'X /
      DATA  MIEEE(5)   / '8000000000000000'X /


#if defined(CERNLIB_QMCV64)
      OPEN (6,FILE='tie3d_64.lis',STATUS='UNKNOWN')
 9001 FORMAT ('1Program TIEEED 64-bit executing'/1X)
#endif
#if defined(CERNLIB_QMCV32)
      OPEN (6,FILE='tie3d_32.lis',STATUS='UNKNOWN')
 9001 FORMAT ('1Program TIEEED 32-bit executing'/1X)
#endif

      ITER = 0
   12 WRITE (6,9001)

      DO  13  J=1,NEL
      CBADTO(J) = ' '
      CBADFR(J) = ' '
   13 CNOTE(J)  = ' '
      JWHICH = 0

      FACT = 2.**60
      VAL  = 17.123456789012D0
      VAL  = VAL * FACT

      DO 14 I=1,NEL
      FACT = 1.0D0 + FLOAT(I) * 0.00001D0
      VALU = VAL * FACT
      IF (ITER.EQ.0)  THEN
          ORG(I)=  VALU
        ELSE
          ORG(I)= -VALU
        ENDIF
      VAL = VAL/2.
   14 CONTINUE

C--                Special values

      CNOTE(32) = '-> high CVX range'
      IORG(32) = MSPEC(1)
      IORG(33) = MSPEC(2)
      IORG(34) = MSPEC(3)
      IORG(35) = MSPEC(4)
      CBADTO(32)(4:4) = '!'
      CBADTO(33)(4:4) = '!'
      CBADTO(34)(4:4) = '!'
      CBADTO(35)(4:4) = '!'

      CNOTE(71) = '-> exact 1.0'
       ORG(71) = 1.0D0
       ORG(72) = 1.0D0
      CBADTO(71)(4:4) = '!'
      CBADTO(72)(4:4) = '!'

      CNOTE(97) = '-> NaN'
      IORG(97) = IBADMD
      CBADTO(97)(4:4) = '!'

      CNOTE(144) = '-> +ve overflow'
      IORG(144) = IOVPMD
      CBADTO(144)(4:4) = '!'

      CNOTE(184) = '-> -ve overflow'
      IORG(184) = IOVNMD
      CBADTO(184)(4:4) = '!'

      CNOTE(161) = '-> test zero'
      IORG(161) = 0
      IORG(162) = 0
      CBADTO(161)(4:4) = '!'

#if defined(CERNLIB_QMCV64)
      CALL UCOPY (ORG,ORGPR,NEL)
#endif
#if defined(CERNLIB_QMCV32)
      CALL UCOPY (ORG,ORGPR,NED)
#endif

C----              'Writing'

      NDN  = 0
      NUSE = 0
   42 NUSE = NUSE + 1
      IF (NUSE.EQ.11)  NUSE = 41
      NWDS = MIN (NEL-NDN,NUSE)
      CBADTO(NDN+1)(1:1) = '.'

      CALL VZERO (MTRANA,NED)
#if defined(CERNLIB_QMCV64)
      CALL UCOCOP (IORG(NDN+1),MTRANA,NWDS,1,1,2)
#endif
#if defined(CERNLIB_QMCV32)
      CALL UCOPY  (IORG(NDN+1),MTRANA,2*NWDS)
#endif
      MTRANB(2*NWDS+1) = 77
      CALL IE3FOD (MTRANA,MTRANB,NWDS,JBAD)
      IF (JBAD.LT.0 .OR. JBAD.GT.NWDS)      GO TO 82
      IF (JBAD.NE.0)  CBADTO(NDN+JBAD)(3:3) = 't'
      IF (MTRANB(2*NWDS+1).NE.77)   GO TO 91

#if defined(CERNLIB_QMCV64)
      JTAK = 1
      JPUT = NDN + 1
      DO 44  J=1,NWDS
      ITONEW(JPUT) = ISHFT (MTRANB(JTAK),32) .OR. MTRANB(JTAK+1)
      JTAK = JTAK + 2
   44 JPUT = JPUT + 1
#endif
#if defined(CERNLIB_QMCV32)
      CALL UCOPY (MTRANB,ITONEW(NDN+1),2*NWDS)
#endif

      NDN = NDN + NWDS
      IF (NDN.LT.NEL)              GO TO 42

C--                Special values

      CNOTE(51) = '<- +ve overflow'
      CNOTE(52) = '<- -ve overflow'
      ITONEW(51)  = IOVPCD
      ITONEW(52)  = IOVNCD
      CBADTO(51)(4:4) = ':'
      CBADTO(52)(4:4) = ':'

      CNOTE(61) = '<- NaN'
      ITONEW(61)  = IBADCD
      CBADTO(61)(4:4) = ':'

      CNOTE(72) = '<- exact 1.0'
      ITONEW(72) = IONECD
      CBADTO(72)(4:4) = ':'

      CNOTE(81) = '<- high IEEE range'
      ITONEW(81)  = MIEEE(1)
      ITONEW(82)  = MIEEE(2)
      CBADTO(81)(4:4) = ':'
      CBADTO(82)(4:4) = ':'

      CNOTE(85) = '<- low IEEE range'
      ITONEW(85)  = MIEEE(3)
      ITONEW(86)  = MIEEE(4)
      CBADTO(85)(4:4) = ':'
      CBADTO(86)(4:4) = ':'

      CNOTE(162) = '<- test -0.0'
      ITONEW(162) = IZERCD
      CBADTO(162)(4:4) = ':'

C----              'reading'

      NDN  = 0
      NUSE = 0
   52 NUSE = NUSE + 1
      IF (NUSE.EQ.11)  NUSE = 41
      NWDS = MIN (NEL-NDN,NUSE)

      CALL VZERO (MTRANA,NED)
#if defined(CERNLIB_QMCV64)
      JTAK = NDN + 1
      JPUT = 0
      DO 54  J=1,NWDS
      MTRANA(JPUT+1) = ISHFT (ITONEW(JTAK),-32)
      MTRANA(JPUT+2) =        ITONEW(JTAK) .AND. MSK32R
      JTAK = JTAK + 1
   54 JPUT = JPUT + 2
#endif
#if defined(CERNLIB_QMCV32)
      CALL UCOPY  (ITONEW(NDN+1),MTRANA,2*NWDS)
#endif
      MTRANB(2*NWDS+1) = 77
      CALL IE3TOD (MTRANA,MTRANB,NWDS,JBAD)
      IF (JBAD.LT.0 .OR. JBAD.GT.NWDS)      GO TO 84
      IF (JBAD.NE.0)  CBADFR(NDN+JBAD)(2:2) = 'f'
      IF (MTRANB(2*NWDS+1).NE.77)   GO TO 92

#if defined(CERNLIB_QMCV64)
      CALL UCOCOP (MTRANB,IFRNEW(NDN+1),NWDS,1,2,1)
#endif
#if defined(CERNLIB_QMCV32)
      CALL UCOPY  (MTRANB,IFRNEW(NDN+1),2*NWDS)
#endif

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
      NEXP(I) = JBYT (IORG(I),21,11) - 1024
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
     +     IORG(I),IFRNEW(I),ITONEW(I),
     +                 CBADFR(I), CNOTE(I),
     +      I=IA,IE)
      IF (IE.LT.NEL)         GO TO 71

 9071 FORMAT (A,'    bad ',13X,
     F'exp',10X,'original',10X,'frnew',13X,'tonew',7X,'flags')
 9074 FORMAT (/I4,A,D11.4,I5,3Z18.16,1X,A,1X,A)

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
 9091 FORMAT (1X/' IE3FOD overshoots for NDN/NWDS =',2I5)
      STOP

   92 WRITE (6,9092)  NDN,NWDS
 9092 FORMAT (1X/' IE3TOD overshoots for NDN/NWDS =',2I5)
      STOP
      END
