*
* $Id: zprint.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zprint.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
#if !defined(CERNLIB_ANORMAL)
      SUBROUTINE ZPRINT(IZ,IDD,KABEL,KFORMA,NL1,NL2)
#endif
#if defined(CERNLIB_ANORMAL)
      SUBROUTINE AZPRIN(IZ,IDD,KABEL,KFORMA,NL1,NL2)
#endif
C
C     ******************************************************************
C     *                                                                *
C     *       PRINTS CONTENTS OF BANK ID WITH TITLE KEY                *
C     *                                                                *
C     *       KFORMA=1HI   IN INTEGER I10                              *
C     *              1HB   IN OCTAL OR HEXADECIMAL                     *
C     *              1HA   IN ALPHANUMERIC (A4,A6,A10)                 *
C     *              1HF   IN F10.3                                    *
C     *              1HE   IN E15.7                                    *
C     *              1HO   SAME AS 1HB                                 *
C     *              1HZ   SAME AS 1HB                                 *
C     *              NNH(FORMAT)  PRINTS THE DATA IN USER FORMAT       *
C     *                           NN IS THE LENGHT OF THE STRING       *
C     *                                                                *
C     ******************************************************************
C
      PARAMETER (MINL=3)
      CHARACTER *120 SLINE,SBUFF
      DIMENSION IZ(1),IC(10),C(10),IST(5)
      EQUIVALENCE (IC(1),C(1))
C
      DIMENSION KEY(20),JFORMA(9),NAME(4)
      DIMENSION KABEL(1)
C
      DATA JFORMA/1HI,1HB,1HA,1HF,1HE,1HO,1HZ,1H(,1HU/
#if defined(CERNLIB_SINGLE)||defined(CERNLIB_BESM6)||defined(CERNLIB_PDP10)
      DATA IST/10,5,10,10,5/
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_NORD))
      DATA IST/10,10,20,10,5/
#endif
#if defined(CERNLIB_NORD)
      DATA IST/10,5,20,10,5/
#endif
#if defined(CERNLIB_UNIVAC)
      DATA IST/10,8,20,10,5/
#endif
      DATA IDOL/1H$/
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      LOUT   = IZ(JZ + 4)
      NARG   = 6
      CALL NOARG(NARG)
      IDATA=0
      IF(NARG.GT.1)IDATA=IDD
      N1     = 1
      N2     = IZ(JZ + 14)
      NR     = 0
      IF(IDATA.GT.0.AND.IDATA.LT.IZ(JZ+14))GO TO 2
      I1     = 0
      ILAST  = 0
      NL     = 0
      ND     = 0
      CALL UCTOH1('DUMP',NAME,4)
      GO TO 3
C
   2  I1=IDATA
      N2=IZ(IDATA)-2
      ND=N2
      ILAST=IDATA+N2+4
      NID    = IZ(ILAST)
      NR     = IZ(ILAST - 2)
      CALL UBLOW(IZ(ILAST-3),NAME,4)
   3  CALL VBLANK(KEY,20)
      IFORMA = JFORMA(4)
C
      GO TO (9,9,7,6,5,4),NARG
C
   4  IF (NL2.NE.0)N2 = NL2
   5  IF (NL1.NE.0)N1 = NL1
   6  CONTINUE
      IF(KFORMA.NE.0)  CALL UCTOH1(KFORMA,IFORMA,1)
   7  CONTINUE
      IF (KABEL(1).EQ.0)                         GO TO 9
      CALL UCTOH1(KABEL,KEY,20)
      DO 8 I = 1,20
      IF (KEY(I).NE.IDOL)                        GO TO 8
      CALL VBLANK(KEY(I),20-I+1)
      GO TO 9
   8  CONTINUE
C
   9  I1     = I1 + N1
      I2     = I1 + N2 - 1
C
      IF(IDATA.NE.0)NL=IZ(ILAST-1)
      IF(IDATA.NE.0)WRITE(LOUT,6000)NAME,NR,KEY,ND,NL,IDATA,NID
      IF(IDATA.NE.0)ND=N2
      IF(IDATA.EQ.0)WRITE(LOUT,6001)NAME,KEY,I1,I2
C
      IF (NL.LE.0)                               GO TO 11
      WRITE(LOUT,7000)
      K1     = 1
      DO 12 I = 1,NL,10
      K2     = MIN0(NL,K1 + 9)
      WRITE(LOUT,1000) I,(IZ(IDATA-K),K=K1,K2)
      K1     = K1 + 10
 12   CONTINUE
      IF (I2.LT.I1)RETURN
      WRITE(LOUT,7001)
 11   CONTINUE
      IF (I2.LT.I1)RETURN
C
      IF (IFORMA.NE.JFORMA(8))                   GO TO 13
      RETURN
C
 13   CONTINUE
C
C--- Inserted by HG
      IF(IFORMA.EQ.JFORMA(9).AND.IDATA.GT.0)  THEN
C--- USER FORMAT
      CALL ZPBANK(IZ,IDD,N1,N2+N1-1)
      RETURN
      ENDIF
C---
      DO 10 I = 1,7
      IF (IFORMA.NE.JFORMA(I))                   GO TO 10
      JF     = I
      IF (JF.GT.5)JF = 2
      GO TO 15
  10  CONTINUE
      JF     = 4
  15  ISTEP  = IST(JF)
      I      = N1 - ISTEP
C
      JCNT=0
      DO 111 J = I1,I2,ISTEP
      I      = I + ISTEP
      K      = J + ISTEP - 1
      IF (K.GT.I2)K = I2
      M      = 0
      DO 40 L = J,K
      M      = M + 1
      GO TO (20,50,50,30,30),JF
  20  IC(M)  = INTARG(IZ(L))
      GO TO 40
  30  C(M)   = FLOARG(IZ(L))
  40  CONTINUE
C
  50  CONTINUE
      SLINE=' '
      GO TO (60,70,80,90,100),JF
C
C             IN INTEGER
C
  60  WRITE(SLINE,1000)(IC(L),L = 1,M)
      GO TO 110
C
C             IN BINARY (OCTAL OR HEXADECIMAL)
C
  70  WRITE(SLINE,2000)(IZ(L),L = J,K)
      GO TO 110
C
C             IN ALPHANUMERIC
C
  80  WRITE(SLINE,3000)(IZ(L),L = J,K)
      GO TO 110
C
C             IN F10.3
C
  90  WRITE(SLINE,4000)(C(L),L = 1,M)
      GO TO 110
C
C             IN E15.7
C
 100  WRITE(SLINE,5000)(C(L),L = 1,M)
C
 110  CONTINUE
*--- suppress multiple lines if at least MINL of them
         IF(JCNT.EQ.0) THEN
            JCNT=1
            SBUFF=SLINE
            LSTART=I
            LEFT=I
         ELSEIF(SLINE.EQ.SBUFF) THEN
            JCNT=JCNT+1
            LEFT=I
         ELSE
            IF(JCNT.GE.MINL) THEN
               WRITE(LOUT,10110) LSTART
               WRITE(LOUT,10100) LEFT,SBUFF
            ELSE
               LF=LSTART-ISTEP
               DO 240 JJ=1,JCNT
                  LF=LF+ISTEP
                  WRITE(LOUT,10100) LF,SBUFF
  240          CONTINUE
            ENDIF
            SBUFF=SLINE
            JCNT=1
            LSTART=I
            LEFT=I
         ENDIF
C
  111 CONTINUE
      IF(JCNT.GE.MINL)  THEN
         WRITE(LOUT,10110) LSTART
         WRITE(LOUT,10100) LEFT,SBUFF
      ELSEIF(JCNT.GT.0)  THEN
         LF=LSTART-ISTEP
         DO 260 JJ=1,JCNT
            LF=LF+ISTEP
            WRITE(LOUT,10100) LF,SBUFF
  260    CONTINUE
      ENDIF
      RETURN
C
#if defined(CERNLIB_CDC)
 2000 FORMAT(5(2X,O20))
 3000 FORMAT(10(2X,A10))
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)
 2000 FORMAT(5(2X,O22))
 3000 FORMAT(10(2X,A8))
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CONVEX))
 2000 FORMAT(10(2X,Z8))
 3000 FORMAT(20(2X,A4))
#endif
#if defined(CERNLIB_UNIVAC)
 2000 FORMAT(8O14)
 3000 FORMAT(20(2X,A4))
#endif
#if defined(CERNLIB_NORD)
 2000 FORMAT(5(2X,O11))
 3000 FORMAT(20(2X,A4))
#endif
#if defined(CERNLIB_BESM6)
 2000 FORMAT(5(2X,O16))
 3000 FORMAT(10(2X,A6))
#endif
#if defined(CERNLIB_PDP10)
 2000 FORMAT(5(2X,I10))
 3000 FORMAT(10(2X,A5))
#endif
 1000 FORMAT(10(2X,I10))
 4000 FORMAT(10(2X,F10.3))
 5000 FORMAT(5(5X,E15.7))
 6000 FORMAT(/,10X,' +++ BANK ',4A1,' NO =',I6,'   ',20A1
     +,'    ND =',I6,'    NL =',I6,'    ID =',I6,'   NID =',I6,' +++')
 6001 FORMAT(/,10X,' +++ BANK ',4A1,3X,20A1
     +,'   BETWEEN LOCATIONS ',I6,' AND ',I6,' +++')
 7000 FORMAT('  LINKS')
 7001 FORMAT('  DATA')
10100 FORMAT(1X,I5,A120)
10110 FORMAT(1X,I5,'  ',5('===='), ' and following lines identical to:')
 
      END
