*
* $Id: cdlinc.F,v 1.3 1998/09/25 09:29:12 mclareni Exp $
*
* $Log: cdlinc.F,v $
* Revision 1.3  1998/09/25 09:29:12  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.2  1996/07/01 06:52:09  jamie
* Linux support in CDLINC
*
* Revision 1.1.1.1  1996/02/28 16:24:21  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDLINC (CLINE, LENGTH, IDATA, NDATA)
*     ===============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLINC (CLINE, LENGTH, IDATA*, *NDATA*)                 *
*                                                                      *
*   Encodes a character string into a buffer IDATA                     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CLINE    Character string of maximum 80 characters               *
*     LENGTH   Length of the string                                    *
*     IDATA    Array to contain the encoded information                *
*     NDATA    Length of the array IDATA used so far                   *
*                                                                      *
*   Called by CDATOI                                                   *
*                                                                      *
*   Original Code : CPLINE in CMZ Package                              *
*                                                                      *
************************************************************************
*
      CHARACTER       CLINE*(*)
      CHARACTER*4     FWORD(5), KBLANK
      CHARACTER*1     KONE, CAR, KTWO*(3)
      CHARACTER*80    KLINE
      DIMENSION       LINE(20), ILINE(20)
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_QMCVY)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))
      PARAMETER       (IND1=2, IND2=1)
*.
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_QMCVY))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))
      PARAMETER       (IND1=1, IND2=2)
#endif
      CHARACTER       KENDIF*5, KELSE*4, KEND*3, KEYW*8, KCONT*8, KRET*6
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_F90))
      INTEGER*2       IWD1(2), IWD2(2)
#endif
#if (!defined(CERNLIB_CRAY))&&(defined(CERNLIB_F90))
      INTEGER(2)      IWD1(2), IWD2(2)
#endif
#if !defined(CERNLIB_CRAY)
      EQUIVALENCE     (IWORD1, IWD1(1))
      EQUIVALENCE     (IWORD2, IWD2(1))
#endif
#if defined(CERNLIB_CRAY)
      INTEGER         IWD1(2), IWD2(2)
#endif
      DIMENSION       IDATA(9), ICODE(6)
      SAVE            KBLAN, IFIRST
*
      DATA            FWORD /'+DEC','+KEE','+SEQ','+CDE','+SEL'/
      DATA            ICODE /   125,   124,   123,   122,   121,   120/
      DATA            KENDIF, KEND, KRET /'ENDIF', 'END', 'RETURN'/
      DATA            KELSE, KCONT /'ELSE', 'CONTINUE'/
      DATA            KBLANK /'    '/
      DATA            IFIRST /0/
#include "zebra/q_sbit.inc"
* Ignoring t=pass
#include "zebra/q_sbyt.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      IF (IFIRST.EQ.0) THEN
        IFIRST = 1
        CALL UCTOH (KBLANK, KBLAN, 4, 4)
      ENDIF
*
      KLINE = CLINE
      CALL UCTOH (CLINE, LINE, 4, 80)
      IF (LENGTH.EQ.0) THEN
        NDATA = NDATA +1
        IDATA(NDATA)=0
        GO TO 999
      ENDIF
#if !defined(CERNLIB_CRAY)
      IWORD2 = 0
#endif
#if defined(CERNLIB_CRAY)
      IWD1(1) = 0
      IWD1(2) = 0
      IWD2(1) = 0
      IWD2(2) = 0
#endif
      KONE   = KLINE(:1)
      KTWO   = KLINE(2:4)
      CALL CDIFCH (KONE, IKONE)
#if !defined(CERNLIB_CRAY)
      IWORD1 = IKONE
#endif
#if defined(CERNLIB_CRAY)
      IWD1(2) = IKONE
#endif
      ILASTW = (LENGTH+3)/4
*
* *** Treatment of special cases : +DECK, +KEEP, +SEQ, +CDE,+SELF,+SELF.
*
      IF (KONE.EQ.'+') THEN
        DO 10 NCAS = 1, 4
          IF (KLINE(1:4).EQ.FWORD(NCAS)) THEN
            IWD1(IND1) = ICODE(NCAS)*128
            GO TO 20
          ENDIF
   10   CONTINUE
        IF (KLINE(1:4).NE.FWORD(5))         GO TO 40
        IWD1(IND1) = ICODE(5)*128
        IF (KLINE(6:6).EQ.'.'.AND.LENGTH.EQ.6) THEN
          IWD1(IND1) = ICODE(6)*128
#if !defined(CERNLIB_CRAY)
          IDATA(NDATA+1) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
          IDATA(NDATA+1) = IWD1(2)
          IDATA(NDATA+1) = MSBYT (IWD1(1), IDATA(NDATA+1), 17, 16)
#endif
          NDATA = NDATA + 1
          GO TO 999
        ENDIF
   20   IWD1(IND1) = IWD1(IND1) + ILASTW
        CALL CDIFRC (CLINE, ILINE, ILASTW, 4*ILASTW)
        DO 30 IW = 2, ILASTW
          IDATA(NDATA+IW) = ILINE(IW)
   30   CONTINUE
#if !defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWD1(2)
        IDATA(NDATA+1) = MSBYT (IWD1(1), IDATA(NDATA+1), 17, 16)
#endif
        NDATA = NDATA + ILASTW
        GO TO 999
      ENDIF
*
   40 NWI    = 1
      IFIRW  = 1
      IF (ILASTW.EQ.1)                      GO TO 80
      DO 50 I = 2, 20
        IF (LINE(I).EQ.KBLAN)               GO TO 50
        IFIRW = I
        GO TO 60
   50 CONTINUE
   60 CONTINUE
*
* *** For ENDIF , ELSE , END (without label) and RETURN (without label),
* *** use code in bits 24 to 30 = 127, 126, 118, 116
*
      IF (KLINE(:6).EQ.' ') THEN
        NC1 = (IFIRW-1)*4+1
   70   IF (KLINE(NC1:NC1).EQ.'.') THEN
          NC1 = NC1 + 1
          GO TO 70
        ENDIF
        IF (LENGTH-NC1.GT.5)                GO TO 170
        KEYW = KLINE(NC1:LENGTH)
        CALL CLTOU (KEYW(:LENGTH-NC1+1))
        IF (KEYW.EQ.KENDIF) THEN
          IWD1(IND1) = 127*128 + NC1
        ELSE IF (KEYW.EQ.KELSE) THEN
          IWD1(IND1) = 126*128 + NC1
        ELSE IF (KEYW.EQ.KEND) THEN
          IWD1(IND1) = 118*128 + NC1
        ELSE IF (KEYW.EQ.KRET) THEN
          IWD1(IND1) = 116*128 + NC1
        ELSE
          GO TO 170
        ENDIF
        NDATA = NDATA + 1
#if !defined(CERNLIB_CRAY)
        IDATA(NDATA) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
        IDATA(NDATA) = IWD1(2)
        IDATA(NDATA) = MSBYT (IWD1(1), IDATA(NDATA), 17, 16)
#endif
        GO TO 999
      ENDIF
*
   80 IF (KTWO.NE.' ') THEN
*
* ***   Bit 31 = 1
*
        IWD1(IND1) = 16384
        NWI    = 2
        CALL CDIFRC (KTWO, IDATA(NDATA+2), 1, 3)
      ENDIF
*
      IF (KONE.EQ.'C') THEN
        ICOM = 4
      ELSE IF (KONE.EQ.'c') THEN
        ICOM = 8
      ELSE IF (KONE.EQ.'*') THEN
        ICOM = 12
      ELSE
        ICOM = 0
      ENDIF
      IF (ICOM.NE.0) THEN
*
* ***   Bit 32 = 1  and bits 1-4 = 4 or 8 or 12 depending
* ***                  upon KONE = C or c or *
*
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_LENDIAN))&&(!defined(CERNLIB_SUN))&&(!defined(CERNLIB_VAXVMS))
        IWD1(IND1) = IWD1(IND1) + 32768
#endif
#if defined(CERNLIB_DECS)||defined(CERNLIB_IBMVM)||defined(CERNLIB_LENDIAN)||defined(CERNLIB_SUN)||defined(CERNLIB_VAXVMS)
        IWORD1  = MSBIT1 (IWORD1, 32)
#endif
#if defined(CERNLIB_CRAY)
        IWD1(1) = MSBIT1 (IWD1(1), 16)
#endif
*
        IWD1(IND2) = ICOM
*
        IF (KTWO.EQ.'.')THEN
*
* ***     Bit 2 =1 and Bit 31 = 0
*
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_LENDIAN))&&(!defined(CERNLIB_SUN))&&(!defined(CERNLIB_VAXVMS))
          IWD1(IND1) = 32768
#endif
#if defined(CERNLIB_DECS)||defined(CERNLIB_IBMVM)||defined(CERNLIB_LENDIAN)||defined(CERNLIB_SUN)||defined(CERNLIB_VAXVMS)
          IWORD1  = MSBIT0 (IWORD1, 31)
#endif
#if defined(CERNLIB_CRAY)
          IWD1(1) = MSBIT0 (IWD1(1), 15)
#endif
          NWI = 1
          IWD1(IND2) = IWD1(IND2) + 2
        ENDIF
        IF (ILASTW.LE.2)                    GO TO 170
*
*  **   Is there a box ?
*  **   Look for last isolated character
*
        LEN2 = LENOCC (KLINE(:(LENGTH-1)))
        IF ((LENGTH-LEN2).GT.7) THEN
*
*  **     A box is there ====> Bit 1 =1
*
          IWD1(IND2) = IWD1(IND2) + 1
          ILASTW = (LEN2+3)/4
          CALL CDIFCH (KLINE(LENGTH:LENGTH), IKLIN)
          IWD2(IND2) = LENGTH*256 + IKLIN
*
*  **     Look for the first isolated character
*
          NC1   = (IFIRW-1)*4+1
          NC2   = NC1+3
          IPOS  = NC1
          NKAR  = 0
          DO 90 IC = NC1, NC2
            IF (KLINE(IC:IC).EQ.' ')        GO TO 90
            IPOS = IC
            NKAR = NKAR+1
   90     CONTINUE
          IF (NKAR.EQ.1) THEN
            DO 100 IST = IFIRW+1, ILASTW
              IF (LINE(IST).EQ.KBLAN)       GO TO 100
              IFIRW = IST
              CALL CDIFCH (KLINE(IPOS:IPOS), IKLIN)
              IWD2(IND1) = IPOS*256 + IKLIN
              GO TO 110
  100       CONTINUE
          ENDIF
  110     NWI=NWI+1
#if !defined(CERNLIB_CRAY)
          IDATA(NDATA+NWI) = IWORD2
#endif
#if defined(CERNLIB_CRAY)
          IDATA(NDATA+NWI) = IWD2(2)
          IDATA(NDATA+NWI) = MSBYT (IWD2(1), IDATA(NDATA+NWI), 17, 16)
#endif
        ENDIF
*
*  **   Is any character repeated ?
*
        IF (ILASTW.EQ.1)                    GO TO 170
        DO 120 I = IFIRW+2, ILASTW-1
          IF (LINE(I).NE.LINE(I-1))         GO TO 170
  120   CONTINUE
*
*  **   Are the characters identical ?
*  **   NFCOL2 = first column number of the second non-blank character
*  **   NLCOL2 = last    "      "             "       "         "
*
        NFCOL2 = IFIRW*4+1
        NLCOL2 = NFCOL2+3
        DO 130 I = NFCOL2+1, NLCOL2
          IF (KLINE(I:I).NE.KLINE(I-1:I-1)) GO TO 170
  130   CONTINUE
        CAR    = KLINE(NFCOL2:NFCOL2)
*
*  **   Check whether the non-blank characters in the first and
*  **   the last word are also identical to CAR
*
        NFCOL  = (IFIRW-1)*4 + 1
        NLCOL  = NFCOL + 3
        NFIRST = NFCOL
        NLAST  = NFCOL
        DO 150 J = 1, 2
          DO 140 I = NFCOL, NLCOL
            IF (KLINE(I:I).EQ.' ') THEN
              IF (J.EQ.1) NFIRST = I + 1
              GO TO 140
            ENDIF
            IF (KLINE(I:I).NE.CAR)          GO TO 170
            NLAST = I
  140     CONTINUE
          NFCOL = (ILASTW-1)*4 + 1
          NLCOL = NFCOL + 3
  150   CONTINUE
*
*  **   All characters are identical
*  **   NFIRST = number of the first non-blank character
*  **   NLAST  =   "           last     "         "
*
        IWD1(IND1) = IWD1(IND1) + NLAST*128 + NFIRST
        CALL CDIFCH (CAR, IKLIN)
        IWD1(IND2) = IWD1(IND2) + 256*IKLIN
#if !defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWD1(2)
        IDATA(NDATA+1) = MSBYT (IWD1(1), IDATA(NDATA+1), 17, 16)
#endif
        NDATA = NDATA + NWI
        GO TO 999
      ELSE IF (KONE.NE.'%' .AND. KONE.NE.'+' .AND. KLINE(6:6).EQ.' '
     +   .AND. LENGTH.GT.7) THEN
*
*  **   For cases : CONTINUE , END and RETURN preceded by a label,
*  **   use code in bits 24 to 30 = 117 , 119 or 115
*
        NC1    = 7
  160   IF (KLINE(NC1:NC1).EQ.' ') THEN
          NC1  = NC1 + 1
          GO TO 160
        ENDIF
        IF (LENGTH-NC1.GT.7)                GO TO 170
        KEYW   = KLINE(NC1:LENGTH)
        CALL CLTOU (KEYW(:LENGTH-NC1+1))
        IF (KEYW.EQ.KEND) THEN
          IWD1(IND1) = IWD1(IND1) + 119*128 + NC1
        ELSE IF(KEYW.EQ.KCONT) THEN
          IWD1(IND1) = IWD1(IND1) + 117*128 + NC1
        ELSE IF(KEYW.EQ.KRET) THEN
          IWD1(IND1) = IWD1(IND1) + 115*128 + NC1
        ELSE
          GO TO 170
        ENDIF
        CALL CDIFCH (KLINE(5:5), IKLIN)
        IWD1(IND2) = IWD1(IND2) + 256*IKLIN
#if !defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
        IDATA(NDATA+1) = IWD1(2)
        IDATA(NDATA+1) = MSBYT (IWD1(1), IDATA(NDATA+1), 17, 16)
#endif
        NDATA = NDATA+NWI
        GO TO 999
      ENDIF
  170 CONTINUE
      IWD1(IND1) = IWD1(IND1) + ILASTW*128 + IFIRW
#if !defined(CERNLIB_CRAY)
      IDATA(NDATA+1) = IWORD1
#endif
#if defined(CERNLIB_CRAY)
      IDATA(NDATA+1) = IWD1(2)
      IDATA(NDATA+1) = MSBYT (IWD1(1), IDATA(NDATA+1), 17, 16)
#endif
      IF (ILASTW.EQ.1) THEN
        NDATA = NDATA + NWI
        GO TO 999
      ENDIF
      CALL CDIFRC (CLINE, ILINE, ILASTW, 4*ILASTW)
      DO 180 IW = IFIRW, ILASTW
        JW    = NDATA + NWI + IW - IFIRW + 1
        IDATA(JW) = ILINE(IW)
  180 CONTINUE
      NDATA = NDATA + NWI + ILASTW - IFIRW + 1
*                                                             END CDLINC
  999 END
#endif
