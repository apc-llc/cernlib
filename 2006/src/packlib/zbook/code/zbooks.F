*
* $Id: zbooks.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zbooks.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZBOOKS(IZ,ID,ND,NL)
C
C     ******************************************************************
C     *                                                                *
C     *       CREATES A BANK WITH ND DATA WORDS AND NL LINKS           *
C     *                                                                *
C     *       IZ(ID-NL-1) = ND+NL+6                                    *
C     *       IZ(ID-NL)   = POINTER FOR NL TH LINK                     *
C     *       IZ(ID- 1)   = POINTER FOR FIRST LINK                     *
C     *       IZ(ID)      = ND+2                                       *
C     *       IZ(ID+ 1)   = FIRST USED WORD                            *
C     *       IZ(ID+ND)   = LAST USER WORD                             *
C     *       IZ(ID+ND+1) = NAME OF THE BANK                           *
C     *       IZ(ID+ND+2) = USER BANK NUMBER                           *
C     *       IZ(ID+ND+3) = NL                                         *
C     *       IZ(ID+ND+4) = RELATIVE ADDRESS OF ID                     *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      IF (.NOT.ZIDOK(IZ,ID))                     GO TO 10
C
C             BANK ALREADY EXISTS    DELETE IT
C
      CALL ZERROR(IZ,200,'ZBOOKS',ID)
      CALL ZDELET(IZ,ID)
C
  10  NID    = LOCF(ID(1)) + IZ(JZ + 16) + 1
      IZ(JZ + 18) = NID
      ID(1)  = 0
      NN     = ND + NL + 6
      IF (NN.LE.IZ(JZ + 1))                      GO TO 20
      CALL ZGARB(IZ)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      NID    = IZ(JZ + 18)
      IF (NN.LE.IZ(JZ + 1))                      GO TO 20
      NMOVE  = NN - IZ(JZ + 1) + 1
      CALL ZMOVE(IZ,NMOVE)
      IF (NN.LE.IZ(JZ + 1))                      GO TO 20
      CALL ZERROR(IZ,100,'ZBOOKS',ID)
      GO TO 99
C
  20  IFIRST = IZ(JZ + 15)
      IZ(IFIRST) = NN
      NZERO  = NL
      IF (IZ(JZ + 17).EQ.0)NZERO = NZERO + ND + 1
#if !defined(CERNLIB_BESM6)
      IF(NZERO.GT.0) CALL VZERO(IZ(IFIRST+1),NZERO)
#endif
#if defined(CERNLIB_BESM6)
      IF(NZERO.GT.0) CALL UZERO(IZ(IFIRST+1),1,NZERO)
#endif
      IDATA=IFIRST+NL+1
      IZ(IDATA)=ND+2
      IZLAST=IDATA+ND+5
      CALL UCTOH('    ',IZ(IZLAST - 4),4,4)
      IZ(IZLAST - 3) = 0
      IZ(IZLAST - 2) = NL
      IZ(IZLAST - 1) = NID
      IZ(NID)=IDATA
      IZ(IZLAST) = 0
      IZ(JZ + 15) = IZLAST
      IZ(JZ + 1) = IZ(JZ + 14) - IZLAST
      IZ(JZ + 8) = IZ(JZ + 8) + 1
      IF (NID.LT.IZ(JZ + 12))IZ(JZ + 12) = NID
      IF (NID.GT.IZ(JZ + 13))IZ(JZ + 13) = NID
C
  99  IZ(JZ + 17) = 0
      RETURN
      END
