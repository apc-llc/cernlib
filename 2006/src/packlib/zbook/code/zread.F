*
* $Id: zread.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zread.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZREAD(IZ,ID,LUN,KEYI)
C
C     ******************************************************************
C     *                                                                *
C     *       COPIES ID FROM MASS STORAGE TO MEMORY                    *
C     *         A, IF ID DOES NOT EXIST IN MEMORY                      *
C     *            IT WILL BE CREATED                                  *
C     *         B, IF ID EXISTS AND BIG ENOUGH-->STRAIGTH COPY         *
C     *         C, IF ID EXISTS BUT NOT BIG ENOUGH-->                  *
C     *            PUSH BEFORE COPY                                    *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      DIMENSION KD(51),NK(51)
      LOGICAL ZIDOK
C
      DATA MAXLEV/50/
C
C     ------------------------------------------------------------------
C
      KEY    = KEYI
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 95
      NL     = IZ(JD) - 2
      NLUN   = 1
   5  IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 7
      NLUN   = NLUN + 1
      IF (NLUN.LE.NL)                            GO TO 5
      GO TO 95
C
   7  JDIR   = IZ(JD - NLUN)
C
      CALL ZSEARC(IZ,NLUN,KEY,LOC)
      IF (LOC.LE.0)                              GO TO 91
C
      IF     = IZ(LOC + 1)
      ND     = IABS(IZ(LOC + 2))
      LREC   = IZ(JDIR + 3)
      IF (IZ(LOC + 2).LT.0)                      GO TO 30
C
      IF (ZIDOK(IZ,ID))                          GO TO 10
   8  IZ(JZ+17) = 1
      CALL ZBOOKN(IZ,ID,ND-2,0,'    ',0)
      IF (IZ(JZ+6).NE.0)                         GO TO 99
      NID    = IZ(JZ+18)
      IDATA=IZ(NID)
      GO TO 20
C
  10  IDATA=ID(1)
      NOLD=IZ(IDATA)
      IF (NOLD.EQ.ND)                            GO TO 20
      IF (NOLD.GT.ND)                            GO TO 15
C
      CALL ZDROP (IZ,ID)
      GO TO  8
C
  15  CALL ZPUSHS(IZ,IDATA,ND-NOLD,0)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
C
  20  CALL ZIODO(LUN,IF,LREC,IZ(IDATA+1),ND,1)
C
      GO TO 99
C
C             TREE STRUCTURE
C
  30  IF(ZIDOK(IZ,ID)) CALL ZDROP(IZ,ID)
C
      IZ(JZ+17) = 1
      NID    = LOCF(ID(1))+IZ(JZ+16)+1
      IF (ND.LE.IZ(JZ+1))                        GO TO 35
C
      IZ(JZ+18) = NID
      CALL ZGARB(IZ)
      NID    = IZ(JZ+18)
C
  35  CALL ZBOOKN(IZ,IZ(JZ-5),ND-6,0,'JBUF',0)
      JZ=IZ(1)
      JBUF=IZ(JZ-5)
      IF (JBUF.EQ.0)                             GO TO 99
C
      CALL ZIODO(LUN,IF,LREC,IZ(JBUF-1),ND,1)
C
      ND     = IZ(JBUF - 1)
      ILINK  = JBUF + ND - 3
      NLINK  = IZ(ILINK)
      IZ(NID)= JBUF + NLINK
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)
      ID(1)  = IZ(NID)
#endif
      IZ(ILINK + 1) = NID
      IF(NID.LT.IZ(JZ+12))IZ(JZ+12)=NID
      IF(NID.GT.IZ(JZ+13))IZ(JZ+13)=NID
C
C             NOW SCAN TREE TO UPDATE POINTERS AND ADDRESSES
C
      I      = 1
      NK(I)  = NLINK
      IFIRST = ILINK + 2
      KD(I)  = ID(1)
C
 40   IF (NK(I).GT.0)                            GO TO 42
 41   I      = I - 1
      IF(I.LE.0)IZ(JZ-5)=0
      IF (I.LE.0)                                GO TO 99
      GO TO 40
 42   JD     = KD(I) - NK(I)
      IF (IZ(JD).NE.0)                           GO TO 43
      NK(I)  = NK(I) - 1
      IF (NK(I).LE.0)                            GO TO 41
      GO TO 42
 43   NK(I)  = NK(I) - 1
      I      = I + 1
      IF (I.GT.MAXLEV)                           GO TO 94
      ILAST  = IFIRST + IZ(IFIRST) - 1
      NK(I)  = IZ(ILAST - 1)
      KD(I)  = IFIRST + NK(I) + 1
      IFIRST = ILAST + 1
      IZ(JD) = KD(I)
      IZ(ILAST) = JD
      IF (JD.LT.IZ(JZ + 12)) IZ(JZ + 12) = JD
      IF (JD.GT.IZ(JZ + 13)) IZ(JZ + 13) = JD
      GO TO 40
C
C             ID DOES NOT EXIST ON MASS STORAGE
C
  91  CALL ZERROR(IZ,700,'ZREAD ',ID)
      GO TO 99
C
C             LEVEL TOO HIGH
C
  94  CALL ZERROR(IZ,800,'ZREAD ',ID)
      GO TO 99
C
C             UNDEFINED INPUT UNIT
C
  95  CALL ZERROR(IZ,900,'ZREAD ',ID)
C
  99  RETURN
      END
