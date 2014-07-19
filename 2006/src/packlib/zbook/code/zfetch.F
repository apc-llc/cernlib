*
* $Id: zfetch.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zfetch.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZFETCH(IZ,ID,LUNIT,KEY)
C
C     ******************************************************************
C     *                                                                *
C     *       FETCHS BANK ID STORED ON LOGICAL UNIT LUNIT              *
C     *       WITH PASSWORD=KEY AND CREATES A BANK                     *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
#if defined(CERNLIB_BESM6)
      DIMENSION LABNW(2)
      EQUIVALENCE(LABNW(1),LAB),(LABNW(2),NW)
#endif
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      N1     = 0
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
C
#if !defined(CERNLIB_BESM6)
   10 READ(LUNIT)LAB,NW
#endif
#if defined(CERNLIB_BESM6)
  10  CALL SREAD(LUNIT,LABNW(1),LABNW(2),0)
#endif
      IF (NW.NE.0)                               GO TO 15
C
      IF (N1.EQ.1)                               GO TO 99
      N1     = 1
#if !defined(CERNLIB_BESM6)
      REWIND LUNIT
#endif
#if defined(CERNLIB_BESM6)
      CALL REWIND(LUNIT)
#endif
      GO TO 10
C
   15 IF (LAB.EQ.KEY)                            GO TO 20
#if !defined(CERNLIB_BESM6)
      READ(LUNIT) W
#endif
#if defined(CERNLIB_BESM6)
      CALL SREAD(LUNIT,W,W,0)
#endif
      GO TO 10
C
  20  N      = JBYT(NW,1,18)
      NL     = JBYT(NW,19,14) - 1
      ND     = N
      IF (NL.GE.0)ND = N - 2
      IF (NL.LT.0)NL = 0
      IF(.NOT.ZIDOK(IZ,ID))  CALL ZBOOKN(IZ,ID,ND,NL,'    ',0)
      IF (ID(1).LE.0)RETURN
      IDATA=ID(1)
      NOLD=IZ(IDATA)
      N1     = ND - NOLD + 2
      IF (N1.EQ.0)                               GO TO 25
      CALL ZPUSHS(IZ,ID,N1,0)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      IDATA=ID(1)
      IF(IZ(IDATA).EQ.NOLD)RETURN
#if !defined(CERNLIB_BESM6)
  25  READ(LUNIT)(IZ(IDATA+J),J=1,N)
#endif
#if defined(CERNLIB_BESM6)
  25  CALL SREAD(LUNIT,IZ(IDATA+1),N,1)
#endif
C
   99 RETURN
      END
