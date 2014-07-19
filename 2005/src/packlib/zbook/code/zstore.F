*
* $Id: zstore.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zstore.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSTORE(IZ,ID,LUNIT,KEY)
C
C     ******************************************************************
C     *                                                                *
C     *       STORES ON LOGICAL UNIT LUNIT THE BANK ID WITH            *
C     *       A PASSWORD=KEY                                           *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
#if defined(CERNLIB_BESM6)
      DIMENSION LABNW(2)
      EQUIVALENCE(LABNW(1),KEI),(LABNW(2),NW)
#endif
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      IDATA=ID(1)
      IF (ZIDOK(IZ,ID))                          GO TO 10
      CALL ZERROR(IZ,300,'ZSTORE',ID)
      RETURN
C
  10  ND=IZ(IDATA)
      NL=IZ(IDATA+ND+1)+1
      NW     = ND
      CALL SBYT(NL,NW,19,14)
#if !defined(CERNLIB_BESM6)
      WRITE (LUNIT)KEY,NW
      WRITE(LUNIT)(IZ(IDATA+J),J=1,ND)
      J      = 0
      WRITE(LUNIT)J,J
      BACKSPACE LUNIT
#endif
#if defined(CERNLIB_BESM6)
      KEI=KEY
      CALL SWRITE(LUNIT,LABNW(1),LABNW(2),0)
      CALL SWRITE(LUNIT,IZ(IDATA+1),ND,1)
      LABNW(1)=0
      LABNW(2)=0
      CALL SWRITE(LUNIT,LABNW(1),LABNW(2),0)
      CALL SBACK(LUNIT)
#endif
C
      RETURN
      END
