*
* $Id: zeploc.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zeploc.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPLOC (IZ,KEY,LOCID,NMEMBS,ICODE)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       RETRIEVES THE LOCATION OF THE IDENTIFIER OF A            *
C     *       BANK LABELLED 'KEY'                                      *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      LOCID  = 0
      JZ     = IZ(1)
      JZ4    = IZ(JZ - 4)
      IF (JZ4.LE.0)                              GO TO 999
C
      JKEY   = IZ(JZ4 - 1)
      LKEY   = 0
      LUP    = IZ(JKEY) - 1
      LDOWN  = 0
C
   10 IF (LUP-LDOWN.LE.1)                        GO TO 999
C
      LMEAN  = (LUP+LDOWN)/ 2
      LKEY   = LMEAN
      IF (KEY-IZ(JKEY+LKEY)) 20,40,30
C
   20 LUP    = LMEAN
      GO TO 10
C
   30 LDOWN  = LMEAN
      LKEY   = LKEY + 1
      GO TO 10
C
   40 JNID   = IZ(JZ4 - 2)
      JNEL   = IZ(JZ4 - 3)
      JCOD   = IZ(JZ4 - 4)
      LOCID  = IZ(JNID + LKEY)
      NMEMBS = IZ(JNEL + LKEY)
      ICODE  = IZ(JCOD + LKEY)
C
  999 RETURN
      END
