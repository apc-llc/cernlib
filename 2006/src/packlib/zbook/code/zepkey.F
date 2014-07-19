*
* $Id: zepkey.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepkey.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPKEY(IZ,ID,KEYI,NELEM,MODE,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *       ROUTINE TO ASSIGN A KEY TO A POINTER AND DEFINE          *
C     *       ITS CHARACTERISTICS                                      *
C     *                                                                *
C     *         ID      POINTER TO A ZBOOK BANK                        *
C     *         KEY     ASSOCIATED KEYWORD                             *
C     *         NELEM   NUMBER OF ELEMENTS OF ID                       *
C     *                 (E.G.  DIMENSION ID(NELEM)                     *
C     *         MODE    DESCRIPTION OF BANK ID FOR AUTOMATIC           *
C     *                 CONVERSION TO MACHINE INDEPENDENT FORMAT       *
C     *             MODE=0  BIT STRING                                 *
C     *                 =1  16 BITS SIGNED INTEGER                     *
C     *                 =2  32 BITS SIGNED INTEGER                     *
C     *                 =3  FLOATING POINTS                            *
C     *                 =4  ALPHANUMERIC                               *
C     *                 =5  16 BITS UNSIGNED INTEGER                   *
C     *                 =6  32 BITS UNSIGNED INTEGER                   *
C     *                 =7  MIXTURE OF FLOATING POINTS AND INTEGERS    *
C     *                 =11 SAME AS 1 (BANK NOT DESTROYED)             *
C     *                 =12         2       ""      ""                 *
C     *                 =13         3       ""      ""                 *
C     *                 =14         4       ""      ""                 *
C     *                 =15         5       ""      ""                 *
C     *                 =16         6       ""      ""                 *
C     *                 =17         7       ""      ""                 *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
C
C     ------------------------------------------------------------------
C
      IERR   = 0
      KEY    = KEYI
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      JZ4    = IZ(JZ - 4)
      LOC    = 0
      IF (JZ4.GT.0)                              GO TO 10
C
C             FIRST CALL TO ZEPKEY ---> CREATE SYSTEM BANKS
C
      CALL ZBOOKN(IZ,IZ(JZ-4),0,4,'*EPK',1)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      JZ4    = IZ(JZ - 4)
C
      CALL ZBOOKN(IZ,IZ(JZ4-1),1,0,'*KEY',1)
      JZ4    = IZ(JZ - 4)
      CALL ZBOOKN(IZ,IZ(JZ4-2),1,0,'*NID',1)
      JZ4    = IZ(JZ - 4)
      CALL ZBOOKN(IZ,IZ(JZ4-3),1,0,'*NEL',1)
      JZ4    = IZ(JZ - 4)
      CALL ZBOOKN(IZ,IZ(JZ4-4),1,0,'*COD',1)
      JZ4    = IZ(JZ - 4)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
C
      LOC    = 1
C
C             NOT THE FIRST CALL .. SEARCH IF KEY IS ALREADY IN TABLE
C
  10  JKEY   = IZ(JZ4 - 1)
      JNID   = IZ(JZ4 - 2)
      JNEL   = IZ(JZ4 - 3)
      JCOD   = IZ(JZ4 - 4)
      NKEY   = IZ(JKEY) - 2
      IF (NKEY.EQ.0)                             GO TO 20
      IF (LOC.NE.0)                              GO TO 20
C
      LUP    = NKEY + 1
      LDOWN  = 0
C
  11  IF (LUP-LDOWN.LE.1)                        GO TO 14
      LMEAN  = (LUP+LDOWN) / 2
      LOC    = LMEAN
      IF (KEY-IZ(JKEY+LOC)) 12,20,13
C
  12  LUP    = LMEAN
      GO TO 11
C
  13  LDOWN  = LMEAN
      LOC    = LOC + 1
      GO TO 11
C
  14  LOC    = -LOC
C
  20  IF (NELEM.LT.0)                            GO TO 50
      IF (LOC.GT.0)                              GO TO 40
      LOC    = -LOC
C
      CALL ZPUSHS(IZ,JKEY,1,0)
      JNID   = IZ(JZ4 - 2)
      CALL ZPUSHS(IZ,JNID,1,0)
      JNEL   = IZ(JZ4 - 3)
      CALL ZPUSHS(IZ,JNEL,1,0)
      JCOD   = IZ(JZ4 - 4)
      CALL ZPUSHS(IZ,JCOD,1,0)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      NCOPY  = NKEY - LOC + 1
      IF (NCOPY.LE.0)                            GO TO 40
C
      DO 30 I=1,NCOPY
      J=LOC+NCOPY-I
      IZ(JNID+J+1)=IZ(JNID+J)
      IZ(JNEL+J+1)=IZ(JNEL+J)
      IZ(JCOD+J+1)=IZ(JCOD+J)
      IZ(JKEY+J+1)=IZ(JKEY+J)
  30  CONTINUE
C
  40  IZ(JKEY + LOC) = KEY
      IZ(JNID + LOC) = LOCF(ID(1)) + IZ(JZ + 16)
      IZ(JNEL + LOC) = NELEM
      IZ(JCOD + LOC) = MODE
      GO TO 99
C
C             NELEM<0    SUPPRESS KEY IF IT EXISTS
C
  50  IF (NKEY.EQ.0)                             GO TO 99
      IF (LOC.LE.0)                              GO TO 99
C
      NCOPY  = NKEY - LOC
      IF (NCOPY.LE.0)                            GO TO 60
C
      DO 55 I=1,NCOPY
      J=LOC+I
      IZ(JNID+J-1)=IZ(JNID+J)
      IZ(JNEL+J-1)=IZ(JNEL+J)
      IZ(JCOD+J-1)=IZ(JCOD+J)
      IZ(JKEY+J-1)=IZ(JKEY+J)
  55  CONTINUE
C
  60  CALL ZPUSHS(IZ,IZ(JZ4-1),-1,0)
      CALL ZPUSHS(IZ,IZ(JZ4-2),-1,0)
      CALL ZPUSHS(IZ,IZ(JZ4-3),-1,0)
      CALL ZPUSHS(IZ,IZ(JZ4-4),-1,0)
C
  99  IERR   = IZ(JZ + 6)
      RETURN
      END
