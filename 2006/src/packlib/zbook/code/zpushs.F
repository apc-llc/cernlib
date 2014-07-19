*
* $Id: zpushs.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zpushs.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZPUSHS(IZ,ID,NDDP,NLLP)
C
C     ******************************************************************
C     *                                                                *
C     *       PUSH BANK ID BY NPUSH WORDS (NPUSH,POSITIVE OR NEGATIVE  *
C     *       SHIFTING ALL BANKS CREATED AFTER ID                      *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IF (ZIDOK(IZ,ID))                          GO TO 5
      CALL ZERROR(IZ,300,'ZPUSHS',ID)
      RETURN
C
   5  IF (NDDP.EQ.0.AND.NLLP.EQ.0)               GO TO 999
      NPUSH  = NDDP + NLLP
      IZ(JZ + 6) = 0
      IF (NPUSH.LE.IZ(JZ + 1))                   GO TO 10
      IDATA=ID(1)
      ILAST=IDATA+IZ(IDATA)+2
      NID    = IZ(ILAST)
      IZ(JZ + 18) = NID
      CALL ZGARB(IZ)
      NID    = IZ(JZ + 18)
      ID(1)  = IZ(NID)
      IF (NPUSH.LE.IZ(JZ + 1))                   GO TO 10
      NMOVE  = NPUSH - IZ(JZ + 1) + 1
      CALL ZMOVE(IZ,NMOVE)
      IF (NPUSH.LE.IZ(JZ + 1))                   GO TO 10
      CALL ZERROR(IZ,100,'ZPUSHS',ID)
      GO TO 999
C
  10  IDATA=ID(1)
      ILAST=IDATA+IZ(IDATA)+2
      IF (ILAST.LE.0)                            GO TO 900
      IF (ILAST.GT.IZ(JZ + 15))                  GO TO 900
      NID    = IZ(ILAST)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 900
      IF (NID.GT.IZ(JZ + 13))                    GO TO 900
      NLINK  = IZ(ILAST - 1)
      IFIRST=IDATA-NLINK-1
      IF (IFIRST.LE.0)                           GO TO 900
      IF (IFIRST.GT.IZ(JZ + 15))                 GO TO 900
      NDP    = NDDP
      IF(NDP.LT.-IZ(IDATA)+2)NDP=-IZ(IDATA)+2
      NLP    = NLLP
      IF (NLP.LT. - NLINK)NLP = - NLINK
C
      IF (NPUSH.EQ.0)                            GO TO 200
      IF (NPUSH.LT.0)                            GO TO 300
C
C=========    A,  NPUSH  POSITIVE
C
      IMAX   = IZ(JZ + 15)
      CALL ZSHIFT(IZ,ILAST+1,IMAX,NPUSH,'ZPUSHS')
      IF (IZ(JZ + 6).NE.0)                       GO TO 999
      IZ(ILAST+NPUSH  )=IZ(ILAST  )
      IZ(ILAST+NPUSH-1)=IZ(ILAST-1)
      IZ(ILAST+NPUSH-2)=IZ(ILAST-2)
      IZ(ILAST+NPUSH-3)=IZ(ILAST-3)
      IF(IZ(JZ-1).NE.0) CALL ZUPLOC(IZ,ILAST-3,ILAST,NPUSH)
      ILAST  = ILAST + NPUSH
C
      IF (NLP.EQ.0)                              GO TO 140
      IF (NLP.LT.0)                              GO TO 170
C
C             NPUSH>0 NLP>0 NDP>0
C             NPUSH>0 NLP>0 NDP=0
C             NPUSH>0 NLP>0 NDP<0
C             NPUSH=0 NLP>0 NDP<0
C
 120  CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
      CALL ZSHIFP(IZ,IFIRST,IDATA,ILAST,NLP)
      GO TO 400
C
C             NPUSH>0 NLP=0 NDP>0
C
 140  CONTINUE
      CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
      GO TO 400
C
C             NPUSH>0 NLP<0 NDP>0
C             NPUSH=0 NLP<0 NDP>0
C
 170  CONTINUE
      CALL ZSHIFP(IZ,IFIRST,IDATA,ILAST,NLP)
      CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
      GO TO 400
C
C========     B,   NPUSH=0
C
 200  IF (NLP.LT.0)                              GO TO 170
      GO TO 120
C
C========     C,   NPUSH NEGATIVE
C
 300  IF (NLP.EQ.0)                              GO TO 330
      IF (NLP.LT.0)                              GO TO 350
C
C             NPUSH<0 NLP>0 NDP<0
C
      CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
      CALL ZSHIFP(IZ,IFIRST,IDATA,ILAST,NLP)
      GO TO 390
C
C             NPUSH<0 NLP=0 NDP<0
C
 330  CONTINUE
      CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
      GO TO 390
C
C             NPUSH<0 NLP<0 NDP>0
C             NPUSH<0 NLP<0 NDP=0
C             NPUSH<0 NLP<0 NDP<0
C
 350  CALL ZSHIFP(IZ,IFIRST,IDATA,ILAST,NLP)
      CALL ZSHIFD(IZ,IDATA,ILAST,NLP,NDP)
C
 390  ILNEW  = ILAST + NPUSH
      IF(NPUSH.GT.0)GO TO 392
      IZ(ILNEW-3)=IZ(ILAST-3)
      IZ(ILNEW-2)=IZ(ILAST-2)
      IZ(ILNEW-1)=IZ(ILAST-1)
      IZ(ILNEW  )=IZ(ILAST  )
      GO TO 394
C
 392  IZ(ILNEW  )=IZ(ILAST  )
      IZ(ILNEW-1)=IZ(ILAST-1)
      IZ(ILNEW-2)=IZ(ILAST-2)
      IZ(ILNEW-3)=IZ(ILAST-3)
C
 394  IF (ILAST + 1.EQ.IZ(JZ + 15))              GO TO 395
      IZ(ILNEW + 1) = NPUSH
      IZ(JZ + 5) = IZ(JZ + 5) - NPUSH
      GO TO 400
C
 395  IZ(JZ + 15) = ILNEW + 1
      IZ(ILNEW + 1) = 0
      IZ(JZ + 1) = IZ(JZ + 1) - NPUSH
C
 400  IZ(IFIRST) = IZ(IFIRST) + NPUSH
      IZ(NID)=IDATA
      ID(1)=IDATA
      GO TO 999
C
C
C             STRUCTURE OVERWRITTEN
C
 900  CALL ZERROR(IZ,400,'ZPUSHS',ID)
C
 999  IZ(JZ + 17) = 0
      IZ(JZ + 7) = IZ(JZ + 7) + 1
      RETURN
      END
