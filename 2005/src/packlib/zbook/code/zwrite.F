*
* $Id: zwrite.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zwrite.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZWRITE(IZ,ID,LUN,KEYI)
C
C     ******************************************************************
C     *                                                                *
C     *       BANK WITH POINTER ID IS COPIED ONTO MASS STORAGE         *
C     *       IF ID ALREADY EXISTS ON LUN THEN IT IS SCRATCHED         *
C     *       AND RECREATED                                            *
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
      JZ     = IZ(1)
      KEY    = KEYI
      IZ(JZ + 6) = 0
      JD     = IZ(JZ - 2)
      IF (JD.LE.0)                               GO TO 900
      N      = IZ(JD) - 2
      NLUN   = 1
   10 IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 20
      NLUN   = NLUN + 1
      IF (NLUN.LE.N)                             GO TO 10
      GO TO 900
C
   20 JDIR   = IZ(JD - NLUN)
      IF (.NOT.ZIDOK(IZ,ID))                     GO TO 310
      IZ(JDIR + 10) = 1
      IPUSH=0
      NXBUF  = IZ(JDIR + 3)
      IDATA=ID(1)
      ND=IZ(IDATA)
      NID=IZ(IDATA+ND+2)
      NLINK=IZ(IDATA+ND+1)
      IF (IZ(JZ + 5).LE.0)                       GO TO 25
      IF (IZ(JZ + 1).GT.42 + 2 * NXBUF)          GO TO 25
C
C            PERFORM GARBAGE COLLECTION
C
      IZ(JZ + 18) = NID
      CALL ZGARB(IZ)
      NID    = IZ(JZ + 18)
      IDATA=IZ(NID)
      JD     = IZ(JZ - 2)
      JDIR   = IZ(JD - NLUN)
C
  25  IF (NLINK.EQ.0)                            GO TO 60
C
C             BANK IS A TREE   COMPUTE TOTAL NUMBER OF WORDS
C
      CALL ZBOOKN(IZ,IZ(JZ-5),NXBUF,0,'*BUF',1)
      JBUF=IZ(JZ-5)
      IF (JBUF.EQ.0)                             GO TO 999
C
      ND     = 0
      I      = 1
      KD(I)=IDATA
      KDI    = KD(I)
      NDATA=IZ(KDI)
      IL=KDI+NDATA+1
      NID    = IZ(IL + 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 330
      IF (NID.GT.IZ(JZ + 13))                    GO TO 330
      NK(I)  = IZ(IL)
      IFIRST = KDI - NK(I) - 1
      ND     = ND + IZ(IFIRST)
      IF (NK(I).LE.0)                            GO TO 60
   30 KDI    = KD(I)
      NLI    = NK(I)
      KD(I + 1) = IZ(KDI - NLI)
      IF (KD(I + 1).NE.0)                        GO TO 50
   40 NK(I)  = NK(I) - 1
      IF (NK(I).NE.0)                            GO TO 30
      I      = I - 1
      IF (I.LE.0)                                GO TO 60
      GO TO 40
   50 KDI    = KD(I + 1)
      NDATA=IZ(KDI)
      IL=KDI+NDATA+1
      NID    = IZ(IL + 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 330
      IF (NID.GT.IZ(JZ + 13))                    GO TO 330
      NK(I + 1) = IZ(IL)
      IFIRST = KDI - NK(I + 1) - 1
      ND     = ND + IZ(IFIRST)
      IF (NK(I + 1).LE.0)                        GO TO 40
      I      = I + 1
      IF (I.GT.MAXLEV)                           GO TO 320
      GO TO 30
C
   60 NR     = (ND - 1 + NXBUF) / NXBUF
C
      CALL ZSEARC(IZ,NLUN,KEY,LOC)
      IF (LOC.LE.0)                              GO TO 80
C
C             ID ALREADY EXISTING   SCRATCH IF DIFFERENT SPACE
C             UNLESS JUST REPLACE
C
      IF     = IZ(LOC + 1)
      NW     = IABS(IZ(LOC + 2))
      NROLD  = (NW + NXBUF - 1) / NXBUF
      IF (NROLD.EQ.NR)                           GO TO 180
      IF (IF.LT.IZ(JDIR + 8))IZ(JDIR + 8) = IF
C
      DO 70 I = 1,NROLD
   70 IZ(JDIR + 9 + IF + I) = 0
      IZ(JDIR + 7) = IZ(JDIR + 7) + NROLD
      IF (IZ(JDIR + 7).LT.NR)                    GO TO 300
      GO TO 120
C
C             INSERTS ID IN THE IDS TABLE
C
   80 LOC    = - LOC
      IF (IZ(JDIR + 7).LT.NR)                    GO TO 300
      IF (IZ(JDIR + 6).GT.0)                     GO TO 90
      IF(IDATA.GT.JDIR)IDATA=IDATA+30
      IPUSH=1
      NIDPAR=LOCF(ID(1))+IZ(JZ+16)+1
      CALL ZPUSHS(IZ,JDIR,30,0)
      JBUF=IZ(JZ-5)
      IF (IZ(JZ + 6).NE.0)                       GO TO 999
      IZ(JDIR + 6) = IZ(JDIR + 6) + 10
C
   90 N      = IZ(JDIR + 5) + JDIR - LOC
      IF (N.EQ.0)                                GO TO 110
      DO 100 I = 1,N
  100 IZ(LOC + N - I + 3) = IZ(LOC + N - I)
  110 IZ(JDIR + 5) = IZ(JDIR + 5) + 3
      IZ(JDIR + 6) = IZ(JDIR + 6) - 1
C
C             SEARCH FOR NR CONSECUTIVE RECORDS
C
  120 IF     = IZ(JDIR + 8)
  130 N      = 1
      DO 140 I = 1,NR
      IF (IZ(JDIR + 9 + IF + I).NE.0)            GO TO 150
      N      = N + 1
  140 CONTINUE
      GO TO 160
C
  150 IF     = IF + N
      IF (IF + NR.LE.IZ(JDIR + 2))               GO TO 130
C
C             GARBAGE COLLECTION NECESSARY
C
      CALL ZDGARB(IZ,NLUN)
C
      IF (IZ(JDIR + 2) - IZ(JDIR + 8).LT.NR - 1) GO TO 999
      GO TO 120
C
  160 IZ(LOC) = KEY
      IZ(LOC + 1) = IF
      IF(IPUSH.EQ.0)GO TO 165
      IF(NIDPAR.EQ.NID)GO TO 165
      ID(1)=IDATA
C
  165 IF (IZ(JDIR + 8).EQ.IF)IZ(JDIR + 8) = IF + NR
      IZ(JDIR + 7) = IZ(JDIR + 7) - NR
      DO 170 I = 1,NR
  170 IZ(JDIR + 9 + IF + I) = KEY
C
  180 IF (NLINK.NE.0)                            GO TO 190
      IZ(LOC + 2) = ND
C
C             NOW WRITE BANK ONTO MASS STORAGE
C
      CALL ZIODO(LUN,IF,NXBUF,IZ(IDATA+1),ND,2)
C
      GO TO 999
C
C             NOW WRITE TREE
C
  190 IZ(LOC + 2) = - ND
C
      IREC   = 0
      IPBUF  = 0
      IPZBK  = 0
      NWBUF  = NXBUF
      I      = 1
      KD(I)=IDATA
      KDI    = KD(I)
      NDATA=IZ(KDI)
      IL=KDI+NDATA+1
      NK(I)  = IZ(IL)
      IFIRST = KDI - NK(I) - 1
      ND     = IZ(IFIRST)
      NWZBK  = ND
  200 IF (NWBUF.GT.NWZBK)                        GO TO 210
      IREC   = IREC + 1
      CALL UCOPY(IZ(IFIRST+IPZBK),IZ(JBUF+IPBUF+1),NWBUF)
      CALL ZIODO(LUN,IF+IREC-1,NXBUF,IZ(JBUF+1),NXBUF,2)
      IPZBK  = IPZBK + NWBUF
      NWZBK  = NWZBK - NWBUF
      IPBUF  = 0
      NWBUF  = NXBUF
      IF (NWZBK.EQ.0)                            GO TO 220
      GO TO 200
  210 CALL UCOPY(IZ(IFIRST+IPZBK),IZ(JBUF+IPBUF+1),NWZBK)
      IPBUF  = IPBUF + NWZBK
      NWBUF  = NWBUF - NWZBK
  220 IPZBK  = 0
      IF (NK(I).LE.0)                            GO TO 290
  230 KDI    = KD(I)
      NLI    = NK(I)
      KD(I + 1) = IZ(KDI - NLI)
      IF (KD(I + 1).NE.0)                        GO TO 250
  240 NK(I)  = NK(I) - 1
      IF (NK(I).NE.0)                            GO TO 230
      I      = I - 1
      IF (I.LE.0)                                GO TO 290
      GO TO 240
  250 KDI    = KD(I + 1)
      NDATA=IZ(KDI)
      IL=KDI+NDATA+1
      NK(I + 1) = IZ(IL)
      IFIRST = KDI - NK(I + 1) - 1
      ND     = IZ(IFIRST)
      NWZBK  = ND
  260 IF (NWBUF.GT.NWZBK)                        GO TO 270
      IREC   = IREC + 1
      CALL UCOPY(IZ(IFIRST+IPZBK),IZ(JBUF+IPBUF+1),NWBUF)
      CALL ZIODO(LUN,IF+IREC-1,NXBUF,IZ(JBUF+1),NXBUF,2)
      IPZBK  = IPZBK + NWBUF
      NWZBK  = NWZBK - NWBUF
      IPBUF  = 0
      NWBUF  = NXBUF
      IF (NWZBK.EQ.0)                            GO TO 280
      GO TO 260
  270 CALL UCOPY(IZ(IFIRST+IPZBK),IZ(JBUF+IPBUF+1),NWZBK)
      IPBUF  = IPBUF + NWZBK
      NWBUF  = NWBUF - NWZBK
  280 IPZBK  = 0
      IF (NK(I + 1).LE.0)                        GO TO 240
      I      = I + 1
      IF (I.GT.MAXLEV)                           GO TO 320
      GO TO 230
C
  290 IREC   = IREC + 1
      IF(IPBUF.GT.0) CALL ZIODO(LUN,IF+IREC-1,NXBUF,IZ(JBUF+1),
     +              IPBUF,2)
C
      CALL ZDELET(IZ,JBUF)
C
      GO TO 999
C
C             NO MORE SPACE ON MASS STORAGE
C
  300 CALL ZERROR(IZ,600,'ZWRITE',ID)
      GO TO 999
C
C             ID DOES NOT EXIST
C
  310 CALL ZERROR(IZ,300,'ZWRITE',ID)
      GO TO 999
C
C             LEVEL TOO HIGH
C
  320 CALL ZERROR(IZ,800,'ZWRITE',ID)
      GO TO 999
C
C             ERROR IN STRUCTURE
C
  330 CALL ZERROR(IZ,400,'ZWRITE',ID)
      GO TO 999
C
C             UNDEFINED OUTPUT UNIT
C
  900 CALL ZERROR(IZ,900,'ZWRITE',ID)
C
  999 RETURN
      END
