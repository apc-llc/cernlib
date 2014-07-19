*
* $Id: zepout.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepout.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPOUT (IZ,LUN,KEYS,NKEYS,IDCAT,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       WRITES NEXT LOGICAL EVENT IDENTIFIED BY THE              *
C     *       LIST OF NKEY KEYWORDS                                    *
C     *       A CATALOG IS WRITTEN AS THE FIRST EP RECORD              *
C     *       AND IT IS IDENTIFIED BY IDCAT                            *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1) , KEYS(1), IH(60) , IDUM(2)
C--- Inserted by HG
      COMMON/ZCFORM/NLFORM,JD(60)
      DIMENSION IFORBF(50)
      EQUIVALENCE (IFORBF(1),JD(11))
C---
      DIMENSION JL(10) , JM(10)
      DIMENSION MODSEP(8)
      LOGICAL ZIDOK
C
      DATA MAXLEV/10/
      DATA IBLA/4H    /
      DATA MODSEP/1,2,3,3,3,2,3,3/
C
#include "zbook/wlength.inc"
C
C     ------------------------------------------------------------------
C
C--- Inserted by HG
      NAMEL=0
      NWFORL=0
C---
      NKEY   = NKEYS
      IERR   = 0
      LC     = 0
      IDS    = IZ(1)
      IZ(IDS+6)=0
C
C     CHECK IF LUN IS DEFINED
C
      JBUF   = IZ(IDS - 3)
      IF (JBUF.LE.0)                             GO TO 10
C
      NUNIT  = IZ(JBUF) - 2
      LBUF   = IUCOMP (LUN,IZ(JBUF + 1),NUNIT)
      IF (LBUF.NE.0)                             GO TO 20
C
C     LUN IS UNDEFINED
C
   10 CALL ZERROR (IZ,900,'ZEPOUT',0)
      GO TO 260
C
C     LUN IS DEFINED
C
   20 JBUF   = IZ(JBUF - LBUF) + 3
      IH(2)  = 1
      IH(4)  = IDCAT
C
      IF (NKEY.LE.0)                             GO TO 30
C
      CALL ZBOOK (IZ,LC,NKEY)
      IF (LC.EQ.0)                               GO TO 260
C
      JBUF   = IZ(IDS - 3)
      JBUF   = IZ(JBUF - LBUF) + 3
C
      CALL UCOPY (KEYS,IZ(LC+1),NKEY)
      CALL ZTOIBM (IZ(LC+1),NKEY,2)
C
C     WRITE CATALOG
C
   30 CALL EPOUTL (LUN,3,4,IH,NKEY,IZ(LC+1),IZ(JBUF),IERR)
C
      IF (IERR.NE.0)                             GO TO 240
C
      JF     = JBUF - 2
      IZ(JF) = 4
C
      IF (NKEY.LE.0)                             GO TO 230
C
C     LOOP ON KEYS
C
      DO 220 N = 1,NKEY
      KEY    = KEYS(N)
C
C     CHECK IF ACTIVE KEY
C
      CALL ZEPLOC (IZ,KEY,NID,NMEMBS,ICODE)
      IF (NID.EQ.0)                              GO TO 220
C
      IDUM(1) = KEY
C
      IH(2)  = 2
      IDUM(2) = ICODE
      CALL ZTOIBM (IDUM,2,2)
C
C     WRITE KEY HEADER
C
      CALL EPOUTL (LUN,3,3,IH,2,IDUM,IZ(JBUF),IERR)
      IF (IERR.NE.0)                             GO TO 250
C
      IF (NMEMBS.LE.0)                           GO TO 220
C
      NMSS   = 0
C
C     LOOP ON MEMBERS
C
      DO 210 NMB = 1,NMEMBS
      IDSAVE = IZ(NID + NMB)
C
C     CHECK IF IT CAN BE WRITTEN
C
      NMAX   = 0
      NST    = 1
      JD(1)  = NID + NMB
      JL(1)  = 2
      JM(1)  = 1
C
   40 ID     = JD(NST)
      NLII   = JM(NST) - JL(NST) + 1
      IDD    = IZ(ID - NLII)
      NLINKS = 0
C
      IF (.NOT.ZIDOK(IZ,IDD))                    GO TO 50
C
      NDATA     = IZ(IDD) - 2
      NLINKS    = IZ(IDD+NDATA+3)
      IF (NDATA.LE.NMAX)                         GO TO 50
C
      NMAX      = NDATA
C
   50 JL(NST) = JL(NST) - 1
      IF (JL(NST).NE.0)                          GO TO 60
C
      NST    = NST - 1
C
   60 IF (NLINKS.NE.0)                           GO TO 70
      IF (NST.NE.1)                              GO TO 40
      GO TO 90
C
   70 NST    = NST + 1
      IF (NST.LE.MAXLEV)                         GO TO 80
C
C     TOO MANY LEVELS
C
      CALL ZERROR (IZ,800,'ZEPOUT',IDSAVE)
      GO TO 230
C
   80 JD(NST) = IDD
      JL(NST) = NLINKS
      JM(NST) = JL(NST)
      GO TO 40
C
C     LOOK FOR SPACE TO COPY THE BANKS
C
   90 IF (ICODE.LE.10)                           GO TO 110
C
      NDATA     = IZ(LC) - 2
      IF (NDATA.GE.NMAX)                         GO TO 110
C
      CALL ZPUSH (IZ,LC,NMAX-NDATA)
      IF (IZ(IDS + 6).EQ.0)                      GO TO 100
C
C     SPACE PROBLEM: STOP
C
      GO TO 230
C
  100 JBUF   = IZ(IDS - 3)
      JBUF   = IZ(JBUF - LBUF) + 3
C
C     WRITE A TREE
C
  110 NST    = 1
      JD(1)  = NID + NMB
      JL(1)  = 2
      JM(1)  = 1
C
  120 ID     = JD(NST)
      NLII   = JM(NST) - JL(NST) + 1
      IDD    = IZ(ID - NLII)
C
      IF (ZIDOK(IZ,IDD))                         GO TO 130
C
C     MISSING BANK
C
      NMSS   = NMSS + 1
      NLINKS = 0
      GO TO 185
C
C     WELL DEFINED BANK
C
  130 NDATA     = IZ(IDD) - 2
      NLINKS    = IZ(IDD+NDATA+3)
      NAME      = IZ(IDD+NDATA+1)
      NUMB      = IZ(IDD+NDATA+2)
C
      IF (NMSS.EQ.0)                             GO TO 140
C
      IH(2)  = 5
      IH(4)  = NMSS
      NMSS   = 0
C
      CALL EPOUTL (LUN,1,4,IH,0,IZ,IZ(JBUF),IERR)
C
      IF (IERR.NE.0)                             GO TO 250
C
  140 IH(2)  = 3
      ISIT   = 1
      IF (NLINKS.NE.0)                ISIT = ISIT + 1
      IF (NAME.NE.IBLA.OR.NUMB.NE.0)  ISIT = ISIT + 2
C--- Inserted by HG
C    CHECK WHETHER NAME IN FORMAT TABLE, GET REF.
C    DO NOTHING IF SAME NAME AS PREVIOUS
      IF(NAME.NE.NAMEL)  THEN
         CALL ZNAMSR(IZ,NAME,IPOS,LAST)
         NAMEL=NAME
         NWFORL=0
      ELSE
         IPOS=0
      ENDIF
C---
C
      GO TO (141,142,143,144), ISIT
C
  141 NWH    = 3
      GO TO 149
C
  142 NWH    = 4
      IH(4)  = NLINKS
      GO TO 149
C
  143 NWH    = 6
C
      CALL ZTOIBM (NAME,1,4)
C
      IH(4)  = JBYT (NAME, 1,16)
      IH(5)  = JBYT (NAME,17,16)
      IH(6)  = NUMB
      GO TO 149
C
  144 NWH    = 7
C
      CALL ZTOIBM (NAME,1,4)
C
      IH(4)  = NLINKS
      IH(5)  = JBYT (NAME, 1,16)
      IH(6)  = JBYT (NAME,17,16)
      IH(7)  = NUMB
C
  149 CONTINUE
C--- Inserted by HG
      IF(IPOS.GT.0.AND.NWH.GE.6)  THEN
C--- BANK HAS A FORMAT - PUT IT IN LOG. REC. HEADER
      CALL ZGTFOR(IZ,IPOS,NWFOR,IH(NWH+1))
      CALL UCOPY(IH(NWH+1),IFORBF,NWFOR)
      NWFORL=NWFOR
      NWH=NWH+NWFOR+1
      IH(NWH)=NWFOR
      ENDIF
C---
      MODE   = MOD (ICODE,10)
      MODEP  = MODSEP(MODE + 1)
      IDB    = IDD
      IF (NDATA.EQ. 0)                           GO TO 180
      IF (ICODE.LE.10)                           GO TO 150
C
      CALL UCOPY (IZ(IDD+1),IZ(LC+1),NDATA)
      IDD    = LC
C
  150 IF (MODE.NE.0)                             GO TO 160
C
      NDATA     = (NDATA*NBIT-1) / 16 + 1
      GO TO 180
C
  160 CONTINUE
C--- Inserted by HG
      IF(NWFORL.GT.0)  GOTO 170
C---
      IF (MODE.EQ.5)                             GO TO 180
C
      CALL ZTOIBM (IZ(IDD+1),NDATA,MODE)
C--- Inserted by HG
      GOTO 180
  170 CONTINUE
      IF(NDATA.GT.0)CALL ZFCONV(IFORBF,NWFOR,IZ(IDD+1),NDATA,1)
C---
C
C     WRITE A BANK
C
  180 CALL EPOUTL (LUN,MODEP,NWH,IH,NDATA,IZ(IDD+1),IZ(JBUF),IERR)
C
      IF (IERR.NE.0)                             GO TO 250
C
  185 JL(NST) = JL(NST) - 1
      IF (JL(NST).NE.0)                          GO TO 190
C
      NST    = NST - 1
      IF (NMSS.EQ.0)                             GO TO 190
C
      IH(2)  = 5
      IH(4)  = NMSS
      NMSS   = 0
C
      CALL EPOUTL (LUN,1,4,IH,0,IZ,IZ(JBUF),IERR)
C
      IF (IERR.NE.0)                             GO TO 250
C
  190 IF (NLINKS.NE.0)                           GO TO 200
      IF (NST.NE.1)                              GO TO 120
      GO TO 210
C
  200 NST    = NST + 1
      JD(NST) = IDB
      JL(NST) = NLINKS
      JM(NST) = JL(NST)
      GO TO 120
C
  210 CONTINUE
C
      IF (NMSS.EQ.0)                             GO TO 220
C
      IH(2)  = 5
      IH(4)  = NMSS
C
      CALL EPOUTL (LUN,1,4,IH,0,IZ,IZ(JBUF),IERR)
C
      IF (IERR.NE.0)                             GO TO 250
C
  220 CONTINUE
C
  230 IH(2)  = 4
C
C     WRITE TRAILER RECORD
C
      CALL EPOUTL (LUN,1,3,IH,0,IZ,IZ(JBUF),IERR)
C
  240 IF (LC.LE.0)                               GO TO 260
C
  250 CALL ZDELET (IZ,LC)
C
  260 IERR   = IERR + IZ(IDS + 6)
C
      RETURN
      END
