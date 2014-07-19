*
* $Id: zdisk.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zdisk.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZDISK(IZ,LUN,NREC,LREC)
C
C     ******************************************************************
C     *                                                                *
C     *       INITIALIZES BANK FOR DISK/LCM OVERFLOW                   *
C     *       POINTER TO THE DISK DESCRIPTION BANK IS                  *
C     *       JD=IZ(JZ-2)   POINTER TO DISK DIRECTORY                  *
C     *                                                                *
C     *       IZ(JD+NLUN)  =  LUN                                      *
C     *       IZ(JD-NLUN)  =  POINTER FOR DIRECTORY FOR LUN            *
C     *                                                                *
C     *       IZ(JDIR+1) = LUN                                         *
C     *               2  = NREC                                        *
C     *               3  = LREC                                        *
C     *               4  = POINTER TO FIRST KEY IN JDIR                *
C     *               5  = POINTER TO LAST KEY IN JDIR+4               *
C     *               6  = NUMBER OF FREE PLACES FOR KEYS              *
C     *               7  = NUMBER OF FREE RECORDS                      *
C     *               8  = POINTER TO FIRST UNUSED RECORD              *
C     *               9  = POINTER TO GARBAGE COLLECTION BANK          *
C     *              10  = 1   IF DATA SET HAS BEEN MODIFIED           *
C     *                                                                *
C     *       IZ(JDIR+11)----->IZ(JDIR+10+NREC)=RECORD DESCRIPTION     *
C     *       IZ(JDIR+10+IREC)=KEY IF IREC CONTAINS ID                 *
C     *                       =0   IF IREC IS FREE                     *
C     *                                                                *
C     *       IZ(JDIR+10+NREC+1) = KEY1                                *
C     *                       2  = FIRST RECORD CONTAINING ID1         *
C     *                       3  = NUMBER OF WORDS IN BANK ID1         *
C     *                       4  = KEY2                                *
C     *                       5  = ETC.                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
#if defined(CERNLIB_UNIVAC)
      CHARACTER * 7 IDIM
#endif
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      IF (IZ(JZ - 2).NE.0)                       GO TO 3
      CALL ZBOOKN(IZ,IZ(JZ-2),1,1,'*ZDI',1)
   3  JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 99
      NL     = IZ(JD) - 2
      NLUN   = 1
C
   5  IF (IZ(JD + NLUN).EQ.0.AND.IZ(JD - NLUN).EQ.0) GO TO 7
      NLUN   = NLUN + 1
      IF(NLUN.GT.NL) CALL ZPUSHS(IZ,JD,1,1)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      GO TO 5
C
   7  CALL ZBOOKN(IZ,IZ(JD-NLUN),NREC+40,0,'*DIR',LUN)
      JD     = IZ(JZ - 2)
      JDIR   = IZ(JD - NLUN)
      IF (JDIR.EQ.0)                             GO TO 99
      JD     = IZ(JZ - 2)
C
      IZ(JD + NLUN) = LUN
      IZ(JDIR + 1) = LUN
      IZ(JDIR + 2) = NREC - 1
      IZ(JDIR + 3) = LREC
      IZ(JDIR + 4) = 11 + NREC
      IZ(JDIR + 5) = IZ(JDIR + 4)
      IZ(JDIR + 6) = 10
      IZ(JDIR + 7) = NREC - 1
      IZ(JDIR + 8) = 1
      IZ(JDIR + 10)=1
#if defined(CERNLIB_UNIVAC)
C
C     ASSIGN A TEMPORARY WORD-ADDRESSABLE FILE 'Z$B' FOR LUN=0
C
      IF (LUN.EQ.0) THEN
      NWMAX  = ((NREC * LREC + 99999) / 100000) * 100000
      IF (NWMAX.LT.1000000) THEN
      ENCODE(7,10,IDIM) NWMAX
      ELSE
      ENCODE(7,11,IDIM) NWMAX
      ENDIF
      ISTAT  = FACSF2('@ASG,T Z$B.,D4///'//IDIM//' . ')
      IF (ISTAT.LT.0) THEN
      WRITE(6,12) ISTAT
      STOP 'ZDISK'
      ENDIF
      ENDIF
   10 FORMAT(I6,1X)
   11 FORMAT(I7)
   12 FORMAT('  LUN=0 (FILE Z$B) CANNOT BE ASSIGNED IN ZDISK. STATUS:',
     + O14)
#endif
C
  99  RETURN
      END
