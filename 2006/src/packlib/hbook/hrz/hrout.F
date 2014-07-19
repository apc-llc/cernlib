*
* $Id: hrout.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrout.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 07/02/95  09.07.24  by  Rene Brun
*-- Author :
      SUBROUTINE HROUT(IDD,ICYCLE,CHOPT)
*.==========>
*.           Store histogram IDD on RZ file at current directory
*.             If option 'T' and IDD=0 save all subdirectories as well
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcntpar.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHOPT
      CHARACTER*128 CHWOLD
      CHARACTER*128 CHOLD, CHDIR, CWDRZ
      INTEGER       IOPT(2), KEYS(2)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTN,IOPT(2))
*.___________________________________________
*
      IQUEST(1)=0
      IF(ICHTOP(ICDIR).LE.0)THEN
         CALL HBUG('Current Directory must be a RZ file','HROUT',IDD)
         IQUEST(1)=1
         RETURN
      ENDIF
*
      CALL HUOPTC (CHOPT,'TN',IOPT)
      IF(IDD.NE.0)IOPTT=0
      IF(IOPTT.NE.0)THEN
         CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
         KLPAT0=NLPAT
         KLPAT=NLPAT
         LQ(LHBOOK-10-KLPAT)=LCDIR
      ENDIF
*
      KEYS(2) = 0
*
      IRET = 3
*
*         Save Histogram(s) in Current Directory
*
   10 CALL HLOOP (IDD,'HROUT ',IRET)
      IF (IRET .EQ. 0)                 GO TO 15
      I4=JBIT(IQ(LCID+KBITS),4)
      IF(I4.NE.0)THEN
         LC=LQ(LCID-1)
         IF (IQ(LCID-2) .NE. 2) THEN
*-- new N-tuple: write buffers and header
            CALL HNBFWR(ID)
            LC=LQ(LCID-1)
            IF (JBIT(IQ(LC),1) .EQ. 0) THEN
               IRET = 2
               GOTO 10
            ENDIF
*-- goto the correct RZ directory
            NCHRZ = IQ(LCID+ZNCHRZ)
            IF(NCHRZ.NE.0)THEN
               CALL RZCDIR(CWDRZ,'R')
               CALL HCDIR(CHOLD,'R')
               CHDIR = ' '
               CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
               IF (CHDIR .NE. CWDRZ) THEN
                  CALL HCDIR(CHDIR,' ')
               ENDIF
            ENDIF
            LC=LQ(LCID-1)
         ENDIF
         CALL SBIT0(IQ(LC),1)
      ENDIF
      KEYS(1) = ID
      CALL HRZOUT(IHDIV,LCID,KEYS,ICYCLE,' ')
      IF(I4.NE.0)THEN
         IF (IQ(LCID-2) .EQ. 2) THEN
            LC=LQ(LCID-1)
            CALL SBIT1(IQ(LC),1)
         ELSE
*-- go back to the current directory
            IF (NCHRZ.NE.0.AND.CHDIR .NE. CWDRZ) THEN
               CALL HCDIR(CHOLD,' ')
               IF (CHOLD .NE. CWDRZ) THEN
                  CALL RZCDIR(CWDRZ,' ')
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IRET  = 2
      IF(IQUEST(1).EQ.0)GO TO 10
*
      CALL HBUG('Problems with file','HROUT',ID)
      GO TO 99
*
*         Save Histogram(s) in Subdirectories if option 'T'
*
   15 IF(IOPTT.NE.0)THEN
   20    KLPAT=KLPAT+1
         IRET=3
         LCDIR=LQ(LCDIR-1)
   30    LQ(LHBOOK-10-KLPAT)=LCDIR
         IF(LCDIR.EQ.0)THEN
            KLPAT=KLPAT-1
            NLCDIR = KLPAT
            LCDIR=LQ(LHBOOK-10-KLPAT)
            IF(KLPAT.LE.KLPAT0)GO TO 90
            LCDIR=LQ(LCDIR)
#if !defined(CERNLIB_BSLASH)
            CALL RZCDIR('\',' ')
#endif
#if defined(CERNLIB_BSLASH)
            CALL RZCDIR('\\',' ')
#endif
            GO TO 30
         ENDIF
         CALL UHTOC(IQ(LCDIR+1),4,CHCDIR(KLPAT),16)
         LIDS  = LQ(LCDIR-2)
         LTAB  = LQ(LCDIR-3)
         LBUFM = LQ(LCDIR-4)
         LTMPM = LQ(LCDIR-5)
         IF(IOPTN.NE.0)THEN
            CALL HMDIR(CHCDIR(KLPAT),'S')
         ELSE
            CALL HCDIR(CHCDIR(KLPAT),' ')
            IF(IQUEST(1).EQ.-1)THEN
               CALL HMDIR(CHCDIR(KLPAT),'S')
            ENDIF
         ENDIF
         IF(IQUEST(1).NE.0)GO TO 90
*
   40    CALL HLOOP (IDD,'HROUT ',IRET)
         IF (IRET .EQ. 0)                 GO TO 20
         I4=JBIT(IQ(LCID+KBITS),4)
         IF(I4.NE.0)THEN
            LC=LQ(LCID-1)
            IF (IQ(LCID-2) .NE. 2) THEN
*-- new N-tuple: write buffers and header
               CALL HNBFWR(ID)
               LC=LQ(LCID-1)
               IF (JBIT(IQ(LC),1) .EQ. 0) THEN
                  IRET = 2
                  GOTO 40
               ENDIF
*-- goto the correct RZ directory
               NCHRZ = IQ(LCID+ZNCHRZ)
               IF(NCHRZ.NE.0)THEN
                  CALL RZCDIR(CWDRZ,'R')
                  CALL HCDIR(CHOLD,'R')
                  CHDIR = ' '
                  CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
                  IF (CHDIR .NE. CWDRZ) THEN
                     CALL HCDIR(CHDIR,' ')
                  ENDIF
               ENDIF
               LC=LQ(LCID-1)
               CALL SBIT0(IQ(LC),2)
            ENDIF
            CALL SBIT0(IQ(LC),1)
         ENDIF
         KEYS(1) = ID
         CALL HRZOUT(IHDIV,LCID,KEYS,ICYCLE,' ')
         IF(I4.NE.0)THEN
            IF (IQ(LCID-2) .EQ. 2) THEN
               LC=LQ(LCID-1)
               CALL SBIT1(IQ(LC),1)
            ELSE
*-- go back to the current directory
               IF (NCHRZ.NE.0.AND.CHDIR .NE. CWDRZ) THEN
                  CALL HCDIR(CHOLD,' ')
                  IF (CHOLD .NE. CWDRZ) THEN
                     CALL RZCDIR(CWDRZ,' ')
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IRET  = 2
         IF(IQUEST(1).EQ.0)GO TO 40
*
         CALL HBUG('Problems with file','HROUT',ID)
         GO TO 99
*
*          Restore Current Directory
*
   90    CALL HCDIR(CHWOLD,' ')
*
      ENDIF
*
   99 RETURN
      END
