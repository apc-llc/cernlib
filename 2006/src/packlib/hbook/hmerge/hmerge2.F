*
* $Id: hmerge2.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hmerge2.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 07/06/94  09.07.24  by  Rene Brun
*-- Author :    Rene Brun, J.C. Marin, Fons Rademakers   26/05/94
      SUBROUTINE HMERGE2(CDIR)
*
#include "hbook/hcbook.inc"
#include "hbook/hcntpar.inc"
#include "hbook/hcntpaw.inc"
*
      INTEGER         IQUEST
      COMMON/QUEST/   IQUEST(100)
*
      CHARACTER*(*)   CDIR
      INTEGER         MXENU
      PARAMETER      (MXENU=512)
      CHARACTER*80    MDIR, RDIR
      CHARACTER*32    VAR
      CHARACTER*8     BLOCK
      REAL            X(MXENU)
      LOGICAL         HEXIST, NTOLD
      INTEGER         I, NKEY, IDN, IOFSET
      INTEGER         LENOCC
*
      PRINT *, 'Merging directory ', CDIR(1:LENOCC(CDIR))
*
      MDIR      = CDIR
      MDIR(3:6) = 'PAWC'
      RDIR      = CDIR
      RDIR(3:6) = 'BIGF'
      CALL HCDIR(MDIR, ' ')
      IF (IQUEST (1) .NE. 0) THEN
         PRINT *, 'New directory ', CDIR(1:LENOCC(CDIR))
         CALL HCDIR(CDIR, ' ')
         CALL HMERGE1(CDIR)
      ENDIF
      CALL HCDIR(CDIR, ' ')
*
      NKEY = 50000
      DO 40 I = 1, NKEY
         CALL RZINK(I, 0, 'S')
         IF (IQUEST(1) .NE. 0) GOTO 999
         IF (JBIT (IQUEST(14),4) .NE. 0) GOTO 40
         IDN = IQUEST(21)
         IOFSET = 100
20       CONTINUE
         IF (HEXIST(IDN+IOFSET)) THEN
            IOFSET = IOFSET + 1
            GOTO 20
         ENDIF
         CALL HRIN(IDN,999,IOFSET)
         IF (IQUEST(1) .NE. 0) THEN
            PRINT *, 'Cannot read object ', IDN
            GOTO 99
         ENDIF
         ID = IDN + IOFSET
         IF (JBIT (IQ(LCID+KBITS),4) .NE. 0) THEN
*
*         Merge Ntuples
*
            NTOLD = .FALSE.
            IF (IQ(LCID-2) .EQ. 2) NTOLD = .TRUE.
*
*         Set addresses for CWN's
*
            IF (.NOT. NTOLD) THEN
               NDIM = IQ(LCID+ZNDIM)
               DO 10 J = 1, NDIM
                  CALL HNTVAR(IDN, J, VAR, BLOCK, NS, IT, IS, IE)
                  CALL HNTGET(IDN, VAR, INDX, IT, IS, IE, IER)
                  CALL HNTGET(ID,  VAR, INDX, IT, IS, IE, IER)
                  IF (IER .NE. 0) THEN
                     PRINT *, 'Error accessing variable: ',
     +                       VAR(1:LENOCC(VAR)),' from Ntuple ', IDN
                     GOTO 99
                  ENDIF
10             CONTINUE
            ENDIF
*
            CALL HNOENT(ID, NENTRY)
            DO 30 IEVENT = 1, NENTRY
               IF (NTOLD) THEN
                  CALL HMERGE3(ID,IOFSET,IEVENT,X,IER)
                  IF (IER .NE. 0) THEN
                     PRINT *, 'Error reading event ', IEVENT,
     +                        ' from Ntuple ', IDN
                     GOTO 99
                  ENDIF
                  CALL HFN(IDN,X)
                  IF (IQUEST(1) .NE. 0) THEN
                     PRINT *, 'Error filling Ntuple ', IDN
                     GOTO 99
                  ENDIF
               ELSE
                  IF (IEVENT .EQ. 1) THEN
                     CALL HGNT(ID, IEVENT, IER)
                  ELSE
                     CALL HGNTF(ID, IEVENT, IER)
                  ENDIF
                  IF (IER .NE. 0) THEN
                     PRINT *, 'Error reading event ', IEVENT,
     +                        ' from Ntuple ', IDN
                     GOTO 99
                  ENDIF
                  CALL HFNT(IDN)
               ENDIF
*
30          CONTINUE
*
         ELSE
*
*         Merge histograms
*
            IF (HEXIST(IDN)) THEN
               CALL HOPERA(IDN, '+', ID, IDN, 1., 1.)
            ELSE
               CALL HRIN(IDN, 999, 0)
               IF (IQUEST(1) .NE. 0) THEN
                  PRINT *, 'Cannot read histogram ', IDN
                  GOTO 99
               ENDIF
            ENDIF
         ENDIF
*
         CALL HDELET(ID)
*
40    CONTINUE
*
      GOTO 999
*
*         Abnormal end
*
99    CONTINUE
*
999   END
