*
* $Id: pcnext.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pcnext.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/08 08/06/94  17.38.07  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/04/93
      SUBROUTINE PCNEXT(IDN, NCHROW, NDIM, NROW, IEND)
*.==========>
*.
*.     Get the next chain file, connect the file and load the Ntuple.
*.     At end of chain, load again the first chain file and Ntuple.
*.     IEND <> 0 at end of chain.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcntpar.inc"
#include "paw/quest.inc"
#include "paw/pawcom.inc"
#include "paw/pntold.inc"
#include "paw/pawchn.inc"
#include "paw/pawcfu.inc"
*
      CHARACTER*80  CCHAIN
      CHARACTER*128 MEMBER, CHPATH, CHWARN
      INTEGER       PCHEVT
      LOGICAL       HNTNEW
*
      NCHROW = NCHROW + NROW
*
      CCHAIN = ' '
      MEMBER = ' '
      IEND   = 0
      NDIM   = 0
      NROW   = 0
      ICYCLE = 9999
*
*-- get CWD and delete previous Ntuple
*
      CALL HIDPOS(IDN, IDPOS)
      LCID   = LQ(LTAB-IDPOS)
      CHPATH = ' '
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         NCHRZ = IQ(LCID+11)
         CALL UHTOC(IQ(LCID+12),4,CHPATH,NCHRZ)
      ELSE
         NCHRZ = IQ(LCID+ZNCHRZ)
         CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHPATH,NCHRZ)
      ENDIF
*
*-- For memory resident Ntuples the path is not stored in the header.
*-- The solution below is not fool proof since the current directory
*-- might have been changed in a comis function. In general the usage
*-- of memory resident Ntuples should be discouraged in chains.
*
      IF (NCHRZ .EQ. 0) THEN
         CALL HCDIR(CHPATH,'R')
      ENDIF
*
      CALL HDELET(IDN)
*
*-- get next chain entry
*
  10  CALL PCHNXT(CCHAIN, LC, MEMBER, LM)
*
*-- if at end of chain then get first entry again
*
      IF (LM .EQ. 0) THEN
         CALL PCHSET(CURCHN, LENOCC(CURCHN), MEMBER, LM)
         IEND = 1
      ENDIF
*
*-- open chain entry
*
      CALL PCHROP(CURCHN, MEMBER(1:LM), IER)
      IF (IER .NE. 0) THEN
         WRITE(CHWARN,1000) MEMBER(1:LM)
         CALL HBUG(CHWARN,'PCNEXT',IDN)
         IF (IEND .EQ. 0) GOTO 10
         GOTO 20
      ENDIF
*
*-- CD to CWD and read Ntuple header
*
      ID1 = IDN - JOFSET
*
      CALL HCDIR(CHPATH,' ')
      IF (IQUEST(1) .NE. 0) THEN
         WRITE(CHWARN,1010) CHPATH(1:LENOCC(CHPATH)), MEMBER(1:LM)
         CALL HBUG(CHWARN,'PCNEXT',IDN)
         IF (IEND .EQ. 0) GOTO 10
         GOTO 20
      ENDIF
      CALL HRIN(ID1, ICYCLE, JOFSET)
*
      CALL HFIND(IDN,'PCNEXT')
      IF (LCID .EQ. 0) THEN
         WRITE(CHWARN,1020) MEMBER(1:LM)
         CALL HBUG(CHWARN,'PCNEXT',IDN)
         IF (IEND .EQ. 0) GOTO 10
         GOTO 20
      ENDIF
*
      CALL HDCOFL
      IF (I4 .EQ. 0) THEN
         WRITE(CHWARN,1030) MEMBER(1:LM)
         CALL HBUG(CHWARN,'PCNEXT',IDN)
         IF (IEND .EQ. 0) GOTO 10
         GOTO 20
      ENDIF
*
      NTOLD = .NOT.HNTNEW(IDN)
*
*-- set addresses in Ntuple header in which the variables will be restored
*-- (normally this is done in PADVAR)
*
      IF (.NOT.NTOLD) THEN
         CALL HBNAME(IDN,' ',0,'$CLEAR')
         DO 30 I = 1, NVART2
            CALL HNTGET(IDN, VAR(I), INDD, IT, IS, IE, IERROR)
            IF (IERROR .NE. 0) THEN
               WRITE(CHWARN,1050) MEMBER(1:LM)
               CALL HBUG(CHWARN,'PCNEXT',IDN)
               IF (IEND .EQ. 0) GOTO 10
               GOTO 20
            ENDIF
   30    CONTINUE
      ENDIF
*
      IF (NTOLD) THEN
         CALL HGNPAR(IDN,'PCNEXT')
         IF (LCIDN.LE.0) THEN
            WRITE(CHWARN,1040) MEMBER(1:LM)
            CALL HBUG(CHWARN,'PCNEXT',IDN)
            IF (IEND .EQ. 0) GOTO 10
            GOTO 20
         ENDIF
         IF (LQ(LCIDN-1).LE.0) THEN
            CALL HDELET(IDN)
            WRITE(CHWARN,1060) MEMBER(1:LM)
            CALL HBUG(CHWARN,'PCNEXT',IDN)
            IF (IEND .EQ. 0) GOTO 10
            GOTO 20
         ENDIF
         NDIM = IQ(LCIDN+2)
         NROW = IQ(LCIDN+3)
      ELSE
         IDNN = IDN
         CALL HPARNT(IDNN,'PCNEXT')
         IF (IDNN .EQ. 0) THEN
            WRITE(CHWARN,1040) MEMBER(1:LM)
            CALL HBUG(CHWARN,'PCNEXT',IDN)
            IF (IEND .EQ. 0) GOTO 10
            GOTO 20
         ENDIF
         IF (IQ(LCID+3) .EQ. -1) THEN
            CALL HDELET(IDN)
            WRITE(CHWARN,1060) MEMBER(1:LM)
            CALL HBUG(CHWARN,'PCNEXT',IDN)
            IF (IEND .EQ. 0) GOTO 10
            GOTO 20
         ENDIF
         NDIM = IQ(LCID+ZNDIM)
         NROW = IQ(LCID+ZNOENT)
      ENDIF
*
      CFILE = MEMBER(1:LM)
*
*-- set the total number of rows for IDN in chain
*
   20 IF (IEND .EQ. 1) THEN
         NCHEVT = PCHEVT(CHPATH, LENOCC(CHPATH), IDN, NCHROW, 1)
      ENDIF
*
1000  FORMAT('+Cannot open file ',A)
1010  FORMAT('+Cannot change directory to ',A,' in ',A)
1020  FORMAT('+Cannot find Ntuple in ',A)
1030  FORMAT('+Not an Ntuple in ',A)
1040  FORMAT('+Cannot read Ntuple header from ',A)
1050  FORMAT('+Variable not found in Ntuple in ',A)
1060  FORMAT('+Bad Ntuple header (try RECOVER) in ',A)
*
      END
