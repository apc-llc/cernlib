*
* $Id: hrecov.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hrecov.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.19.39  by  Rene Brun
*-- Author :    Rene Brun   13/05/93
      SUBROUTINE HRECOV(ID1,CHOPT)
*.==========>
*.           Recovery routine for Ntuples.
*.           HRECOV attempts to read the latest saved header. From the header
*.           information, it looks at all Ntuple extensions to reconstruct
*.           the maximum number of blocks/events.
*.           If successful, the new header is written to the file.
*..=========> ( R.Brun )
*
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcdire.inc"
#include "hbook/hntcur.inc"
#include "hbook/hcntpar.inc"
#include "hbook/hcrecv.inc"
*
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X(522)
      COMMON/QUEST/IQUEST(100)
*
      PARAMETER    (MXSTEP = 16384)
      CHARACTER*(*) CHOPT
      CHARACTER*8   BLOCK
      INTEGER       HNBMIN
*.___________________________________________
*
*-*  Get highest cycle of header
*
      CALL HRIN(ID1,99999,0)
      IF(LCID.LE.0)GO TO 999
      LCIDN=LCID
      CALL RZINK(1,9999,'SC')
      NKEYS=IQUEST(7)
      NWKEY=IQUEST(8)
      IF(NWKEY.NE.2)THEN
         CALL HBUG('Cannot recover old HBOOK files','HRECOV',ID1)
         GO TO 999
      ENDIF
      CALL HUOPTC(CHOPT,'F',IOPTF)
*
*-*  COLUMN-Wise Ntuple case
*
      IF (IQ(LCID-2) .EQ. ZLINK) THEN
*
         IF (IQ(LCID+ZNOENT) .NE. -1) THEN
            CALL HBUG('Ntuple looks healthy, nothing to recover',
     +                'HRECOV',ID1)
            GOTO 999
         ENDIF
*
*-- turn recovery flag on
*
         NRECOV = .TRUE.
*
*-- loop over all blocks
*
         LBLOK = LQ(LCID-1)
         LCHAR = LQ(LCID-2)
         LINT  = LQ(LCID-3)
         LREAL = LQ(LCID-4)
*
   10    IBEVT  = 0
         ISTEP  = MXSTEP
         IFIRST = 0
         CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLOCK, 8)
*
*-- save current block pointer and set all receiving addresses to 0
*
         LR2 = LBLOK
         CALL HNMSET(ID1, ZNADDR, 0)
*
*-- we have to read index variables, so set receiving addresses for them
*
         LBLOK = LR2
         CALL HRECO1(ID1)
*
*-- create or find buffer manager structure for ID1
*
         CALL HNBUFR(ID1)
         IF (IERR .NE. 0) GOTO 40
         NTCUR = ID1
*
         LBLOK = LR2
*
*-- use binary search to find contents for each block
*
   20    IF (IFIRST .EQ. 0) THEN
            IQ(LTMP1+1) = 0
            CALL HGNT2(' ', 0, 0, IBEVT+ISTEP, IERROR)
            IF (IERROR .EQ. 0) THEN
               IQ(LTMP+1) = IBEVT+ISTEP
            ELSE
               IQ(LTMP+1) = 0
            ENDIF
            IFIRST = 1
         ELSE
            CALL HGNTF(ID1, IBEVT+ISTEP, IERROR)
         ENDIF
         IF (IERROR .NE. 0) GOTO 25
*
         CALL HGNTF(ID1, IBEVT+ISTEP+1, IERRO1)
*
   25    IF (IERROR .NE. 0) THEN
            IF (ISTEP .EQ. 0) THEN
               WRITE(LOUT,10300)
               RETURN
            ENDIF
            ISTEP = ISTEP/2
         ELSEIF (IERRO1 .EQ. 0) THEN
            IBEVT = IBEVT + ISTEP
         ELSE
            CALL HGNTF(ID1, IBEVT+ISTEP, IERROR)
*
*-- update the LNAME structure for this block and set the block event counter
*
            CALL HRECO2
            IQ(LBLOK+ZNOENT) = IBEVT+ISTEP
*
            GOTO 30
*
         ENDIF
*
         GOTO 20
*
*-- next block
*
   30    LBLOK = LQ(LBLOK)
         WRITE(LOUT,10100) IBEVT+ISTEP, BLOCK
         IF (LBLOK .NE. 0) GOTO 10
*
*-- after looping over all blocks set global event counter to the
*-- minimum of all block counters
*
         IQ(LCID+ZNOENT) = HNBMIN()
         WRITE(LOUT,10200) IQ(LCID+ZNOENT)
*
*-- Ntuple structure has been changed
*
         LBLOK = LQ(LCID-1)
         CALL SBIT1(IQ(LBLOK),1)
*
         NRECOV = .FALSE.
         GOTO 90
*
   40    NRECOV = .FALSE.
         CALL HBUG('Cannot recover Ntuple','HRECOV',ID1)
         GOTO 999
*
      ELSE
*
*-*  ROW-Wise Ntuple case
*
*-* Find highest extension number
*
         MAXEXT=0
         DO 50 I=1,NKEYS
            CALL RZINK(I,1,'SC')
            IDN=IQUEST(21)
            IEXT=IQUEST(22)
            IF(IDN.EQ.ID1.AND.IEXT.GT.MAXEXT)MAXEXT=IEXT
   50    CONTINUE
         IF(MAXEXT.LE.IQ(LCID+6))THEN
            CALL HBUG('Cannot recover more than current header',
     +                 'HRECOV',ID1)
            GO TO 999
         ENDIF
         NDIM=IQ(LCID+2)
         NPRIME=IQ(LCID+4)
         NOENT=MAXEXT*(NPRIME/NDIM)
         IQ(LCID+3)=NOENT
         IQ(LCID+6)=MAXEXT
         IQ(LCID+7)=NPRIME+1
*
*-* Rebuild the LCONT and LIMS structures
*
         LQ(LCDIR-9)=LCID
         IF(LQ(LCID-1).EQ.0)THEN
            CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCON',0,0,NPRIME,3,0)
            IQ(LCID)=0
         ENDIF
         LQ(LCID-3)=LCONT
         LLIMS=LQ(LCID-2)
         IF(LLIMS.LE.0)THEN
            CALL MZBOOK(IHDIV,LLIMS,LCID,-2,'HLIM',0,0,2*NDIM,3,0)
         ENDIF
         DO 60 I=1,NDIM
            Q(LLIMS+2*I-1)=1.E31
            Q(LLIMS+2*I  )=-1.E31
   60    CONTINUE
*
*-*  If not option option 'F' recompute MIN/MAX info
*
         IF(IOPTF.EQ.0)THEN
            DO 80 IDNEVT=1,NOENT
               CALL HGNF(ID1,IDNEVT,X,IERROR)
               DO 70 I=1,NDIM
                  IF(X(I).LT.Q(LLIMS+2*I-1))Q(LLIMS+2*I-1)=X(I)
                  IF(X(I).GT.Q(LLIMS+2*I  ))Q(LLIMS+2*I  )=X(I)
   70          CONTINUE
   80       CONTINUE
         ENDIF
*
         WRITE(LOUT,10000) NOENT
*
      ENDIF
*
*-*   Now write new header to file
*
   90 CALL HROUT(ID1,ICYCLE,' ')
*
10000 FORMAT(' HRECOV:',I8,' entries correctly recovered')
10100 FORMAT(' HRECOV:',I8,' entries in block ',A,
     +       ' correctly recovered')
10200 FORMAT(' HRECOV:',I8,' complete entries correctly recovered')
10300 FORMAT(' HRECOV: cannot recover,',
     +       ' something is very seriously wrong')
*
  999 END
