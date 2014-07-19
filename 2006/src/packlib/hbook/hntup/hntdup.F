*
* $Id: hntdup.F,v 1.3 1997/02/21 12:15:48 couet Exp $
*
* $Log: hntdup.F,v $
* Revision 1.3  1997/02/21 12:15:48  couet
* -previous commit was wrong
*
* Revision 1.2  1997/02/21 12:12:48  couet
* *** empty log message ***
*
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 09/01/95  13.00.35  by  Fons Rademakers
*-- Author :    Fons Rademakers   31/05/94
      SUBROUTINE HNTDUP(ID1, ID2, NEWBUF, CHTITL, CHOPT1)
*.==========>
*.
*.           Duplicate definition of an Ntuple. A new Ntuple ID2 is created
*.           with the same definitions as ID1, but with 0 entries.
*.
*.   ID1 is the original id, ID2 the new
*.   NEWBUF <0 use buffer size of ID1
*.   NEWBUF =0 use current buffer size(10000 for RWN's)
*.   NEWBUF >0 use NEWBUF as buffer size
*.   CHTITL the new title, when blank use original,
*.   CHOPT1=' ' use original value (disk or memory resident)
*.   CHOPT1='M' to make the Ntuple memory resident
*.   CHOPT1='A' interactive mode: Sets addresses to /PAWCR4/
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcset.inc"
*
      INTEGER       ID1, ID2, MXVAR,NEWBUF
      PARAMETER    (MXVAR = 512)
      REAL          RLOW(MXVAR), RHIGH(MXVAR)
      REAL*8        IDUM
      CHARACTER*(*) CHTITL, CHOPT1
      CHARACTER*4   CDUM
      CHARACTER*8   CHOPT,CHOPT2, BLOCK, TAGS(MXVAR)
      CHARACTER*80  CHTIT, CHRZ
      LOGICAL       MEMORY, NTOLD
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPT(1),IOPTA),(IOPT(2),IOPTM)
*
*-- check if ID1 already in table, if not it should be read in first
*
      CHOPT2=CHOPT1
      CALL HUOPTC(CHOPT2,'AM',IOPT)
      CALL HFIND(ID1,'HNTDUP')
      IF(LCID.EQ.0)RETURN
      ID=ID1
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) RETURN
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         NTOLD = .TRUE.
      ELSE
         NTOLD = .FALSE.
      ENDIF
*
*-- Ntuple will be created in the current directory
*
      CALL HCDIR(CHRZ, 'R')
*
*-- Handle RWN's
*
      IF (NTOLD) THEN
*
         NCHRZ  = IQ(LCID+11)
         NWPRIM = IQ(LCID+4)
         MEMORY = .FALSE.
         IF (NCHRZ .EQ. 0) MEMORY = .TRUE.
*
*-- get type (disk or memory resident)
*
         IF (IOPTM.EQ.0) THEN
            IF (MEMORY) CHRZ = ' '
         ELSE
            CHRZ = ' '
         ENDIF
*
*-- set primary allocation
*
         IF (NEWBUF.EQ.0) NWPRIM = 10000
         IF (NEWBUF.GT.0) NWPRIM = NEWBUF
*
*-- get title and column definition
*
         NVAR = MXVAR
         CALL HGIVEN(ID1, CHTIT, NVAR, TAGS, RLOW, RHIGH)
         IF (CHTITL .NE. ' ') CHTIT = CHTITL
*
*-- define RWN
*
         CALL HBOOKN(ID2, CHTIT, NVAR, CHRZ, NWPRIM, TAGS)
*
         RETURN
      ENDIF
*
*-- Handle CWN's
*
*-- Duplicate disk resident Ntuple may not have the same ID (without offset)
*-- as the original when in the same directory. A memory resident Ntuple
*-- may not have the same ID (since we can't control where it will be written
*-- and we want to prevent the user from overwriting the original).
*
      NDIM   = IQ(LCID+ZNDIM)
      NCHRZ  = IQ(LCID+ZNCHRZ)
      NWPRIM = IQ(LCID+ZNPRIM)
      IF (NCHRZ.NE.0 .AND. NWPRIM.GT.0) THEN
         MEMORY = .FALSE.
         CHTIT  = ' '
         CALL UHTOC(IQ(LCID+ZNCHRZ+1), 4, CHTIT, NCHRZ)
         IF (CHTIT.EQ.CHRZ .AND. IQ(LCID+ZID).EQ.ID2) THEN
            CALL HBUG('Duplicate can not have given ID','HNTDUP',ID2)
            RETURN
         ENDIF
      ELSE
         MEMORY = .TRUE.
         IF (IQ(LCID+ZID) .EQ. ID2) THEN
            CALL HBUG('Duplicate can not have given ID','HNTDUP',ID2)
            RETURN
         ENDIF
      ENDIF
*
*-- get title if not overriden
*
      IF (CHTITL .EQ. ' ') THEN
         CHTIT = ' '
         ITIT1 = IQ(LCID+ZITIT1)
         NWTIT = IQ(LCID+ZNWTIT)
         CALL UHTOC(IQ(LCID+ITIT1), 4, CHTIT, 4*NWTIT)
      ELSE
         CHTIT = CHTITL
      ENDIF
*
*-- get type (disk or memory resident)
*
      IF (IOPTM.EQ.0) THEN
         IF (MEMORY) THEN
            CHOPT = 'M'
         ELSE
            CHOPT = 'D'
         ENDIF
      ELSE
         CHOPT = CHOPT1
      ENDIF
*
*-- Set the buffer size
*
      IBOLD=IBSIZE
      IF (NEWBUF.EQ.0) NWPRIM = IBSIZE
      IF (NEWBUF.GT.0) NWPRIM = NEWBUF
      IF (NEWBUF.GE.0) IBSIZE = NWPRIM
*
*-- create duplicate Ntuple header
*
      CALL HBNT(ID2, CHTIT, CHOPT)
      IBSIZE=IBOLD
*
*-- get pointer to new Ntuple
*
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID2)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown Ntuple','HNTDUP',ID2)
         RETURN
      ENDIF
      LR2 = LQ(LTAB-IDPOS)
*
*-- use the original buffer size
*
      IF (NEWBUF.LT.0) THEN
         IF (CHOPT .EQ. 'D') THEN
            IQ(LR2+ZNPRIM) = IABS(NWPRIM)
         ELSE
            IQ(LR2+ZNPRIM) = -IABS(NWPRIM)
         ENDIF
      ENDIF
*
*-- get definition of original columns and call HBNAME for each of them
*-- NOTE: addresses are dummy and need to be reset before using the Ntuple
*
      DO 10 I = 1, NDIM
         CALL HNTVDEF(ID1, I, CHTIT, BLOCK, ITYPE)
         IF (ITYPE .NE. 5) THEN
            CALL HBNAME(ID2, BLOCK, IDUM, CHTIT)
         ELSE
            CALL HBNAMC(ID2, BLOCK, CDUM, CHTIT)
         ENDIF
         IF(IOPTA.NE.0)THEN
            CALL HNTVAR(ID1, I, CHTIT, BLOCK, NSUB, IT, IS, IE)
            CALL HNTGET(ID1, CHTIT, INDD, IT, IS, IE, IERROR)
            CALL HNTGET(ID2, CHTIT, INDD, IT, IS, IE, IERROR)
         ENDIF
10    CONTINUE
*
*-- reset addresses
*
      IF(IOPTA.EQ.0)CALL HNMSET(ID2, ZNADDR, 0)
*
      END
