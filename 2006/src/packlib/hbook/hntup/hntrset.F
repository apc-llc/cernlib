*
* $Id: hntrset.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntrset.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/06 27/06/94  18.44.21  by  Rene Brun
*-- Author :    Fons Rademakers   23/06/94
      SUBROUTINE HNTRSET(ID1, CHTITL)
*.==========>
*.
*.           Duplicate definition of CWN.
*.
*.   Reset the header of a CWN. Assumes LCID is correctly set (called
*.   from HRESET).
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
#include "hbook/hntcur.inc"
*
      CHARACTER*(*)  CHTITL
      CHARACTER*80   CHRZ
      LOGICAL        MEMORY
*
* Reset Table A (see HBNT)
*
      IQ(LCID+ZNOENT) = 0
      IQ(LCID+ZID)    = ID1
      IQ(LCID+ZIFTMP) = 2
*
      MEMORY = .FALSE.
      IF (IQ(LCID+ZNPRIM) .LT. 0) MEMORY = .TRUE.
*
      CALL HCDIR(CHRZ, 'R')
      IF (CHRZ(1:6) .EQ. '//PAWC') THEN
         IQ(LCID+ZNPRIM) = -IABS(IQ(LCID+ZNPRIM))
         IQ(LCID+ZNCHRZ) = 0
         CHRZ = ' '
      ELSE
         IQ(LCID+ZNCHRZ) = LENOCC(CHRZ)
      ENDIF
      CALL UCTOH(CHRZ,IQ(LCID+ZNCHRZ+1),4,80)
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
      IF (NWTITL .NE. 0) THEN
         NMORE = NWTITL - IQ(LCID+ZNWTIT)
         IF(NMORE.NE.0)THEN
            CALL MZPUSH(IHDIV,LCID,0,NMORE,' ')
         ENDIF
         ITIT1 = IQ(LCID+ZITIT1)
         CALL UCTOH(CHTITL,IQ(LCID+ITIT1),4,NCHT)
      ENDIF
      IQ(LCID+ZNWTIT) = NWTITL
*
* For memory resident Ntuples drop all content buffers
*
      IF (MEMORY) CALL HNBUFD(ID1)
*
* Reset table B for each block (see HBNT)
*
      LBLOK = LQ(LCID-1)
*
10    IQ(LBLOK+ZNOENT) = 0
*
* Reset all reference links that were pointing to the dropped content buffers.
* For disk resident Ntuples the content buffers were not dropped so no
* need to reset the reference links.
*
      IF (MEMORY) THEN
         LNAME = LQ(LBLOK-1)
         NDIM  = IQ(LBLOK+ZNDIM)
* Only if reference links were allocated (done in HFNT2) (one link to many
* is allocated, therefore the .GE. test)
         IF (IQ(LNAME-3) .GE. NDIM) THEN
            DO 20 I = 1, NDIM
               LQ(LNAME-I) = 0
20          CONTINUE
         ENDIF
      ENDIF
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
* Reset table C for each block (see HBNT)
*
      CALL HNMSET(ID1,ZNRZB,1)
      CALL HNMSET(ID1,ZIBANK,1)
      CALL HNMSET(ID1,ZITMP,0)
      CALL HNMSET(ID1,ZIFCON,2)
      CALL HNMSET(ID1,ZIFBIT,1)
*
* Reset misc
*
      NTCUR = 0
      LBLOK = LQ(LCID-1)
      CALL SBIT1(IQ(LBLOK),1)
*
      END
