*
* $Id: hrename.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrename.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 06/06/94  20.09.36  by  Fons Rademakers
*-- Author :    Rene Brun   26/05/94
      SUBROUTINE HRENAME(ID1,CHOLD,CHNEW)
*.==========>
*.           To rename column CHOLD of ntuple ID into CHNEW
*..=========> ( R. Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcntpar.inc"
*
      CHARACTER*(*) CHOLD, CHNEW
      CHARACTER*32  COLD, CNEW, CIVAR
      CHARACTER*8   TAGS, TOLD, TNEW, BLOCK
      INTEGER       HNBPTR, HNMPTR
      LOGICAL       NTOLD
*.___________________________________________
*
      ID     = ID1
      IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)THEN
         LCID=0
         CALL HBUG('Unknown Ntuple','HRENAME',ID)
         GO TO 99
      ENDIF
      IDLAST = ID1
      LCID   = LQ(LTAB-IDPOS)
      I4     = JBIT(IQ(LCID+KBITS),4)
      IF( I4.EQ.0 ) RETURN
*
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         NTOLD = .TRUE.
      ELSE
         NTOLD = .FALSE.
      ENDIF
*
      IF (NTOLD) THEN
         NVAR  = IQ(LCID+2)
         ITAG1 = IQ(LCID+10)
      ELSE
         NVAR  = IQ(LCID+ZNDIM)
         ITIT1 = IQ(LCID+ZITIT1)
      ENDIF
*
* Loop over all ntuple column names until we find CHOLD
*
      IF (NTOLD) THEN
         TOLD=CHOLD
         TNEW=CHNEW
         DO 10 I = 1, NVAR
            CALL UHTOC( IQ(LCID+ITAG1+2*(I-1)), 4, TAGS, 8 )
            IF(TAGS.EQ.TOLD)THEN
               CALL UCTOH(TNEW,IQ(LCID+ITAG1+2*(I-1)),4,8)
               GO TO 99
            ENDIF
  10     CONTINUE
      ELSE
         COLD = CHOLD
         CNEW = CHNEW
         CALL HVXIST(COLD, BLOCK, CIVAR, IT, IS, IE)
         IF (IE .NE. 0) THEN
            LBLOK = HNBPTR(BLOCK)
            LNAME = LQ(LBLOK-1)
            IOFF  = HNMPTR(COLD)
*
*-- make sure there is enough space in the LCHAR bank
*
            LL   = LENOCC(CNEW)
            JMAX = IQ(LCID+ZIFCHA)-1 + LL
            IF (JMAX .GT. IQ(LCHAR-1)) THEN
               CALL MZPUSH(IHDIV, LCHAR, 0, LL, 'I')
            ENDIF
*
*-- in ZLNAME: the length of the new variable name
*
            IQ(LNAME+IOFF+ZLNAME) = LL
*
*-- in ZNAME: the pointer to the new name in LCHAR
*--
*-- NOTE: the space of the old name is not recuperated since that
*-- would involve a pass over all variables to update all pointers
*-- to the LCHAR bank. This is not a problem since HRENAME will not
*-- be called that often.
*
            I = IQ(LCID+ZIFCHA)
            IQ(LNAME+IOFF+ZNAME) = I
            CALL UCTOH(CNEW, IQ(LCHAR+I), 4, LL)
            IQ(LCID+ZIFCHA) = IQ(LCID+ZIFCHA) + (LL+3)/4
*
*-- Ntuple structure has been changed.
*
            LBLOK = LQ(LCID-1)
            CALL SBIT1(IQ(LBLOK),1)
         ENDIF
      ENDIF
*
  99  END
