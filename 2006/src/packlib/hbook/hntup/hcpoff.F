*
* $Id: hcpoff.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hcpoff.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 27/07/94  14.26.12  by  Fons Rademakers
*-- Author :    Fons Rademakers   22/07/94
      SUBROUTINE HCPOFF(IDD,VAR1,NVAR,IVOFF,IER)
*.==========>
*.
*.           Copy in array IVOFF the current offsets in the dynamic
*.           memory buffers.
*.           In case of error IER <> 0.
*.           This routine is used by the chain mechanism in PAW.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hntcur.inc"
*
      CHARACTER*(*)  VAR1(*)
      CHARACTER*32   VAR
      INTEGER        IVOFF(*), HNMPTR
*
      IER = 0
*
      IF (NVAR .EQ. 0) RETURN
*
*-- Minimal test for maximum speed. Just set the pointers to the
*-- buffer and tmp structures. The N-tuple header must already be in memory.
*-- No reading from disk will be done.
*
      IF (NTCUR.NE.IDD .OR. NTCUR.EQ.0) THEN
         CALL HNBUFF(IDD, .FALSE.)
         IF (IERR .NE. 0) RETURN
         IF (IQ(LTMP1+1) .EQ. 0) RETURN
         NTCUR = IDD
      ENDIF
*
      IDLAST = IDD
*
*-- basic test
*
      IF (NVAR .NE. IQ(LTMP1+1)) THEN
         IER = 1
         RETURN
      ENDIF
*
      LCID = LQ(LTMP-2)
*
      LBLOK  = LQ(LCID-1)
      LCHAR  = LQ(LCID-2)
      LINT   = LQ(LCID-3)
      LREAL  = LQ(LCID-4)
*
*-- simulate the loop HGNT1 makes over HGNT2 to get the offsets back in
*-- the right order
*
      IVAR = 0
*
   10 LNAME  = LQ(LBLOK-1)
*
      DO 20 I = 1, NVAR
*
         VAR  = VAR1(I)
         IOFF = HNMPTR(VAR)
         IF (IOFF .LT. 0) GOTO 20
*
         IVAR = IVAR + 1
         JTMP = ZNTMP1*(IVAR-1) + 2
         IVOFF(I) = IQ(LTMP1+JTMP+2)
*
   20 CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
*-- another sanity check
*
      IF (IVAR .NE. NVAR) IER = 2
*
      END
