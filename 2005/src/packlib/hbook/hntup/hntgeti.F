*
* $Id: hntgeti.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntgeti.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 23/01/94  18.26.37  by  Fons Rademakers
*-- Author :    Fons Rademakers   23/01/94
      SUBROUTINE HNTGETI(ID1,ICOM,INDX,TVAR,ITYPE,ISIZE,IELEM,IER)
*.==========>
*.
*.           Depending on the common (PAWCR8, PAWCR4, PAWC32) and the
*.           array index in this common this routine returns the variable
*.           VAR in Ntuple ID and the type ITYPE and the size ISIZE
*.           (in bytes) of the variable. IELEM returns the number of words
*.           taken by the variable. IELEM=1 for a scalar and >1 for an array.
*.           If index is not legal for this common and Ntuple then IER<>0.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcntpaw.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR
      CHARACTER*8   BLOCK
      CHARACTER*32  VAR
      INTEGER       INDCR4, INDCR8, INDC32
*
      ID    = ID1
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown Ntuple','HNTGETI',ID1)
         IER = 1
         RETURN
      ENDIF
      LCID  = LQ(LTAB-IDPOS)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) THEN
         CALL HBUG('Not an Ntuple','HNTGETI',ID)
         IER = 1
         RETURN
      ENDIF
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         CALL HBUG('Old Ntuple, cannot use HNTGETI','HNTGETI',ID)
         IER = 1
         RETURN
      ENDIF
*
      IF (ICOM.NE.4 .AND. ICOM.NE.8 .AND. ICOM.NE.32) THEN
         CALL HBUG('Illegal value for ICOM','HNTGETI',ID)
         IER = 1
         RETURN
      ENDIF
*
      IER    = 0
*
      INDCR4 = 1
      INDCR8 = 1
      INDC32 = 1
*
      NDIM = IQ(LCID+ZNDIM)
*
*-- Loop over all variables till we find the index we want.
*
*-- Increment in the meanwhile the INDC.. counters in the same way as we
*-- do in HWPDCL (so there is a correct match between the COMMON
*-- statements in the routine produced by HUWFUN and the actual COMMON blocks
*-- in which the variables are returned
*
      DO 10 I = 1, NDIM
*
         CALL HNTVAR(ID, I, VAR, BLOCK, NSUB, ITYPE, ISIZE, IELEM)
         CALL CLTOU(VAR)
*
         IF (ITYPE .NE. 5) THEN
            IF (ISIZE .EQ. 4) THEN
               IF (ICOM .NE. 4) GOTO 10
               IF (INDX .EQ. INDCR4) THEN
                  TVAR = VAR
                  RETURN
               ELSE
                  INDCR4 = INDCR4 + IELEM
               ENDIF
            ELSEIF (ISIZE .EQ. 8) THEN
               IF (ICOM .NE. 8) GOTO 10
               IF (INDX .EQ. INDCR8) THEN
                  TVAR = VAR
                  RETURN
               ELSE
                  INDCR8 = INDCR8 + IELEM
               ENDIF
            ENDIF
         ELSE
            IF (ICOM .NE. 32) GOTO 10
            IF (INDX .EQ. INDC32) THEN
               TVAR =VAR
               RETURN
            ELSE
               INDC32 = INDC32 + IELEM
            ENDIF
         ENDIF
*
  10  CONTINUE
*
      IER = 2
*
      END
