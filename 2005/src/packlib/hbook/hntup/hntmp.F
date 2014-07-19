*
* $Id: hntmp.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntmp.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  15.15.22  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNTMP(IDD)
*.==========>
*.
*.           Temporary buffers for new (variable row length) N-tuples.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine creates and finds the tmp buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      NDIM = IQ(LCID+ZNDIM)
      NW   = 1 + ZNTMP*NDIM
*
*-- create LTMP structure for N-tuple IDD
*
      IF (LQ(LCDIR-5) .EQ. 0) THEN
*
*-- enough space left?
*
         NW1  = 1 + ZNTMP1*NDIM
         NTOT = NW + NW1 + NDIM + 2*33
         CALL HSPACE(NTOT,'HNTMP',IDD)
         IF (IERR.NE.0) GOTO 70
         IDLAST = IDD
*
         CALL MZBOOK(IHDIV,LTMPM,LCDIR,-5,'HTMP',2,1,NW,2,1)
         LTMP = LTMPM
         IQ(LTMP-5) = IDD
         CALL MZBOOK(IHDIV,LTMP1,LTMP,-1,'HTMP1',NDIM,0,NW1,2,-1)
*
      ELSEIF (IQ(LTMP-5) .NE. IDD) THEN
*
*-- find tmp bank for ntuple IDD
*
         LTMP = LQ(LCDIR-5)
   20    IF (IQ(LTMP-5) .EQ. IDD) GOTO 40
         IF (LQ(LTMP) .NE. 0) THEN
            LTMP = LQ(LTMP)
            GOTO 20
         ENDIF
*
*-- no tmp structure for this ntuple; create them
*-- first check if there is enough space left?
*
         NW1  = 1 + ZNTMP1*NDIM
         NTOT = NW + NW1 + NDIM + 2*33
         CALL HSPACE(NTOT,'HNTMP',IDD)
         IF (IERR.NE.0) GOTO 70
         IDLAST = IDD
*
         CALL MZBOOK(IHDIV,LTMP,LTMP,0,'HTMP',2,1,NW,2,1)
         IQ(LTMP-5) = IDD
         CALL MZBOOK(IHDIV,LTMP1,LTMP,-1,'HTMP1',NDIM,0,NW1,2,-1)
      ENDIF
*
*-- check if new variables were defined since previous call
*
   40 LTMP1 = LQ(LTMP-1)
      LQ(LTMP-2) = LCID
      NWP   = IQ(LTMP-1)
      IF (NWP .NE. NW) THEN
         ND = NW - NWP
         CALL MZPUSH(IHDIV, LTMP, 0, ND, 'I')
         NWP = IQ(LTMP1-1)
         ND  = 1+ZNTMP1*NDIM - NWP
         NLP = IQ(LTMP1-3)
         NL  = NDIM - NLP
         CALL MZPUSH(IHDIV, LTMP1, NL, ND, 'I')
      ENDIF
*
   70 RETURN
      END
 
