*
* $Id: hnbuf.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbuf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  15.14.26  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNBUF(IDD)
*.==========>
*.
*.           Buffer manager for new (variable row length) N-tuples.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine creates and finds the contents buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
*-- create LBUF structure and the contents banks for N-tuple IDD
*
      NDIM = IQ(LCID+ZNDIM)
      NWP  = IABS(IQ(LCID+ZNPRIM))
*
      IF (LQ(LCDIR-4) .EQ. 0) THEN
*
*-- enough space left?
*
         NTOT = NDIM+2+NDIM*NWP+(NDIM+1)*33
         CALL HSPACE(NTOT,'HNBUF',IDD)
         IF (IERR.NE.0) GOTO 70
         IDLAST = IDD
*
         CALL MZBOOK(IHDIV,LBUFM,LCDIR,-4,'HBUF',NDIM,NDIM,2,2,0)
         LBUF = LBUFM
         IQ(LBUF-5) = IDD
*
*-- Attention: first word needs to be zeroed
*
         DO 10 I = 1, NDIM
            CALL MZBOOK(IHDIV,L,LBUF,-I,'HCON',0,0,NWP,1,1)
   10    CONTINUE
      ELSEIF (IQ(LBUF-5) .NE. IDD) THEN
*
*-- find buffer bank for ntuple IDD
*
         LBUF = LQ(LCDIR-4)
   20    IF (IQ(LBUF-5) .EQ. IDD) GOTO 40
         IF (LQ(LBUF) .NE. 0) THEN
            LBUF = LQ(LBUF)
            GOTO 20
         ENDIF
*
*-- no buffer structure and contents banks for this ntuple; create them
*-- first check if there is enough space left?
*
         NTOT = NDIM+2+NDIM*NWP+(NDIM+1)*33
         CALL HSPACE(NTOT,'HNBUF',IDD)
         IF (IERR.NE.0) GOTO 70
         IDLAST = IDD
*
         CALL MZBOOK(IHDIV,LBUF,LBUF,0,'HBUF',NDIM,NDIM,2,2,0)
         IQ(LBUF-5) = IDD
*
*-- Attention: first word needs to be zeroed
*
         DO 30 I = 1, NDIM
            CALL MZBOOK(IHDIV,L,LBUF,-I,'HCON',0,0,NWP,1,1)
   30    CONTINUE
      ENDIF
*
*-- check if new variables (in new blocks) were defined since
*-- previous call
*
   40 IF (NDIM .GT. IQ(LBUF-2)) THEN
         LOLD  = IQ(LBUF-2)
         LNEED = NDIM - LOLD
*
*-- enough space?
*
         NTOT = LNEED+(NWP+33)*LNEED
         CALL HSPACE(NTOT,'HNBUF',IDD)
         IF (IERR.NE.0) GOTO 70
         IDLAST = IDD
*
         CALL MZPUSH(IHDIV, LBUF, LNEED, 0, 'I')
*
*-- Attention: first word needs to be zeroed
*
         DO 50 I = LOLD+1, NDIM
            CALL MZBOOK(IHDIV,L,LBUF,-I,'HCON',0,0,NWP,1,1)
   50    CONTINUE
      ENDIF
*
*-- check if the buffer size of the buffer corresponds with the buffer
*-- size in the N-tuple description. If not change the buffers accordingly
*
      L  = LQ(LBUF-1)
      NW = IQ(L-1)
      IF (NWP .NE. NW) THEN
         ND = NWP - NW
         DO 60 I = 1, NDIM
            CALL MZPUSH(IHDIV, LQ(LBUF-I), 0, ND, ' ')
   60    CONTINUE
      ENDIF
*
*-- create and set TMP buffers
*
      CALL HNTMP(IDD)
*
   70 RETURN
      END
 
