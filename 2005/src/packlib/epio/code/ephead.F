*
* $Id: ephead.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: ephead.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPHEAD(IBUF)
C. ROUTINE TO PRESET A PHYSICAL HEADER FOR AN OUTPUT UNIT, PLUS THE
C. OUTPUT UNIT FLAG CONTROL WORD.
C. INPUT/OUTPUT
C. IBUF      UNIT BUFFER
C. FORMAT OF THE HEADER
C.   1       PHYS. BLOCK LENGTH (IN 16 BIT WORDS)
C.   2       PHYS. BLOCK HEADER LENGTH (IN 16 BIT WORDS)
C.   3       PHYS. BLOCK NUMBER
C.   4       POINTER TO FIRST LOG. RECORD START, OR ZERO IF NONE
C.   5       RUN NUMBER
C.   6       PHYS. BLOCK TYPE
C.   7       CONTAINS  29954 if 16 bit word p.h., 522144444 (hex 1F1F4ABC)
C.           if 32 bit p.h.
C.   8       CONTAINS  31280 if 16 word p.h., else as word 7
C.   9       NUMBER OF HEADERLESS BLOCKS FOLLOWING
C.  10       TAPE FORMAT VERSION NUMBER = 8012
C.  11       LOGICAL RECORD WORD LENGTH IN BITS (16 AND 32 SUPPORTED)
C.  12       LENGTH OF STANDARD PHYS. HEADER  (in 16 bit words !)
#include "epio/epiocom.inc"
      DIMENSION IBUF(20)
      K=LIST(6)
      IF(LIST(ISTART+29).EQ.0)  THEN
*--- 16 bit p.h.
         NSL=LIST(6)
         IBUF(K+1)=NSL
         IBUF(K+2)=NSL
         IBUF(K+3)=0
         IBUF(K+4)=0
         IBUF(K+5)=LIST(ISTART+4)
         IBUF(K+6)=LIST(ISTART+9)
         IBUF(K+7)=29954
         IBUF(K+8)=31280
         IBUF(K+9)=0
         IBUF(K+10)=8012
         IBUF(K+11)=LIST(ISTART+3)
         IBUF(K+12)=NSL
C--- BUNCH INTO HEADER PORTION
         CALL BUN16W(IBUF,K+1,IBUF,1,K)
      ELSE
*--- 32 bit p.h.
         NSL=2*LIST(6)
         IBUF(K+1)=NSL
         IBUF(K+2)=NSL
         IBUF(K+3)=0
         IBUF(K+4)=0
         IBUF(K+5)=LIST(ISTART+4)
         IBUF(K+6)=LIST(ISTART+9)
         IBUF(K+7)=522144444
         IBUF(K+8)=IBUF(K+7)
         IBUF(K+9)=0
         IBUF(K+10)=8012
         IBUF(K+11)=LIST(ISTART+3)
         IBUF(K+12)=NSL
C--- BUNCH INTO HEADER PORTION
         CALL CTOIBM(IBUF(K+1),K,2)
         CALL BUN32W(IBUF,K+1,IBUF,1,K)
      ENDIF
      IF(LIST(ISTART+7).EQ.0)  LIST(ISTART+7)=NSL
      LIST(ISTART+14)=NSL
C--- OUTPUT UNIT FLAG
      IERR=0
#if defined(CERNLIB_VAX)
      IF(LIST(ISTART+16).EQ.0)CALL EPOPEN_VAX(1,IERR)
#endif
#if defined(CERNLIB_APOLLO)
      IF(LIST(ISTART+16).EQ.0) CALL EPOPEN_APOLLO(1)
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      IF(LIST(ISTART+16).EQ.0) CALL EPOPEN(1,IERR)
#endif
        if(IERR.ne.0)CALL EPERRH(LIST(ISTART+10),IERR)
      LIST(ISTART+16)=1
      RETURN
      END
