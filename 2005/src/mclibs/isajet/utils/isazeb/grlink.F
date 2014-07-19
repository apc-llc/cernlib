*
* $Id: grlink.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: grlink.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GRLINK (USER,NLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Reserve a location in the ZLINKA common (general link area)
C-   GRLINK will bomb if no more free room
C-
C-   Outputs :
C-   NLINK = reserved structural link location
C-
C-   ENTRY RRLINK: release a reserved structural link location
C-                 returns false if NLINK was not reserved
C-                 will not release the link if USER does not match
C-
C-   Input:
C-   USER  = user identifier (character*8)
C-   NLINK = location to release
C-
C-   Created   9-SEP-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zlinka.inc"
      INTEGER NRL(NRLINK),I,II,NLINK,K
      CHARACTER*8 USER,USERS(NRLINK)
      CHARACTER*80 MSG
      DATA NRL/NRLINK*0/
C----------------------------------------------------------------------
C
      DO 1 I=1,NRLINK
      II=I
      IF (NRL(I) .EQ. 0) GO TO 3
    1 CONTINUE
C
C           no more room
C
      CALL INTMSG(
     $' No more structural links available, list of users follows')
      DO 2 I=1,NRLINK,8
        MSG(1:10)=USERS(I)//'  '
        DO 22 K=1,7
   22   MSG=MSG(1:K*10)//USERS(I+K)//'  '
        CALL INTMSG(MSG)
    2 CONTINUE
      CALL ABORT()
C
C       keep track of reserved link
C
    3 NLINK=II
      NRL(II)=II
      USERS(II)=USER
      RETURN
C
C----------------------------------------------------------------------
      ENTRY RRLINK(USER,NLINK)
C
C        release link if called by owner
      IF(USER.EQ.USERS(NLINK)) NRL(NLINK)=0
C
  999 RETURN
      END
