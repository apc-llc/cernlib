*
* $Id: gslink.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gslink.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GSLINK (USER,NLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Reserve a location in the ZLINKA common (general link area)
C-   GSLINK will bomb if no more free room
C-
C-   Outputs :
C-   NLINK = reserved structural link location
C-
C-   ENTRY RSLINK: release a reserved structural link location
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
#include "itapes.inc"
#include "zlinka.inc"
      INTEGER NSL(NSLINK),I,II,NLINK,K
      CHARACTER*80 MSG
      CHARACTER*8 USER,USERS(NSLINK)
      DATA NSL/NSLINK*0/
C
      DO 1 I=1,NSLINK
      II=I
      IF (NSL(I) .EQ. 0) GO TO 3
    1 CONTINUE
C
C           no more room
C
      WRITE(ITLIS,11)
   11 FORMAT('No more structural links available, list of users',
     &' follows.')
      DO 2 I=1,NSLINK,8
        MSG(1:10)=USERS(I)//'  '
        DO 22 K=1,7
   22   MSG=MSG(1:K*10)//USERS(I+K)//'  '
        WRITE(ITLIS,12) MSG
   12   FORMAT(A80)
    2 CONTINUE
C
C       keep track of reserved link
C
      STOP
    3 NLINK=II
      NSL(II)=II
      USERS(II)=USER
      RETURN
C
C----------------------------------------------------------------------
      ENTRY RSLINK(USER,NLINK)
C
C        release link if called by owner
      IF(USER.EQ.USERS(NLINK)) NSL(NLINK)=0
C
  999 RETURN
      END
