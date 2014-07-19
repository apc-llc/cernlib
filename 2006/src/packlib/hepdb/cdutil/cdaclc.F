*
* $Id: cdaclc.F,v 1.1.1.1 1996/02/28 16:24:45 mclareni Exp $
*
* $Log: cdaclc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDACLC(CHUSER,CHLIST,IRC)
 
      CHARACTER*(*) CHUSER,CHLIST
      CHARACTER*255 USER,LIST,TEMP
 
      IRC   = 0
*
*     Wild-card: anything goes
*
      IF(CHLIST.EQ.'*') GOTO 10
*
*     No list: access denied
*
      LLIST = LENOCC(CHLIST)
      IF(LLIST.EQ.0)    GOTO 20
 
      LIST  = CHLIST(1:LLIST)
      CALL CLTOU(LIST(1:LLIST))
 
      LUSER = LENOCC(CHUSER)
 
      USER  = CHUSER(1:LUSER)
      CALL CLTOU(USER(1:LUSER))
*
*     Single user
*
      LBLNK = INDEX(LIST(1:LLIST),' ')
      IF(LBLNK.EQ.0.AND.USER(1:LUSER).NE.LIST(1:LLIST)) GOTO 20
*
*     General case
*
      CALL CDNWRD(' ',LIST(1:LLIST),NWORDS)
 
      DO 30 N=0,NWORDS-1
         CALL CDWORD(TEMP,N,' ',LIST(1:LLIST),IRC)
         IF(TEMP(1:LENOCC(TEMP)).EQ.USER(1:LUSER)) GOTO 10
30    CONTINUE
      GOTO 20
 
10    IRC   = 1
 
20    END
