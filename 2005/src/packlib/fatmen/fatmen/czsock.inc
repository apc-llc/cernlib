*
* $Id: czsock.inc,v 1.1.1.1 1996/03/07 15:17:53 mclareni Exp $
*
* $Log: czsock.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:53  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_CZSOCK_INC
#define CERNLIB_FATMEN_CZSOCK_INC
*
*
* czsock.inc
*
      COMMON/CZSOCK/LUNCZ,IADTCP,LBUF,ISKIN,ISKOUT,IPROT,
     +              JUNIT(99),JSKIN(99),JSKOUT(99),CUNODE,
     +              CHNODE(99),CHUSER(99)
      CHARACTER*12  CHNODE,CUNODE,CHUSER
      PARAMETER (LNODE=12)
      PARAMETER (LUSER=12)
      PARAMETER (MAXSOC=99)
*
*     IPROT: 0 - TCP/IP (TCPAW)
*            1 - DECnet (included in CZ)
*     CHNODE & CHUSER store the node and user names that correspond
*     to JUNIT
*

#endif
