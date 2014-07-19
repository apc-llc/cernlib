*
* $Id: farstg.inc,v 1.1.1.1 1996/03/07 15:17:53 mclareni Exp $
*
* $Log: farstg.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:53  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FARSTG_INC
#define CERNLIB_FATMEN_FARSTG_INC
*
*
* farstg.inc
*
*
*     Wait time for remote job:
*        IWTNET - time to wait if network is down
*        IWTACK - time for acknowledgement
*        IWTPND - time when entry is pending
*        IWTEXE - time when entry is executing
*
      COMMON/FARSTG/IWTNET,IWTACK,IWTPND,IWTEXE,MAXNET,MAXACK

#endif
