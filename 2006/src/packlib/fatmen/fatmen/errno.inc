*
* $Id: errno.inc,v 1.1.1.1 1996/03/07 15:17:53 mclareni Exp $
*
* $Log: errno.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:53  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_ERRNO_INC
#define CERNLIB_FATMEN_ERRNO_INC
*
*
* errno.inc
*
*
*     Extract from /usr/include/sys/errno.h
*             or   sys$library:errno.h
*
*define  ECONNRESET     54    /* Connection reset by peer */
*define  ETIMEDOUT      60    /* Connection timed out */
*define  ECONNREFUSED   61    /* Connection refused */
      IFATRC(1)=54
      IFATRC(2)=60
      IFATRC(3)=61
      IFATRC(4)=3
      IRETRY(1)=60
      IRETRY(2)=5
      IRETRY(3)=5
      IRETRY(4)=60

#endif
