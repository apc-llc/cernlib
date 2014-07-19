*
* $Id: errcray.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: errcray.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_ERRCRAY_INC
#define CERNLIB_FATMEN_ERRCRAY_INC
*
*
* errcray.inc
*
*define  ECONNRESET     145   /* Connection reset by peer */
*define  ETIMEDOUT      151   /* Connection timed out */
*define  ECONNREFUSED   152   /* Connection refused */
      IFATRC(1)=145
      IFATRC(2)=151
      IFATRC(3)=152
      IFATRC(4)=3
      IRETRY(1)=60
      IRETRY(2)=5
      IRETRY(3)=5
      IRETRY(4)=60

#endif
