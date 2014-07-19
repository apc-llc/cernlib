*
* $Id: errsgi.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: errsgi.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_ERRSGI_INC
#define CERNLIB_FATMEN_ERRSGI_INC
*
*
* errsgi.inc
*
*define  ECONNRESET     120   /* Connection reset by peer */
*define  ETIMEDOUT      126   /* Connection timed out */
*define  ECONNREFUSED   127   /* Connection refused */
      IFATRC(1)=120
      IFATRC(2)=126
      IFATRC(3)=127
      IFATRC(4)=3
      IRETRY(1)=60
      IRETRY(2)=5
      IRETRY(3)=5
      IRETRY(4)=60

#endif
