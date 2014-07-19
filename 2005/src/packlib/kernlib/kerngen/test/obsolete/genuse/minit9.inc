*
* $Id: minit9.inc,v 1.1.1.1 1996/02/15 17:55:05 mclareni Exp $
*
* $Log: minit9.inc,v $
* Revision 1.1.1.1  1996/02/15 17:55:05  mclareni
* Kernlib
*
*
*
* minit9.inc
*
      IF (LUNGU.EQ.0)  LUNGU= 11
      MODEGU= MODEG
      IF (IFLGU.GE.0)  MODEGU=MODEU
      WRITE (ITB,9007) IFLGU,MODEGU
 9007 FORMAT (1X/8H IFLGU = ,I3,2X,A3,5H-RUN.)
      REWIND LUNGU
