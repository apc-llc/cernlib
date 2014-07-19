*
* $Id: istrrd.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: istrrd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION ISTRRD(ISTR,N)
*
*   INPUT THE STRING FROM MAP-FILE, USING
*   CHANNEL NUMBER 1.
*   N - NUMBER OF CHARACTERS READ
*
      CHARACTER*(*) ISTR
      ISTRRD=0
      READ (1,FMT='(A)',IOSTAT=IE,END=1,ERR=2) ISTR
      N=MNBLAN(MJSCHA(ISTR),132)
      RETURN
*  EOF:
    1 ISTRRD=1
      RETURN
*  ERROR:
    2 ISTR='CST -- READ ERROR'
      J=MJSCHA(ISTR(19:19))
      N=10
      CALL CBIS(IE,J,N)
      CALL TISTR(ISTR(1:29-N))
      STOP
      END
