*
* $Id: adiv.inc,v 1.1.1.1 1996/02/15 17:48:43 mclareni Exp $
*
* $Log: adiv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
*
* adiv.inc
*
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =  1, IDIM
             Z(J)  =  X(J) / Y(J)
  10         CONTINUE
          RETURN
          END
