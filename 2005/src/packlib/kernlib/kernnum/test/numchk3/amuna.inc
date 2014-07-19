*
* $Id: amuna.inc,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: amuna.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
*
* amuna.inc
*
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =  1, IDIM
             Z(J)  =  Z(J) - X(J) * Y(J)
  10         CONTINUE
          RETURN
          END
