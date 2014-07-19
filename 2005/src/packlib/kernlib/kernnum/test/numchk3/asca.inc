*
* $Id: asca.inc,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: asca.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
*
* asca.inc
*
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =1, IDIM
             Z(J)  =  S*X(J) + Y(J)
  10         CONTINUE
          RETURN
          END
