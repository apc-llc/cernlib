*
* $Id: iylex.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: iylex.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.44.00  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.28  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE IYLEX(ILENG,IVEC,IX,IY,NFOUND)
      DIMENSION IVEC(*),IX(*),IY(*)
C
      NFOUND = 0
      DO J=1,ILENG
        IF(IVEC(J).LE.IX(J)) THEN
          NFOUND = NFOUND+1
          IY(NFOUND) = J
        END IF
      ENDDO
      RETURN
      END
