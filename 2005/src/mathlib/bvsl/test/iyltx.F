*
* $Id: iyltx.F,v 1.1.1.1 1996/03/21 17:19:54 mclareni Exp $
*
* $Log: iyltx.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.44.00  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.28  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE IYLTX(ILENG,IVEC,IX,IY,NFOUND)
      DIMENSION IVEC(*),IX(*),IY(*)
C
      NFOUND = 0
      DO J=1,ILENG
        IF(IVEC(J).LT.IX(J)) THEN
          NFOUND = NFOUND+1
          IY(NFOUND) = J
        END IF
      ENDDO
      RETURN
      END
