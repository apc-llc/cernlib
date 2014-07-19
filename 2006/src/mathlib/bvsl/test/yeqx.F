*
* $Id: yeqx.F,v 1.1.1.1 1996/03/21 17:19:54 mclareni Exp $
*
* $Log: yeqx.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.43.59  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.27  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE YEQX(ILENG,VEC,X,IY,NFOUND)
      DIMENSION VEC(*),X(*),IY(*)
C
      NFOUND = 0
      DO J=1,ILENG
        IF(VEC(J).EQ.X(J)) THEN
          NFOUND = NFOUND+1
          IY(NFOUND) = J
        END IF
      ENDDO
      RETURN
      END
