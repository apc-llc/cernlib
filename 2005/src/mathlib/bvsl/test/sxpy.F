*
* $Id: sxpy.F,v 1.1.1.1 1996/03/21 17:19:54 mclareni Exp $
*
* $Log: sxpy.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.43.59  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.27  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE SXPY(NW,IV,Y,X,S)
      DIMENSION IV(*),X(*),Y(*)
      DO I= 1, NW
          Y(IV(I)) = S*X(IV(I))+Y(IV(I))
      ENDDO
      RETURN
      END
