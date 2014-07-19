*
* $Id: xypwz.F,v 1.1.1.1 1996/03/21 17:19:54 mclareni Exp $
*
* $Log: xypwz.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.43.59  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.27  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE XYPWZ(NW,IV,S,Y,X,T,W,Z)
      DIMENSION IV(*),X(*),Y(*),W(*),Z(*)
      DO I= 1, NW
          Y(IV(I)) = S*X(IV(I))*Y(IV(I))+T*W(IV(I))*Z(IV(I))
      ENDDO
      RETURN
      END
