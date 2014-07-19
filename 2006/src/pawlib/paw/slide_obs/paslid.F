*
* $Id: paslid.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: paslid.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
*CMZ :  1.10/00 20/09/90  10.54.06  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE PASLID
      CHARACTER*80 SLICMD
      CHARACTER*32 CHEXIT
*.______________________________________
*
      CALL KUAPPL(LUNAPP,MACFLG,CHEXIT)
      IF(LUNAPP.EQ.5)THEN
         CALL KUGETE(SLICMD,L)
         IF(L.GT.0)CALL SLIDE(SLICMD)
         RETURN
      ENDIF
*
   10 CONTINUE
      READ(LUNAPP,'(A)',ERR=20 ,END=30 )SLICMD
      IF (SLICMD.EQ.CHEXIT)THEN
         CALL SLIDE('<DRAW>')
         RETURN
      ENDIF
      CALL SLIDE(SLICMD)
      GOTO 10
   20 PRINT *,'*** PASLID: Error in reading'
      RETURN
   30 PRINT *,'*** PASLID: End of file'
      END
