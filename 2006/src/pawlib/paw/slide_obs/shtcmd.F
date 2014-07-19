*
* $Id: shtcmd.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: shtcmd.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 20/09/90  10.54.07  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE SHTCMD
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
*.______________________________________
*
      IPARK1=LSTCMD(JCD,1)
      IPARK2=LSTCMD(JCD,2)
      RPARK3=PARCMD(JCD,1)
      RPARK4=PARCMD(JCD,2)
      KK=JCD-1
   10 IF (KK.GE.1) THEN
         IF (LSTCMD(KK,1).GT.IPARK1) THEN
            LSTCMD(KK+1,1)=LSTCMD(KK,1)
            LSTCMD(KK+1,2)=LSTCMD(KK,2)
            PARCMD(KK+1,1)=PARCMD(KK,1)
            PARCMD(KK+1,2)=PARCMD(KK,2)
            KK=KK-1
            GO TO 10
         ELSE
            LSTCMD(KK+1,1)=IPARK1
            LSTCMD(KK+1,2)=IPARK2
            PARCMD(KK+1,1)=RPARK3
            PARCMD(KK+1,2)=RPARK4
         ENDIF
      ENDIF
      END
