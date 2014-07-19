*
* $Id: igchwk.F,v 1.5 1998/01/30 15:22:16 couet Exp $
*
* $Log: igchwk.F,v $
* Revision 1.5  1998/01/30 15:22:16  couet
* - APOLLO version removed
*
* Revision 1.4  1998/01/28 14:34:20  couet
* - GGDM driver removed
*
* Revision 1.3  1997/10/23 12:29:53  mclareni
* NT mods
*
* Revision 1.2  1996/09/25 14:57:18  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/03 23/08/93  11.16.51  by  O.Couet
*-- Author :    O.Couet   28/01/92
      SUBROUTINE IGCHWK(IWTYP,IX,IY,IW,IH)
*.===========>
*.
*.  This routine allows to change the workstation type parameters
*. in the file higz_windows.dat.
*.
*..==========>
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
      CHARACTER*64    UHOME
      CHARACTER*128   TNAME
      LOGICAL         INIT,OPND,EXIST1
      SAVE            INIT,LUHOME,UHOME
      CHARACTER*48    CHWKTY(10)
#if !defined(CERNLIB_KERNEL)||defined(CERNLIB_MSDOS)
      CHARACTER*80    FILEN
#endif
#if defined(CERNLIB_CRAY)
      INTEGER         GETENV
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CHARACTER*3 CHLUN
#endif
      DATA            INIT   /.FALSE./
      DATA            LUHOME /0/
      DATA            UHOME  /' '/
*.______________________________________
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
      LUNWIN=20
      DO 10 I=20,99
         INQUIRE(UNIT=I,OPENED=OPND)
         IF(.NOT.OPND)THEN
            LUNWIN=I
            OPND=.TRUE.
            GOTO 20
         ENDIF
   10 CONTINUE
#endif
#if (!defined(CERNLIB_KERNEL))&&(defined(CERNLIB_GKS)||defined(CERNLIB_DI3000))
      LUNWIN=0
      OPND=.FALSE.
#endif
*
      IF(.NOT.OPND)RETURN
*
#if defined(CERNLIB_IBM)
   20 FILEN='/HIGZWIN DATA A'
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_IPSC))
   20 FILEN='higz_windows.dat'
#endif
#if defined(CERNLIB_IPSC)
   20 FILEN='higz_windows'
#endif
#if !defined(CERNLIB_IBM)
*
      INQUIRE(FILE=FILEN,EXIST=EXIST1)
      IF(.NOT.EXIST1)THEN
*
         IF (.NOT.INIT) THEN
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
            UHOME=' '
            CALL GETENVF('HOME',UHOME)
#endif
#if defined(CERNLIB_VAX)
            UHOME='SYS$LOGIN:'
#endif
#if !defined(CERNLIB_IBM)
            LUHOME=LENOCC(UHOME)
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_IBM))
         IF (UHOME(LUHOME:LUHOME) .NE. '/' 
#ifdef CERNLIB_WINNT
     *       .OR. UHOME(LUHOME:LUHOME) .NE. '\'
#endif
     *                      ) THEN
              LUHOME=LUHOME+1
              UHOME(LUHOME:LUHOME)='/'
         ENDIF
#endif
#if !defined(CERNLIB_IBM)
            INIT=.TRUE.
         ENDIF
*
         TNAME = FILEN
         FILEN = UHOME(1:LUHOME)//TNAME
      ENDIF
#endif
*
   30 INQUIRE(FILE=FILEN,EXIST=EXIST1)
      IF(EXIST1)THEN
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED',STATUS='OLD'
     +,       IOSTAT=ISTA)
      ELSE
         RETURN
      ENDIF
*
      DO 40 I=1,10
         READ(LUNWIN,'(A)')CHWKTY(I)
   40 CONTINUE
      CLOSE(LUNWIN)
      OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED',STATUS='UNKNOWN'
     +, IOSTAT=ISTA)
      WRITE(CHWKTY(IWTYP), '(1X,I4.4,1X,I4.4,1X,I4.4,1X,I4.4)')IX,IY,
     +IW,IH
      DO 50 I=1,10
         WRITE(LUNWIN,'(A)',ERR=999)CHWKTY(I)
   50 CONTINUE
      CLOSE(LUNWIN)
*
  999 END
