*
* $Id: ikfile.F,v 1.4 1998/01/30 15:22:36 couet Exp $
*
* $Log: ikfile.F,v $
* Revision 1.4  1998/01/30 15:22:36  couet
* - APOLLO version removed
*
* Revision 1.3  1998/01/28 14:34:42  couet
* - GGDM driver removed
*
* Revision 1.2  1997/10/23 12:29:55  mclareni
* NT mods
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_PHIGS))&&(!defined(CERNLIB_MSDOS))
*CMZ :  1.23/03 01/09/95  12.03.27  by  O.Couet
*-- Author :    O.Couet   08/02/91
      SUBROUTINE IKFILE(IWTYP)
*.===========>
*.
*. Open the file higz_windows.dat :
*.
*.  1) in the current working directory
*.  2) in the HOME directory
*.  3) create this file in the home directory
*.     if it doesn't exits.
*.
*. and read in this file the informations concerning
*. the workstation type IWTYP
*.
*..==========>
#include "higz/hikern.inc"
      CHARACTER*64    UHOME
      CHARACTER*128   TNAME
      LOGICAL         INIT,OPND,EXIST1
      SAVE            INIT,LUHOME,UHOME
#if defined(CERNLIB_IBM)
      CHARACTER*24 CHREAD
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
      LUNWIN=20
      DO 10 I=20,99
         INQUIRE(UNIT=I,OPENED=OPND)
         IF(.NOT.OPND)THEN
            LUNWIN=I
            OPND=.TRUE.
            GOTO 20
         ENDIF
   10 CONTINUE
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
      WINSIZ(1)=0
      WINSIZ(2)=0
      WINSIZ(3)=600
      WINSIZ(4)=600
      IF(.NOT.OPND)RETURN
*
      INQUIRE(FILE=FILEN,EXIST=EXIST1)
      IF(EXIST1)THEN
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED',STATUS='OLD'
     +,       IOSTAT=ISTA)
         IF (ISTA.NE.0) THEN
            CALL IGERR('Cannot open higz_windows.dat','IKFILE')
            GOTO 999
         ELSE
            GOTO 50
         ENDIF
      ENDIF
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
#endif
*
   30 INQUIRE(FILE=FILEN,EXIST=EXIST1)
      IF(EXIST1)THEN
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED',STATUS='OLD'
     +,       IOSTAT=ISTA)
         IF (ISTA.NE.0) THEN
            CALL IGERR('Cannot open higz_windows.dat','IKFILE')
            GOTO 999
         ENDIF
      ELSE
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED',STATUS='NEW'
     +,       IOSTAT=ISTA)
         IF(ISTA.EQ.0)THEN
            DO 40 I=1,10
               WRITE(LUNWIN,'('' 0000 0000 0600 0600'')',ERR=999)
   40       CONTINUE
            CLOSE(LUNWIN)
            GOTO 30
         ELSE
            OPND=.FALSE.
         ENDIF
      ENDIF
*
   50 IF(OPND)THEN
         DO 60 I=1,IWTYP
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_IBM))
            READ(LUNWIN,'(4(1X,I4),1X,A)',ERR=999,END=999)
     +          (WINSIZ(J),J=1,4),CHHOST
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))
            READ(LUNWIN,'(4(1X,I4),1X,A)',ERR=999,END=999)
     +          (WINSIZ(J),J=1,4),CHREAD
            CHHOST=CHREAD
#endif
#if !defined(CERNLIB_X11)
            READ(LUNWIN,'(4(1X,I4))',ERR=999,END=999)
     +          (WINSIZ(J),J=1,4)
#endif
   60    CONTINUE
         CLOSE(LUNWIN)
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         WRITE(CHLUN,'(I3)')LUNWIN
         FILEN='FILEDEF '//CHLUN//'  CLEAR'
         CALL VMCMS(FILEN,ISTAT)
         FILEN='/HIGZWIN DATA A'
#endif
      ENDIF
*
  999 END
#endif
