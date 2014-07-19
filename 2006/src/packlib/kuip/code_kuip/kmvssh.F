*
* $Id: kmvssh.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kmvssh.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.00/16 05/02/93  15.42.20  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   16/10/92
      SUBROUTINE KMVSSH(CMD)
      CHARACTER*(*) CMD
*
*  execute a shell command on MVS
*  if CMD.eq.' ' envoke interactive shell
*
#if !defined(CERNLIB_NEWLIB)
*  this is a merging of code from KXSHEL and MVSTSO by Michael Dahlinger
      CHARACTER*261  COMMAND
      CHARACTER*16   PARA

      DATA           PARA   / 'PANEL(ISR@PRIM)' /
      DATA           NCPARA / 15 /

      L=LENOCC(CMD)
      IF (L.GT.0) THEN
C======================================================================|
C  execute TSO-commands via call to ISPLINK
C  can only be exectued inside ISPF environment
C  M.D. 25/4/91                                                        |
C======================================================================|
         CALL ISPLINK('CONTROL  ','ERRORS  ','RETURN  ')
         COMMAND = 'CMD('//CMD//')'
         lcmd1=LENOCC(COMMAND)
C -- get the return code also
         IRET= ISPLNK('SELECT  ',lcmd1,COMMAND)

      ELSE

*--       IBMMVS = CALL THE ISPF/PDF PRIMARY OPTION MENU
*--               (PANEL NAME = ISR@PRIM)
*--               NO COMMAND IS TRANSMITTED

*--       WARNING = ROUTINE ISPLNK IS FOUND IN LIBRARY:
*--            ISP.V2R2M0.ISPLOAD (STRASBOURG NAME...) WHICH MUST
*--            BE ADDED AT THE LINK LEVEL
* return after occurence of error
         ISTAT = ISPLNK ( 'CONTROL' , 'ERRORS' , 'RETURN' )
         IF ( ISTAT .NE. 0 ) THEN
            PRINT *, '*** SYSTEM FAILURE IN CALLING THE ISPF/PDF'
     +      , ' CONTROL ERROR RETURN, RC= ', ISTAT
            RETURN
         ENDIF

         ISTAT = ISPLNK ( 'SELECT', NCPARA, PARA )
         IF ( ISTAT .NE. 0 .AND. ISTAT .NE. 4 ) THEN
            PRINT *, '*** SYSTEM FAILURE IN CALLING THE ISPF/PDF'
     +      , ' PRIMARY OPTION MENU, RC= : ', ISTAT
            RETURN
         ENDIF
* reset into line mode
         ISTAT = ISPLNK ( 'CONTROL' , 'DISPLAY' , 'LINE' , 1)
         IF ( ISTAT .NE. 0 ) THEN
            PRINT *, '*** SYSTEM FAILURE IN CALLING THE ISPF/PDF'
     +      , ' CONTROL DISPLAY LINE 1, RC= ', ISTAT
            RETURN
         ENDIF
      ENDIF

#endif
#if defined(CERNLIB_NEWLIB)
      CHARACTER*80   MVSCMD(2)
      CHARACTER*80   VARIABLE
      CHARACTER*80   VALUE
      INTEGER        NCMDL
      INTEGER        IRET
      INTEGER        SPACE(2)

*     Test for enough memory for shell
      CALL $SPACEM(SPACE(1),SPACE(2))
      IF (SPACE(2) .LT. 200 000) THEN
         PRINT *, '***ERROR*** Not enough storage available for SHELL'
     +,           '. Command rejected'
         RETURN
      ENDIF
      NCMDL=1
      L=LENOCC(CMD)
      IF (L.GT.0) THEN
         I=INDEX(CMD,' ')
         IF (I.EQ.0) I=L
         IF (CMD(L:L) .EQ. '\' ) THEN
            CMD(L:L)=' '
            MVSCMD(2)='EXIT'
            NCMDL=2
         ENDIF
         PRINT *,'EXECUTING ... ',CMD(1:L)
         MVSCMD( 1)=CMD
      ELSE
         MVSCMD( 1)='?'
         VARIABLE='SAVE_MEMBER'
         CALL VARGET(VARIABLE,VALUE,IRET)
         IF (IRET.EQ.0 .AND. VALUE .NE. ' ')
     +      MVSCMD(1)='CHANGE '//VALUE
      ENDIF
*--
*--   NEWLIB is the DESY shell
*--
      CALL NEWLIB(MVSCMD,NCMDL)

#endif

      END
#endif
