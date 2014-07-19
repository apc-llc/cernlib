*
* $Id: papiaf.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: papiaf.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          10/11/95  19.23.19  by  Timo Hakulinen
*-- Author :    Alfred Nathaniel   01/05/93
      SUBROUTINE PAPIAF
*
*           Send a special command to the Piaf server
*
#if defined(CERNLIB_CZ)
#include "hbook/hcpiaf.inc"
*
      CHARACTER CMD*16,ARGS*80,CHSRC*128,CHTRG*80,CHOPT*8,CHLCL*80
      INTEGER   ITMP
*
*=========================================================
*
      IF(.NOT.CONNPF) THEN
         PRINT *,' *** Piaf server not connected'
         RETURN
      ENDIF

      CALL PFSOCK(0)

      CALL KUPATL(CMD,NPAR)

      IF(CMD.EQ.'CAT' .OR. CMD.EQ.'CP' .OR. CMD.EQ.'LS'  .OR.
     +   CMD.EQ.'MV'  .OR. CMD.EQ.'RM' .OR. CMD.EQ.'PWD' .OR.
     +   CMD.EQ.'MKDIR' .OR. CMD.EQ.'RMDIR') THEN
*
* remote shell commands
*
         CALL CUTOL(CMD)
         IF (CMD.EQ.'pwd') THEN
            NCH  = 1
            ARGS = ' '
         ELSE
            CALL KUGETE(ARGS,NCH)
         ENDIF
         IF(CMD.EQ.'ls') THEN
            CMD='ls -lFL'
            IF(NCH.EQ.0) THEN
               NCH  = 1
               ARGS = ' '
            ENDIF
         ELSEIF(NCH.EQ.0) THEN
            PRINT *,' *** Remote command requires arguments'
            RETURN
         ENDIF
         L=LENOCC(CMD)+1
         IF(L+NCH.GT.74) THEN
            PRINT *,' *** Remote command line too long'
            RETURN
         ENDIF
         CALL CZPUTA('SHELL:'//CMD(:L)//ARGS(:NCH),ISTAT)
         CALL PFLOOP(ISTAT)

      ELSEIF(CMD.EQ.'MESSAGE') THEN
*
* back door for possible new commands
*
         CALL KUGETE(ARGS, NCH)
         CALL CZPUTA('MESS :'//ARGS, ISTAT)
         CALL PFLOOP(ISTAT)

      ELSEIF(CMD.EQ.'DISCONNECT') THEN
*
* close connection
*
         CALL PFCLOS

      ELSEIF(CMD.EQ.'LOGLEVEL') THEN
*
* set log level on client and server side
*
         CALL KUGETI(LOGLPF)
         WRITE(ARGS,'(I12)') LOGLPF
         CALL CZPUTA('LOGLEV'//ARGS,ISTAT)

      ELSEIF(CMD.EQ.'STATUS') THEN
*
* receive server status information
*
         CALL CZPUTA('STATUS',ISTAT)
         CALL PFLOOP(ISTAT)

      ELSEIF(CMD.EQ.'MODE') THEN
*
* set server mode
*
         CALL KUGETC(ARGS,NCH)
         NSLAV = 0
         IF (NPAR .GE. 2) THEN
            CALL KUGETI(NSLAV)
         ENDIF
         WRITE(CHTRG,'(I12)') NSLAV
         CALL CZPUTA('MODE :'//ARGS(1:3)//CHTRG,ISTAT)
   10    CALL CZGETA(ARGS,ISTAT)
         IF (ARGS(1:5) .NE. 'MODE*') THEN
            PRINT *, ARGS(1:LENOCC(ARGS))
            GOTO 10
         ENDIF
         CALL PFLOOP(ISTAT)

      ELSEIF(CMD.EQ.'STAGE') THEN
*
* submit a staging command
*
         CALL KUGETF(CHSRC,LSRC)
         CALL KUGETF(CHTRG,LTRG)
         CALL KUGETC(CHOPT,LOPT)
* use itmp to overcome a compiler bug on irix (wrong alignment of
* temporary storage allocated to the return value of index intrinsic
* when it's a direct parameter to a subroutine call)
         ITMP=INDEX(CHOPT,'L')
         CALL PFLOCL(CHSRC,CHLCL,ITMP,ISTAT)
         IF (ISTAT.NE.0) GOTO 99
         IF (CHLCL.NE.' '.AND.ITMP.EQ.0) CHOPT='L'//CHOPT
         CALL CZPUTA('STAGE:'//CHOPT//CHTRG,ISTAT)
         CALL CZPUTA(CHSRC,ISTAT)
         IF (CHLCL.NE.' ') CALL CZPUTA(CHLCL,ISTAT)
         CALL PFLOOP(ISTAT)

      ELSEIF(CMD.EQ.'GET') THEN
*
* get a file from the server
*
         CALL KUGETF(CHSRC,LSRC)
         CALL KUGETF(CHTRG,LTRG)
         CALL KUGETC(CHOPT,LOPT)
         CALL KUGETI(LRECL)
         IF(CHOPT.EQ.'RZ') THEN
            LRECL=0
            CALL PZGETB(CHSRC,CHTRG,LRECL,ISTAT)
         ELSEIF(CHOPT.EQ.'BIN') THEN
            IF(LRECL.EQ.0) THEN
               PRINT *, ' *** RECL needed for BIN transfer'
            ELSE
               CALL PZGETB(CHSRC,CHTRG,LRECL,ISTAT)
            ENDIF
         ELSEIF(CHOPT.EQ.'T') THEN
            CALL PZGETA(CHSRC,CHTRG,LRECL,ISTAT)
         ELSE
            PRINT *, ' *** Invalid file format '//CHOPT
         ENDIF

      ELSEIF(CMD.EQ.'PUT') THEN
*
* send a file to the server
*
         CALL KUGETF(CHSRC,LSRC)
         CALL KUGETF(CHTRG,LTRG)
         CALL KUGETC(CHOPT,LOPT)
         LRECL=0
         IF(CHOPT.EQ.'RZ') THEN
            CALL PZPUTB(CHSRC,CHTRG,LRECL,ISTAT)
         ELSEIF(CHOPT.EQ.'BIN') THEN
            CALL PZPUTB(CHSRC,CHTRG,LRECL,ISTAT)
         ELSEIF(CHOPT.EQ.'T') THEN
            CALL PZPUTA(CHSRC,CHTRG,LRECL,ISTAT)
         ELSE
            PRINT *, ' *** Invalid file format '//CHOPT
         ENDIF

      ELSE
         PRINT *,'PAPIAF: '//CMD//' ?? '
      ENDIF
*
#endif
#if !defined(CERNLIB_CZ)
      PRINT *,' PAW not compiled with communication option'
#endif
*
  99  END
