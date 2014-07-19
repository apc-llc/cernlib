*
* $Id: tsysreq.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: tsysreq.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
      CHARACTER*240 COMMAND
      CHARACTER*80  IBUF
      CHARACTER*8   SERVICE
      INTEGER       IRC
      INTEGER       REPLEN
      PARAMETER     (REPLEN=100)
      CHARACTER*132  TMSREP(REPLEN)
 
      IRC = 0
 
      SERVICE = 'TMS'
      COMMAND = 'Q VID I25001 - I25404'
 
 500  CONTINUE
      I = REPLEN
      CALL SYSREQ(SERVICE,COMMAND, IRC, TMSREP, I)
 
      DO 20 J=1,I-1
      WRITE (6,200) TMSREP(J)
 200  FORMAT(1X,A80)
 20   CONTINUE
 
      IF (IRC .EQ. 2) THEN
      COMMAND = TMSREP(I)
      GOTO 500
      ENDIF
C       Print the Last Line
      WRITE (6,200) TMSREP(I)
 
 9999 CONTINUE
      PRINT *,'SYSREQ(Fortran): RC(',IRC,')'
 
      END
