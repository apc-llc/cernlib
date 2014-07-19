*
* $Id: fmlogi.F,v 1.1.1.1 1996/03/07 15:17:48 mclareni Exp $
*
* $Log: fmlogi.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:48  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if !defined(CERNLIB_IBMRT)
      SUBROUTINE FMLOGI(IRC)
#endif
#if defined(CERNLIB_IBMRT)
      SUBROUTINE FMLOGI_(IRC)
#endif
 
      EXEC SQL BEGIN DECLARE SECTION
#if defined(CERNLIB_ORACLE)
      CHARACTER*20 UID,PWD
*     SQL/DS complains if longer than 8 bytes...
#endif
#if defined(CERNLIB_SQLDS)
      CHARACTER*8  UID,PWD
#endif
 
      EXEC SQL END DECLARE SECTION
 
      EXEC SQL INCLUDE SQLCA
 
      UID = 'FATMEN'
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','PWD',PWD,IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_IBMRT))
      CALL GETENVF('PWD',PWD)
#endif
#if defined(CERNLIB_IBMRT)
      CALL GETENVF_('PWD',PWD)
#endif
 
      EXEC SQL WHENEVER SQLERROR GOTO 100
 
      EXEC SQL CONNECT :UID IDENTIFIED by :PWD
 
      IRC = 0
      WRITE(6,90) UID
90    FORMAT(/,' CONNECTED TO ORACLE/SQL AS USER: ' , A20,/)
 
      RETURN
 
100   CONTINUE
 
      WRITE(6,110)
110   FORMAT(/, ' Error connecting to ORACLE/SQL')
 
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CERNVM))
      CALL VMCMS('EXEC TELL FATONE Error connecting to ORACLE/SQL',IRC)
#endif
 
      EXEC SQL WHENEVER SQLERROR CONTINUE
      WRITE(6,1010) (SQLEMC(I),I=1,70)
1010  FORMAT(/,1X,70A1,/)
 
      EXEC SQL ROLLBACK WORK RELEASE
 
      IRC = 1
 
      RETURN
      END
