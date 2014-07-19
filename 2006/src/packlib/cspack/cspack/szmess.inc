*
* $Id: szmess.inc,v 1.4 1996/12/12 16:47:43 jamie Exp $
*
* $Log: szmess.inc,v $
* Revision 1.4  1996/12/12 16:47:43  jamie
* CPP on IBM seems to pass C comments to Fortran?
*
* Revision 1.3  1996/11/18 07:32:51  jamie
* forgot #endif
*
* Revision 1.2  1996/11/15 08:07:09  jamie
* szmess must be includable multiple times
*
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
* #ifndef CERNLIB_CSPACK_SZMESS_INC
* #define CERNLIB_CSPACK_SZMESS_INC
*
*
* szmess.inc
*
*
*     Send message to client indicating success/failure
*
        LEND = INDEX(CHM(1:4),' ')
        IF(LEND.EQ.0) LEND = 4
        IF(IRC.EQ.0) THEN
           WRITE(CHMAIL,8001) CHM(1:LEND),IRC
        ELSEIF(IRC.GT.0) THEN
           WRITE(CHMAIL,8002) CHM(1:LEND),IRC
        ELSEIF(IRC.LT.0) THEN
           WRITE(CHMAIL,8003) CHM(1:LEND),IRC
        ENDIF
 
        CALL CZPUTA(CHMAIL,ISTAT)
*

* #endif
