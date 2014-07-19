*
* $Id: read_.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: read_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. **************************************
.
.     INTEGER FUNCTION  DATAC( A,$ )
.     ONE CARD IMAGE IS COPIED FROM FILE READ$ TO USER ARRAY A
.     FUNCTION VALUE = NUMBER OF WORDS RECEIVED
.     ARRAY  A  IS SUPPOSED TO BE AT LEAST 14 WORDS LONG
. *   $ = EOF RETURN.   *** THE STATUS IS COPIED INTO A(1),  AND
. *   THE CALLING ROUTINE MAY THEN CHECK THE NATURE OF THE EOF  ***
. *   FOR 'BLANK' CARDS ONE NORMALY RECEIVES DATAC=0
. *   STRIPS OFF THE FLAG FOR @ADDED CARD IMAGES
.
. **************************************
 
          AXR$
          FORT$
$(1)
DATAC*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,XARG
          L         A0,0,XARG         . LOC( A )
          LXI,U     A0,JEOF
 DO FOR , ER        READ$
 DO FTN , ER        AREAD$
          LXI,U     A0,0
          J         RETUR2
JEOF      DL        A2,0,A3           . LOC( A ),  LOC( $ )
          S         A0,0,A2           . A(1) = STATUS
          J         0,A3
          END
