*
* $Id: pcpiaf.inc,v 1.3 1996/04/24 08:30:28 dinofm Exp $
*
* $Log: pcpiaf.inc,v $
* Revision 1.3  1996/04/24 08:30:28  dinofm
* The level of the PAW-PIAF PROTOCOL changed from 1 to 2 (this allows PIAF
* to detect whether the PAW client is an old one (version < 96a) or a new
* one (version >= 96a).
*
* Revision 1.2  1996/03/01 16:50:25  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCPIAF_INC
#define CERNLIB_PAW_PCPIAF_INC
*
*
* pcpiaf.inc
*
*CMZ :  2.04/12 11/02/94  18.09.44  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   01/05/93
*
*-- Update for every major incompatible change in Piaf client-server protocol.
*-- A change in VERSPF requires the building of a Piaf server with a new
*-- file extension.
*
      CHARACTER   VERSPF*8
      PARAMETER  (VERSPF = '931013')
*
*-- Update for every minor incompatible change in Piaf client-server protocol.
*
      INTEGER     IPROTO
      PARAMETER  (IPROTO = 2)

#endif
