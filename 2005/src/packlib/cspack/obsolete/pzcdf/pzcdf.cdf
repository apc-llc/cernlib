*
* $Id: pzcdf.cdf,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: pzcdf.cdf,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
*----------------------------------------------------------------
*
* PATTE Command definition file (CDF)
*
*----------------------------------------------------------------
>NAME PZCDF
>MENU PATTE
>Guidance
PATTE shell Commands
 
*----------------------------------------------------------------
>Command CONNECT
>Guidance
Use the CONNECT command to connect to PIAF
 
>Action PZCONN
>PARAMETERS
GROUP 'Group or Throng name' C D=' '
PASS  'Password            ' C D=' '
 
*----------------------------------------------------------------
>Command STAGE
>Guidance
Use the STAGE command to move a file to the PIAF
file system.
.
 
>Action PZCOPY
>PARAMETERS
LOCAL  'Local file name ' C D=' '
+
REMOTE 'Remote file name' C D=' '
CHOPT  'Options         ' C D=' '
