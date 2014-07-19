/*
 * $Id: kuesvr.c,v 1.2 2005/04/18 15:23:10 mclareni Exp $
 *
 * $Log: kuesvr.c,v $
 * Revision 1.2  2005/04/18 15:23:10  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:05  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/09 06/07/94  17.33.53  by  Alfred Nathaniel*/
/*-- Author :*/

#include "kuip/kuip.h"

int   pid    = 0;
char *editor = "vi";
char *cmd    = '\0';
char *file   = '\0';
char *sfile  = '\0';


main(int argc, char **argv)
{
   int     c, fid;
   char   *arg;
   char    shcmd[512];
   char    msg[BUFSIZ];
   time_t  mtime;
   struct stat buf;

   /* scan options */
   while (--argc) {
      arg = *++argv;
      c = *arg;
      if (c == '-') {
         c = *++arg;
         switch (c) {
            case 'p':
               if (*++arg == 0)
                  arg = *++argv;
               pid = atoi(arg);
               break;
            case 'e':
               if (*++arg == 0)
                  arg = *++argv;
               editor = arg;
               break;
            case 'c':
               if (*++arg == 0)
                  arg = *++argv;
               cmd = arg;
               break;
	    case 't':
	       if (*++arg == 0)
		  arg = *++argv;
	       sfile = arg;
	       break;
            default:
               fprintf(stderr,"Usage: kuesvr -p pid -c cmd -t tmpfile [-e editor] file\n");
               exit(1);
         }
      } else {
         file = arg;
         break;
      }
   }

   if (!pid || !file || !cmd || !sfile) {
      fprintf(stderr,"Usage: kuesvr -p pid -c cmd -t tmpfile [-e editor] file\n");
      exit(1);
   }

   sprintf(shcmd, "%s %s", editor, file);

   stat(file, &buf);
   mtime = buf.st_mtime;

   system(shcmd);

   stat(file, &buf);

   /* open kuip edit server message file */
   fid = open(sfile, O_CREAT|O_APPEND|O_WRONLY, 0600);
   if (fid == -1) {
      perror("open");
      exit(1);
   }

   /* lock the file */
   if (lockf(fid, F_LOCK, (off_t)0) == -1) {
      perror("lockf");
      exit(1);
   }

   /* if file was not changed prepend a - to the filename */
   if (buf.st_mtime != mtime)
      sprintf(msg, "%s %s\n", file, cmd);
   else
      sprintf(msg, "-%s %s\n", file, cmd);

   write(fid, msg, strlen(msg));

   /* unlock the file */
   if (lockf(fid, F_ULOCK, (off_t)0) == -1) {
      perror("un-lockf");
      exit(1);
   }

   close(fid);

   /* signal the mother process (pid) that the editor terminated */
   kill(pid, SIGUSR1);
}
