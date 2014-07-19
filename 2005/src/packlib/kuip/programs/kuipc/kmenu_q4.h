/*
 * $Id: kmenu_q4.h,v 1.3 1997/03/14 17:15:56 mclareni Exp $
 *
 * $Log: kmenu_q4.h,v $
 * Revision 1.3  1997/03/14 17:15:56  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:33:18  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1  1996/04/16 13:29:46  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */


"typedef struct _KmCommand {     /*					*/ \
  struct _KmCommand *next;	/* link to next command			*/ \
  char		*path;		/* command path				*/ \
  char		*name;		/* command name				*/ \
  int		hidden;		/* flag if command is invisible		*/ \
  int		level;		/* depth of submenus			*/ \
  int		total;		/* total number of parameters		*/ \
  int		mandatory;	/* number of mandatory parameters	*/ \
  KmParameter	**par;		/* list of total parameter descriptions	*/ \
  int		list_par;	/* index+1 of parameter taking a list	*/ \
  int		xcount;		/* count number of action calls		*/ \
  SUBROUTINE	*action_F;	/* action routine			*/ \
  IntFunc	*action_C;	/* action routine			*/ \
  SUBROUTINE	*user_help_F;	/* user help routine			*/ \
  pCharFunc	*user_help_C;	/* user help routine			*/ \
  int		nguidance;	/* number of lines in guidance text	*/ \
  char		**guidance;	/* help text				*/ \
  int		nkeyword;	/* number of lines for keywords		*/ \
  char		**keyword;	/* list of keywords			*/ \
  int		nhlink;		/* number of lines for links		*/ \
  char		**hlink;	/* list of links			*/ \
  int		argc;		/* number of arguments entered		*/ \
  char		**argv;		/* argc argument values			*/ \
  char		*argline;	/* argument line as entered		*/ \
  int		*argoffs;	/* argc offsets into argline for KUGETE	*/ \
} KmCommand;			/*					*/ \
"
