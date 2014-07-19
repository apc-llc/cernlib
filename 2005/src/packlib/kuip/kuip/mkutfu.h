/*
 * $Id: mkutfu.h,v 1.1.1.1 1996/03/08 15:33:01 mclareni Exp $
 *
 * $Log: mkutfu.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */
/*** Utility functions which do NOT refer any Motif/X11 include files ***/
#ifndef _mkutfu_
#define _mkutfu_
/* km_strempty : Returns true (1) if string is empty (only containing blanks) */
extern C_PROTO_1(int           km_strempty,
                                  char *);
/* km_strip : Strip blanks from both sides of a string */
extern C_PROTO_1(char         *km_strip,
                                  char *);
/* km_strend : Searches backwards for character c, ignoring ' ' and '\n' */
extern C_PROTO_2(char         *km_strend,
                                  char *,
                                  int);
extern C_PROTO_4(void          km_file_type,
                                  char *,
                                  char *,
                                  char *,
                                  int);
/* km_scan_dir : Scan directory using filter */
extern C_PROTO_4(char        **km_scan_dir,
                                  char *,
                                  char *,
                                  int,
                                  int);
extern C_PROTO_0(char         *km_get_br_filter);

extern C_PROTO_5(int           km_select,
                                  int type,
                                  char *message,
                                  int nb_ans,
                                  char **answ,
                                  int def_ans);

extern C_PROTO_2(char         *km_prompt,
                                  char *,
                                  char *);

extern C_PROTO_2(char         *km_prompt_answer,
                                  char *,
                                  char *);

extern C_PROTO_1(char         *km_passwd_prompt,
                                  char *);

/* km_palette : C callable interface for palette (command MULTI_PANEL) */
extern C_PROTO_2(void          km_palette,
                                  char *title,
                                  char *geometry);

#endif /* _mkutfu_ */

